use Test;

# #14916 (accessor survey): `RakuAST::Name`'s questions about its parts.
# Answer types are rakudo 2026.09's: nqp comparisons answer `0` / `1`,
# `is-simple` and `has-colonpairs` a Bool.

plan 27;

my $foo = RakuAST::Name.from-identifier('foo');
my $multi = RakuAST::Name.from-identifier-parts('Foo', 'Bar', 'baz');
my $global = RakuAST::Name.from-identifier-parts('GLOBAL', 'x');
my $core = RakuAST::Name.from-identifier-parts('CORE', 'x');
my $parent = RakuAST::Name.from-identifier-parts('PARENT', 'x');
my $anon = RakuAST::Name.new(RakuAST::Name::Part::Empty.new, RakuAST::Name::Part::Empty.new);
my $pkg = RakuAST::Name.new(RakuAST::Name::Part::Simple.new('Foo'), RakuAST::Name::Part::Empty.new);

is $foo.is-identifier, 1, 'a lone simple part is an identifier';
is $multi.is-identifier, 0, 'a qualified name is not';
is $anon.is-identifier, 0, 'neither is an anonymous one';

is-deeply $foo.is-simple, True, 'a simple name is simple';
is-deeply $pkg.is-simple, False, 'a name ending in an empty part is not';

is $foo.is-multi-part, 0, 'one part is not multi-part';
is $multi.is-multi-part, 1, 'three parts are';
is $anon.is-multi-part, 0, 'the anonymous name is not';

is $global.is-global-lookup, 1, 'GLOBAL::x is a global lookup';
is $multi.is-global-lookup, 0, 'Foo::Bar::baz is not';

is $pkg.is-package-lookup, 1, 'Foo:: is a package lookup';
is $foo.is-package-lookup, 0, 'foo is not';

is $core.is-pseudo-package, 1, 'CORE::x starts with a pseudo-package';
is $parent.is-pseudo-package, 0, 'PARENT is not one';
is $global.is-pseudo-package, 0, 'GLOBAL is not one';
is RakuAST::Name.from-identifier-parts('OUR', 'x').is-pseudo-package, 1, 'OUR is';
is $anon.is-pseudo-package, 1, 'the anonymous name is';

is $foo.is-installable, 1, 'a plain name is installable';
is $anon.is-installable, 0, 'the anonymous name is not';
is-deeply $foo.has-colonpairs, False, 'a name without colonpairs';

is $multi.first-part.raku, 'RakuAST::Name::Part::Simple.new("Foo")', 'first-part';
is $multi.last-part.raku, 'RakuAST::Name::Part::Simple.new("baz")', 'last-part';
is $multi.root-part.raku, 'RakuAST::Name::Part::Simple.new("Foo")', 'root-part';
is $foo.first-part.raku, $foo.last-part.raku, 'one part is first and last';

# A name parsed from source answers the same.
my $parsed = 'Foo::Bar::baz()'.AST.statements[0].expression.name;
is $parsed.is-multi-part, 1, 'a parsed qualified name is multi-part';
is $parsed.last-part.raku, 'RakuAST::Name::Part::Simple.new("baz")', 'its last part';
is-deeply 'say(1)'.AST.statements[0].expression.name.is-simple, True, 'a parsed simple name';
