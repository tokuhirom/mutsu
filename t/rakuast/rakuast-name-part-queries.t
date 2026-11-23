use Test;

# #14916 (accessor survey): `RakuAST::Name`'s questions about its parts.
# Every predicate answers a Bool, as on rakudo 2026.11 (#18976).

plan 27;

my $foo = RakuAST::Name.from-identifier('foo');
my $multi = RakuAST::Name.from-identifier-parts('Foo', 'Bar', 'baz');
my $global = RakuAST::Name.from-identifier-parts('GLOBAL', 'x');
my $core = RakuAST::Name.from-identifier-parts('CORE', 'x');
my $parent = RakuAST::Name.from-identifier-parts('PARENT', 'x');
my $anon = RakuAST::Name.new(RakuAST::Name::Part::EmptyEdge.new, RakuAST::Name::Part::EmptyEdge.new);
my $pkg = RakuAST::Name.new(RakuAST::Name::Part::Simple.new('Foo'), RakuAST::Name::Part::EmptyEdge.new);

is-deeply $foo.is-identifier, True, 'a lone simple part is an identifier';
is-deeply $multi.is-identifier, False, 'a qualified name is not';
is-deeply $anon.is-identifier, False, 'neither is an anonymous one';

is-deeply $foo.is-simple, True, 'a simple name is simple';
is-deeply $pkg.is-simple, False, 'a name ending in an empty part is not';

is-deeply $foo.is-multi-part, False, 'one part is not multi-part';
is-deeply $multi.is-multi-part, True, 'three parts are';
is-deeply $anon.is-multi-part, False, 'the anonymous name is not';

is-deeply $global.is-global-lookup, True, 'GLOBAL::x is a global lookup';
is-deeply $multi.is-global-lookup, False, 'Foo::Bar::baz is not';

is-deeply $pkg.is-package-lookup, True, 'Foo:: is a package lookup';
is-deeply $foo.is-package-lookup, False, 'foo is not';

is-deeply $core.is-pseudo-package, True, 'CORE::x starts with a pseudo-package';
is-deeply $parent.is-pseudo-package, False, 'PARENT is not one';
is-deeply $global.is-pseudo-package, False, 'GLOBAL is not one';
is-deeply RakuAST::Name.from-identifier-parts('OUR', 'x').is-pseudo-package, True, 'OUR is';
is-deeply $anon.is-pseudo-package, True, 'the anonymous name is';

is-deeply $foo.is-installable, True, 'a plain name is installable';
is-deeply $anon.is-installable, False, 'the anonymous name is not';
is-deeply $foo.has-colonpairs, False, 'a name without colonpairs';

is $multi.first-part.raku, 'RakuAST::Name::Part::Simple.new("Foo")', 'first-part';
is $multi.last-part.raku, 'RakuAST::Name::Part::Simple.new("baz")', 'last-part';
is $multi.root-part.raku, 'RakuAST::Name::Part::Simple.new("Foo")', 'root-part';
is $foo.first-part.raku, $foo.last-part.raku, 'one part is first and last';

# A name parsed from source answers the same.
my $parsed = 'Foo::Bar::baz()'.AST.statements[0].expression.name;
is-deeply $parsed.is-multi-part, True, 'a parsed qualified name is multi-part';
is $parsed.last-part.raku, 'RakuAST::Name::Part::Simple.new("baz")', 'its last part';
is-deeply 'say(1)'.AST.statements[0].expression.name.is-simple, True, 'a parsed simple name';
