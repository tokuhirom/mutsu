use Test;
use experimental :rakuast;

plan 16;

# `RakuAST::Name`'s colonpair queries and modifier, `is-package-search`,
# `base-name` and the `canonicalize` of an indirect name, as
# raku-doc/doc/Type/RakuAST/Name.rakudoc documents them and rakudo 2026.11
# answers (#18976).

my $n = RakuAST::Name.new(
    RakuAST::Name::Part::Simple.new("Foo"),
    colonpairs => (RakuAST::ColonPair::Value.new(key => "ver", value => RakuAST::StrLiteral.new("1.1")),),
);
is-deeply $n.has-colonpair("ver"), True, 'has-colonpair: present';
is-deeply $n.has-colonpair("auth"), False, 'has-colonpair: absent';
is $n.first-colonpair("ver").DEPARSE, ':ver("1.1")', 'first-colonpair';
is-deeply $n.first-colonpair("auth"), Nil, 'first-colonpair of a missing key is Nil';
is $n.without-colonpair("ver").DEPARSE, 'Foo', 'without-colonpair';
is $n.DEPARSE, 'Foo:ver("1.1")', '... leaves the name itself alone';

my $f = RakuAST::Name.from-identifier("Foo");
my $pair = RakuAST::ColonPair::Value.new(key => "ver", value => RakuAST::StrLiteral.new("1.1"));
is $f.add-colonpair($pair).key, 'ver', 'add-colonpair answers the pair';
is $f.DEPARSE, 'Foo:ver("1.1")', '... and adds it to the name';

is-deeply 'Foo::'.AST.statements[0].expression.name.is-package-search, False,
    'Foo:: is not a package search';
is-deeply 'my $x; ::($x)'.AST.statements[1].expression.name.is-package-search, True,
    '::($x) is';
is 'my $x; ::($x)::Foo'.AST.statements[1].expression.name.canonicalize, '($x)::Foo',
    'canonicalize of an indirect name';
is 'Foo::'.AST.statements[0].expression.name.base-name.DEPARSE, 'Foo',
    'base-name drops the trailing ::';
is $n.base-name.DEPARSE, 'Foo:ver("1.1")', '... and keeps the colonpairs';

my $p = RakuAST::Name.from-identifier-parts('IO', 'Path');
is-deeply ($p.is-multi-part, $p.is-simple, $p.is-identifier, $p.is-empty),
    (True, True, False, False), 'the predicates answer Bools';
is-deeply $p.simple-identifier, '', 'simple-identifier of a multi-part name';
is-deeply RakuAST::Name.from-identifier("a").is-indirect-lookup, False,
    'is-indirect-lookup of a plain name';
