use Test;

# A variable declaration's `is TYPE` and `is dynamic` traits in RakuAST,
# measured on rakudo 2026.09. `is NAME` naming a type is a container type,
# `Trait::Is(type => Type::Simple)`; `is dynamic` is a named trait and leaves
# the variable without a `*` twigil.

plan 12;

sub decl($src) { $src.AST.statements.head.expression }

my $set = decl(Q[my %h is SetHash]);
isa-ok $set.traits.head, RakuAST::Trait::Is, '`is SetHash` is a Trait::Is';
isa-ok $set.traits.head.type, RakuAST::Type::Simple, 'holding a Type::Simple';
ok $set.traits.head.type.raku.contains('from-identifier("SetHash")'), 'naming the container type';

my $own-decl = Q[class Foo is Hash { }; my %f is Foo].AST.statements[1].expression;
isa-ok $own-decl.traits.head.type, RakuAST::Type::Simple, 'a type the unit declares is a container type too';

my $dyn = decl(Q[my $x is dynamic]);
ok $dyn.traits.head.raku.contains('name => RakuAST::Name.from-identifier("dynamic")'),
    '`is dynamic` is a named trait';
nok $dyn.raku.contains('twigil'), 'which leaves the variable without a twigil';

is EVAL(Q[my %h is SetHash = <a b>; %h<c>++; %h.^name ~ ' ' ~ %h.elems].AST),
    'SetHash 3', 'a SetHash container survives the round trip';
is EVAL(Q[my %b is BagHash = <a a b>; %b<a>].AST),
    2, 'so does a BagHash';
is EVAL(Q[my @a is List = 1, 2; @a.^name].AST),
    'List', 'and a List';
is EVAL(Q[my %m is Map = a => 1; %m.^name].AST),
    'Map', 'and a Map';
is EVAL(Q[class Foo2 is Hash { }; my %f is Foo2; %f<k> = 1; %f.^name].AST),
    'Foo2', 'and a container class the unit declares';
is EVAL(Q[my $x is dynamic = 5; $x + 1].AST),
    6, '`is dynamic` survives it';
