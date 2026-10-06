use Test;

# The traits and the shape of a declaration in RakuAST, measured on rakudo
# 2026.09:
#
# - a grammar's `is Base` / `does R` are `Trait::Is(type => ...)` /
#   `Trait::Does` in its `traits`, the implicit `Grammar` parent no trait;
# - `my $x is marked(5)` is a `Trait::Is(name => marked, argument => (5))`;
#   a class's `is labelled(5)` is the same; `is SetHash` (a type) is
#   `Trait::Is(type => ...)`;
# - `my @a[2;3] = ...` is a `VarDeclaration::Simple` with a `shape` (one
#   statement per dimension) and the data as its initializer;
# - an enum's and an augmented class's `does R` is a `Trait::Does`;
# - a proto's `--> Str` is its signature's `returns`.
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 35;

sub exprs($src) { $src.AST.statements.map(*.expression) }
sub same($src, $expected, $desc) {
    my $parsed = EVAL($src);
    my $round = EVAL($src.AST);
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- grammar parents and roles
{
    my @g = exprs(Q[grammar GB1 { token t { a } }; grammar GD1 is GB1 { token u { b } }]);
    nok @g[0].traits.elems, 'a grammar with no parent has no traits';
    is @g[1].traits.elems, 1, '`grammar G is B` has one trait';
    isa-ok @g[1].traits[0], RakuAST::Trait::Is, 'a Trait::Is';
    isa-ok @g[1].traits[0].type, RakuAST::Type::Simple, 'over the parent type';
    my @r = exprs(Q[role GR1 { }; grammar GD2 does GR1 { token t { a } }]);
    isa-ok @r[1].traits[0], RakuAST::Trait::Does, '`grammar G does R` has a Trait::Does';
    nok @r[1].traits.elems > 1, 'and not an `is Grammar` trait';
}

# --- a variable's own traits
{
    my @v = exprs(Q[multi sub trait_mod:<is>(Variable $v, :$marked!) { }; my $x is marked; my $y is marked(5) = 1; my %h is marked(1, 2)]);
    isa-ok @v[1], RakuAST::VarDeclaration::Simple, '`my $x is marked` is a declaration';
    is @v[1].traits[0].name.canonicalize, 'marked', 'with a Trait::Is by name';
    nok @v[1].traits[0].argument.defined, 'bare';
    isa-ok @v[2].traits[0].argument, RakuAST::Circumfix::Parentheses, '`is marked(5)` has an argument';
    isa-ok @v[2].initializer, RakuAST::Initializer::Assign, 'and an initializer after the traits';
    like @v[3].traits[0].argument.gist, /'ApplyListInfix'/, 'a list of arguments is a comma list';
    my @t = exprs(Q[my %s is SetHash]);
    isa-ok @t[0].traits[0].type, RakuAST::Type::Simple, '`is SetHash` is a container type, not a name';
}

# --- a class's own traits
{
    my @c = exprs(Q[multi sub trait_mod:<is>(Mu:U $c, :$labelled!) { }; class CL1 is labelled(5) { }; class CL2 is labelled { }]);
    is @c[1].traits[0].name.canonicalize, 'labelled', '`is labelled(5)` is a Trait::Is by name';
    isa-ok @c[1].traits[0].argument, RakuAST::Circumfix::Parentheses, 'with its argument';
    is @c[2].traits[0].name.canonicalize, 'labelled', 'a bare `is labelled` is by name too';
    nok @c[2].traits[0].type.defined, 'not a parent type';
}

# --- shaped arrays
{
    my @a = exprs(Q[my @a1[3]; my @a2[2;3]; my @a3[3] = 1, 2, 3; my Int @a4[2]]);
    isa-ok @a[0].shape, RakuAST::SemiList, '`my @a[3]` has a shape';
    is @a[0].shape.statements.elems, 1, 'of one dimension';
    is @a[1].shape.statements.elems, 2, '`my @a[2;3]` has two dimensions';
    isa-ok @a[2].initializer, RakuAST::Initializer::Assign, 'an initialized one keeps its initializer';
    isa-ok @a[2].initializer.expression, RakuAST::ApplyListInfix, 'with the data, not the shape wrapper';
    isa-ok @a[3].type, RakuAST::Type::Simple, '`my Int @a[2]` keeps its type';
}

# --- does-roles of an enum and an augment
{
    my @e = exprs(Q[role ER1 { }; enum EE1 does ER1 <a b>]);
    isa-ok @e[1].traits[0], RakuAST::Trait::Does, '`enum E does R` has a Trait::Does';
    my @a = Q[role AR1 { }; use MONKEY-TYPING; augment class Str does AR1 { }].AST.statements;
    is @a[2].expression.scope, 'augment', '`augment class` has the augment scope';
    isa-ok @a[2].expression.traits[0], RakuAST::Trait::Does, 'and its role as a Trait::Does';
}

# --- a proto's return type
{
    my @p = exprs(Q[proto sub pf(Str $s --> Int) {*}]);
    is @p[0].multiness, 'proto', 'a proto with a return type';
    like @p[0].gist, /'returns' .* 'Type::Simple'/, 'has it in its signature';
}

# --- the round trip is the parsed program
same Q[grammar GB3 { token TOP { <x> }; token x { a } }; grammar GD3 is GB3 { token x { b } }; (GD3.parse("b") ?? "y" !! "n")], 'y',
    'a grammar inherits from its written parent';
same Q[role GR4 { method hello { "hi" } }; grammar GD4 does GR4 { token TOP { a } }; GD4.hello], 'hi', 'a grammar composes a role';
same Q[my @a[3] = 1, 2, 3; @a.join(",")], '1,2,3', 'a shaped array with data';
same Q[my @a[2]; @a.shape.raku], '(2,)', 'and its shape';
same Q[role ER5 { method hi { "hi" } }; enum EE5 does ER5 <a b>; EE5::a.hi], 'hi', 'an enum that does a role';
same Q[role AR6 { method rot { self.flip } }; use MONKEY-TYPING; augment class Str does AR6 { }; "abc".rot], 'cba', 'an augment that does a role';
same Q[proto sub pf7(Str $s --> Int) {*}; multi sub pf7(Str $s --> Int) { $s.chars }; pf7("abc")], 3, 'a proto with a return type';
