use Test;

# A variable declaration's `is default(EXPR)` trait and a typed declaration
# across the RakuAST boundary (ADR-10723 Stage 1). Measured on rakudo
# 2026.09: the trait is `Trait::Is(name => …"default", argument =>
# Circumfix::Parentheses(…))` in `traits`, ahead of the `initializer`. This
# file passes under both mutsu and raku.

plan 11;

sub decl(Str $src) { $src.AST.statements[0].expression }

{
    my $d = decl(Q[my $x is default(3) = 5]);
    isa-ok $d.traits[0], RakuAST::Trait::Is, 'is default is a Trait::Is';
    like $d.traits[0].gist, /'from-identifier("default")'/, 'named default';
    like $d.traits[0].gist, /'argument => RakuAST::Circumfix::Parentheses.new('/,
        'with a parenthesized argument';
    like $d.gist, /'traits      => (' .* 'initializer => '/, 'ahead of the initializer';
}

is decl(Q[my $y]).traits.elems, 0, 'a plain declaration has no traits';

# Write direction.
is EVAL(Q[my $x is default(3); $x].AST), 3, 'is default round-trips';
is EVAL(Q[my $x is default(3) = 5; $x = Nil; $x].AST), 3,
    'and restores the default when Nil is assigned';
is EVAL(Q[my $x is default(3) = 5; $x].AST), 5, 'an initializer still wins';
is EVAL(Q[my @a is default(7); @a[2]].AST), 7, 'an array element default';
is EVAL(Q[my Int $n = 4; $n].AST), 4, 'a typed declaration round-trips';
throws-like { EVAL Q[my Int $n = 4; $n = "x"].AST }, X::TypeCheck::Assignment,
    'and keeps its type constraint';
