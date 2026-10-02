use Test;

# Assignment to a subscript across the RakuAST boundary (ADR-10723 Stage 1).
# Measured on rakudo 2026.09: an assignment to `@a[…]` (or `%h<…>`) is folded
# into the postcircumfix as its `assignee`, while `%h{…}` keeps an
# `Assignment` infix over the subscript. This file passes under both mutsu
# and raku.

plan 14;

{
    my $e = Q[my @a; @a[0] = 2].AST.statements[1].expression;
    isa-ok $e, RakuAST::ApplyPostfix, '@a[0] = 2 is an ApplyPostfix';
    isa-ok $e.postfix, RakuAST::Postcircumfix::ArrayIndex, 'over an ArrayIndex';
    isa-ok $e.postfix.assignee, RakuAST::IntLiteral, 'whose assignee is the value';
    like $e.gist, /'assignee => RakuAST::IntLiteral.new(2)'/, 'the assignee renders';
}

{
    my $e = Q[my @a; @a[0][1] = 7].AST.statements[1].expression;
    isa-ok $e.operand, RakuAST::ApplyPostfix, 'a nested write subscripts a subscript';
    nok $e.operand.postfix.assignee, 'only the outermost subscript carries the assignee';
}

{
    my $e = Q[my %h; %h{"k"} = 1].AST.statements[1].expression;
    isa-ok $e, RakuAST::ApplyInfix, '%h{"k"} = 1 stays an ApplyInfix';
    isa-ok $e.infix, RakuAST::Assignment, 'with an Assignment infix';
    isa-ok $e.left.postfix, RakuAST::Postcircumfix::HashIndex, 'over a HashIndex';
}

# Write direction.
{
    my @a;
    EVAL Q[@a[0] = 2; @a[2] = 4].AST;
    is @a.raku, '[2, Any, 4]', 'positional assignments round-trip';
}

{
    my %h;
    EVAL Q[%h{"a"} = 1; %h{"b"}{"c"} = 2].AST;
    is %h<a>, 1, 'an associative assignment round-trips';
    is %h<b><c>, 2, 'and autovivifies an intermediate Hash';
}

{
    my @a;
    EVAL Q[@a[1][0] = "x"].AST;
    is @a[1][0], 'x', 'a nested positional write autovivifies an Array';
}

is EVAL(Q[my @a; my $r = (@a[0] = 5); $r].AST), 5, 'the assignment is an expression that yields the value';
