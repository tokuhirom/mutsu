use v6;
use Test;

# `.=` is a mutating method call at method-postfix / dotty-infix precedence
# (operators.rakudoc), far TIGHTER than the conditional `?? !!`, so it is legal
# unparenthesized inside a ternary branch. mutsu used to reject it with
# `X::Syntax::ConditionalOperator::PrecedenceTooLoose`, because the parser lowers
# `$v .= uc` to the same `AssignExpr` shape as the genuinely-too-loose `$v = ...`.
#
# The loose assignment operators must keep being rejected.

plan 10;

{
    my $v = 'a';
    is (1 ?? $v.=uc !! 9), 'A', 'a tight `.=` in the then-branch is accepted';
    is $v, 'A', 'and it mutated the variable';
}

{
    my $v = 'a';
    is (1 ?? $v .= uc !! 9), 'A', 'the spaced dotty-infix form works too';
}

{
    my $v = 'a';
    is (0 ?? 9 !! $v.=uc), 'A', 'and in the else-branch';
}

{
    # A list-assignment context reaches the guard through a different parse path.
    my $v = 'a';
    my @z = 1 ?? $v.=uc !! 9;
    is-deeply @z, ['A'], 'a `.=` branch inside a list assignment is accepted';
}

{
    my $v = 'ab';
    is (1 ?? $v.=uc.=lc !! 9), 'ab', 'a chained `.=` in a branch works';
}

{
    my %h = k => 'a';
    is (1 ?? %h<k>.=uc !! 9), 'A', 'a `.=` on a hash element works';
    my @a = 'x',;
    is (1 ?? @a[0].=uc !! 9), 'X', 'a `.=` on an array element works';
}

# The loose operators are still rejected.
throws-like 'my $a = 0; 1 ?? $a = 5 !! 6',
    X::Syntax::ConditionalOperator::PrecedenceTooLoose,
    'a bare `=` in a ternary branch is still rejected';

throws-like 'my $a = 0; 1 ?? $a += 5 !! 6',
    X::Syntax::ConditionalOperator::PrecedenceTooLoose,
    'a compound assignment in a ternary branch is still rejected';
