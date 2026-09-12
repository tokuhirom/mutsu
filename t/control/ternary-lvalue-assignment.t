use v6;
use Test;

plan 16;

# `?? !!` sits at ITEM ASSIGNMENT precedence and is right-associative, so an
# assignment written after the else branch takes the WHOLE conditional as its
# lvalue -- it does not nest inside that branch. Every assertion below was
# checked against rakudo itself.

# --- plain `=`, statement position ------------------------------------------
{
    my $x = 5;
    my $y = 7;
    my $r = 1 ?? $y !! $x = 3;
    is $r, 3, 'the trailing = returns the assigned value';
    is $x, 5, 'the UNSELECTED branch is not written';
    is $y, 3, 'the selected (then) branch is what got assigned';
}

{
    my $x = 5;
    my $y = 7;
    my $r = 0 ?? $y !! $x = 3;
    is $r, 3, 'a false condition still returns the assigned value';
    is $x, 3, 'the selected (else) branch is what got assigned';
    is $y, 7, 'the unselected then branch is untouched';
}

# --- inside parentheses ------------------------------------------------------
{
    my $x = 5;
    my $y = 7;
    my $r = (1 ?? $y !! $x = 3);
    is "$r $x $y", '3 5 3', 'a parenthesized group reads the same way';
}

# --- compound assignment operators ------------------------------------------
{
    my $x = 5;
    my $y = 7;
    my $r = (1 ?? $y !! $x += 3);
    is "$r $x $y", '10 5 10', '+= writes through the selected branch';
}

{
    my $x = 5;
    my $y = 7;
    my $r = (0 ?? $y !! $x += 3);
    is "$r $x $y", '8 8 7', '+= on a false condition writes the else branch';
}

# `//=` is the shape Pop's `@!keyboard[ $scancode ?? $key !! %cache{$key} //=
# ... ]` uses: the conditional picks the container, the compound assignment
# fills it in only when it is not already there.
{
    my %cache;
    my $r = 0 ?? 1 !! %cache<k> //= 2;
    is $r, 2, '//= fills in the undefined element the else branch selected';
    is %cache<k>, 2, 'the hash element really was written';
}

{
    my %cache;
    my $r = 1 ?? 3 !! %cache<k> //= 4;
    is $r, 3, '//= leaves a defined selected branch alone';
    nok %cache<k>.defined, 'the unselected element stays untouched';
}

# --- as a subscript ----------------------------------------------------------
{
    my @a = 0 .. 9;
    my %cache;
    is @a[ 0 ?? 1 !! %cache<k> //= 4 ], 4, 'the whole form works as a subscript';
}

# --- what is still an error --------------------------------------------------
# An assignment BETWEEN `??` and `!!` really is too loose, and rakudo rejects it.
{
    my $died = False;
    try {
        EVAL 'my $a = 5; my $r = 1 ?? $a = 9 !! 2;';
        CATCH { default { $died = True } }
    }
    ok $died, 'an assignment in the THEN branch is still refused';
}

# `.=` is a mutating method call at method-postfix precedence, far tighter than
# the conditional, so it stays legal inside a branch.
{
    my $v = 'ab';
    my $r = 1 ?? $v .= uc !! 9;
    is "$r $v", 'AB AB', '.= inside the then branch is still tight enough';
}

done-testing;
