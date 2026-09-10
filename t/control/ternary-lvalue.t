use Test;

plan 12;

# Basic ternary lvalue: assign through the selected branch.
{
    my $x = 1;
    my $y = 2;
    (1 ?? $x !! $y) = 99;
    is $x, 99, 'true branch is assigned';
    is $y, 2, 'false branch untouched';
}

{
    my $a = 10;
    my $b = 20;
    (0 ?? $a !! $b) = 77;
    is $a, 10, 'true branch untouched when cond is false';
    is $b, 77, 'false branch is assigned';
}

# RHS is evaluated exactly once, only in the taken branch.
{
    my $calls = 0;
    my $p = 0;
    my $q = 0;
    (1 ?? $p !! $q) = do { $calls++; 5 };
    is $p, 5, 'taken branch got the value';
    is $q, 0, 'other branch untouched';
    is $calls, 1, 'RHS evaluated once';
}

# Index expressions as branches.
{
    my @arr = 1, 2, 3;
    (1 ?? @arr[0] !! @arr[1]) = 100;
    is-deeply @arr, [100, 2, 3], 'index lvalue branch is assigned';
}

# Nested ternary on a branch.
{
    my $m = 0;
    my $n = 0;
    my $o = 0;
    (0 ?? $m !! (1 ?? $n !! $o)) = 42;
    is $m, 0, 'nested: outer false branch chosen';
    is $n, 42, 'nested: inner true branch assigned';
    is $o, 0, 'nested: inner false branch untouched';
}

# The assignment expression yields the assigned value.
{
    my $x = 1;
    my $y = 2;
    my $r = ((1 ?? $x !! $y) = 5);
    is $r, 5, 'ternary lvalue assignment returns the assigned value';
}
