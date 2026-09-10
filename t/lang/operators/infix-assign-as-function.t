use Test;

plan 7;

# The assignment operator called as a function assigns to its first argument.

{
    my $x = 1;
    infix:<=>($x, 0);
    is $x, 0, 'infix:<=>($x, 0) assigns 0 to $x';
}

{
    my $x = 1;
    &infix:<=>.($x, 0);
    is $x, 0, '&infix:<=>.($x, 0) assigns 0 to $x';
}

{
    my Int $y;
    lives-ok { &infix:<=>($y, 3) }, 'assignment as function respects a matching type';
    is $y, 3, 'the value was assigned';
    dies-ok { &infix:<=>($y, 'foo') }, 'assignment as function type-checks (dies on mismatch)';
}

# Also works when the target is captured by an enclosing closure.
{
    my Int $z = 5;
    my $set = { &infix:<=>($z, 9) };
    $set();
    is $z, 9, 'assignment as function through a closure-captured target';
    my $bad = { &infix:<=>($z, 'nope') };
    dies-ok { $bad() }, 'type check applies to a closure-captured typed target';
}
