use Test;
plan 18;

# .= mutating method call
{
    my $s = "hello";
    $s .= uc;
    is $s, 'HELLO', '.= uc';
}
{
    my $s = "  hello  ";
    $s .= trim;
    is $s, 'hello', '.= trim';
}
{
    my $s = "Hello";
    $s .= lc;
    is $s, 'hello', '.= lc';
}

# !!! fatal stub
{
    my $died = False;
    try { !!!; CATCH { default { $died = True } } }
    ok $died, '!!! throws exception';
}

# !!! with message
{
    my $msg = '';
    try { !!! "custom error"; CATCH { default { $msg = .Str } } }
    ok $msg.contains("custom"), '!!! with custom message';
}

# ??? admonitory stub
{
    # ??? warns but does not die
    ??? "test warning";
    pass '??? does not die';
}

# S/// non-destructive substitution
{
    my $s = "hello";
    my $r = S/l/r/ given $s;
    is $r, 'herlo', 'S/// returns modified string';
    is $s, 'hello', 'S/// does not modify original';
}

# min= compound assignment
{
    my $a = 10;
    $a min= 5;
    is $a, 5, 'min= assigns minimum';

    $a min= 8;
    is $a, 5, 'min= keeps smaller value';
}

# max= compound assignment
{
    my $a = 5;
    $a max= 10;
    is $a, 10, 'max= assigns maximum';

    $a max= 3;
    is $a, 10, 'max= keeps larger value';
}

# +^ integer bitwise negation
is +^0, -1, '+^0 is -1';
is +^255, -256, '+^255 is -256';

# ?^ boolean bitwise negation
is ?^True, False, '?^True is False';
is ?^False, True, '?^False is True';
is ?^0, True, '?^0 is True (0 is falsy)';
is ?^1, False, '?^1 is False (1 is truthy)';

done-testing;
