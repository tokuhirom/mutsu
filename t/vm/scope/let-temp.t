use Test;

plan 11;

# temp: always restores at scope exit
{
    my $x = 1;
    { temp $x = 42; is $x, 42, "temp: value is set inside block" }
    is $x, 1, "temp: value is restored after block";
}

# let: keeps value on successful block exit
{
    my $x = 1;
    { let $x = 42; is $x, 42, "let: value is set inside block" }
    is $x, 42, "let: value is kept after successful block";
}

# let: restores on exception
{
    my $x = 1;
    try { let $x = 42; die "oops" }
    is $x, 1, "let: value is restored after exception";
}

# let: restores when block returns Nil
{
    my $x = 1;
    { let $x = 42; Nil }
    is $x, 1, "let: value is restored when block returns Nil";
}

# temp with bare form (no assignment)
{
    my $x = 10;
    { temp $x; $x = 99; is $x, 99, "bare temp: value can be changed" }
    is $x, 10, "bare temp: value is restored after block";
}

# let with bare form (no assignment)
{
    my $x = 10;
    { let $x; $x = 99; 1 }
    is $x, 99, "bare let: value is kept on success";
}

# temp in nested blocks
{
    my $x = 1;
    { temp $x = 2; { temp $x = 3 }; is $x, 2, "nested temp: inner restored" }
    is $x, 1, "nested temp: outer restored";
}
