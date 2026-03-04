use Test;

plan 8;

# CATCH in bare block
{
    my $caught = 0;
    {
        die "test";
        CATCH { default { $caught = 1 } }
    }
    ok $caught, "CATCH in bare block catches exceptions";
}

# CATCH in do block
{
    my $lived = 0;
    do { die "test"; CATCH { default {} } };
    $lived = 1;
    ok $lived, "CATCH in do block catches exceptions";
}

# CATCH in if block
{
    my $caught = 0;
    sub test-if($x) {
        if $x {
            die "test";
            CATCH { default { $caught = 1 } }
        }
    }
    test-if(1);
    ok $caught, "CATCH in if branch catches exceptions";
}

# CATCH in if block inside sub (value context)
{
    sub test-if-val($x) {
        if $x {
            die "blah";
            CATCH { default {} }
        }
        else {
            die "blah";
        }
    }
    my $lived = 0;
    test-if-val(1);
    $lived = 1;
    ok $lived, "CATCH in if branch inside sub catches exceptions";
}

# CATCH in else branch should not affect if branch
{
    sub test-else($x) {
        if $x {
            die "from-if";
        }
        else {
            die "from-else";
            CATCH { default {} }
        }
    }
    my $died = 0;
    try { test-else(1) };
    $died = 1 if $!;
    ok $died, "CATCH in else branch does not affect if branch exceptions";
}

# CATCH in try block
{
    my $caught = 0;
    try {
        die "test";
        CATCH { default { $caught = 1 } }
    }
    ok $caught, "CATCH in try block catches exceptions";
}

# CATCH in sub body
{
    my $caught = 0;
    sub test-sub-catch {
        die "test";
        CATCH { default { $caught = 1 } }
    }
    test-sub-catch();
    ok $caught, "CATCH in sub body catches exceptions";
}

# Nested CATCH - inner catches first
{
    my $inner-caught = 0;
    my $outer-caught = 0;
    {
        {
            die "test";
            CATCH { default { $inner-caught = 1 } }
        }
        CATCH { default { $outer-caught = 1 } }
    }
    ok $inner-caught && !$outer-caught, "Inner CATCH catches before outer";
}
