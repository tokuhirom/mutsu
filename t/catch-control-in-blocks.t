use Test;

plan 6;

# CATCH in bare block
{
    my $caught = False;
    {
        CATCH { default { $caught = True } }
        die "test";
    }
    ok $caught, "CATCH works in bare block";
}

# CATCH in sub
{
    sub foo() {
        CATCH { default { return "caught" } }
        die "test";
    }
    is foo(), "caught", "CATCH works in sub";
}

# CATCH in for loop - catches and continues
{
    my @results;
    for 1..3 -> $i {
        CATCH { default { @results.push("caught $i") } }
        die "err" if $i == 2;
        @results.push("ok $i");
    }
    is @results.join(", "), "ok 1, caught 2, ok 3", "CATCH in for loop catches per iteration";
}

# CATCH in while loop
{
    my $caught = False;
    my $i = 0;
    while $i < 3 {
        CATCH { default { $caught = True } }
        $i++;
        die "test" if $i == 2;
    }
    ok $caught, "CATCH works in while loop";
}

# CATCH in if block
{
    my $caught = False;
    if True {
        CATCH { default { $caught = True } }
        die "test";
    }
    ok $caught, "CATCH works in if block";
}

# CATCH in else block
{
    my $caught = False;
    if False {
        say "nope";
    } else {
        CATCH { default { $caught = True } }
        die "test";
    }
    ok $caught, "CATCH works in else block";
}
