use Test;

# §1.5 slice S2: `$x++` / `$x--` write back through the compile-time-baked local
# slot (store_named_scalar_rmw_result), not a by-name `code.locals` search.

plan 8;

# basic postfix inc/dec on a local
{
    my $x = 5;
    $x++;
    $x++;
    $x--;
    is $x, 6, 'postfix ++/-- on a local';
}

# a shadowing inner-block increment writes the inner binding; the outer is intact
{
    my $x = 10;
    {
        my $x = 100;
        $x++;
        is $x, 101, 'inc of an inner shadow writes the inner slot';
    }
    is $x, 10, 'the outer binding is untouched by the shadow inc';
}

# a `:=` alias sees the increment through the shared binding
{
    my $a = 1;
    my $b := $a;
    $a++;
    is $a, 2, ':= source after inc';
    is $b, 2, ':= alias reflects the inc';
}

# `our` package var increment persists across sub calls
{
    our $g = 0;
    sub bump-g { $g++ }
    bump-g();
    bump-g();
    is $g, 2, 'our-var inc via bare name persists';
}

# read after write in the same frame reads the mirrored slot, not a stale value
{
    my $n = 0;
    $n++;
    my $seen = $n;
    is $seen, 1, 'same-frame read after inc sees the new value';
}

# hot loop
{
    my $c = 0;
    $c++ for ^1000;
    is $c, 1000, 'inc in a hot loop';
}
