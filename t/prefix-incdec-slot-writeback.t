use Test;

# §1.5 slice S3: prefix `++$x` / `--$x` write back through the compile-time-baked
# local slot (exec_pre_increment_op / exec_pre_decrement_op), not a by-name
# `code.locals` search.

plan 8;

# basic prefix inc/dec on a local (returns the NEW value)
{
    my $x = 5;
    is ++$x, 6, 'prefix ++ returns and stores the new value';
    is --$x, 5, 'prefix -- returns and stores the new value';
    is $x, 5, 'the local reflects the writeback';
}

# a shadowing inner-block prefix inc writes the inner binding; outer is intact
{
    my $x = 10;
    {
        my $x = 100;
        is ++$x, 101, 'prefix ++ of an inner shadow writes the inner slot';
    }
    is $x, 10, 'the outer binding is untouched by the shadow prefix inc';
}

# `our` package var prefix inc persists across sub calls
{
    our $g = 0;
    sub bump-g { ++$g }
    bump-g();
    bump-g();
    is $g, 2, 'our-var prefix inc via bare name persists';
}

# same-frame read after prefix inc sees the mirrored slot value
{
    my $n = 0;
    ++$n;
    is $n, 1, 'same-frame read after prefix inc sees the new value';
}

# prefix inc in a hot loop
{
    my $c = 0;
    ++$c for ^1000;
    is $c, 1000, 'prefix inc in a hot loop';
}
