use Test;

plan 9;

# A `with`/`orwith`/`else` block establishes a FRESH topic scope, so it must work
# even when nested inside a `for ^N { }` whose implicit `$_` is read-only.
# Previously the `orwith`/`else`/`with LITERAL` topicalization used a plain
# `$_ = ...` assignment, which threw X::Assignment::RO in that context.

{
    my @seen;
    for ^2 { with 5 { @seen.push($_) } }
    is-deeply @seen, [5, 5], 'with LITERAL inside for ^N';
}

{
    my @seen;
    my $a = Nil;
    my $b = 7;
    for ^2 { with $a { } orwith $b { @seen.push($_) } }
    is-deeply @seen, [7, 7], 'orwith inside for ^N';
}

{
    my @seen;
    for ^2 { with Nil { } else { @seen.push(.defined) } }
    is-deeply @seen, [False, False], 'with/else inside for ^N';
}

{
    my @seen;
    my $a = Nil;
    for ^2 { with $a { } orwith Nil { } else { @seen.push("else") } }
    is-deeply @seen, ["else", "else"], 'orwith/else inside for ^N';
}

# `orwith` aliases its topic, so `$_` mutation writes back to the source.
{
    my $x = 5;
    with Nil { } orwith $x { $_++ }
    is $x, 6, 'orwith aliases topic for writeback';
}

# `with LITERAL` keeps the read-only topic semantics.
{
    my $err;
    {
        with 5 { $_ = 9 }
        CATCH { default { $err = .^name } }
    }
    ok $err.defined, 'with LITERAL keeps $_ read-only';
}

# Standalone (non-loop) cases still behave.
{
    my $hit = 0;
    with 42 { $hit = $_ }
    is $hit, 42, 'plain with LITERAL';
}

{
    my $hit = 0;
    with Nil { } orwith 99 { $hit = $_ }
    is $hit, 99, 'plain orwith';
}

# The map-block + for-modifier regression that motivated the fix.
{
    sub pick-one($x) { with Nil { } orwith $x { }; $x }
    my @r;
    for ^2 {
        my @m = (1, 2, 3).map({ pick-one($_) });
        @m[$_] = 9 for (0, 1);
        @r.push(@m.join(","));
    }
    is-deeply @r, ["9,9,3", "9,9,3"], 'orwith in map block under nested for-loops';
}
