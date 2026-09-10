use Test;

# `{*}` rw-redispatch (ledger §D, multi-dispatch VM-ization): Rakudo redispatches
# `{*}` using the proto's CURRENT (body-mutated) parameter, so a candidate's own
# `is rw` write chains back through the proto parameter to the caller's container.
# Regression pin: previously mutsu passed the proto's entry-time args to the
# candidate, so the body mutation was invisible to the candidate and the
# candidate's rw write was lost.

plan 9;

# 1. proto body mutates rw param, candidate adds to it
{
    proto pr($x is rw) { $x = 99; {*} }
    multi pr(Int $x is rw) { $x = $x + 1 }
    my $v = 10; pr($v);
    is $v, 100, 'proto body mutation visible to candidate, candidate write propagates';
}

# 2. proto param name differs from candidate param name (positional alias)
{
    proto pr($a is rw) { $a = 50; {*} }
    multi pr(Int $b is rw) { $b = $b * 2 }
    my $v = 5; pr($v);
    is $v, 100, 'redispatch aliases by position, not by name';
}

# 3. no body mutation; candidate mutates from caller value
{
    proto pr($x is rw) { {*} }
    multi pr(Int $x is rw) { $x = $x + 7 }
    my $v = 3; pr($v);
    is $v, 10, 'candidate rw write propagates without body mutation';
}

# 4. candidate does not mutate; proto body mutation still wins
{
    proto pr($x is rw) { $x = 77; {*} }
    multi pr(Int $x is rw) { $x }
    my $v = 1; pr($v);
    is $v, 77, 'proto body mutation survives a non-mutating candidate';
}

# 5. two positional params, second is rw
{
    proto pr($a, $b is rw) { $b = $b + 100; {*} }
    multi pr(Int $a, Int $b is rw) { $b = $b + $a }
    my $v = 5; pr(3, $v);
    is $v, 108, 'rw chains for a non-leading positional rw param';
}

# 6. is raw param
{
    proto pr($x is raw) { $x = 20; {*} }
    multi pr(Int $x is raw) { $x = $x + 5 }
    my $v = 1; pr($v);
    is $v, 25, 'is raw param chains like is rw';
}

# 7. candidate computes via a temporary, then assigns
{
    proto pr($x is rw) { $x = 7; {*} }
    multi pr(Int $x is rw) { my $t = $x * 2; $x = $t }
    my $v = 1; pr($v);
    is $v, 14, 'candidate write through a temporary still propagates';
}

# 8. non-rw proto is unchanged (no spurious writeback)
{
    proto nn($x) { {*} }
    multi nn(Int $x) { $x + 1 }
    my $v = 41;
    is nn($v), 42, 'non-rw proto returns candidate value';
    is $v, 41, 'non-rw proto does not mutate the caller variable';
}
