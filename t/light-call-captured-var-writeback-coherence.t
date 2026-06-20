use Test;

# Slice F (env<->locals coherence): a sub that mutates a captured outer lexical
# (`sub take($n) { $seen = $n }`) reaches `env` by name, but the caller's local
# slot was kept coherent only by the reverse `sync_locals_from_env` pull. The
# 0-arg fast path (#3317), named/qualified path (#3323), and method path (#3322)
# already write such mutations through; this covers the positional-light and
# named-light dispatch paths (the common `sub f($n) { ... }` / `sub f(:$v) { ... }`
# shapes) AND their cached fast paths (a second call hits the cache, which must
# also drain the write-through or the second call's mutation is lost). Run with
# `MUTSU_NO_REVERSE_SYNC=1` to confirm coherence without the reverse pull.

plan 12;

# Positional param, single captured-outer write.
{
    my $seen = 0;
    sub take($n) { $seen = $n }
    take(5);
    is $seen, 5, 'positional-light captured-outer write visible';
}

# Named param, single captured-outer write.
{
    my $got;
    sub with-named(:$value) { $got = $value }
    with-named(value => 7);
    is $got, 7, 'named-light captured-outer write visible';
}

# Two pure overwrites through the cached positional-light path.
{
    my $last = 0;
    sub setit($n) { $last = $n }
    setit(1);
    setit(9);
    is $last, 9, 'second cached positional-light call overwrites caller';
}

# Compound accumulation across calls (callee reads then writes the captured var).
{
    my $acc = 0;
    sub addto($n) { $acc += $n }
    addto(3);
    addto(4);
    is $acc, 7, 'compound += accumulates across cached positional-light calls';
}

# Read-modify-write accumulation.
{
    my $c = 10;
    sub bump($x) { $c = $c + $x }
    bump(1);
    bump(1);
    is $c, 12, 'read-modify-write accumulates across calls';
}

# Two captured-outer scalars written in one positional call.
{
    my $p = 0;
    my $q = 0;
    sub setpq($a, $b) { $p = $a; $q = $b }
    setpq(5, 9);
    is $p, 5, 'first captured-outer scalar written via positional-light';
    is $q, 9, 'second captured-outer scalar written via positional-light';
}

# Captured-outer array push.
{
    my @log;
    sub rec($x) { @log.push($x) }
    rec("a");
    rec("b");
    is @log.join(","), "a,b", 'captured-outer array push via positional-light';
}

# Captured-outer string mutate.
{
    my $name = "x";
    sub setname($s) { $name = $s }
    setname("hi");
    is $name, "hi", 'captured-outer string write via positional-light';
}

# Named param accumulation across cached calls.
{
    my $total = 0;
    sub addn(:$by) { $total += $by }
    addn(by => 2);
    addn(by => 3);
    is $total, 5, 'named-light captured-outer accumulates across cached calls';
}

# Caller slot stays coherent for a later expression.
{
    my $base = 0;
    sub setbase($n) { $base = $n }
    setbase(40);
    is $base + 2, 42, 'caller slot coherent in a later expression';
}

# Post-increment of a captured-outer scalar across calls.
{
    my $ticks = 0;
    sub tick($unused) { $ticks++ }
    tick(0);
    tick(0);
    is $ticks, 2, 'post-increment of captured-outer var accumulates across calls';
}
