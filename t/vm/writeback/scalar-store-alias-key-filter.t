use Test;

# #10691: the scalar-store fast path probes the env for a slot's
# `__mutsu_sigilless_alias::` key only when that key may exist (a bit per key
# ever written), not whenever any bind exists anywhere. The probe still has to
# fire for a name that really is aliased, however the alias was made.

plan 8;

# An unrelated bind leaves plain stores alone.
{
    my @data = 1, 2, 3;
    my @unused := @data;
    my $acc = 0;
    $acc = $acc + $_ for 1..10;
    is $acc, 55, 'plain stores are unaffected by an unrelated bind';
}

# A sigilless bind: a store through the alias reaches the source.
{
    my $c = 1;
    my \d := $c;
    d = 4;
    is $c, 4, 'a store through a sigilless alias reaches its source';
}

# A scalar bind made inside a sub, on free variables.
{
    my $a = 1;
    my $b;
    sub bind-free { ($b := $a) }
    bind-free();
    $b = 7;
    is $a, 7, 'a store through a sub-made alias reaches its source';
    $a = 9;
    is $b, 9, 'and a store to the source shows through the alias';
}

# Two aliases of one source, made in the same scope.
{
    my $s = 0;
    my $p := $s;
    my $q := $s;
    $p = 3;
    is $q, 3, 'a store through one alias is seen through another';
    $q = 5;
    is $s, 5, 'and reaches the source';
}

# A raw parameter aliases the caller's variable.
{
    sub bump(\x) { x = x + 1 }
    my $n = 41;
    bump($n);
    is $n, 42, 'a store through a raw parameter reaches the caller';
}

# A loop over a bound variable keeps writing through.
{
    my $total = 0;
    my $t := $total;
    $t = $t + $_ for 1..4;
    is $total, 10, 'repeated stores through an alias all land';
}
