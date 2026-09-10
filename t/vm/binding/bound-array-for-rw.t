use v6;
use Test;

plan 15;

# Stage 1 (env↔locals coherence): a `:=`-bound array cell-izes both vars so
# they share one outer `ContainerRef`. A for-rw loop's topic/param writeback
# must write THROUGH that shared cell so the bound source observes the
# mutation. Previously the writeback matched a raw `Value::Array` directly,
# missed the ContainerRef-wrapped bound array, and silently dropped the write
# (both the bound var AND its source stayed unchanged).

# --- implicit topic ($_) for-rw on a bound array ---
{
    my @b = (1, 2, 3);
    my @a := @b;
    for @a { $_++ }
    is-deeply @a, [2, 3, 4], 'bound array: topic $_++ updates the bound var';
    is-deeply @b, [2, 3, 4], 'bound array: topic $_++ propagates to source';
}

# --- explicit assign to topic ---
{
    my @b = (1, 2, 3);
    my @a := @b;
    for @a { $_ = 99 }
    is-deeply @a, [99, 99, 99], 'bound array: $_ = X updates the bound var';
    is-deeply @b, [99, 99, 99], 'bound array: $_ = X propagates to source';
}

# --- named rw param ---
{
    my @b = (5, 6);
    my @a := @b;
    for @a -> $x is rw { $x += 10 }
    is-deeply @a, [15, 16], 'bound array: -> $x is rw updates the bound var';
    is-deeply @b, [15, 16], 'bound array: -> $x is rw propagates to source';
}

# --- multi-param rw ---
{
    my @b = (1, 2, 3, 4);
    my @a := @b;
    for @a -> $x, $y is rw { $y = 0 }
    is-deeply @b, [1, 0, 3, 0], 'bound array: multi-param rw propagates to source';
}

# --- named param binding an array element propagates `.push` ---
{
    my @b = ([1], [2]);
    my @a := @b;
    for @a -> @row { @row.push(9) }
    is-deeply @b, [[1, 9], [2, 9]], 'bound array: named @-param .push propagates';
}

# --- non-bound array still works (regression guard) ---
{
    my @b = (1, 2, 3);
    for @b { $_++ }
    is-deeply @b, [2, 3, 4], 'non-bound array: topic writeback still works';
}
{
    my @b = (5, 6);
    for @b -> $x is rw { $x += 10 }
    is-deeply @b, [15, 16], 'non-bound array: rw param writeback still works';
}

# --- read-only loop does not spuriously mutate ---
{
    my @b = (1, 2, 3);
    my @a := @b;
    my $sum = 0;
    for @a { $sum += $_ }
    is $sum, 6, 'bound array: read-only loop sums correctly';
    is-deeply @b, [1, 2, 3], 'bound array: read-only loop leaves source unchanged';
}

# --- three-level: bound chain ---
{
    my @c = (1, 2);
    my @b := @c;
    my @a := @b;
    for @a { $_ *= 10 }
    is-deeply @c, [10, 20], 'bound chain: for-rw reaches the original source';
}

# --- element assign and push still propagate (regression for bind sharing) ---
{
    my @b = (1, 2, 3);
    my @a := @b;
    @a[0] = 99;
    @a.push(4);
    is-deeply @b, [99, 2, 3, 4], 'bound array: element assign + push still propagate';
}

# --- mutation through source visible via bound var ---
{
    my @b = (1, 2, 3);
    my @a := @b;
    @b[1] = 50;
    is @a[1], 50, 'bound array: source mutation visible through bound var';
}

done-testing;
