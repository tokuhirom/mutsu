use v6;
use Test;

plan 11;

# Stage 1 (env↔locals coherence), hash "site A": a `:=`-bound hash cell-izes
# both vars into one shared `ContainerRef`. A for-rw loop over `%h.values` /
# `%h.kv` must write THROUGH that shared cell so the bound source observes the
# value mutation. Two sites read the source as a raw `Value::Hash` without
# deref: the pre-captured key order (loop setup) and the per-element writeback.

# --- .values topic ($_) ---
{
    my %g = (a => 1, b => 2);
    my %h := %g;
    for %h.values { $_++ }
    is-deeply %g, {a => 2, b => 3}, 'bound hash: .values topic $_++ propagates to source';
    is-deeply %h, {a => 2, b => 3}, 'bound hash: .values topic $_++ visible via bound var';
}

# --- .values named rw param ---
{
    my %g = (a => 1, b => 2);
    my %h := %g;
    for %h.values -> $v is rw { $v += 10 }
    is-deeply %g, {a => 11, b => 12}, 'bound hash: .values -> $v is rw propagates';
}

# --- .kv rw param ---
{
    my %g = (a => 1, b => 2);
    my %h := %g;
    for %h.kv -> $k, $v is rw { $v = 99 }
    is-deeply %g, {a => 99, b => 99}, 'bound hash: .kv -> $k, $v is rw propagates';
}

# --- explicit assign to .values topic ---
{
    my %g = (x => 5);
    my %h := %g;
    for %h.values { $_ = 42 }
    is-deeply %g, {x => 42}, 'bound hash: .values $_ = X propagates';
}

# --- bound chain ---
{
    my %c = (a => 1, b => 2);
    my %b := %c;
    my %a := %b;
    for %a.values { $_ *= 10 }
    is-deeply %c, {a => 10, b => 20}, 'bound hash chain: .values rw reaches original source';
}

# --- non-bound hash still works (regression guard) ---
{
    my %g = (a => 1, b => 2);
    for %g.values { $_++ }
    is-deeply %g, {a => 2, b => 3}, 'non-bound hash: .values topic writeback still works';
}
{
    my %g = (a => 1, b => 2);
    for %g.values -> $v is rw { $v += 100 }
    is-deeply %g, {a => 101, b => 102}, 'non-bound hash: .values rw writeback still works';
}

# --- read-only loop does not mutate ---
{
    my %g = (a => 1, b => 2);
    my %h := %g;
    my $sum = 0;
    for %h.values { $sum += $_ }
    is $sum, 3, 'bound hash: read-only .values sums correctly';
    is-deeply %g, {a => 1, b => 2}, 'bound hash: read-only .values leaves source unchanged';
}

# --- element write + bound array regression still green ---
{
    my %g = (a => 1);
    my %h := %g;
    %h<b> = 2;
    is-deeply %g, {a => 1, b => 2}, 'bound hash: element write still propagates';
}

done-testing;
