use v6;
use Test;

# Pin for a regression surfaced while fixing #8353's "too few positionals"
# check: a bare WhateverCode (`*.value`, `-*.value`, ...) compiles with the
# legacy binder's sentinel params list `["_"]` -- it reads its implicit
# argument through the dynamically-scoped topic `$_` (set by whatever
# topicalizes it, e.g. `.sort`'s per-element call), not through a real
# positional bind. A Pair-shaped element (from a Hash/Bag) that
# `pair_as_positional` promotes to a `ValuePair` is only counted as
# positional by the legacy binder for a signature with a REAL plain name,
# never for this bare `["_"]` shape -- so #8353's new "too few positionals"
# check, applied without excluding it, wrongly saw "0 positionals supplied"
# and rejected every call. `.sort`'s fallback then collapsed every rejected
# key to `Nil`, making the whole sort non-deterministic (order depended on
# the Bag's internal hash-iteration order).

plan 5;

my $b = Bag.new("a", "a", "b", "b", "b", "c");
is $b.sort(-*.value).map(*.key).join(","), "b,a,c",
    'Bag.sort(-*.value) is deterministically descending by weight';
is $b.sort(*.value).map(*.key).join(","), "c,a,b",
    'Bag.sort(*.value) is deterministically ascending by weight';

my %h = a => 2, b => 3, c => 1;
is %h.sort(-*.value).map(*.key).join(","), "b,a,c",
    'Hash.sort(-*.value) is deterministically descending by value';

my $mx = Mix.new("a", "a", "b", "b", "b", "c");
is $mx.sort(-*.value).map(*.key).join(","), "b,a,c",
    'Mix.sort(-*.value) is deterministically descending by weight';

# Directly exercises the arity check itself: a bare WhateverCode called with
# a genuine Pair argument (the shape `.sort`'s SortCaller feeds it) must
# still bind, not reject as a short call.
my $wc = -*.value;
is $wc(Pair.new("a", 2)), -2,
    'a bare WhateverCode binds a positionally-passed Pair through the topic';
