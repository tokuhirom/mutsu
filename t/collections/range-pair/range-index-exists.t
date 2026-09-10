use v6;
use Test;

# A Range subscript is a *slice* index: it names one index (or key) per element
# and gets one answer per element, exactly like a list index. It used to fall
# through `:exists`'s single-key tail, get stringified ("0..1") and be looked up
# as one key, so every Range answered a single False.
#
# The result *shape* follows the index form, not the element count: a
# one-element slice stays a one-element list, while a plain or parenthesised
# index stays a bare Bool.

plan 25;

# --- Array target ---------------------------------------------------------
my @a; @a[0] = 1; @a[1] = 2;
is-deeply (@a[0..1]:exists), (True, True), 'a Range over filled slots';
is-deeply (@a[0,1]:exists), (True, True), 'the list index it must agree with';

my @holes; @holes[0] = 1; @holes[3] = 2;
is-deeply (@holes[0..3]:exists), (True, False, False, True),
    'a Range reports each hole individually';

is-deeply (@a[^2]:exists), (True, True), 'a `^N` Range is a slice too';
is-deeply (@a[0..^1]:exists), (True,), 'an excluded-end Range keeps list shape';
is-deeply (@a[1..0]:exists), (), 'an empty Range answers an empty list';

my @shaped[3]; @shaped[0] = 1;
is-deeply (@shaped[0..2]:exists), (True, False, False),
    'a Range over a shaped array reports its unassigned slots';

# --- Hash target ----------------------------------------------------------
my %num = "0" => 1, "1" => 2;
is-deeply (%num{0..1}:exists), (True, True), 'a numeric Range as hash keys';

my %h = a => 1, b => 2;
is-deeply (%h{"a".."b"}:exists), (True, True), 'a string Range as hash keys';

my %one = a => 1;
is-deeply (%one{"a".."c"}:exists), (True, False, False),
    'a string Range reports each missing key';

# A Range is a slice of keys, never a path into a nested hash -- the
# multi-dimensional `%h{'a';'b'}` subscript is the other one.
my %nested = a => { b => 1 };
is-deeply (%nested{"a".."b"}:exists), (True, False),
    'a Range over a nested hash is still a key slice';
is-deeply (%nested{"a";"b"}:exists), True,
    'the multidim subscript still walks into the nested hash';

# --- QuantHash targets ----------------------------------------------------
my $s = set <a b>;
is-deeply ($s{"a".."b"}:exists), (True, True), 'a Range over a Set';
my $b = bag <a a b>;
is-deeply ($b{"a".."b"}:exists), (True, True), 'a Range over a Bag';
my $m = (a => 1.5, b => 2.5).Mix;
is-deeply ($m{"a".."b"}:exists), (True, True), 'a Range over a Mix';

# --- Negation and the other adverbs --------------------------------------
is-deeply (@a[0..1]:!exists), (False, False), 'a negated Range slice';
is-deeply (@a[0..1]:kv), (0, 1, 1, 2), ':kv over a Range slice';
is-deeply (@a[0..1]:p), (0 => 1, 1 => 2), ':p over a Range slice';
is-deeply (@a[0..1]:k), (0, 1), ':k over a Range slice';

# --- Result shape follows the index form, not the element count ----------
is-deeply (@a[0]:exists), True, 'a plain index answers a bare Bool';
is-deeply (@a[(0)]:exists), True, 'a parenthesised index is still one index';
is-deeply (@a[*-1]:exists), True, 'a WhateverCode index is still one index';
is-deeply (@a[0,]:exists), (True,), 'a one-element list index stays a list';
is-deeply (@a[0..0]:exists), (True,), 'a one-element Range stays a list';

my @single = 1;
is-deeply (@single[*]:exists), (True,), 'a `*` slice of one element stays a list';
