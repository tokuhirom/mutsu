use Test;

plan 5;

# gh#8783: a lazy Seq produced by `gather @list.map: *.take` only yielded ONE
# element to a later lazy consumer, because the inline `.map` fast path
# (`eval_map_over_items`) has no way to snapshot/resume mid-iteration when a
# `take` hits the take-limit signal mid-loop — it ran inside a
# `with_nested_registers` boundary that isn't itself resumable across pulls.
# Eager reification (`my @a = ...`) always worked, since it drives the
# gather with a take limit large enough to never trigger the bug.

sub via-colon-call(@list) { gather @list.map: *.take; }
sub via-call(@list) { gather @list.map(*.take); }
sub via-block(@list) { gather { @list.map: *.take } }
sub via-for-loop(@list) { gather for @list { take $_ } }

is via-colon-call([1, 2, 3, 4]).map({ $_ * 10 }).elems, 4,
    'gather @list.map: *.take yields every element to a later lazy .map';
is via-call([1, 2, 3, 4]).map({ $_ * 10 }).elems, 4,
    'gather @list.map(*.take) yields every element to a later lazy .map';
is via-block([1, 2, 3, 4]).map({ $_ * 10 }).elems, 4,
    'gather { @list.map: *.take } yields every element to a later lazy .map';
is via-for-loop([1, 2, 3, 4]).map({ $_ * 10 }).elems, 4,
    'gather for @list { take $_ } still yields every element (regression guard)';

my @eager = via-colon-call([1, 2, 3, 4]);
is @eager.map({ $_ * 10 }).elems, 4,
    'eager reification into an Array still works (regression guard)';
