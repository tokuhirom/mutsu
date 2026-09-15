use v6;
use Test;

# #8314: the name-keyed dispatch memos are tagged with a version that NAMES the
# registry's functions map rather than counting writes to it, so a version
# recurs when the map does -- which it does on every call of a routine that
# declares an inner `my sub` (installed on entry, taken away again on exit).
#
# That is a correctness contract, not just a speed one. These are the shapes
# where serving a memo from "the same version" would be a silent mis-dispatch
# if two different maps could ever share a version.

plan 9;

# 1. A routine-local `my sub` shadows an outer sub of the same name for calls
#    inside the routine, and only inside it -- on every call, not just the
#    first. A memo that outlived the routine's own map state would answer the
#    outer call site with the inner routine (or the reverse).
sub pick() { 'outer' }
sub inner-shadows() {
    my sub pick() { 'inner' }
    pick();
}
is pick(), 'outer', 'the outer sub before the shadowing routine has run';
is inner-shadows(), 'inner', 'the routine-local declaration wins inside';
is pick(), 'outer', 'the outer call site is untouched after the routine ran';
is inner-shadows(), 'inner', 'and the routine still picks its own on a re-call';
is pick(), 'outer', 'and the outer one still does too';

# 2. THE hazard: two routines that each install a same-named inner `my sub`.
#    Each entry takes the map to a different state; each exit takes it back to
#    the same shared one. A generation that counted steps instead of naming
#    content would hand both excursions the same stamp, and one routine's
#    resolution would be served inside the other.
sub caller-a() {
    my sub which() { 'a' }
    which();
}
sub caller-b() {
    my sub which() { 'b' }
    which();
}
is caller-a(), 'a', 'first routine resolves its own inner sub';
is caller-b(), 'b', 'second routine resolves its own, not the first routine one';
is caller-a(), 'a', 'and the first still resolves its own after the second ran';

# 3. The same alternation driven from a loop, so the map cycles many times:
#    every iteration must agree.
my @seen;
for ^8 {
    @seen.push(caller-a());
    @seen.push(caller-b());
    @seen.push(pick());
}
is @seen.join(','), ('a,b,outer' xx 8).join(','),
    'the resolution is stable across repeated map excursions';

done-testing;
