use Test;
plan 7;

# KEEP/UNDO phasers declared inside a loop body used to never run at all when
# the iteration was interrupted by `last`/`next` -- the dispatch was simply
# never reached, not just mis-decided. Real raku always runs UNDO (never
# KEEP) in that case: an interrupted iteration's trailing value is undefined
# (`return_value` is `None`), which per the definedness rule already used for
# bare blocks (`should_run_success_queue`) routes to UNDO.
# See todo/tickets/loop-body-keep-undo-not-run-on-last-next.md.

# `for` loop, single iteration, interrupted by `last`.
my $s1 = "";
for 1 { KEEP { $s1 ~= "K" }; UNDO { $s1 ~= "U" }; last }
is $s1, "U", 'for-loop: UNDO runs once when the sole iteration exits via last';

# `for` loop, two iterations, each interrupted by `next`.
my $s2 = "";
for 1, 2 { KEEP { $s2 ~= "K" }; UNDO { $s2 ~= "U" }; next }
is $s2, "UU", 'for-loop: UNDO runs on every iteration interrupted via next';

# `while` loop, interrupted by `last` on the first iteration.
my $s3 = "";
my $i = 0;
while $i < 3 {
    $i++;
    KEEP { $s3 ~= "K" };
    UNDO { $s3 ~= "U" };
    last if $i == 1;
}
is $s3, "U", 'while-loop: UNDO runs when the iteration exits via last';

# `while` loop, every iteration interrupted by `next`.
my $s4 = "";
$i = 0;
while $i < 3 {
    $i++;
    KEEP { $s4 ~= "K" };
    UNDO { $s4 ~= "U" };
    next;
}
is $s4, "UUU", 'while-loop: UNDO runs on every iteration interrupted via next';

# `for` loop, normal (uninterrupted) completion still runs KEEP -- guards
# against the fix regressing the already-working non-interrupted path.
my $s5 = "";
for 1, 2, 3 { KEEP { $s5 ~= "K" }; UNDO { $s5 ~= "U" }; 1 }
is $s5, "KKK", 'for-loop: KEEP still runs on normal (uninterrupted) completion';

# Ordering verified against real raku: LEAVE + KEEP/UNDO together, `last`
# runs UNDO strictly BEFORE LEAVE (same relative order as normal completion,
# where KEEP also precedes LEAVE).
my $s6 = "";
for 1 {
    LEAVE { $s6 ~= "L" }
    KEEP { $s6 ~= "K" };
    UNDO { $s6 ~= "U" };
    last;
}
is $s6, "UL", 'for-loop: UNDO runs before LEAVE on a last-interrupted exit';

# Ordering verified against real raku: an explicit `next` runs its NEXT
# phasers FIRST (synchronously, as part of the `next` transfer itself), THEN
# UNDO, THEN LEAVE -- the OPPOSITE order from normal (uninterrupted)
# completion, where the order is KEEP/UNDO, then LEAVE, then NEXT.
my $s7 = "";
for 1 {
    LEAVE { $s7 ~= "L" }
    KEEP { $s7 ~= "K" };
    UNDO { $s7 ~= "U" };
    NEXT { $s7 ~= "N" }
    next;
}
is $s7, "NUL", 'for-loop: NEXT, then UNDO, then LEAVE on a next-interrupted exit';
