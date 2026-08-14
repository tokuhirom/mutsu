use Test;

# Regression test for todo/tickets/cue-loop-lexical-shared-lane-residue.md.
#
# A `for` loop that re-declares `my $a = 0` each iteration and hands `$a` to
# a `:every` cue's `cas` callback must see each iteration start back at 0 —
# not carry forward the previous iteration's accumulated count.
#
# Root cause: `env` only mirrors a local's CURRENT value when something in
# the SAME frame reads it BY NAME (`compute_needs_env_sync`'s per-store env
# write gate). A loop-body local only ever touched via a NAME-KEYED `cas`
# inside a NESTED cue callback never earns that mirror, so a stale value
# synced back into `env` from a FINISHED spawn's cross-thread write survived
# the next iteration's `my $a = 0` redeclaration (which only clears the
# LOCAL SLOT, not the stale env mirror) — and the NEXT spawn's
# `clone_for_thread_for_block` clone inherited that stale env value as the
# starting point for the fresh cas counter.
#
# Fixed by publishing the calling frame's local-slot values into env
# whenever `clone_for_thread_excluding` clones for a spawn (previously this
# refresh was hardcoded to fire only for the bareword `start` function, so
# every OTHER thread-spawning construct — `.cue`, `Promise.start`,
# `Thread.start`, a `whenever` worker — inherited the same staleness).
plan 3;

my @counts;
for 1..3 {
    my $a = 0;
    my $c = $*SCHEDULER.cue({ cas $a, {.succ} }, :every(0.02));
    sleep 0.4;
    $c.cancel;
    @counts.push($a);
}

# Each round ticks independently for ~0.4s at a 0.02s interval (~20 ticks
# nominal), so every round should land in roughly the same range. With the
# bug, round N accumulated every prior round's count too (a monotonically
# growing sum, e.g. 20, 40, 60); fixed, each round restarts near the same
# value regardless of round number. The `1.6x + 10` bound is generous
# headroom for CI timing jitter while still well below the ~2x/~3x growth
# the bug produced.
ok @counts[0] > 5, "round 1 saw a healthy number of cue ticks ({@counts.join(', ')})";
ok @counts[1] < @counts[0] * 1.6 + 10,
    "round 2 did not accumulate round 1's count ({@counts.join(', ')})";
ok @counts[2] < @counts[0] * 1.6 + 10,
    "round 3 did not accumulate earlier rounds' count ({@counts.join(', ')})";
