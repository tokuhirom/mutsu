use Test;

# Regression pin for todo/deep/cue-loop-lexical-shared-lane-residue.md.
#
# A loop-body `my $a = 0` redeclaration used to leave a stale value behind in
# `env["a"]`: `cas $a, ...` reaches its target variable as a string constant
# (`__mutsu_cas_var("a", ...)`), invisible to the ordinary free-variable scan
# that decides which locals need their env mirror kept current. So the env
# mirror was only ever updated as a side effect of a LATER `sync_shared_vars_to_env`
# reconciling a finished cue tick's accumulated count back into the caller's
# env -- never by the loop's own `my $a = 0` redeclaration. The next round's
# fresh `$a` local slot was reset to 0, but `env["a"]` stayed at the previous
# round's final value; a freshly cued worker thread's `atomic_current_value`
# fallback (`shared.get(value_key).or_else(|| self.env.get(name))`) read that
# stale env entry as its base for the round's first tick, so counts leaked
# and accumulated across rounds (10, 20, 30, ... instead of ~10 every round).
#
# This was fixed as a side effect of commit 3b5a0efc5 ("stop inline
# start-block spawns from clobbering a later-declared local"), which added
# `CompiledCode::rw_arg_env_sync_syms`: a `cas` target reached through ANY
# nested closure (not only a bareword `start` block) is now bubbled up and
# folded into `needs_env_sync` for the owning frame's local slot, so
# `my $a = 0`'s own declaration keeps `env["a"]` current on every loop
# redeclaration -- closing the gap regardless of which call spawns the
# worker thread (`$*SCHEDULER.cue`, `Promise.start`, bareword `start`, ...).
#
# `:times(N)` (rather than `:every` + a sampling window) makes the final
# count exact and deterministic, so this test needs no timing tolerance --
# a fresh round that still leaked would fail with an exact multiple of 10
# (20, 30, 40) rather than needing a fuzzy threshold.

plan 4;

for 1..4 -> $round {
    my $a = 0;
    my $c = $*SCHEDULER.cue({ cas $a, {.succ} }, :times(10));
    sleep 0.5;  # wait for the cue to run all 10 ticks
    LEAVE .cancel with $c;
    is $a, 10, "round $round: \$a resets fresh, no leak from a previous round";
}
