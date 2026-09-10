use Test;

# Regression pin for `todo/deep/inline-start-blocks-clobber-a-later-declared-variable.md`.
#
# Root cause: a `start { ... }` block passed inline as a call argument spawns
# BEFORE the variable it is being assigned to has its real value. The
# pre-spawn sync used to blindly publish every local (including the
# not-yet-assigned variable's stale placeholder) into the cross-thread
# `shared_vars` store; a later, unrelated `await`/`.result` would then pull
# that stale value back over the correct one. Fixed by gating the pre-spawn
# cross-thread publish on `needs_env_sync` (per-slot: "does any code reach
# this local by name"), while keeping the plain `env` mirror unconditional.
#
# Subtests 3-4 pin the harder half of the fix: a `cas` target reaches its
# variable only as a string constant (invisible to the free-var op scan), so
# `needs_env_sync` must have a dedicated channel (`rw_arg_env_sync_syms`) for
# rw-arg-sink builtins, kept OUT of `free`/`free_writes` so it does not change
# closure capture/cell-promotion decisions. Subtest 4 additionally requires an
# EARLIER, unrelated `start`+`cas` block on a different variable to already
# have run in the same process before the real regression reproduces.

plan 5;

# 1. Two `start` blocks as inline `.allof` arguments; `await` the resulting
#    Promise itself.
{
    my $p = Promise.allof(start { 1 }, start { 2 });
    await $p;
    is $p.WHAT, Promise, 'await on the assembled Promise does not clobber it';
}

# 2. Same shape, but the awaited thing is completely unrelated to $p --
#    proves the corruption happens at spawn time, not inside await/allof.
{
    my $p = Promise.allof(start { 1 }, start { 2 });
    is $p.WHAT, Promise, '(Promise) right after assignment';
    await Promise.in(0.01);
    is $p.WHAT, Promise, 'unrelated await does not clobber a Promise assigned around inline starts';
}

# 3. A `cas` target inside a `start` block, standalone -- must still work
#    (rw-arg-sink completeness must not require an earlier block to prime it).
{
    my $n = 0;
    Promise.allof(start { cas $n, -> $v { $v + 1 } }).result;
    is $n, 1, 'cas inside a start block reconciles to the caller on its own';
}

# 4. The actual Gap 4 sentinel: an unrelated earlier start+cas block (on a
#    different variable) must not strand a later start+cas block's update.
{
    { my $seen = []; Promise.allof(start { cas $seen, -> @c { flat @c, 1 } }).result; }
    my $n = 0;
    Promise.allof(start { cas $n, -> $v { $v + 1 } }).result;
    is $n, 1, 'cas after an unrelated prior start+cas block still reconciles';
}
