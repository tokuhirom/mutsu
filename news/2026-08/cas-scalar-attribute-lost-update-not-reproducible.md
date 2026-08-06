# `cas($!attr, …)` lost-update ticket investigated: not reproducible, no fix needed

`todo/tickets/cas-on-scalar-attribute-loses-updates.md` claimed `roast/S17-lowlevel/cas.t`
(the "CAS on linked list with Scalar attribute head works" subtests) and
`roast/S17-lowlevel/cas-int.t` failed deterministically (5/5 and 3/3) against a
**debug** binary, with roughly a quarter of concurrently-CAS'd linked-list nodes
silently lost — the signature of a compare-and-swap succeeding against a stale
view instead of the live one.

## What the investigation found

**`cas.t` no longer reproduces.** The ticket's own repro command
(`MUTSU_BIN=target/debug/mutsu MUTSU_FUDGE=1 prove -e 'scripts/run-roast-test.sh'
roast/S17-lowlevel/cas.t`) passed 5/5 on current `main`. Standalone repros with up
to 16 competing threads and 2000 nodes each (32,000 total CAS attempts), plus a
variant that artificially widens the read-to-CAS window with 2000 iterations of
busy-work between `my $orig = $!head` and the `cas($!head, $orig, $next)` call,
also completed with the correct node count every time.

**The ticket's own hypothesis doesn't hold either.** It suspected the compiler's
CAS pre-sync for attribute variables (`src/compiler/expr_call.rs`, the
`GetLocal`+`SetGlobal`+`Pop` sequence emitted before `__mutsu_cas_var` for a
`!`-prefixed target) races the real atomic swap by writing a stale local snapshot
into the shared attribute cell. Tracing `OpCode::SetGlobal`'s handler
(`src/vm/vm_exec_dispatch.rs`) shows this is not what happens: its end-of-handler
cell mirror (`mirror_attr_env_to_cell`) is gated on
`is_array_hash_attr_twigil`, which matches only `@!`/`@.`/`%!`/`%.` — a plain
scalar `!head` is explicitly excluded (see the comment on
`is_subscriptable_attr_twigil` in `src/vm/vm_var_assign_computed_attr.rs`: "a
scalar attribute is cell-direct... deliberately NOT used by the whole-value ops
"). So the pre-sync's `SetGlobal` writes only into the legacy `env` mirror for a
scalar attribute, never into the live cell `builtin_cas_var`'s
`self_attr_cell_target` path (`src/runtime/builtins_atomic_cas.rs`) actually reads
and CASes — it cannot cause the described lost update. (The pre-sync still isn't
provably dead: `self_attr_cell_target` requires `self` to already be a bare
`Instance`, so a role/mixin method reaching `cas($!attr, ...)` through a `Mixin`
wrapper falls back to the by-name `shared_vars` store, where the env pre-sync is
the only thing keeping that path's read current — matching the precedent in
`builtins_atomic_cas.rs` for a boxed-lexical CAS reached through a role method,
roast `S12-construction/roles-6e.t`. Removing the pre-sync outright was judged too
risky to land without a case that actually exercises that fallback.)

**`cas-int.t`'s debug failure is a timeout-budget artifact, not a race.** Running
it directly (no `prove` wrapper) shows all 24 subtests pass given enough wall
time — the 2-dimensional integer array CAS section (4 threads × 10,000 iterations
× 4 attempts of a busy-retry loop) just takes past 30 seconds in an unoptimized
debug build. `scripts/run-roast-test.sh`'s timeout is what turns that into "Bad
plan: planned 24, ran ~11". A release build finishes the same file in 5-7
wallclock seconds every time.

## Disposition

No code change. The ticket is closed as not currently reproducible; if a similar
symptom resurfaces, don't re-derive the pre-sync-races-the-cell hypothesis — it
was checked and ruled out for the plain-Instance case. Look instead at whether the
report came from a debug-build timeout (as `cas-int.t`'s did) before assuming a
data race.
