# A `for`-loop pointy-block variable now stays frozen when captured by a closure created inside another closure built in the loop body

A `for`-loop pointy-block variable must be a fresh per-iteration binding: a
closure that captures it should see the value of *its own* iteration
forever. This already worked for a closure created directly in the loop
body, but was lost the instant the closure was created one closure-*call*
deep from the loop body — an IIFE-factory shape:

```raku
my $callback = -> { "base" };
for (10, 20) -> $v {
    $callback = -> $fn { -> { "$v:{$fn()}" } }($callback);
}
say $callback();   # raku: 20:10:base — mutsu (before this fix): 20:20:base
```

This exact shape is `Cro::HTTP::Router`'s `around` middleware chaining
(`RouteSet.transformer`), observed as `t/http-router.rakutest` test 437 ("The
around blocks are called in top-to-bottom order").

## Design

The fix followed [ADR-0027](../../docs/adr/0027-loop-frozen-value-capture-cascade.md)
(designed by a Fable agent, implemented in a follow-up Sonnet session): a new
per-frame register `Interpreter::frame_owned`, mirroring
`frame_authoritative`'s save/seed/consult lifecycle, cascades a loop-frozen
capture vouch through nested closure creation — but gated on the live value
kind at each consuming closure's creation. A name cascades as loop-frozen
only when its currently captured value is plain; never when it is a
`ContainerRef` (a live shared cell, already force-installed by the
unconditional cell-overwrite merge). This preserves the exact invariant that
originally excluded `owned_captures` from `frame_authoritative` (the
`roast/S17-lowlevel/lock.t` stale-snapshot busy-wait hazard) while fixing the
nesting bug.

## An additional prerequisite gap found during implementation

The ADR's analysis assumed the OUTER closure in the repro (created directly
in the loop body) was already correctly marked `owned_captures = {v}` by the
existing mechanism. Implementing and testing the cascade against the actual
repro showed this assumption was wrong: a `for`-loop's own pointy parameter
is bound via a direct `env`/local-slot store
(`vm_for_loop_body.rs`'s per-iteration binding), which never runs through
the generic declaration path (`exec_set_var_dynamic_op`) that populates
`Interpreter::loop_local_vars` for an ordinary loop-body `my` declaration.
So `compute_owned_captures` never marked a closure over the pointy parameter
itself as loop-owned in the first place — invisible for a closure invoked
standalone (its captured value has no competing binding to lose to at
invocation time), but exactly the missing piece for the nested-IIFE shape,
where the inner closure IS invoked from a context with a colliding live
binding of the same name. Fixed by having the for-loop body's per-iteration
setup register its own pointy parameter name(s) into `loop_local_vars` too
— reusing the exact name set `push_loop_local_scope` was already computing
for ADR-0023's `active_loop_param_names`, so this is additive bookkeeping,
not a new computation.

## Result

pin = `t/loop-var-nested-closure-freeze.t` (direct-closure baseline,
independently-stored-IIFE baseline, no-loop negative pin, the bug repro
itself, a depth-3 transitivity variant, and a mutated/cell-valued capture
liveness pin). Verified against `raku` for every case.
`roast/S17-lowlevel/lock.t` (the canary for the live-cell gate) stays green
across repeated runs. `t/http-router.rakutest` test 437 itself could not be
directly re-measured — the vendored Cro::HTTP suite currently hits an
unrelated, pre-existing parse-time failure (`Can't use unknown trait 'is' ->
'query'`, tracked in
`todo/deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md`)
before reaching that test — but the isolated repro this ticket was filed
against (Cro-independent, the file's own primary reproduction case) now
matches `raku` exactly.
