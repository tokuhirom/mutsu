# A control-flow signal raised while forcing a lazy `gather`/closure return now reaches the nearest CATCH

Two related delivery bugs, filed together as
`todo/deep/lazy-gather-return-outside-scope-swallowed-in-nested-block.md`,
both now fixed: a `return` whose lexically-captured target routine has
already exited the dynamic call stack (a "dead" return — raku converts it
into a catchable `X::ControlFlow::Return` "right at the return site") was
either swallowed outright or delivered to the wrong place once the routine
call chain grew past a single frame.

## Root cause 1: `try`/CATCH always let a `return` signal sail past, even a dead one

`vm_try_catch_ops.rs`'s `try`/implicit-CATCH dispatch treated ANY
`is_return()` error as something it must never catch — correct for a LIVE
return (one still hunting for its target further up the dynamic stack; a
`try` block is not a return boundary and must not intercept it), but wrong
for a DEAD one, which can never be caught by unwinding further no matter how
many more frames it passes through. So a dead return blew straight past
every `try`/CATCH along its way, either escaping as a raw internal message
at the top level, or (once an unrelated routine call frame happened to
"catch" it as an untargeted return, see below) being silently discarded with
no exception ever reaching anyone.

Fix: `Interpreter::return_target_is_live` (new, `accessors_stack.rs`) walks
`routine_stack()` and resolves each live (non-block) frame's CURRENT
registration id via `registration_clone_id` — the exact mechanism ADR-0037
§2.3's `classify_eval_context_routine` already uses to classify an `EVAL
..., context => $ctx` unit's return target as `Live`/`Dead`, generalized
here to an ordinary (non-EVAL) closure's own captured `return`. `try`'s
`is_return()` arm now checks, right at its own dispatch point: does the
signal's `return_target_callable_id` still name a live frame? If not, it
converts to a real `X::ControlFlow::Return` immediately and routes it
through this `try`'s own `dispatch_to_catch_handler` — exactly like any
other exception — instead of endlessly propagating a signal that can
provably never find its target. A genuinely live return is untouched: it
keeps propagating past `try`/CATCH boundaries exactly as before.

## Root cause 2: a `return` forced while reifying a `gather` never got a target at all

Separately, a `return` executed while a lazy `gather` body is being FORCED
(via `.Str`/`~`, a discarded statement, or any other forcing path) never had
`return_target_callable_id` resolved — unlike an ordinary non-routine
closure's own `return`, which `vm_closure_dispatch.rs` already resolves from
the closure's captured `__mutsu_callable_id` env marker. An untargeted
return falls back to "the first enclosing routine call frame catches it
unconditionally" (the correct rule when there really is no target) — so a
gather's dead return was silently "caught" by whatever unrelated routine
happened to be forcing it (e.g. a `subtest`/`call-it`-style wrapper), which
just returned early with no exception ever raised — exactly the shape the
real vendored `Test.rakumod`'s `subtest(&subtests) { subtests(); CATCH
{...} }` hits.

Fix: both of mutsu's gather-forcing entry points —
`Interpreter::force_lazy_list_vm` (`vm_helpers_lazy.rs`, the VM-native
force path) and `Interpreter::force_lazy_list_bridge` (`resolution_lazy.rs`,
the tree-walk-era method-dispatch bridge `.Str` and friends route through)
— now resolve the target the same way `vm_closure_dispatch.rs` does: if the
propagated error is an untargeted `is_return()`, read
`__mutsu_callable_id` off the gather's OWN captured env (the routine that
WROTE the `gather`, snapshotted at `MakeGather` time) and tag it onto the
error before it propagates further. From there root cause 1's liveness
check (and the existing per-frame matching in `vm_call_named_inner.rs`/
`vm_closure_dispatch.rs`) does the rest.

## Verified

- `throws-like 'my sub f() { gather { return } }; ~f()', X::ControlFlow::Return;`
  (the exact `roast/S32-exceptions/misc2.t` assertion that surfaced this)
  now passes under both the real vendored `Test.rakumod`
  (`MUTSU_REAL_TEST=1`) and the native provider.
- Both minimal repros from the ticket (a bare mainline block with its own
  CATCH; a closure invoked through a plain user sub) now match raku exactly.
- A live return (target still on the stack) and a live gather-forced return
  still propagate past an intervening `try` and actually return from their
  target, unchanged.

Pin: `t/return-target-dead-reaches-nearest-catch.t` (9 assertions, all green
under `raku` too).

## A third, narrower, separately-filed gap

Writing the regression test surfaced one more adjacent bug: an EXPLICIT
`.sink()` METHOD call on a gather-based lazy list never runs the body at all
(a short-circuit in the native `.sink` dispatch marks it "consumed" for
`X::Seq::Consumed` bookkeeping without ever forcing it) — distinct from this
finding (forcing never happens at all, vs. forcing happening but its signal
being misdelivered) and not required by any currently-tracked roast test.
Filed as `todo/tickets/lazylist-sink-method-does-not-force-gather-body.md`
rather than folded into this fix.
