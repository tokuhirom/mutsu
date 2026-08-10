# `done` in a `supply { }` block no longer leaks `CX::Return` when created inside a method

A bare `done` inside a `supply { ... }` block desugars to `$emitter.done();
<terminator>` (`src/parser/primary/ident/supply.rs`). The terminator used to be
an ordinary `Stmt::Return(Nil)`. For a closure created inside a *method*, a
routine `return` signal gets stamped with that method's callable id (via the
captured `__mutsu_callable_id` env entry) and propagates to the enclosing
routine frame — but the method already returned by the time the on-demand
body runs (at tap time), so the signal had nowhere to land and escaped all the
way to the tap/react driver as an uncaught `CX::Return`, killing the supply
with a quit instead of completing it normally. A closure created in a plain
`sub` has no such id, so it happened to work by accident (caught by the
"no target: fall through to catch locally" branch).

Minimal repro:

```raku
class A {
    method pp() { supply { done } }
}
react { whenever A.new.pp() -> $x { say "got $x" } }
say 'ok';
```

mutsu previously died with `A react block: Died because of the exception: CX::Return`;
now it prints `ok`, matching `raku`.

## Fix

Gave the desugar its own dedicated terminator, `ast::Stmt::SupplyBodyDone`
(compiled to `OpCode::SupplyBodyDone`, raising a new `Control::SupplyBodyDone`
signal distinct from both `Control::Return` and `Control::ReactDone`). Unlike
`Return`, it never consults a target callable id — it always ends just the
closure that raised it (the on-demand body lambda itself, or a nested
`whenever` closure within it), regardless of whether that closure was created
inside a sub or a method. Unlike `ReactDone`, it never terminates an
*enclosing* react loop — nesting a `done`-completed supply inside a react
stays scoped to that inner supply.

The signal is absorbed in three places, matching the different closure
dispatch paths a supply/whenever body can run through:

- `vm_closure_dispatch.rs`'s per-frame closure boundary (mirrors the existing
  `is_succeed()` local-catch arm).
- `run_on_demand_body` (`supply_promise.rs`), for the top-level on-demand
  body lambda.
- `call_supply_tap` (`supply_promise.rs`), for a nested `whenever` closure
  within a `supply { }` block.
- The thread-driven consumer loops that already special-cased
  `is_react_done() || is_last()` to avoid treating a supply's own control
  signal as an unhandled exception (`native_methods/encoding.rs`'s
  `run_supply_act_loop`, `native_supplier_methods.rs`,
  `native_supply_mut_methods.rs`, `vm_react_subscriptions.rs`,
  `supply_promise.rs`'s replay loops) now also recognize
  `is_supply_body_done()` the same way — a `whenever` body's `done`, nested
  inside a `supply { }` block and driven asynchronously (e.g. from a
  `Supply.interval` timer thread), raises this same signal.
- `vm_try_catch_ops.rs`'s `try`/CONTROL control-signal pass-through lists
  also recognize it, so a `try { done }` inside a supply body isn't
  mistakenly routed to a CATCH handler.

Pinned by `t/supply-done-in-method-supply-block.t`. Verified against `raku`
for the ticket's repro variants (plain `.tap`, `react { whenever }`, `done`
mid-body not running trailing statements, `emit` before `done`) and the
original Cro-level repro (`Cro::HTTP2::Response.push-promises` returning
`supply { done }` from a method, used by `Cro::HTTP2::ResponseSerializer`).

See `todo/tickets/supply-done-in-method-supply-block-escapes-as-cx-return.md`
(now resolved) for the original diagnosis.
