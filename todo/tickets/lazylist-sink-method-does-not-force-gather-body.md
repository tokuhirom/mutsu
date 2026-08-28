# An explicit `.sink()` method call on a gather-based `LazyList` never runs the body

## Repro

```raku
my sub f2() { gather { die "boom" } }
f2().sink;
say "after";
```

- raku: dies (`boom`), never prints `after`.
- mutsu: prints `after` — the `die` inside the `gather` body never runs at
  all.

Same gap for a `return` inside the gather: `f2().sink` on `my sub f2() {
gather { return } }` silently no-ops instead of raising a catchable
`X::ControlFlow::Return`.

## Root cause

`src/builtins/methods_0arg/dispatch_core_math.rs`'s native `"sink"` method
arm has a dedicated `ValueView::LazyList(ll)` case:

```rust
ValueView::LazyList(ll) => {
    // Sinking a gather-based LazyList marks it as consumed.
    // Needed for `$s-lazy.sink; $s-lazy.is-lazy` to throw X::Seq::Consumed.
    let is_gather = ll.env.get("__mutsu_lazylist_from_gather").is_some();
    if is_gather {
        crate::value::lazylist_consume(&ll);
        Some(Ok(Value::NIL))
    } else {
        None // fall through to runtime for non-gather lazy lists
    }
}
```

For the gather case it calls `lazylist_consume(&ll)` — which only flips the
LazyList's internal "already consumed" bookkeeping (so a LATER `.is-lazy`
throws `X::Seq::Consumed`, per the roast assertion the comment cites) — and
returns `Some(Ok(Value::NIL))` immediately, WITHOUT ever running the gather's
body. This native dispatch layer (`builtins/methods_0arg/`) is documented as
"Pure Rust native methods... No AST execution needed" — it has no
`&mut Interpreter` and genuinely cannot run the gather's compiled bytecode.
So it needs to return `None` here too (fall through to the runtime, exactly
like the `else` branch already does for a non-gather lazy list), letting the
interpreter-level dispatch actually force it.

This is adjacent to, but distinct from, the bug fixed by
`t/return-target-dead-reaches-nearest-catch.t` (a `return`-signal delivery
bug once forcing DOES happen) — here forcing never happens at all for the
explicit `.sink()` METHOD call form specifically. Every OTHER way of sinking
a gather-based LazyList already forces correctly:

- a bare discarded statement (`f2();`) — goes through `OpCode::SinkPop` /
  `sink_discarded_call_value`, which already force it.
- `.Str`/`~` and the other methods in `should_force_lazy_list`'s whitelist
  (`src/runtime/methods_native_bypass.rs`) — go through
  `force_lazy_list_bridge`.

Only `.sink()` called explicitly as a method is wired to this
short-circuiting native arm instead of either of those paths.

## Fix sketch (not done here — scope/time boundary of a different task)

1. Change the `ValueView::LazyList(ll)` gather arm above to `return None`
   (fall through), matching the non-gather `else` branch.
2. In the runtime fallback (`src/runtime/methods_call_dispatch.rs`), add
   `"sink"` to `Interpreter::should_force_lazy_list`'s whitelist
   (`src/runtime/methods_native_bypass.rs`) so the "Force LazyList and
   re-dispatch as Seq" block (around line 3794) actually forces it via
   `force_lazy_list_bridge`, then re-dispatches `.sink` on the resulting
   `Value::seq(items)`.
3. Verify the re-dispatch still satisfies the ORIGINAL roast assertion the
   removed short-circuit was protecting (`$s-lazy.sink; $s-lazy.is-lazy`
   must throw `X::Seq::Consumed`) — a freshly-built `Value::seq(items)` is
   a DIFFERENT `Arc<SeqBody>` from the original `ll`, so consuming IT does
   not, by itself, mark the ORIGINAL `LazyList` (still referenced by
   `$s-lazy`) as consumed. Likely needs an explicit
   `crate::value::lazylist_consume(&ll)` call alongside the forced
   re-dispatch, not instead of it. Confirm against `raku` directly and find
   the exact roast test(s) this `is-lazy`/`X::Seq::Consumed` comment refers
   to before changing the consumption bookkeeping.

## Impact

Narrow: only the explicit `.sink()` METHOD call on a `gather`-sourced lazy
list. Found incidentally while regression-testing
`news/2026-08/gather-lazy-force-signal-delivery.md`; not blocking any
currently-tracked roast test.
