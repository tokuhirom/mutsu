# `emit` reaches the supply from a nested sub

`emit` is caught by the innermost *dynamically* enclosing `supply`, not the
lexically enclosing one, so a routine that is not written inside the block still
emits into it when called from within:

```raku
sub e($x) { emit $x }
supply { e(1); emit 2 }     # raku emits 1 then 2
```

mutsu emitted only `2`. The value from the nested sub vanished silently — no
error, no warning, just a missing item.

The cause was the lowering. `supply { ... }` is compiled by binding an emitter
`Supplier` as the body's parameter (`__mutsu_supply_emitter_<id>`) and
rewriting every `emit x` **written in the body** to
`$__mutsu_supply_emitter_N.emit(x)`. A sub's body is not rewritten — a TODO in
`rewrite_supply_stmt` had recorded exactly this, noting that rewriting it
surfaced a closure-capture gap, so nested-sub `emit` was left as a runtime
no-op. The bare `emit` builtin then raised `CX::Emit`, which nothing downstream
of a sub call catches.

The fix drops the lexical approach for that case and makes the emitter
dynamically available, which is what raku's control-exception semantics amount
to:

- `Interpreter::active_supply_emitters` is a stack of the emitter `Supplier`s
  whose supply code is currently running.
- `run_on_demand_body` pushes the emitter it just built around the body call, so
  a sub called from the body finds it.
- `call_supply_tap` wraps the tap/`whenever` callback invocations in
  `native_supplier_methods`. It recovers the emitter from the callback's own
  captured env (the callback *is* written inside the supply block, so it
  captured `__mutsu_supply_emitter_<id>`) and pushes it for the duration of the
  call. An unrelated tap callback has no such lexical and pushes nothing.
- The `emit` builtin now emits through the top of that stack, falling back to
  the on-demand emit buffer, and only raises `CX::Emit` when there is no
  enclosing supply at all — which is still what a `CONTROL` block observes.

This covers all four shapes: `emit` in the body, in a sub declared in the body,
in a sub declared outside it, and in a sub called from a `whenever` over either
a static `Supply` or a live `Supplier`.

## Effect on Cro

`Cro::HTTP2::GeneralParser` emits every parsed message through a helper declared
inside its `supply` block:

```raku
sub emit-response($sid, $message) {
    with %push-promises-by-promised-id{$sid}:delete { .set-response($message) }
    else { emit $message }
}
```

so the entire HTTP/2 layer produced nothing at all. Against the upstream suite:
`http2-request-parser` goes from 0 passing to 60 of 64, `http2-response-parser`
from 0/2 to 8 passing with a clean exit, `http2-request-serializer` from 4 to 8,
and `http2-frame-serializer` from 3 subtests reached to 13.

Pinned by `t/emit-from-a-nested-sub.t`, which passes identically under raku.
