# A `supply` block's lexicals are private to the block

A `my` variable declared inside `supply { ... }` used to alias a same-named
lexical of the calling scope, in **both** directions: the caller's value won
inside the block's `whenever` bodies, and the block's value escaped back into
the caller when the block returned. `todo/deep/supply-block-lexicals-alias-the-caller.md`
recorded it as one bug with two halves that had to be fixed together — fixing
the escape alone made the shadowing worse, because the `whenever` bodies had
been relying on the leak to see the block's lexical at all.

Both halves are fixed now.

## Why the caller won inside a `whenever` body

`run_whenever_with_value` builds each `whenever` callback from the AST with
`Value::make_sub`, capturing the live env by value. Such a callback has no
`CompiledCode`, so it carried no `authoritative_free_vars` and no
`authoritative_captures` — and the call-time env merge in `call_sub_value`
gives the *caller* priority over an unvouched capture (a `Proxy` FETCH body has
to see the current value of a lexical its STORE twin mutates). The callback is
dispatched by whatever thread emits into the supply, and that thread's ambient
env is the main script's, so a caller lexical that merely shared a name shadowed
the block's own: lexical scoping degrading into dynamic scoping.

`exec_whenever_scope_op` now hands `run_whenever_with_value` the set of names
the enclosing supply body declared with `my` (its `my_declared_sym` minus its
free variables), and the new `Value::make_sub_owning` seeds them as the
callback's `authoritative_captures`. The captured binding is then installed with
overwrite, exactly as the compiled-closure path already did. Per-invocation
mutation still accumulates: that state lives in `closure_env_overrides`, keyed
by the closure instance, not in the caller's env.

This is deliberately restricted to a `supply { ... }` body. A `react { ... }`
block compiles *inline* into the enclosing frame, so `my_declared_sym` there is
the whole frame's declarations — including the lexicals sibling `whenever`s are
supposed to share (`t/react-whenever-shared-lexical.t`). The new
`CompiledCode::is_supply_block_body` flag draws that line; it is set from the
generated emitter parameter (`__mutsu_supply_emitter_N`) that
`supply { ... }` lowers to.

The mark needs one relay. `call_sub_value` does not run the lambda's own
compiled chunk — it re-compiles `data.body` from the AST — so the fresh chunk
would lose the flag. `pending_supply_block_body` carries it across that one
call, and is *taken* on entry to `eval_block_value_inner` so a block compiled
from inside the body does not inherit it.

## Why the block's value escaped

The exit merge in `call_sub_value` skips a value equal to the body-entry
snapshot, on the grounds that an unchanged capture is not a mutation. But a name
the body *declared* with `my` is never equal to that snapshot — the snapshot
holds the **caller's** value, so a fresh `my` always looked like a mutation.
Both merge branches now skip a name the body declared with its own `my` (and did
not also use as a free variable), which is the same rule
`push_block_declared_keys` and the compiled-closure exit merge already applied;
only this interpreter path was missing it. Names the closure lexically owns
(`authoritative_captures`) are skipped for the same reason.

## Effect

`Cro::HTTP2::FrameParser.transformer` is exactly the first shape — a
`supply { my $buffer = Buf.new; ... whenever $in { ... } }` whose test emits
from a `start` block while holding its own `my $buffer`. The parser used to see
the test's buffer prepended to the first packet and reject the HTTP/2 preface,
which was all 16 failures of the upstream `t/http2-frame-parser.rakutest`; it is
down to 8, and the same shape appears in `Cro::HTTP::RequestParser` and
`Cro::HTTP::ResponseParser`.

Pin: `t/supply-block-lexical-privacy.t`.

## Left open

Emitting from another thread still leaks the block's lexical back to the
caller — the cross-thread `shared_vars` lane, a different mechanism from the
exit merge fixed here. The ordinary same-thread tap is correct. See
`todo/tickets/supply-block-lexical-leaks-through-thread-lane.md`.
