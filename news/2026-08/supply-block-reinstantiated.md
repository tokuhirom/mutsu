# Two live instances of one `supply` block stop feeding each other

Building a supply chain by calling the same routine repeatedly fed the pipeline
its own output forever:

```raku
sub xform(Supply $in --> Supply) {
    supply whenever $in -> $v { emit "w($v)" }
}
my $s = supply emit 'REQ';
$s = xform($s);
$s = xform($s);
$s.tap(-> $v { say $v });        # w(w(REQ)) in Rakudo; an infinite loop in mutsu
```

Two *different* routines chained fine — the trigger was one routine used twice.

## Why

`supply { … }` lowers to `Supply.on-demand(-> $__mutsu_supply_emitter_N { … })`,
and `emit x` inside the block is rewritten to `$__mutsu_supply_emitter_N.emit(x)`.
That name is generated per **parse site**, which keeps sibling supply blocks
apart, but every runtime **instance** of one parse site shares it. Tapping the
outer instance runs the inner instance's body nested inside it, so the inner
binding of the shared name was live when the outer body's `whenever` callback
finally ran — and the outer `emit` went into the inner supply, whose value came
straight back to the outer body.

`exec_whenever_scope_op` already guards against exactly this class of shadowing:
it hands each `whenever` callback a list of `owned_lexicals` (the closure's
`authoritative_captures`), names that a same-named lexical in whatever frame
dispatches the callback must never shadow. But that list was built from the
supply body's `my` declarations, and the emitter is the body's *parameter*, so it
was never in it. It has no compiled local slot either — a supply body's `locals`
is empty — so it could not be recovered from the code object's local table.

## Fix

`CompiledCode` now carries `supply_emitter_sym`, the interned emitter parameter
name, set wherever `is_supply_block_body` is set: in the compiler
(`expr_closure.rs`, from the lambda's parameter) and on the interpreter's
re-entrant carrier path, where it rides along with `pending_supply_block_body`
from `resolution_call_sub` into `resolution_eval` — falling back to the
`SubData`'s parameter list for a body compiled on the fly with no
`CompiledCode`. `exec_whenever_scope_op` adds it to `owned_lexicals`.

This is what a Cro middleware pipeline builds — `Cro::HTTP::Router`'s
`!append-middleware` wraps its pipeline once per component, and
`Cro::HTTP::Middleware::Request.transformer` wraps again — so a `before-matched`
middleware in a route block used to spin forever instead of handling one
request.

Pinned by `t/supply-block-reinstantiated.t`.
