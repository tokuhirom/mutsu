# proto method body now runs for a type-object invocant

Found by the ADR-0019 E9c design pass (2026-08-13, Rakudo v2026.06). A `proto
method` with a non-trivial body (side effects before `{*}`) ran that body for
instance invocants, but for a TYPE-OBJECT invocant mutsu dispatched straight
to the multi candidate and never ran the proto body.

```raku
class P {
    proto method m($x) { say "proto($x)"; {*} }
    multi method m(Int $x) { say "int($x)" }
}
P.m(5);
# raku:  proto(5) / int(5)
# mutsu: int(5)            (proto body never ran)

class Q is P { multi method m(Str $s) { say "str($s)" } }
Q.m("a");
# raku:  proto(a) / str(a)
# mutsu: str(a)
```

`try_proto_method_body` (`src/runtime/dispatch_proto.rs`) required
`ValueView::Instance` for its class-name lookup and returned `None` for
anything else, so the whole proto-governance layer (body execution,
`ProtoMethodCtx`, the `{*}` redispatch) was bypassed for a type-object
receiver — the call fell through to ordinary multi resolution.

Fixed by also accepting a `ValueView::Package` invocant (excluding `new`,
which already has its own proto interception via `dispatch_new` earlier in
the dispatch chain, so widening the generic gate to it would risk running
that constructor's proto body twice). `run_proto_method` and the `{*}`
redispatch handler (`dispatch_proto_call.rs`) already supported a `Package`
invocant — that machinery is shared with the `.new` proto-constructor path,
which already exercises it — so no further changes were needed there; `self`
inside the proto body is correctly the type object itself, matching raku.

New test: `t/proto-method-body-type-object-invocant.t`.
