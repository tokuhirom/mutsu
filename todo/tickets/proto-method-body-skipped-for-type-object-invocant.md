# proto method body is skipped when the invocant is a type object

Found by the ADR-0019 E9c design pass (2026-08-13, Rakudo v2026.06). A `proto method` with a
non-trivial body (side effects before `{*}`) runs that body for instance invocants, but for a
TYPE-OBJECT invocant mutsu dispatches straight to the multi candidate and never runs the
proto body.

## Divergence

```raku
class P {
    proto method m($x) { say "proto($x)"; {*} }
    multi method m(Int $x) { say "int($x)" }
}
P.m(5);
# raku:  proto(5) / int(5)
# mutsu: int(5)            (proto body never runs)

class Q is P { multi method m(Str $s) { say "str($s)" } }
Q.m("a");
# raku:  proto(a) / str(a)
# mutsu: str(a)
```

The candidate itself is still resolved correctly (including inherited candidates); only the
proto body's execution is lost, so the divergence is observable exactly when the body does
more than `{*}` (logging, argument normalization, rw mutation before redispatch).

## Root cause

The proto interception entry, `try_proto_method_body` (`dispatch_proto.rs:300-401`), requires
`ValueView::Instance` at `:317-320` and returns `None` for type objects, so the whole
proto-governance layer (body execution, `ProtoMethodCtx`, the `{*}` redispatch) is bypassed;
the call falls through to ordinary multi resolution.

## Where it sits relative to ADR-0019 E9c

Adjacent, not in scope: E9c rewrites the `{*}` handler (`call_proto_dispatch`), not the
interception gate. Fixing this means teaching `try_proto_method_body` (and
`run_proto_method`'s invocant plumbing, `dispatch_proto.rs:244-298`) to accept a type-object
invocant — `self` inside the proto body must then be the type object. Verify with raku first
what `self.new`-style shapes inside a proto body do for type-object invocants before wiring.
The pin for the probes above lands with the fix.
