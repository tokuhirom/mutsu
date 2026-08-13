# An explicit `proto method` in a subclass no longer inherits the parent's candidates

Found by the ADR-0019 E9-pre raku ground-truth campaign (2026-08-12, verified against
Rakudo v2026.06). This was one of two divergence tickets left open after the E9a
flat-deferral-expansion cutover landed; the other,
`native-array-push-defer-fallback-broken.md`, is fixed in a sibling PR.

## The bug

```raku
class P { multi method m(Int $x) { "p-int" } }
class C is P {
    proto method m($x) { {*} }
    multi method m(Str $x) { "c-str" }
}
C.new.m(5);
```

In raku this raises `X::Multi::NoMatch` — the explicit `proto method m` written on `C`
starts a fresh candidate set, so `P`'s `Int` candidate is simply not reachable through
it. mutsu instead resolved `P`'s candidate and returned `"proto-p-int"`.

The inverse direction already worked and stays correct: a proto declared on a *parent*,
with no proto written in the child, keeps governing candidates the child adds (pinned by
`t/proto-star-cross-mro-candidates.t`) — only a proto the child (or some middle class)
writes *itself* draws a boundary.

## Root cause and fix

Proto-method `{*}` redispatch (`dispatch_proto_call.rs`) re-enters
`call_method_with_values` by name, which then walked the *entire* MRO for multi
candidates regardless of which class actually declared the governing proto. There was no
way to tell, from inside that redispatch, "stop looking past the class that wrote this
proto."

A new `Interpreter::proto_redispatch_boundary: Option<(Symbol, Symbol)>` field records
`(method_name, owner_class)` — `owner_class` is whichever class `lookup_proto_method`'s
MRO walk actually found the explicit proto body on. It is set bracket-style (saved and
restored, not a one-shot consumed flag) around the redispatch call, so a candidate that
itself triggers a *nested* proto redispatch does not clobber the outer boundary. Both
places that collect multi candidates across the MRO — the real dispatch walk in
`resolution_method.rs` and the `X::Multi::NoMatch` diagnostic's signature-listing walk in
`class.rs` — now truncate the MRO at that owner whenever the boundary names the method
being resolved, so an ancestor's candidates beyond the proto's own declaring class are
invisible to it.

The boundary is only set when `lookup_proto_method` actually names an owner for an
in-flight `{*}` redispatch, so the purely implicit case (no proto written anywhere below
the ancestor that owns it) never enters this branch and stays untouched.

New pin: `t/proto-explicit-child-fresh-candidates.t`, including a mid-MRO case where the
governing proto is neither the receiver's own class nor the ultimate ancestor, confirming
the boundary tracks the actual declaring class rather than just "the receiver's class."
