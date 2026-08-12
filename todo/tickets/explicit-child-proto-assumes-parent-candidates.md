# An explicit proto in a subclass must NOT assume parent multi candidates

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06).

## Divergence

```raku
class P { multi method m(Int $x) { "p-int" } }
class C is P {
    proto method m($x) { {*} }
    multi method m(Str $x) { "c-str" }
}
C.new.m(5);
# raku:  X::Multi::NoMatch — the explicit child proto only sees C's own candidates
#        ("Cannot resolve caller m(C:D: Int:D); none of these signatures matches: (C $:: Str $x, *%_)")
# mutsu: resolves P's Int candidate through the child proto and returns "proto-p-int"
```

Only an IMPLICIT proto (a `multi method` declaration with no proto at that class) clones the
nearest MRO proto and merges parent candidates. Writing `proto method m` explicitly in the
child starts a fresh candidate set: parent multis of the same name become unreachable through
it. The inverse direction — proto in the PARENT governing candidates a child adds — works in
both and is pinned by `t/proto-star-cross-mro-candidates.t`.

## Affected code

Proto-method `{*}` dispatch: `src/runtime/dispatch_proto_call.rs` (re-enters
`call_method_with_values` by name with `proto_method_skip`, which then walks the full MRO
candidate set regardless of which class declared the governing proto). After ADR-0019 E8c the
proto is found via `Registry::method_entry_proto` per MRO level (`dispatch_proto.rs`); the fix
needs the candidate-collection step to stop at/filter by the proto's own declaring class when
that proto is explicit.

Requires distinguishing explicit from implicit protos in the registry (mutsu currently may not
record which classes declared an explicit proto vs inherited one — verify while fixing).

The E9-pre pin for this (a `throws-like X::Multi::NoMatch`) lands with the fix.
