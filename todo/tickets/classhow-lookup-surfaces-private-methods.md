# `.^lookup(name)` finds a private method; real Raku answers `Nil`

Found while investigating ADR-0019 E7 step 5 (`todo/deep/adr0019-e5-e7-entry-routing.md`
"E7 step 5"), a real correctness fix for `Interpreter::classhow_lookup`
(`src/runtime/methods_classhow_lookup.rs`). Unrelated to that fix's MRO-walk scope, so
recorded here instead of expanding the E7 step 5 PR.

**Repro**:

```raku
class A {
    method !secret { "shh" }
}
say A.^lookup("secret").defined;
```

- `raku`: `False` — `.^lookup` does not surface a private method by its bare (unqualified,
  no `!`) name.
- mutsu (`target/debug/mutsu`, confirmed on the pre-E7-step-5 and post-E7-step-5 code —
  this is not something the MRO-walk fix touches): `True`.

**Root cause (read from the code, not yet fixed)**: `classhow_lookup`'s first tier
(`class_def.methods.get(method_name)`) never checks `def.is_private` before building and
returning a `Value::make_sub` for the match — every other visibility-aware dispatch path in
the codebase (`resolve_method_with_owner_impl`'s `Public` filtering,
`resolve_sequence`'s `MethodVisibility::Public` tier from ADR-0019 E7 step 3) explicitly
skips `is_private` defs, but `classhow_lookup` does not.

**Why this is left open**: it is a visibility-filtering bug, not an MRO-walk bug — a
different bug shape from what E7 step 5 targeted, so folding it in would violate the
"one consumer family / one bug per sub-PR" discipline the whole E7 box has followed.
Fixing it requires deciding whether ALL of `classhow_lookup`'s per-level candidates
(own-class and inherited, after the E7 step 5 MRO-walk fix) should skip `is_private`, and
whether that should reuse `resolve_sequence`'s `MethodVisibility::Public` filter directly
(architecturally the more unified answer, consistent with E7's general direction) instead
of a local ad-hoc `!def.is_private` check.

**Suggested fix shape**: in `classhow_lookup`'s per-level lookup loop, skip a level's
`defs.first()` when `is_private` (or, more thoroughly, filter `defs` to non-private before
taking `.first()`), matching the `is_private` skip every other visibility-aware resolver in
the codebase already applies.
