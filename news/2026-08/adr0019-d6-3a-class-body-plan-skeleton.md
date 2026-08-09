# ADR-0019 D6-3a: typed `body_plan` skeleton for class declarations

`CompiledClassDeclPlan` gained `body_plan: Vec<ClassBodyOp>`, the first slice of D6-3
("`body_plan` introduction, additive") from the D6/D9 `legacy_body` removal design
(`todo/deep/adr0019-d6-d9-legacy-body-removal.md`).

`ClassBodyOp` is a new typed enum — `Attr`, `Method`, `Does`, `ClassSub`, `CodeAlias`,
`ProtoMethod`, `LeavePhaser`, `Other` — computed at plan lowering by a new
`class_body_plan` free function that mirrors `run_class_body`'s own dispatch loop
exactly: the same `SyntheticBlock`-flattened top level, classified the same way the
runtime `match` does, with nested-sub `has` declarations appended at the end (the same
order `own_attribute_names` already uses). The already-typed arms carry only a
name/marker — `Attr`/`Does`/`ClassSub` a `Symbol`, `Method` nothing at all — since
their real payload already lives in `attr_decls`/`method_decls`/`parent_arg_chunks`;
the remaining arms (`CodeAlias`/`ProtoMethod`/`LeavePhaser`/`Other`) carry `chunk:
None` plus the raw statement, to be precompiled by D6-3b/c.

This slice is purely additive: no non-test code reads `body_plan` yet (D6-3d wires
`run_class_body` to it, behind an env-var instrument per the C6e-3a precedent). Since
`cargo clippy -- -D warnings` lints the non-test target, a `#[cfg(test)]`-only reader
does not silence the dead-code lint on its own — the field and enum carry
`#[allow(dead_code)]`, matching the existing `current_pos`/`is_rw` precedent in the
same file, alongside a compiler unit test
(`class_declarations_precompute_body_plan`) that reads the field and pins the
invariant the design calls for: `body_plan.len()` equals the flattened statement
count (independently re-derived in the test itself, since the parser's interstitial
`SetLine` markers make a hand-counted literal brittle), and the typed-op sequence
matches source order for one example of every kind.

Verified via the full `t/` suite (28,019 tests) and the `S12-class` roast files.

Next: D6-3b (compiling the `Other` arm's chunks, the largest and highest-value
reader per the reader inventory).
