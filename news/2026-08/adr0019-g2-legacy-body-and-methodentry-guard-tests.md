# ADR-0019 G2 closed: legacy_body and MethodEntry-mirror guard tests

ADR-0019's completion gate G2 asks for architectural guard tests covering four regressions: a
migrated declaration re-entering `stmt_pool`, a declaration plan retaining `legacy_body`, dispatch
bypassing the canonical `MethodEntry` table, or introspection reading a hand-maintained name table.
The `stmt_pool` clause already had coverage; the other three were verified ad hoc at their own
closing PRs but had no permanent regression test.

Two new Rust unit tests close the gap:

- `legacy_body_survives_only_on_the_proto_decl_plan` (`src/compiler/mod.rs`) compiles a sub, a
  non-trivial proto, a role, and a class together and Debug-formats one plan of each kind, asserting
  the `legacy_body:` field is absent from `CompiledSubDeclPlan`/`CompiledClassDeclPlan`/
  `CompiledRoleDeclPlan` and present only on `CompiledProtoDeclPlan` — the one deliberate, permanent
  exception ADR-0019's C8 already decided to keep.
- `class_def_carries_no_method_mirror_field` (`src/compiler/mod.rs`) Debug-formats a default
  `ClassDef` and asserts no `methods:` field boundary appears, distinct from the legitimate
  `native_methods:` field. `ClassDef` gained a `Debug` derive (`src/runtime/decl_types.rs`) to make
  this test possible; since `ClassDef` also derives `Default`, a reintroduced `methods` field would
  otherwise compile silently everywhere and default to empty without forcing any call site to notice
  — this is exactly the mirror F4c removed to make dispatch read only from the canonical `Registry`/
  `MethodEntry` table.

The fourth clause, "introspection reads a hand name table," turned out to already be covered: F3's
cutover repurposed the pre-existing `raw_rows_cover_every_introspection_name_in_order`
(`src/builtins/native_method_row.rs`) from a one-time verification into a live regression guard that
asserts introspection and `RAW_ROWS`'s `INTROSPECTABLE`-flagged subset stay equal and in order for
every builtin owner, on every test run.

All four G2 sub-clauses are now enforced by permanent, always-run tests, closing ADR-0019's G2.
