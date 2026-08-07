# ADR-0019 D2b prep: `ClassAttributeDef` becomes a named struct

D2b ("type full attribute descriptors") needs to attach a `CompiledDeclExpr` child
chunk to an attribute's default/constraint expressions later in the D2 sequence
(D2c), which cannot be done cleanly against a bare 7-tuple. This slice is the
mechanical prerequisite: `ClassAttributeDef` (`src/runtime/mod.rs`) changes from

```rust
pub(crate) type ClassAttributeDef = (
    String, bool, Option<Expr>, bool, Option<Option<String>>, char, Option<Expr>,
);
```

to a named struct with the same field order —
`name, is_public, default, is_rw, is_required, sigil, where_constraint` — and every
construction/destructuring site across the codebase (31 files under
`src/runtime/`) is updated to match. Zero behavior change: this is a pure
rename/reshape, not a logic change.

While converting the positional pattern in `is_native_default_constructible`
(`src/runtime/methods_object.rs`), a pre-existing mismatch surfaced: the old
tuple pattern bound the tuple's 5th field (`is_required`) to a local variable
named `type_constraint`, and the surrounding code used it as though it held
an actual type-constraint string (those live in the registry's
`attribute_types` side table, not on `ClassAttributeDef` at all). This is
carried over unchanged — fixing it is out of scope for a "zero behavior
change" mechanical PR — and is recorded as
`todo/tickets/native-ctor-gate-reads-is-required-as-type-constraint.md` for a
follow-up session.

Verified with `cargo test --lib` (670 tests) and the local class/role/attribute
`prove` surface (88 files, 732 tests), all passing unchanged.
