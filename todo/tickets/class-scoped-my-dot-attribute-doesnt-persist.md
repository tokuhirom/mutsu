# `my $.counter` (class-scoped shared "attribute") doesn't persist mutations across method calls

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Language/traits.rakudoc:42`). Re-verified against `raku` v2026.06 on 2026-08-26
and the mechanism located; still open.

## Minimal repro

```raku
class Foo {
    my $.counter;
    method imm() { return $.counter++ }
}
say Foo.imm for ^5;
```
- `raku`: `0 1 2 3 4`
- `mutsu`: `0 0 0 0 0` — the counter never advances

## What raku declares

`my $.counter;` in a class body declares a class-scoped lexical **and** a public
accessor method. Measured:

```
Foo.^attributes            ()            # NOT an attribute
Foo.^methods               (POPULATE counter imm peek)
Foo.^can('counter').elems  1
Foo.counter                5             # readable on the type object
Foo.new.counter            5             # and on an instance -- one shared slot
```

## What mutsu does

`Foo.^can('counter')` is `0` and `counter` is absent from `Foo.^methods` — no
accessor is generated at all. The declaration *is* recognised: it takes the
`decl.is_our || decl.is_my` arm in
`src/runtime/registration_class_body_attr.rs`, which stores the initial value in
`ClassDef::class_level_attrs` and then `SkipTail`s past the per-instance
attribute registration. Reads/writes from *outside* a method work through the
`has_class_level_attr` fallbacks in `src/runtime/methods_native_bypass.rs` and
`set_class_level_attr` in `src/runtime/methods_mut_method_lvalue.rs` — this is
what `t/class-level-attrs.t` already covers.

The gap is **inside a method body**. `$.counter` there compiles to the plain
variable `Var(".counter")` (bytecode: `PostIncrement(".counter")`), and mutsu's
`$.attr` mechanism is a per-call env mirror: on method entry the VM seeds an env
entry `".attr"` from the *instance's* attribute map
(`src/vm/vm_method_dispatch.rs:661` / `:1799`) and writes it back on assignment
(`src/vm/vm_var_assign_local.rs:412`, `src/vm/vm_misc_assign.rs:600`,
`src/vm/vm_var_assign_set_local.rs:1976`). A class-level attribute is in
`class_level_attrs`, not in the instance map, so `".counter"` is never seeded
(reads `Nil`, hence the `0` from `++`) and the write-back has no destination —
it dies with the call frame.

## Why this is more than a one-line fix

Three separate pieces are needed, and the middle one touches the dual-store
mechanism CLAUDE.md is actively paying down:

1. **Accessor generation.** `my $.x` / `our $.x` must install a real `counter`
   method (visible to `^can`/`^methods`), not rely on dispatch fallbacks.
2. **Method-body routing.** `$.x` inside a method must resolve to the
   `class_level_attrs` slot of the *enclosing* class. Adding a fourth
   seed-and-writeback site to the `".attr"` env mirror would work but grows the
   dual store; routing the dot-twigil read/write straight at the class-level
   store (a canonical cell, per ADR-0013/ADR-0039's "one cell" principle) is the
   architecturally right shape and is what should be built.
3. **`self`-less invocation.** `Foo.imm` is called on the *type object*, so the
   routing must not depend on an instance being present.

## Affected files (starting point)

- `src/runtime/registration_class_body_attr.rs` — the `is_our || is_my` arm
  (accessor generation)
- `src/vm/vm_method_dispatch.rs` — method-entry `".attr"` seeding
- `src/vm/vm_var_assign_local.rs`, `src/vm/vm_misc_assign.rs`,
  `src/vm/vm_var_assign_set_local.rs` — `".attr"` write-back
- `src/runtime/methods_native_bypass.rs`, `src/runtime/methods_mut_method_lvalue.rs`
  — the existing `has_class_level_attr` / `set_class_level_attr` accessors
- `t/class-level-attrs.t` — the existing pin, to be extended with the
  inside-a-method case
