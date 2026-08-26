# `my $.x` / `our $.x` (class-scoped shared "attribute") now works inside method bodies, and is a real, reflectable accessor

`my $.counter;` in a class body declares a class-scoped **lexical** plus a
public accessor method — not a per-instance attribute. Reads and writes from
*outside* a method already worked (`t/class-level-attrs.t`, via
`has_class_level_attr`/`set_class_level_attr` in
`src/runtime/class_introspection.rs`), but the mutation never persisted from
*inside* a method:

```raku
class Foo {
    my $.counter;
    method imm() { return $.counter++ }
}
say Foo.imm for ^5;
```

- `raku`: `0 1 2 3 4`
- mutsu (before this fix): `0 0 0 0 0`

## Root cause, re-verified against the ticket's analysis

The ticket's diagnosis of the *symptom* held up, but the codebase had moved on
since it was written: the "`$.attr` is a per-call env mirror seeded from the
instance's attribute map" description was stale. mutsu's attribute access is
actually **cell-direct** (`src/vm/vm_var_assign_computed_attr.rs`,
`read_self_attr_cell`/`write_self_attr_cell`): every scalar attribute read
inside a method goes straight through `self`'s shared `InstanceAttrs` cell, no
per-call env copy at all. The bug was that this cell-direct path only ever
looked at the *instance's* attribute map — a class-level attribute is never
stored there (it lives in `ClassDef::class_level_attrs`), so the lookup missed
every time regardless of whether `self` was even present (`Foo.imm` is called
on the type object, with no instance at all). `read_self_attr_cell` returning
`None` made the slotless-attribute increment path
(`try_slotless_attr_incdec`) fall through to a generic name-keyed scalar
default of `0`, which is discarded when the call frame pops — hence the
constant `0`.

## The fix — three pieces, as the ticket predicted

1. **Accessor generation.** `my $.x` / `our $.x` now install a real,
   reflectable accessor: `collect_class_methods`/`class_method_table`
   (`.^methods`/`.^method_table`), `collect_can_methods` (`.^can`), and
   `classhow_lookup_impl` (`.^lookup`/`.^find_method`, shared by both) all
   check `ClassDef::class_level_attrs` alongside the existing per-instance
   `class_def.attributes` check. Every name in `class_level_attrs` is public
   by construction — a `!`-twigil is a parse error on `my`/`our`, confirmed
   against `raku` — so no extra publicity bookkeeping was needed.
   `Foo.^attributes` correctly stays empty (real Raku: it is a lexical, not an
   instance attribute), matching the existing outside-a-method dispatch.
2. **Method-body routing — the canonical-cell fix.** Rather than adding a
   fourth seed-and-writeback site to a `".attr"` env mirror (the dual-store
   pattern CLAUDE.md's `runtime/mod.rs` warns against extending), the
   cell-direct `read_attr_cell_by_key`/`write_attr_cell_by_key` tail functions
   now fall back to `get_class_level_attr`/`set_class_level_attr` when the
   instance-cell lookup finds nothing. Those two functions already read and
   write `ClassDef::class_level_attrs` directly through the shared
   `Arc<RwLock<Registry>>` — the single canonical store, not a mirror — so
   this reuses the exact machinery `t/class-level-attrs.t` already exercised
   for the outside-a-method case, with zero new storage. `get_class_level_attr`
   /`has_class_level_attr`/`set_class_level_attr` (and the `Interpreter::class_mro`
   they call) were relaxed from `&mut self` to `&self` to make them callable
   from the `&self` cell-access tail functions — a pure widening, since both
   `Interpreter::registry()`/`registry_mut()` are already interior-mutability
   (`&self`) accessors.
3. **`self`-less invocation.** Falls out of (2) for free: the fallback keys
   off `method_class_stack_top_str()` (the currently-running method's
   *declaring* class), not off `self` at all, so it works identically whether
   the receiver is a concrete instance or the bare type object (`Foo.imm`).
   Inheritance was verified against `raku`: a subclass method that never
   declared its own `my $.x` still sees the ancestor's value, because
   `get_class_level_attr`'s existing MRO walk from the declaring class already
   covers it, and a per-instance `has $.x` on a subclass still shadows an
   inherited class-level `our $.x` of the same name (the cell-direct instance
   lookup is tried first and wins).

## Verification

`t/class-level-attrs.t` grew from 11 to 32 assertions, covering: `my`/`our
$.x` read+write from inside a method on the type object; the type object and
every instance sharing exactly one slot across method calls; `@.x`/`%.x`
class-level container attributes (`.push`, key-set); instance-attribute
shadowing of an inherited class-level attribute from inside a method; and
`.^methods`/`.^can`/`.^attributes` introspection. All 32 pass under both
`raku` and mutsu. A targeted roast sweep (`S12-attributes/defaults.t`,
`S12-class/attributes.t`, `S12-enums/misc.t`, `S12-enums/thorough.t`,
`S12-introspection/{attributes,can,meta-class,methods,walk}.t`,
`S12-methods/multi.t`, `S14-roles/mixin-6e.t`) shows no regressions.

## Left open

`Foo.counter++` — post-increment on the accessor *call* from OUTSIDE a method
body (as opposed to `$.counter++` inside one, which this fix covers) — is
still unsupported; recorded separately in
`todo/tickets/class-level-attr-postfix-incdec-outside-method.md` since it is a
distinct, smaller gap in a different code path (a generic method-call-lvalue
mechanism, not the cell-direct attribute path this fix touches).
