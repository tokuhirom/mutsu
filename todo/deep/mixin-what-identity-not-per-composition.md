# A role-mixed native value's `.WHAT` returns the shared base type, not a distinct per-composition type object

## Root cause

`dispatch_what()` (`src/runtime/methods_introspect.rs`) handles `ValueView::Mixin(inner, _)` (a value produced by `does`/`but` on a native representation, e.g. `%h does SomeRole`) by recursing into the *inner* base value's own `.WHAT` and discarding the mixin's `overrides` entirely:

```rust
ValueView::Mixin(inner, mixins) => {
    if let Some(allo) = crate::value::types::allomorph_type_name(inner, mixins) {
        return Ok(Value::package(Symbol::intern(&allo)));
    }
    return self.call_method_with_values(inner.as_ref().clone(), "WHAT", args.clone());
}
```

So `(%h does R).WHAT` returns the plain shared `Package("Hash")` value — the exact same value every other `Hash` in the process shares — even though `%h.^name` correctly reports the composed name `Hash+{R}` (via a different code path, `dispatch_caret_name`, which does look at the role markers in `overrides`). Confirmed directly:

```raku
my role R { }
my %h;
%h does R;
say %h.^name;        # Hash+{R}      (mutsu: correct)
say %h.WHAT.^name;    # mutsu: Hash   -- raku: Hash+{R}
say %h.WHAT === Hash; # mutsu: True   -- raku: False
```

Real Rakudo gives a role-mixed value's `.WHAT` a **fresh, distinct anonymous type object** per composition (base type + role set) — not the shared base package, and not a fresh object per instance either. Two independent points confirm this precisely:

- `Hash::Restricted`'s `is restricted` trait (`roast`-adjacent dist, vendored locally under `tmp/hash-restricted/`) calls `v.var.WHAT.^set_name("$name(restricted)")` right after `does`-mixing a restriction role onto the variable, intending to rename *only that hash's* type, not every `Hash` in the program.
- `roast/S14-roles/instantiation.t` ("Punned role classes have the same .WHAT"): two **separately** `.new()`-ed instances of the same punned role must satisfy `$obj.WHAT === $obj2.WHAT` (True) — the type identity is per-composition, shared across every instance of it, not per-object.

## What was tried and reverted

A same-session attempt made `.WHAT` on a `Mixin` value return `Value::mixin_parts(Arc::new(base_what), mixins.clone())` — i.e. reuse the **instance's own** `overrides` `Gc` handle as the WHAT's overrides, so `.^set_name` on the WHAT and `.^name` on the instance would round-trip through the same shared map. This fixed `Hash::Restricted`'s scenario correctly (verified: `%h1.^name` became `Hash(restricted)`, `%h2` — an unrelated hash — stayed `Hash`), but **broke `roast/S14-roles/instantiation.t`**: `$obj` and `$obj2` (two `SampleRole.new` instances) each carry their own per-instance `overrides` map (attribute storage etc.), so reusing it as the *type* identity made `$obj.WHAT === $obj2.WHAT` false. Reverted; see the inline comment left at the `ValueView::Mixin` arm of `dispatch_what()` for the full account.

This proves the fix cannot be "reuse whichever `overrides` map the instance already has" — the correct type-object identity is **keyed by the composition (base type + set of mixed-in roles/markers), not by the instance**. Two values with the same base type and the same role set must return `===`-identical `.WHAT` type objects; two values with different instance data but the same composition must still share it (per the punned-role-instantiation invariant); a value with a *different* composition (e.g. a different role set, or a `.^set_name`-renamed composition) must get its own distinct type object.

## Why this is large

A correct fix needs a **composition-keyed anonymous-type-object cache**: something like `HashMap<(base_type_name, sorted_role_key_set), Value>` (or an interned `Gc<MixinOverrides>` keyed identically), consulted by `dispatch_what()` so that:

- The same composition on different instances yields the identical (by `Gc` pointer / `===`) type object.
- `.^set_name`/other `.^`-mutations on that shared type object are visible on every instance of that exact composition (matching Rakudo's real per-class-not-per-instance metaobject mutation), but do NOT leak to instances with a different composition, and never touch the *base* type's own shared `Package` value.
- The cache needs a stable, well-defined key derived from `mixins: &MixinOverrides` (which currently also carries non-composition data like per-instance attribute values for role-declared attributes — those must be excluded from the key, only the role/type markers matter) and the base type name.
- Lifetime/GC interaction: the cache holding `Value`s (anonymous type objects) needs to not become a permanent leak of every ad-hoc composition ever produced (e.g. a hot loop doing `$x but SomeRole` in a tight loop with varying attribute values but the same role set) — this likely wants a `Gc`-participating cache the collector can reclaim once the last composition instance drops, not a plain process-lifetime `HashMap`.
- Every other `Mixin` consumer (`~~`/smartmatch, `.isa`, `.^can`, `does`, `.new` on a mixin's `.WHAT`, `nqp::` introspection ops, etc.) needs auditing for whether it currently assumes `.WHAT` unwraps straight to the shared base `Package` — changing what `.WHAT` returns is a wide blast radius across dispatch code, not confined to `methods_introspect.rs`.

This is squarely a "needs design before touching" problem per `todo/README.md`'s split (dual-store-refactor-sized, not a single-session slice), so it stays in `todo/deep/` rather than `todo/tickets/`.

## Affected files

- `src/runtime/methods_introspect.rs` — `dispatch_what()`'s `ValueView::Mixin` arm (the actual bug; has the revert's explanatory comment as of this writing)
- `src/runtime/methods_classhow_dispatch.rs` — `dispatch_classhow_method`'s `"set_name"`/`"name"` handlers, which already correctly special-case `ValueView::Mixin(_, overrides)` when reached directly (not through `.WHAT`); a composition-keyed WHAT would let `Hash::Restricted`'s call chain reach this existing correct logic instead of the `Package` branch.
- `src/value/value_methods_a.rs` / `src/value/view.rs` — `Value::mixin`/`Value::mixin_parts`, the `Gc<MixinOverrides>` plumbing a cache would build on.

## Repro

```raku
use Hash::Restricted;   # tmp/hash-restricted/Hash-Restricted-0.0.9/lib locally
my %h1 is restricted = a => 1, b => 2;
say %h1.^name;                       # mutsu: Hash+{restrict-current} -- raku: Hash(restricted)
say %h1.^name.ends-with('(restricted)');  # mutsu: False -- raku: True
```

Also blocks 2 subtests of the `Hash::Restricted` dist test suite ("is the name changed ok" x2, one per `%h1`/`%h2` case) — cosmetic relative to the dist's core restriction behavior, which does not depend on the name.
