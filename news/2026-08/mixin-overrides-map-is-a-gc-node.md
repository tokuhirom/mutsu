# The `Mixin` overrides map is a GC node, so its aliased write is sound

Found 2026-08-02 while adding the Miri gate that closes ADR-0013; fixed 2026-08-03.

## The claim that turned out to be wrong

[ADR-0013](../../docs/adr/0013-container-interior-mutability-cellvalue.md) §8 recorded the
provenance fix as "fixed at every call site at once", on the reasoning that the `UnsafeCell` went
into `GcBox.value`, so every `Gc<T>` became interior-mutable at the primitive.
`src/value/aliased_mut.rs` agreed, documenting its `arc_contents_mut` as "Currently unused … kept
as the audited primitive for any future still-`Arc` container".

Both were wrong by one call site. `arc_contents_mut` had a **live caller**:

```
src/runtime/methods_classhow_dispatch.rs:109
    let map = unsafe { crate::value::arc_contents_mut(overrides) };
    map.insert("__mutsu_type_name__".to_string(), Value::str(new_name.clone()));
```

## Root cause

`Mixin` was the one container variant that never migrated to the GC:

```rust
// src/value/mod.rs
Mixin(Arc<Value>, Arc<HashMap<String, Value>>),
```

A plain `Arc<T>` payload has **no `UnsafeCell`**, so `&mut *(Arc::as_ptr(arc) as *mut T)` derives a
`&mut` from a `*const` — the shape ADR-0013 removed everywhere else. The write itself is
semantically wanted (`$type.^set_name($name)` must be visible through every alias of the mixed-in
object, matching Rakudo's in-place mutation of the anonymous metaobject); it was the
*representation* that was out of step with the rest of the codebase.

```raku
my $obj = 42 but role { method greet { 'hi' } };
$obj.^set_name('Greeter');
say $obj.^name;    # Greeter
```

The ticket asserted this path "reports a Stacked Borrows error" under Miri. **That did not
reproduce** — see the measurement section at the end; the honest reason to make this change is
uniformity plus the collector improvement below, not a UB fix that Miri can demonstrate.

## The fix

The overrides map is now a GC node — `Mixin(Arc<Value>, Gc<MixinOverrides>)`, where
`MixinOverrides = HashMap<String, Value>` — which is the same migration every other container
already had, and the option the ticket picked as "the one that matches the rest of the codebase".
The alternative (have `^set_name` rebuild the mixin and write it back through the variable) was
rejected because it changes the aliasing semantics the code deliberately relies on.

Concretely:

- **`Trace for MixinOverrides`.** The map holds arbitrary `Value`s (a `but`-mixed method closure,
  a role-punned instance, an allomorph's `Str` half), any of which can close a cycle. Being a real
  node rather than an inlined `Arc` wrapper is a *collector* improvement too: `gc_trace` yields it
  once and its own `Trace` impl walks its edges, so the `uniquely_owned` gate the `Arc` shape
  needed — which conservatively stopped tracing a shared wrapper, under-collecting cycles routed
  through it — is gone for mixins.
- **`gc_contents_mut` at the `^set_name` write**, replacing `arc_contents_mut`.
- **`Gc::ptr_eq` / `Gc::as_ptr`** at the mixin-identity sites (`propagate_mixin_update_by_arc` and
  the multidim-subscript push path), and `Gc::new` at the handful of construction sites, all of
  which funnel through `Value::mixin` / `Value::mixin_parts`.
- **`arc_contents_mut` deleted** along with its last call site, so the codebase has exactly one
  aliased-container-write primitive again. The ~35 SAFETY comments that cited the `Arc` primitive
  as their contract now name the `Gc` one (they were already `Gc` call sites; the reference had
  been stale since ADR-0013 and would have dangled outright once the function went away).

## Why the Miri gate now sees it

The ticket noted the gate could not see this shape ("no test in the subset reaches `^set_name`").
That is fixed, and not by waiting for `todo/tickets/magic-vars-should-be-built-lazily.md`: the
gate's filter is `cargo miri test --lib gc::`, a *substring* match, so `value::value_gc::tests::*`
is already inside the subset. `a_mixin_overrides_write_is_visible_through_every_alias` reproduces
the `^set_name` shape at the `Value` level — build a mixin, clone it (the metamethod gets a clone
that shares the node), write the overrides through `gc_contents_mut`, assert the clone observes
it — with no `Interpreter::new()`, so it runs under Miri today. The gate's exact command was run
locally on the pinned nightly to confirm.

An interpreter-level counterpart (`a_mixin_rename_is_visible_through_an_alias` in
`gc::soundness_smoke`) joins its siblings under `#[cfg_attr(miri, ignore)]` and starts running
when lazy magic vars land. `t/metamodel-set-name.t` remains the Raku-level pin.

## What Miri actually says about the two shapes

Measured on the gate's pinned nightly (`nightly-2026-08-01`), four throwaway probes, run under
both `-Zmiri-stacked-borrows` (the default) and `-Zmiri-tree-borrows`:

| probe | Stacked | Tree |
| --- | --- | --- |
| `Arc::as_ptr as *mut` write, no `&T` live across it | **ok** | **ok** |
| `gc_contents_mut` write, no `&T` live across it | ok | ok |
| `Arc::as_ptr as *mut` write, a Deref'd `&T` used after it | **UB** | — |
| `gc_contents_mut` write, a Deref'd `&T` used after it | **UB** | — |

So the two shapes behave *identically* under Miri: the aliased write on its own is accepted for
both, and both are UB if a shared borrow into the payload is used across the write. This
contradicts ADR-0013 §2's claim that the `UnsafeCell` gives a `&mut` with valid provenance "even
while shared `&` borrows into the same node exist" — the `UnsafeCell` fixes how `Gc::as_ptr`
*derives* its pointer (no intermediate reference), not the caller's obligation. The accurate
statement of the contract is the one `gc_contents_mut`'s own SAFETY doc already makes: no other
`&`/`&mut` into the value may be dereferenced for the lifetime of the returned borrow. Followed up
the same week: ADR-0013 §8 now carries the measurement table and the call-site audit it prompted.

That does not make this change pointless — it removes the codebase's last `as_ptr as *mut` cast
so there is one audited primitive with one documented contract, and it turns the overrides map
into a collectable node — but the "fixes UB" framing the ticket inherited from the ADR is not
something the gate can show.
