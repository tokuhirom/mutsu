# The `Mixin` overrides map is still `Arc`-backed, so its aliased write is still provenance UB

Found 2026-08-02 while adding the Miri gate that closes ADR-0013.

## The claim that turned out to be wrong

[ADR-0013](../../docs/adr/0013-container-interior-mutability-cellvalue.md) §8 records the provenance
fix as "fixed at every call site at once", on the reasoning that the `UnsafeCell` went into
`GcBox.value`, so every `Gc<T>` became interior-mutable at the primitive. `src/value/aliased_mut.rs`
agreed, documenting its `arc_contents_mut` as "Currently unused … kept as the audited primitive for
any future still-`Arc` container".

Both are wrong by one call site. `arc_contents_mut` has a **live caller**:

```
src/runtime/methods_classhow_dispatch.rs:109
    let map = unsafe { crate::value::arc_contents_mut(overrides) };
    map.insert("__mutsu_type_name__".to_string(), Value::str(new_name.clone()));
```

## Root cause

`Mixin` is the one container variant that never migrated to the GC:

```rust
// src/value/mod.rs:1354
Mixin(Arc<Value>, Arc<HashMap<String, Value>>),
```

A plain `Arc<T>` payload has **no `UnsafeCell`**, so `&mut *(Arc::as_ptr(arc) as *mut T)` derives a
`&mut` from a `*const` — exactly the Stacked/Tree Borrows violation ADR-0013 removed everywhere else.
The write itself is semantically wanted (`$type.^set_name($name)` must be visible through every alias
of the mixed-in object, matching Rakudo's in-place mutation of the anonymous metaobject); it is the
*representation* that makes it unsound.

## Repro

```raku
my $obj = 42 but role { method greet { 'hi' } };
$obj.^set_name('Greeter');
say $obj.^name;    # Greeter
```

Under `cargo miri test` this path reports a Stacked Borrows error; under a normal build it works and
no test fails, which is precisely why it survived.

## Why it is not a one-liner

`Mixin`'s overrides map is read on hot paths (`.^name`, method-override lookup), so the fix has to
keep reads cheap:

- **Give `Mixin` the `Gc` shape** (`Gc<HashMap<String, Value>>`) so the write can route through the
  sound `crate::gc::gc_contents_mut`. Needs a `Trace` impl and touches the NaN-box tagging for the
  variant — the same migration every other container already had.
- Or **stop mutating in place**: have `^set_name` rebuild the mixin value and write it back through
  the variable, which changes the aliasing semantics the current code deliberately relies on.

The first is the one that matches the rest of the codebase. Until it lands, keep the honest warning in
`aliased_mut.rs`'s module header and **do not add a second `arc_contents_mut` call site**.

## Gate

The Miri job (`.github/workflows/miri.yml`) is informational at first partly because of this: the
GC/container subset it runs must be clean before the job can become blocking.
