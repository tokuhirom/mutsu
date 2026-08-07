# Native default-constructor gate reads `is_required` where it means to read a type constraint

`Interpreter::is_native_default_constructible` in `src/runtime/methods_object.rs` decides,
per attribute, whether a class's default `.new` can take the fully-native fast path instead
of falling through to the interpreter-driven constructor. Its per-attribute closure binds:

```rust
let type_constraint = &attr.is_required;
```

`attr` is a `ClassAttributeDef` (`src/runtime/mod.rs`). `is_required` is
`Option<Option<String>>` — `None` = not required, `Some(None)` = required, `Some(reason)` =
required with a reason string. The variable is then used a few lines below as if it held the
attribute's *type constraint* (a `Option<String>` naming a type like `Int` or `Str`), which is
a completely different piece of data that actually lives in the registry's
`attribute_types: HashMap<(String, String), String>` side table, keyed by `(class, attr)` —
not on `ClassAttributeDef` at all.

## History

This is not a new bug. It was carried over unchanged from the original 7-tuple shape of
`ClassAttributeDef` (`(name, is_public, default, is_rw, is_required, sigil, where_constraint)`):
the closure destructured the tuple positionally as
`|(name, _, _, _is_required, type_constraint, sigil, _where_constraint)|`, binding the tuple's
5th positional field (`is_required`) to a variable literally named `type_constraint`. ADR-0019
D2b's mechanical tuple→struct conversion (`refactor(runtime): turn ClassAttributeDef into a
named struct`) preserved this exactly rather than fixing it, per that PR's "zero behavior
change" scope — it left the struct-field equivalent `&attr.is_required` with the same
misleading local name and an inline `NOTE:` comment pointing here.

## Effect

Needs investigation to characterize precisely, but the shape of the bug: the "does this
attribute have a type constraint that the native ctor can check" gate is actually testing
"is this attribute `is required`", which means:

- An attribute with a real type constraint but no `is required` trait skips whatever
  `is_simple_native_ctor_constraint`-style validation the type-constraint branch was meant to
  apply (since `type_constraint` will be `None` there, as `is_required` defaults to `None`).
- An attribute that IS `is required` (regardless of type) gets routed through whatever
  branch the code takes when `type_constraint` looks like `Some(_)`/`Some(None)` — i.e. is
  required-ness is spuriously read as "has *some* type constraint value", including the
  `Some(None)` (required-without-reason) case looking exactly like a `Some(_)` type
  constraint that happens to be empty.

Net effect: `is_native_default_constructible`'s per-attribute gate may incorrectly admit or
reject a class for the native fast constructor path based on `is required`-ness instead of
actual type constraints, for classes with typed attributes and/or required attributes. Whether
this is currently masked by other checks in the same function (the type-check the fast path
itself performs once it thinks it's "native-constructible"), or produces a wrong result some
observer can see (e.g. a required-but-untyped attribute wrongly treated as needing a type
check, or a genuinely-typed non-required attribute skipping validation it should get) needs a
minimal repro to pin down.

## Where to look

- `src/runtime/methods_object.rs`, `is_native_default_constructible`, the
  `class_def.attributes.iter().all(|attr| { ... })` closure (~line 113 onward at the time of
  writing) — read the branches that consume `type_constraint` right after the `NOTE:` comment,
  and compare against what `registry.attribute_types.get(&(cls.clone(), attr.name.clone()))`
  would actually report.
- The native fast-constructor path itself (nearby in the same file / `methods_object_default_ctor.rs`)
  to see whether it separately re-validates types, which would explain why this hasn't been
  caught by roast yet (i.e. the gate is over/under-permissive but a second correct check downstream
  papers over it — or doesn't).

## Suggested minimal repro starting point

A class with one attribute that has both a real type constraint and no `is required`, plus a
sibling class with an `is required` untyped attribute, exercised through `.new` in a way that
would observably differ between the native and interpreter constructor paths (e.g. a bad-typed
value assigned via `BUILD`-adjacent means, or timing/side-effect visible only on the interpreter
path). Compare `raku` and `mutsu` behavior once a concrete divergence is found.
