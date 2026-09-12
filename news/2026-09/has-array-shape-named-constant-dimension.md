# A `has @.a[N]`/`HAS T @.x[N] is CArray` shape survives a named `constant` dimension

`CompiledAttrDecl::declared_shape` (`src/opcode.rs`) is extracted statically from the
compiler-generated `Array.new(:shape(N))` default that `has @.a[N]` compiles to, and that
extraction only ever understood a literal integer `N`. A dimension written as a named `constant`
(`constant N = 5; has @.a[N]`) therefore came back `declared_shape: None`, indistinguishable from a
plain unshaped `has @.a` — even though the *runtime* shape was always correct (`Array.new` itself
evaluates `N` at build time, so `.shape` on a built instance already reported right).

Two consumers read the class-level `declared_shape` rather than a built instance's own shape, and
both saw the gap:

- Constructor coercion of a caller-provided value (`Foo.new(a => [1, 2, 3])`) into the declared
  shape, which silently left the array unshaped.
- NativeCall's `cstruct_layout` (ADR-0090), which *refuses* an embedded `HAS T @.x[N] is CArray`
  member's layout rather than guess a wrong element count — the right behavior for a genuinely
  unresolvable shape, but a false positive here, since the shape was resolvable, just not
  statically. `Image::Libexif`'s `ExifData` hits exactly this: `HAS ExifContent @.ifd[EXIF_IFD_COUNT]`
  with `EXIF_IFD_COUNT` a named constant.

Registration time (`class_body_has_decl` and its `role`/runtime-`has`/`augment class` counterparts)
has an env the static extraction never did, so `CompiledAttrDecl` now also records whether `default`
matched the shaped-array pattern at all (`dynamic_shape`) even when the literal extraction failed.
`Interpreter::resolve_dynamic_attr_shape` uses that flag to evaluate `default` — exactly
`Array.new(:shape(N))`, evaluated the same way `is default(...)` already is — and read the real
shape back off the built value, falling back to `None` (the prior, safe behavior) only when the
dimension genuinely cannot be resolved at registration time (e.g. it depends on instance state).

Fixes #8032.
