# ADR-0128: A package-scoped enum's identity is its qualified name; its display name is the declared one

- **Status**: Accepted (2026-09-27); implemented in the same PR as this decision
  ([#9654](https://github.com/tokuhirom/mutsu/issues/9654)).
- **Related**: [ADR-0047](0047-type-identity-is-a-declaration-site-not-a-registry-name.md)
  (type identity is a declaration site, not a name — the lexical `my class` half of the
  same problem), [ADR-0056](0056-nativecall-types-display-only-qualification.md)
  (a display-only name mapping kept apart from identity comparisons).

## Context

`Interpreter::register_enum_decl` inserted every enum into `registry.enum_types` under the
bare declared name, and every enum value carried that bare name as its type symbol. Two
packages declaring the same short name therefore shared one registry entry — the last
declaration won, for the qualified spelling too:

```raku
module A { our enum pn <x y>; our sub f { pn.enums.keys.sort } }
module B { our enum pn <z>;   our sub f { pn.enums.keys.sort } }
say A::f();   # raku: (x y)   mutsu: (z)
```

CSS::Module's `CSS21::Metadata` and `CSS3::Metadata` both declare `our enum prop-names`,
so every CSS::TagSet test died on the wrong enum.

Rakudo's display has a quirk that constrains the fix: a nested *class* reports its
qualified name (`A::K.^name` is `A::K`), but an enum reports the name it was declared
with (`A::pn.^name` is `pn`, `A::pn::x.raku` is `pn::x`, gist `(pn)`), even though the two
enums are distinct type objects.

## Decision

1. **Identity.** An enum declared while the current package is not `GLOBAL` is
   registered under the package-qualified key (`A::pn`) — the key a nested class gets —
   via `Interpreter::enum_registry_key`. Enum values carry that key, so `.enums`,
   smartmatch, `.pred`/`.succ`, coercion and object-hash identity all distinguish the two
   types without any per-site change. The short-name env binding and the
   `Pkg::name`/`Pkg::name::value` bindings point at the qualified type object.
2. **Resolution.** Bare-name lookups already walk the current package chain for nested
   types (`resolve_type_in_current_package`, `resolve_suppressed_type`); the enum joins
   them: an enum in a class or role body registers its short name as class-scoped like a
   nested class, a role-body enum is a `DeferredBodyOpKind::TypeDecl` (registered under
   the role's package whoever composes it), `is export` exports the type name as well as
   its values, and `Interpreter::resolve_enum_type_key` resolves a source spelling (a
   `MAIN` parameter constraint, a coercion call `E(1)`) to its key.
3. **Display.** The declared name is recorded in a process-global table
   (`src/value/enum_display.rs`) consulted by `user_facing_type_name` and by the enum
   display sites (`.^name`, `.raku`, type-check messages). It is process-global for the
   same reason ADR-0056's table is: the display layer has no interpreter context. Like
   ADR-0056, identity comparisons never read it.

## Consequences

- A top-level enum (`GLOBAL`) and a `unit module` enum (whose body runs with `GLOBAL` as
  the current package) keep their bare key, so the common case is unchanged.
- `.WHICH` of a package-scoped enum value now names the qualified type (`A::pn|0`), where
  Rakudo prints `pn|0` and even conflates the two types as object-hash keys; mutsu keeps
  them distinct, which follows `===`.
- A parameter constraint written with the short name still matches a same-named type of
  another package through `type_matches`' short-name bridge. That is pre-existing and
  shared with nested classes (`module A { class K {}; sub f(K $x) {} }` accepts a
  `B::K`); it is not addressed here.

## Rejected alternatives

- **ADR-0047-style mangling (`pn\0A`).** It would reuse the `\0` display stripping, but
  the key would then be unreachable by the package-chain probes that already find
  `A::pn`-shaped nested types, and a `::` inside the suffix would be split as a name
  segment by the demangler.
- **Qualify only on a collision.** Identity would depend on declaration order, the exact
  fragility ADR-0047 D1 removed for lexical classes.
