# `does PositionalBindFailover` fails with `X::InvalidType` — the role isn't in `BUILTIN_PARENT_TYPES`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/PositionalBindFailover.rakudoc:34`).

## Root cause

`class Foo does PositionalBindFailover { ... }` fails to even compose — `class`
registration rejects the role as an unknown type name. `validate_class_parents`
(`src/runtime/registration_class_validate.rs`, ~line 228) accepts a `does`/`is` parent
name only if it is a registered class, a registered role, a registered enum, or appears in
the hardcoded `BUILTIN_PARENT_TYPES` list (`src/runtime/registration_class_decl.rs`,
lines 26-105). `"PositionalBindFailover"` is missing from that list — even though sibling
core roles like `"Positional"`, `"Associative"`, `"Stringy"` are present, and even though
`PositionalBindFailover` IS recognized as a valid type name by the separate, more general
type-constraint checker in `src/runtime/utils/type_constraints.rs` (line 193) — that
checker is just not consulted here.

Note: other core roles that a class might `does` (`Iterable`, `Iterator`,
`PredictiveIterator`, `Dateish`, `Sequence`) are ALSO absent from `BUILTIN_PARENT_TYPES`,
but those work anyway because they happen to already be pre-registered as real roles in
`self.registry().roles` (verified: `class Foo does Iterator { method pull-one {1} }`
composes and runs fine). `PositionalBindFailover` has no such registry entry, so it falls
through every check and hits the `X::InvalidType` error path.

## Minimal repro

```raku
class Foo does PositionalBindFailover { }
```

- `raku`: composes without error.
- `mutsu` (`target/debug/mutsu`): `X::InvalidType: Invalid typename 'PositionalBindFailover'`.

## Scope note — this ticket is about the immediate crash, not full failover semantics

Adding `"PositionalBindFailover"` to `BUILTIN_PARENT_TYPES` (or registering it as a real
empty/marker role, matching how `Iterator` etc. already work) unblocks class composition,
but the doc's fuller example additionally requires the actual runtime behavior: an object
that `does PositionalBindFailover` and defines `.iterator` should have that iterator
consulted by positional-context binding/subscripting (e.g. `@a[^5]`, treating missing
tail elements as `Nil`). That deeper behavior overlaps with the already-**Deferred**
"Custom `does Iterable`/`does Iterator` protocol" cluster
(`iterating.rakudoc`/`Iterator.rakudoc`) noted in `docs/doc-diff-backlog.md`'s Deferred
section — this ticket only covers the shallow "recognize the type name so the class can
compile at all" gap; the follow-on iterator-consultation behavior should be picked up
alongside that existing deferred cluster, not duplicated here.

## Affected files (starting point)

- `src/runtime/registration_class_decl.rs` (`BUILTIN_PARENT_TYPES` — add
  `"PositionalBindFailover"`, and consider auditing for `"Iterable"`, `"Iterator"`,
  `"PredictiveIterator"`, `"Dateish"`, `"Sequence"` too, even though those currently work
  via a different registry path)
