# `.HOW.^name` on a hash literal is missing the `+{<anon>}` mixin suffix

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Language/mop.rakudoc:93`).

## Repro

```raku
say (%).HOW.^name
```

- `raku`: `Perl6::Metamodel::ClassHOW+{<anon>}`
- `mutsu` (`target/debug/mutsu`): `Perl6::Metamodel::ClassHOW` (missing the
  `+{<anon>}` suffix)

## Root cause hypothesis

Rakudo's `ClassHOW` metaclass instance for a value like a bare hash literal apparently
has an anonymous role mixed into it (hence `+{<anon>}` in its own `.^name`), which
mutsu's `.HOW` implementation doesn't replicate — mutsu's `HOW` metaclass object
gists as the bare class name with no mixin annotation. This is a small, cosmetic-ish
gap in how deep mutsu's `.HOW`/metaclass modeling goes, not a functional dispatch bug
(the earlier line in the same doc, `$metadata.^mro` = `((ClassHOW) (Any) (Mu))`,
already matches raku).

## Affected files (starting point)

- Wherever `.HOW` produces its metaclass instance/gist (grep for `ClassHOW` in
  `src/runtime/`) — likely `src/runtime/class_introspection.rs` or
  `src/runtime/methods_classhow_lookup.rs`.
