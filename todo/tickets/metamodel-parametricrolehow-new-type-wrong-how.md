# `Metamodel::ParametricRoleHOW.new_type(...)` returns a type whose `.HOW` is `ClassHOW`, not `ParametricRoleHOW`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/ParametricRoleHOW.rakudoc:35`).

## Repro

```raku
my \zipi := Metamodel::ParametricRoleHOW.new_type( name => "zape", group => "Zape");
say zipi.HOW;
```

- `raku`: `Perl6::Metamodel::ParametricRoleHOW.new`
- `mutsu` (`target/debug/mutsu`): `Perl6::Metamodel::ClassHOW.new`

Verified directly with `raku -e` / `target/debug/mutsu -e` on this exact snippet (using
`my \zipi :=`, so this is independent of the separate `constant NAME := Metamodel::
ClassHOW.new_type(...)` "immutable" bug tracked in
`todo/deep/direct-metamodel-classhow-new-type-immutable-error.md` — that ticket is about
a spurious readonly error on `constant` binding; this one is about which `.HOW`
metaclass the returned type object reports, and reproduces cleanly with `:=`, no
`constant` involved).

## Root cause hypothesis

mutsu's `Metamodel::ParametricRoleHOW.new_type` implementation (or its generic
`Metamodel::*.new_type` dispatch) doesn't thread through which specific `*HOW` class
was invoked — it appears to always construct a type object tagged with `ClassHOW`
regardless of which `Metamodel::*HOW` the `.new_type` call was made on. Grep for
`"new_type"` and `"ParametricRoleHOW"` in `src/runtime/` to find the dispatch and see
whether it's hardcoded to `ClassHOW` or genuinely shared/generic code that lost the
caller's metaclass identity.

## Affected files (starting point)

- Wherever `Metamodel::*.new_type` is implemented (grep for `"new_type"` in
  `src/runtime/`).
