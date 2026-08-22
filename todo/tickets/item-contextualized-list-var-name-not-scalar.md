# `$(LIST).VAR.^name` reports `List` instead of `Scalar`

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Language/structures.rakudoc:26` and `Language/mop.rakudoc:120`).

## Root cause hypothesis

The item contextualizer `$(...)` is supposed to wrap its operand in a `Scalar`
container (this is exactly what `mop.rakudoc`'s "VAR" section documents: "The
presence of a `Scalar` object indicates that the object is itemized"). mutsu's
`$(...)` appears to produce a value whose `.VAR` reports the *inner* type (`List`)
rather than `Scalar`, i.e. the itemization isn't actually creating/tagging a Scalar
container the way `.VAR` introspection expects.

## Minimal repro

```raku
say $(4, 5).VAR.^name;      # raku: Scalar; mutsu: List
say $(1, 2, 3).VAR ~~ Scalar;  # raku: True;   mutsu: False
```

Both are the same underlying bug (from two different doc files, `structures.rakudoc`
and `mop.rakudoc`).

For contrast, a plain (non-itemized) list already reports correctly:

```raku
say (1, 2, 3).VAR ~~ Scalar;  # raku AND mutsu: False
```

## Affected files (starting point)

- Wherever `$(...)` (the item contextualizer, distinct from a `$var` sigil) is
  compiled/evaluated — likely `src/compiler/expr.rs` and the corresponding VM op that
  builds the itemized value. Also `.VAR`'s implementation (probably in
  `src/runtime/methods_object_*` or a dedicated introspection module) — needs to
  recognize the itemized-List representation as "wrapped in a Scalar" the same way it
  presumably already does for `my $x = (1,2,3)`.
