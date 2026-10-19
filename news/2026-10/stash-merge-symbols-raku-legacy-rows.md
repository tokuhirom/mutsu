# `Stash.merge-symbols` and `Raku.legacy` are method-table rows

`Stash.merge-symbols` and `Raku.legacy` used to be answered by by-name gates on the interpreter's
slow path. Both are now rows of the method table (ADR-11276 §9.198).

Two answers changed to match Rakudo:

- `MY::.merge-symbols` now dies with "No such method", because a `PseudoStash` is not a `Stash`.
- `$*RAKU.legacy` now raises `X::Parameter::InvalidConcreteness`, because the method takes only
  the type object.

That exception's invocant message now breaks lines at 72 columns, the way Rakudo's
`naive-word-wrapper` does, so the text matches Rakudo's for any method and class name.
