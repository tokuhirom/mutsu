# `«=»` hyper-assignment to a nested-tuple destructuring target silently does nothing

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:482`).
(The harness itself mis-bucketed this as `raku-drift` because the underlying multi-line block's
first line already matched, but the second line is a genuine, reproducible divergence —
re-verified directly against current `raku`.)

## Root cause hypothesis

`«=»` (hyper-assignment) with a nested-tuple LHS is documented to recursively assign
element-wise into a matching nested-tuple RHS shape:

```raku
my ($a, $b, $c);
(($a, $b), $c) «=» ((1, 2), 3);
say "$a, $c";       # raku: 1, 3
```

mutsu does not perform the assignment at all: it emits "Useless use of $a/$b/$c/constant N in
sink context" warnings (implying it evaluated `($a, $b), $c` and `((1, 2), 3)` as two
independent sink-context list expressions, not as a hyper-assignment), and `$a`/`$c` remain
undefined afterward — `say "$a, $c"` then throws "Use of uninitialized value" warnings and
prints `, ` (both empty) instead of `1, 3`.

This suggests the hyper-assignment compiler/parser path recognizes `«=»`/`»=»`/`<<=>>` for a
flat LHS (a plain array/list of scalars — the `@a »+=» 1` case one line above in the same doc
example works fine), but does not recognize a **parenthesized, nested-tuple** LHS as a valid
hyper-assignment target, falling back to parsing the whole thing as an ordinary
comma-expression statement instead.

## Minimal repro

```raku
my ($a, $b, $c);
(($a, $b), $c) «=» ((1, 2), 3);
say "$a, $c";
```

- `raku`: `1, 3`
- `mutsu` (`target/debug/mutsu`): several sink-context / uninitialized-value warnings, then
  `, ` (both `$a` and `$c` stay undefined).

## Affected files (starting point)

- The hyper-operator (`»`/`«`) parsing/compiling path, likely in `src/parser/` (wherever
  `«=»`/`»=»` hyper-assignment is recognized) and `src/compiler/expr.rs` (wherever a hyper
  assignment LHS is walked to determine element targets) — needs to accept a parenthesized
  nested-list LHS, not just a flat array/list.
