# `.raku` on an array-literal element that is itself an array literal drops the `$`-itemization prefix

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:707`).

## Root cause hypothesis

Every element of an array literal (`[...]`) is documented to be `Scalar`-itemized (contained),
including nested array-literal elements. `.raku` on such an itemized element therefore prefixes
it with `$` to show it round-trips as a single scalar item, not a flattenable list:

```raku
say .raku for [3,2,[1,0]];
# raku:  3 / 2 / $[1, 0]
```

mutsu prints the third element as plain `[1, 0]` (no `$` prefix), meaning the nested
array-literal element is not being wrapped in an item container the way `raku`'s array-literal
construction itemizes each of its elements. This is a distinct manifestation from the already-
ticketed [item-contextualized-list-var-name-not-scalar.md](item-contextualized-list-var-name-not-scalar.md)
(which is about the `$(LIST)` contextualizer operator specifically) — here there is no `$(...)`
involved at all, the itemization is supposed to come purely from being a nested element of an
outer `[...]` array-literal construction.

## Minimal repro

```raku
say .raku for [3,2,[1,0]];
```

- `raku`:
  ```
  3
  2
  $[1, 0]
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  3
  2
  [1, 0]
  ```

## Affected files (starting point)

- Array-literal (`[...]`) construction/compilation — likely `src/compiler/expr.rs` or
  `src/vm/vm_data_ops.rs` (array-literal construction opcode), wherever each array-literal
  element value is stored; nested-array-literal elements need to be item-contained the same way
  a plain scalar element already presumably is (since `.raku` on a non-nested scalar element
  works correctly, only nested-array elements lose the wrapper).
