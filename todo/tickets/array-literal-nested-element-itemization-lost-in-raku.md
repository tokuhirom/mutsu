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

## Re-measured 2026-08-26: this is ADR-0040, not a local array-literal fix

The hypothesis above ("nested array-literal elements need to be item-contained
the way a plain scalar element already is") is the right *description* but the
wrong *scope*. Rakudo's `List.raku` takes its invocant raw (`\SELF`) and prefixes
`$` when `nqp::iscont(SELF)` — i.e. `.raku` is reporting whether the value it was
called on sits in a container, and `for [3,2,[1,0]]` aliases `$_` to the
element's Scalar container. The `$` prefix is therefore not a property of
"nested array literal" at all; it is the same property as

```
$ mutsu -e 'my @c = [<a b>],[<c d>]; say @c[0].raku'   # ["a", "b"]   raku: $["a", "b"]
$ mutsu -e 'my @c = [<a b>],; for @c { say .raku }'    # ["a", "b"]   raku: $["a", "b"]
```

which is exactly `todo/deep/element-itemization-lost-in-scalar-binding.md` /
[ADR-0040](../../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md).
Special-casing the array-literal constructor would fix the one printed line
while leaving `@c[0].raku` wrong, and would put a second, competing itemization
rule next to the one ADR-0040 designs. **Close this ticket with ADR-0040's slice
work, not before it.**

## Affected files (starting point)

- Array-literal (`[...]`) construction/compilation — likely `src/compiler/expr.rs` or
  `src/vm/vm_data_ops.rs` (array-literal construction opcode), wherever each array-literal
  element value is stored; nested-array-literal elements need to be item-contained the same way
  a plain scalar element already presumably is (since `.raku` on a non-nested scalar element
  works correctly, only nested-array elements lose the wrapper).
