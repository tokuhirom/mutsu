# A nested array-literal element now renders with its `$` itemization prefix

`say .raku for [3,2,[1,0]]` printed `[1, 0]` for the third element where raku prints
`$[1, 0]`. The finding came out of the doc-diff harness
(`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:707`) and sat open because its
first root-cause hypothesis — "the array-literal constructor needs to item-contain its
nested array-literal elements" — was the right *description* of the symptom but the wrong
*scope*.

## Why it was not a local fix

Rakudo's `List.raku` takes its invocant raw (`\SELF`) and prefixes `$` when
`nqp::iscont(SELF)`. The `$` is therefore reporting whether the value `.raku` was called
on **sits in a container** — and `for [3,2,[1,0]]` aliases `$_` to the element's `Scalar`
container. That is not a property of "being a nested array literal" at all; it is the same
property as

```
my @c = [<a b>],[<c d>]; say @c[0].raku      # mutsu: ["a", "b"]   raku: $["a", "b"]
```

Special-casing the array-literal constructor would have fixed the one printed line while
leaving `@c[0].raku` wrong, and would have put a second, competing itemization rule next to
the one [ADR-0040](../../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md)
designs. The ticket's own 2026-08-26 re-measurement said so explicitly and asked that it be
closed by ADR-0040's slice work, not before it.

## What actually fixed it

ADR-0040 slice 2 — itemization at the *construction* sites. A real `Array`'s elements are
`Scalar` containers, so a `[...]` literal, a list-assign into `@a`/`%h`, `%(...)`, and the
`.Array`/`.Hash` coercions all itemize each stored aggregate on the way in
(`exec_make_array_op` / `exec_make_array_no_flatten_op` / `coerce_to_array` /
`build_hash_from_items_with_key_coercion`, via the new `itemize_real_array_elements`
helper). Every downstream element producer — `[i]`, slices, `.head`/`.tail`/`.first`,
`map`/`grep`/`sort`/`reverse`, `.pairs`/`.kv`, the implicit topic — inherits the flag with
no per-method work, which is exactly why the store was the right place for it.

Both forms now agree with raku:

```
$ mutsu -e 'say .raku for [3,2,[1,0]]'
3
2
$[1, 0]
$ mutsu -e 'my @c = [<a b>],[<c d>]; say @c[0].raku'
$["a", "b"]
```

Pinned in `t/element-store-itemization.t` (rows 01-18 and 23 of ADR-0040 §1.3's divergence
matrix, no longer `todo`-marked, plus a dedicated Slice 2 section of 24 dual-oracled
assertions).
