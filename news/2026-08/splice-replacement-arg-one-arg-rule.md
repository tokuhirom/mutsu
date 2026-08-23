# `.splice` got its own one-arg rule, shared by all three splice paths

`.splice($offset, $size, *@replacement)` used to flatten **every**
`Array`/`List`-kind replacement argument, regardless of how many were passed:

```raku
my @a = 1,2,3;
@a.splice(1,1,"x",[7,8]);
say @a.raku;
# raku:  [1, "x", [7, 8], 3]   (4 elements)
# mutsu: [1, "x", 7, 8, 3]     (5 elements)
```

This was reported as `todo/tickets/splice-multi-arg-array-incorrectly-flattens.md`,
discovered while writing ADR-0040's acceptance oracle
(`t/element-store-itemization.t`). It is fixed.

## What the rule actually is

The ticket guessed that `splice` follows the same one-arg rule as
`push`/`append`, and separately noticed that an *itemized* single Array
(`$[7,8]`) flattens for `splice` but not for `append`. Asking Rakudo directly
(`Array.^lookup('splice').candidates>>.signature`) explains both facts at once
— `splice` declares three families of candidates for the replacement values:

| candidate | binds when | effect |
| --- | --- | --- |
| `(..., **@new)` | anything else | non-flattening slurpy: one element per argument |
| `(..., @new)` | exactly one argument that does `Positional` | its elements are used |
| `(..., @new is item)` | ditto, itemized (`$[7,8]`) | its elements are used |

So the discriminator is **`Positional`**, and the `is item` candidate is why
`splice` differs from `push`/`append` in *both* directions:

- an itemized single Array still flattens (`@a.splice(1,1,$[7,8])` inserts
  `7, 8`), whereas `@a.append($[7,8])` keeps it whole;
- a single `Hash`/`Set`/`Bag` is `Associative`, not `Positional`, so it stays
  **one** element for `splice`, whereas `@a.append(%h)` flattens it to pairs.

A `Slip` flattens at any arity, because that is what a Slip is — independent
of which candidate binds.

All of this was swept against real `raku` across single `Array` / itemized
`Array` / `List` / itemized `List` / `Seq` / lazy `Seq` / `Range` / `Hash` /
`Set` / `Bag` / `Pair` / `Capture` / `Slip` / type-object arguments, and the
multi-argument counterparts of each.

## What changed

The three copies of the replacement-collection loop — the interpreter's
`do_splice` (`src/runtime/methods_mut_dispatch.rs`), the by-value-invocant
`Interpreter::splice_array_data` (`src/runtime/methods_call_helpers.rs`) and
the VM fast path `try_native_array_splice` (`src/vm/vm_call_method_mut_ops.rs`)
— had each drifted into a *different* wrong rule. They now all call one shared
helper, `crate::runtime::flatten_splice_replacement_args`
(`src/runtime/mod.rs`, next to its `append` sibling `flatten_append_args`),
which owns the one-arg rule, ADR-0040 element itemization and the ADR-0049
`Nil`-to-`Any` decay together.

Unifying them fixed two divergences beyond the reported one, both of which
were the fast path silently skipping what the interpreter path did:

- a single `Seq` or `Range` replacement argument now flattens
  (`@a.splice(1,1,7..9)` inserts `7, 8, 9`);
- `@a.splice(1,1,Nil,7)` now stores `Any`, not `Nil` (ADR-0049 slice 4 was
  implemented only in `do_splice`).

## The renderer half

Making every path itemize kept-whole elements (ADR-0040) exposed a
pre-existing `.raku` rendering gap that `push` already had:
`my @a; @a.push(%h); say @a.raku` printed `[${:x(1)},]` where raku prints
`[{:x(1)},]`. A real array's element *is* a `Scalar` container, so the
itemization it carries is not information and raku does not print the sigil —
`raku_value_as_element` (`src/builtins/methods_0arg/raku_repr.rs`) already
de-itemized `Array` elements for exactly this reason, but not the other two
kinds ADR-0040 itemizes at the store (`Hash`, a Value-level flag, and `Seq`, a
`Scalar` box). It does now, so `push` renders correctly too. Lists are
unaffected — `($(%h), $[1,2]).raku` keeps its sigils, which is why only real
`@`-array elements are routed through that de-itemization.

## Pins

`t/splice-arg-flatten-rule.t` — 42 rows, every one of them verified to pass
under real `raku` as well as mutsu: the single-argument flatten set, the
non-`Positional` single arguments that stay one element, the multi-argument
arity invariants (the reported bug), Slips at both arities, the degenerate
`splice()` / `splice($offset)` forms, ADR-0040 itemization and identity of
kept-whole elements, and the ADR-0049 `Nil` decay.

## Known remaining divergence

A single `Buf` argument still does not flatten (`Blob` does `Positional`, so
raku expands it to its elements). That needs a `BufStorage` arm in
`value_to_list` rather than a `splice` change; it is tracked as
`todo/tickets/splice-single-buf-arg-does-not-flatten.md`.
