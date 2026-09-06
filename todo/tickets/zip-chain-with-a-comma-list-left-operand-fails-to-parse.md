# A chained `Z` whose left operand is a comma list fails to parse

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/operators.rakudoc:3157`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
say (1, 2 Z <a b c> Z <x y>).raku;
# raku:  ((1, "a", "x"), (2, "b", "y")).Seq
# mutsu: Two terms in a row
```

## Narrowed — the doc example's `<+ ->` is a red herring

The doc block that surfaced this uses `Z <+ ->`, which looks like the culprit
and is not:

| Program | mutsu |
|---|---|
| `say <+ ->.raku` | `("+", "-")` — fine |
| `say (<a b> Z <+ ->).raku` | fine |
| `say (1, 2 Z <a b c>).raku` | fine — one `Z`, comma-list left operand |
| `say (1 Z <a b> Z <+ ->).raku` | fine — chained `Z`, **single** left operand |
| `say (1, 2 Z <a b c> Z <x y>).raku` | **Two terms in a row** |
| `say (1, 2 Z <a b c> Z <+ ->).raku` | **Two terms in a row** |

So the trigger is exactly: a **chain of two or more `Z`** whose leftmost
operand is a **comma list**. `Z` is `list infix` (same precedence level as `X`
and `Zop`), so `1, 2 Z <a b c> Z <x y>` parses in raku as one n-ary zip of
three lists with `1, 2` as the first — mutsu appears to close the list after
the first `Z` and then find a second `Z` with nothing to its left.

## Narrowed further, 2026-09-06 (a second session, same day)

**The bracket spelling is worse than the paren one: it parses and answers
wrongly.** That is a silent wrong answer, not a refusal, and it was not in the
first pass:

```raku
say [1, 2 Z <a b> Z <c d>].raku;
# raku:  [(1, "a", "c"), (2, "b", "d")]
# mutsu: [(1, "c"), (((2, "a"),).Seq, "d")]
```

`my @r = 1, 2 Z <a b> Z <c d>` produces the same wrong value. So the family is
one defect with two faces: where the bracket/assign spellings mis-associate, the
paren spelling leaves the tail unconsumed and the statement layer reports
`Confused. Two terms in a row`.

Reading the wrong value tells you the shape: mutsu builds
`[1, ((2 Z <a b>) Z <c d>)]` — the comma list keeps `1` as its own item and the
**whole** `Z` chain becomes the last item — and then lifts only one level, so
`1` lands beside the *inner* zip's result instead of beside `2`.

### The chain-aware lift already exists and is not being reached

`lift_meta_ops_in_paren_list` (`src/parser/primary/container/meta_ops.rs`) is
already written for exactly this: it walks the left-nested same-op chain in a
`while` loop, collects each `right` as a column, and folds the preceding comma
items into the first column. It is called from `finalize_paren_list`.

So the fix is probably **not** to write a new lift — it is to find why that one
does not see this shape. For the paren spelling the list never finishes
(`finalize_paren_list` is never reached); for the bracket/assign spellings the
one-level lift in `src/parser/stmt/args.rs:48` runs instead, which does
`items.push(*left)` on the *outermost* MetaOp and therefore cannot handle a
chain.

### What already works, and bounds the fix

| Program | mutsu |
|---|---|
| `1, 2 X <a b> X <c d>` | correct (8 elements, same as raku) |
| `1, 2 Z+ <3 4> Z+ <5 6>` | correct value `(9, 12)` |
| `(1, 2) Z <a b> Z <c d>` | correct — parenthesised left operand |
| `1 Z <a b> Z <c d>` | correct — single left operand |
| `1, 2 Z <a b>` | correct — single `Z` |
| `1, 2 Z <a b>, 3` | correct |
| `1, 2 Z <a b> X <c d>` | correctly refused as non-associative |

`X` chaining correctly through the same comma-list shape is the strongest hint
that this is reachable without new machinery: whatever path `X` takes, `Z` is
not taking it.

One unrelated nit noticed while measuring: `(1, 2 Z+ <3 4> Z+ <5 6>).raku` is
`(9, 12).Seq` in raku and `(9, 12)` in mutsu — the value is right, the type is
not. Not part of this ticket.

## Where to look

`src/parser/primary/container/paren.rs` (the comma loop that reads each item
with `expression_no_sequence` and never completes for this shape),
`src/parser/primary/container/meta_ops.rs` (`lift_meta_ops_in_paren_list`, the
chain-aware lift that should be doing this work), and
`src/parser/stmt/args.rs:48` (the one-level lift the bracket/assign spellings
use instead).

## Neighbourhood to check when fixing

The other list infixes at that level: `X`, `Xop`, `Zop`, and `xx`. Also the
reverse shape (a comma list on the *right* of the second operator), a three-`Z`
chain, the `[...]`/`my @a = ...`/`my $x = (...)` spellings above (each takes a
different path today), and `.Seq`-ness of the result.
