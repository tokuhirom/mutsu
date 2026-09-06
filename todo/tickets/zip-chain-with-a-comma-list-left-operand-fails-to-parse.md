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

## Where to look

The list-infix chain handling in the parser (`src/parser/`), specifically how a
comma list on the left of a list infix is closed off before the next operator of
the same precedence is read.

## Neighbourhood to check when fixing

The other list infixes at that level: `X`, `Xop`, `Zop` (`1, 2 Z+ <3 4> Z+ ...`),
and `xx`. Also the reverse shape (a comma list on the *right* of the second
operator), and a three-`Z` chain.
