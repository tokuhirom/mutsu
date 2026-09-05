# RakuAST list infixes: junctions, `min` and `max`

raku renders a *list-associative* infix as one flat `ApplyListInfix` carrying
every operand of the chain, where an ordinary infix is a left-nested
`ApplyInfix`. Measured against rakudo 2026.07, the list-associative set mutsu can
produce is:

```
,   andthen   orelse   notandthen   |   &   ^   min   max
```

and these are *not*: `+`, `~`, `*`, `==`, `eq`, `and`, `or`, `&&`, `||`, `//`.

The comma and the `andthen` family were already handled. The junction
constructors and `min`/`max` rendered as nested `ApplyInfix` — a shape rakudo
never produces for them, so silent wrongness rather than a coverage gap.

## Change

`is_list_infix` gained the five operators, which routes them through the
existing `flatten_list_infix` path that already collapses a left-nested
same-operator chain into one operand list.

The lowerer's list-infix arm no longer hardcodes the three `andthen`-family
names: it maps the operator name back through `op_name_to_token_kind`, which
gained rows for `|`, `&` and `^` (`min`/`max` are already `TokenKind::Ident`, so
the catch-all is correct for them). This is the third time this session that
adding a row to that table was the whole fix — it is only used by the RakuAST
lowerer, so a missing row is invisible until a node reaches it.

## Coverage

`t/rakuast-list-infix.t` (12 assertions) pins `min`, `max` and all three
junction constructors as `ApplyListInfix`, that a three-operand chain is **one**
flat list rather than a nest (counting the occurrences, not just looking for the
class), that `+` and `~` are unchanged, and two `EVAL` round trips. It is a
dual-oracle test: it passes verbatim under both mutsu and rakudo 2026.07.
