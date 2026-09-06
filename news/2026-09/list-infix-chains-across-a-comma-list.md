# A chained `Z` whose left operand is a comma list works in all three spellings

Raku's list infixes (`Z`, `X`, `Zop`, `Xop`, `minmax`) are **looser than the
comma**, so `1, 2 Z <a b> Z <c d>` is a three-column zip whose first column is
the comma list `1, 2`. mutsu had three spellings of that source taking three
different code paths, and two of them could not see a chain:

```raku
say (1, 2 Z <a b c> Z <x y>).raku;   # raku: ((1, "a", "x"), (2, "b", "y")).Seq
                                      # mutsu: Confused. Two terms in a row
say [1, 2 Z <a b> Z <c d>].raku;     # raku: [(1, "a", "c"), (2, "b", "d")]
                                      # mutsu: [(1, "c"), (((2, "a"),).Seq, "d")]
my @r = 1, 2 Z <a b> Z <c d>;         # ... same wrong value
```

The bracket and assign spellings were the worse half: they parsed and answered
*silently wrongly*.

## The parenthesised failure was not a parse problem

`(1, 2 Z <a b> Z <c d>)` parsed perfectly. The chain-aware lift
(`lift_meta_ops_in_paren_list`) collected the three columns correctly and then
built `zip(col, col, col, with => infix:<>)` — with an **empty** operator name,
because a bare `Z` has no inner operator. `infix:<>` names nothing, and the
failed lookup surfaced as "Confused. Two terms in a row". That is exactly why
the `Z+` spelling of the very same shape always worked, and why the ticket's
`<+ ->` was a red herring.

`normalize_chained_zip_meta`'s Case 2 had always guarded this with
`if !op.is_empty()`. Its Case 1 and the paren-list lift had not; both do now.

## The other two spellings ran a duplicate, one-level lift

`normalize_comma_list_items` (`src/parser/stmt/assign/comma.rs`) — which the
bracket-array and comma-list paths go through — carried its own
`lift_meta_ops_in_list`: merge the preceding items with the metaop's own `left`,
and stop. That cannot see a chain, because for `1, 2 Z <a b> Z <c d>` the outer
metaop's `left` is *another metaop*, so the result was
`(1, (2 Z <a b>)) Z <c d>`. It now calls the shared, chain-aware
`lift_list_infix_in_arg_list`, and the duplicate is gone.

The same substitution replaced the hand-rolled one-level lift in
`regroup_assign_expr_metaop_rhs` (`src/parser/stmt/args.rs`). All three
spellings agree by construction now rather than by three parallel
implementations.

Reusing the shared lift also brought `minmax` with it, which the comma-list path
never had: `my @m = 1, 2 minmax 3, 4` is `[1, 2, 3, 4]` (i.e. `(1,2) minmax
(3,4)` = `1..4`), not `[1, 2..3, 4]`.

## One more divergence fell out

A chained `Zop` lowers to the multi-way `zip(..., :with)` call, which returned a
`List` where raku returns a `Seq` — the "unrelated nit" the ticket recorded
(`(1, 2 Z+ <3 4> Z+ <5 6>).raku` was `(9, 12)`, not `(9, 12).Seq`). The
no-`:with` path already returned a `Seq`; `builtin_zip_with` now does too,
including its empty case (`zip(with => &infix:<+>).raku` is `().Seq` in raku).

Pinned by `t/list-infix-chain-across-a-comma-list.t` — 24 assertions, all
measured against raku v2026.07 first: the chain in all three spellings, longer
chains and wider columns, `Zop`/`X`/`minmax` chains, `zip`'s `Seq`-ness, and the
rows that must not move (a single `Z`, a comma *after* the right operand, a
single or parenthesised left operand, `xx`, and a Whatever-curried `Z+`).

## Filed, not fixed

`todo/tickets/a-seq-assigned-to-a-scalar-is-not-itemized.md` — `my $s = (1, 2).Seq`
renders `(1, 2).Seq` where raku says `$((1, 2).Seq)`. Noticed while measuring the
`my $s = (1, 2 Z <a b> Z <c d>)` row, but it involves no meta-op at all and
reproduces on the bare `.Seq`.
