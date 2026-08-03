# `Z..` / `X..` / `Z^..^` reach the reduction handler and die "Unsupported reduction operator"

The range and sequence operators are legal metaop bases in rakudo:

```
(1, 2) Z..   (5, 6)     raku: (1..5 2..6)
(1, 2) Z^..^ (5, 6)     raku: (1^..^5 2^..^6)
(1, 2) X..   (5, 6)     raku: (1..5 1..6 2..5 2..6)
```

mutsu answers `Unsupported reduction operator: ..` for all three. The *parse* is
already right — `..`, `..^`, `^..`, `^..^`, `...` and `...^` have been in
`parse_meta_op`'s base list from the start, deliberately ordered longest-first
(`src/parser/expr/precedence_meta_ops/meta_bracket.rs`). What is missing is the
runtime: whatever `Z`/`X` lowers to routes the base operator through the
*reduction* operator table, which has no entry for a range.

Found while adding the missing `^^` / `^` / `===` bases
(`news/2026-08/metaop-doubled-infix-base.md`); it is a different layer from that
fix, which is why it was not folded in.

## Where to look

The error text is the reduction handler's, so start from `Unsupported reduction
operator` in `src/runtime/` and see what `Z`/`X` hand it. A range base has to
build a `Range` value rather than fold, so it probably needs the same treatment
as the other non-numeric bases (`Z~~`, `Z===`, `Zeqv`) that already work.

## Not blocking a roast file today

No file in the current real-`Test` residue fails on this — it surfaced from a
hand probe, not from the sweep. Worth fixing anyway: `Z..` is ordinary Raku and
silently unusable.
