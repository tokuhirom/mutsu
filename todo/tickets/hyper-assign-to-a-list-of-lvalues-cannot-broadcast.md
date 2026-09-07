# A hyper assignment into a flat list of lvalues dies on a scalar RHS

Found 2026-09-07 in the neighbourhood of
`news/2026-09/short-rhs-slice-assign-broadcasts-instead-of-padding.md`, which
made `»=»` a real distributing hyper op for every *other* target shape.
Pre-existing and independent of that change (verified against the same tree
with the change stashed), so it was left out of scope there.

## Repro

```
$ ./target/debug/mutsu -e 'my ($x, $y); ($x, $y) »=» 5; say ($x, $y).raku'
Index out of range. Is: 1, should be in 0..0
  in block <unit> at -e line 1

$ raku -e 'my ($x, $y); ($x, $y) »=» 5; say ($x, $y).raku'
(5, 5)
```

A **list** RHS works on both (`($x, $y) »=» (5, 6)` gives `(5, 6)`), so it is the
scalar RHS specifically. This is the same broadcast raku applies to every other
hyper target — `@a »=» 7` and `%h »=» 7` both fill every slot — so the list of
lvalues is the one shape that cannot take it.

## Root cause

`lower_hyper_assign_target` (`src/parser/expr/precedence_meta_ops/hyper_concat.rs`)
lowers a literal list of lvalues by *positional index into the RHS*: element `i`
of the target is assigned `Expr::Index { target: source, index: i }`. With a
scalar source that is `5[1]`, which is out of range rather than a broadcast.

The lowering is recursive and handles nested sublists
(`(($a, ($b, $c)), $d) »=« ((4, (5, 6)), 7)`, pinned by
`t/hyper-assign-nested-destructuring.t`), which is why it is a separate path
from the ordinary `Expr::HyperOp` route that every other target now takes.

## Fix sketch

Two shapes are available and they trade off differently:

1. **Pre-distribute before destructuring.** Build the RHS as
   `HyperOp { op: "=", left: <the target read>, right: value, dwim_left, dwim_right }`
   and destructure *that* — the hyper machinery already gets the length and the
   dwim arrows right, and the target read `($a, $b)` supplies the arity. The
   risk is the RHS-evaluated-once guarantee: `t/hyper-assign-nested-destructuring.t`'s
   third subtest asserts a `do { … }` RHS runs exactly once, and the current
   lowering buys that with the `__mutsu_hyper_assign_N` temp. Keep the temp.
2. **Index with a dwim.** Replace the raw `source[i]` with an index that cycles
   a shorter source, which is what the hyper dwim rule says anyway. Smaller, but
   it re-implements the dwim rule in the parser instead of reusing it.

Prefer (1) if the once-only evaluation survives; it removes a second
implementation of the distribution rule rather than adding one.

## Check when fixing

`($x, $y) »=» 5` and `($x, $y) «=« 5` (the arrows decide which side adapts);
`($x, $y) »=» (5, 6)`; a nested target with a scalar RHS
(`(($a, $b), $c) »=» 9`); a non-dwim mismatch, which must still raise
`X::HyperOp::NonDWIM`; and all three subtests of
`t/hyper-assign-nested-destructuring.t`, especially the evaluate-once one.
