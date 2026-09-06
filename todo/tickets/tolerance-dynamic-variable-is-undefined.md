# `$*TOLERANCE` is undefined, so `≅` uses the wrong tolerance

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/operators.rakudoc:2507`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
my $x = 1;
say ($x + $*TOLERANCE) ≅ $x;   # raku: False   mutsu: True
say ($x - $*TOLERANCE) ≅ $x;   # both: True
```

## Narrowed — the variable itself is missing

```raku
say $*TOLERANCE;   # raku: 1e-15   mutsu: (Any)
```

So the first line is really `(1 + Any) ≅ 1`, i.e. `1 ≅ 1`, which is trivially
`True`. The asymmetry raku shows is genuine and is the point of the doc example:
`1 + 1e-15` is a different `Num` from `1` while `1 - 1e-15` rounds back to `1`.

`≅` itself is not obviously broken — `1.000001 ≅ 1` and `100 ≅ 100.00001` both
answer `False` in mutsu as in raku — so the fix may be just to define the
dynamic variable with raku's default (`1e-15`) and make `infix:<≅>` /
`infix:<=~=>` read it.

## Where to look

Wherever the built-in dynamic variables are seeded (`$*IN`, `$*OUT`, `$*ERR`,
`$*PID` …) and the `≅` implementation in `src/builtins/`. Per
`raku-doc/doc/Language/operators.rakudoc`, the comparison is
`abs(a - b) <= $*TOLERANCE * max(abs(a), abs(b))` for non-zero operands, with an
absolute comparison when either side is zero — check mutsu's formula against
that at the same time, since a hard-coded tolerance may currently be standing in.

## Neighbourhood to check when fixing

A user-set `$*TOLERANCE` in an inner scope (it is a dynamic variable, so
`{ my $*TOLERANCE = 0.1; say 1 ≅ 1.05 }` must see the override); the `=~=` ASCII
spelling; `Complex` and `Rat` operands; comparisons involving `0`, `Inf` and
`NaN`; and `is-approx` in `Test`, which raku documents in terms of the same
tolerance.
