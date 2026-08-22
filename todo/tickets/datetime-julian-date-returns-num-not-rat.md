# `DateTime.julian-date`/`.modified-julian-date` return `Num` instead of `Rat`

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/DateTime.rakudoc:281,302`).

## Root cause hypothesis

Real raku computes the Julian date as an exact `Rat`, so `.julian-date` and
`.modified-julian-date` print with a short, exact decimal expansion. mutsu computes
the same value using floating-point (`Num`) arithmetic somewhere in the julian-date
calculation, so the result carries binary-float rounding noise and prints with many
more digits.

## Minimal repro

```raku
my $jd = DateTime.new('2021-12-24T12:23:00.43Z').julian-date;
say $jd;
say $jd.WHAT;
```

- `raku`: `2459573.0159772`, `(Rat)`
- `mutsu` (`target/debug/mutsu`): `2459573.015977199`, `(Num)`

Same shape for `.modified-julian-date`: raku's `59572.5159772` (`Rat`) vs. mutsu's
`59572.51597719907` (`Num`).

## Affected files (starting point)

- `src/builtins/methods_0arg/temporal.rs` (or wherever `.julian-date`/
  `.modified-julian-date` are computed) — the arithmetic should stay in `Rat`
  end-to-end (day-fraction + epoch-offset math) instead of converting through `f64`
  at any point.
