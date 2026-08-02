# A radix literal with no fraction is an Int, and a big rational answers `.abs`

Two independent gaps, found together because the first produced the value that
tripped the second. Both surfaced in `roast/S02-literals/radix.t` under the real
`Test` module, where the file aborted at test 61 of 158 with

```
No such method 'abs' for invocant of type 'Rat'
  in sub is-approx at .../Test.rakumod line 304
```

## The radix literal

`:16<dead_beef*16**8>` was a `Rat` in mutsu and an `Int` in rakudo. mutsu built
every radix literal that had a fraction *or* an exponent as a rational and left
it that way.

Rakudo's rule (`radcalc`) is not "the value came out integral" — it is whether
the **fractional digits evaluate to zero**:

| literal | rakudo |
| --- | --- |
| `:10<2.0>` | `Int 2` — even though the plain literal `2.0` is a `Rat` |
| `:16<f*16**1>` | `Int 240` |
| `:16<dead_beef*16**8>` | `Int 16045690981097406464` |
| `:16<f.8*16**2>` | `Rat 3968` — integral, but the fraction was not zero |
| `:16<dead_beef.face>` | `Rat` |

mutsu now decides the same way, in the parser's generic-radix path. Note the
asymmetry is real and deliberate on rakudo's side: the *runtime* string forms
(`:10("2.0")`, `"2.0".parse-base(10)`) stay `Rat`, and mutsu already matched
there.

## The big rational

A rational whose numerator outgrows `i64` is a `BigRat`, a separate `ValueView`
variant from `Rat`. The 0-arg numeric method dispatch knew nothing about it, so
`.abs`, `.sign` and `.sqrt` all declined and were reported as
`No such method 'abs' for invocant of type 'Rat'` — the type name says `Rat`,
which is exactly why this reads as impossible. Worse, `.Int` did not decline: it
answered `(n / d).to_i64().unwrap_or(i64::MAX)`, so
`(16045690981097406464/1).Int` silently returned `9223372036854775807`.

All four are fixed; `.Int` now returns the exact big integer through
`Value::from_bigint`, which normalizes back to `Int` when it fits.

## Effect

`roast/S32-str/parse-base.t` and `roast/S32-num/fatrat.t` now pass completely
under the real `Test` module, and `roast/S02-literals/radix.t` runs all 158
tests instead of aborting at 61 (two unrelated parse-error-classification
failures remain: `:0<...>` and a bare `:2` should be rejected).

Pin: `t/radix-literal-int-and-bigrat-methods.t` — all sixteen assertions also
pass under `raku`.
