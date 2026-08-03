# `Instant` and `Duration` are `Cool` types that `does Real`

rakudo's `Instant.^mro` is `((Instant) (Cool) (Any) (Mu))`, and both `Instant`
and `Duration` `does Real` — so `now ~~ Numeric`, `now ~~ Real` and `now ~~ Cool`
are all `True`, and `Real.abs` applies to both.

mutsu had `Duration` under `Real` alone, and neither type under `Numeric` or
`Cool`. That is not academic. `Test.rakumod` declares its `is-approx` candidates
as `is-approx(Numeric $got, Numeric $expected, …)`, so

```raku
is-approx $*INIT-INSTANT, $manual-init-time, :abs-tol(5), "..."
```

matched **none** of them and fell through to mutsu's native `Test` provider,
which keeps its own counter. The test count reset mid-file, and the module's
`END` plan check then failed on a file that had emitted every assertion:

```
ok 1 - $*INIT-INSTANT is defined
ok 2 -    ... and is-a type Instant
ok 1 -    ... of approximately correct value      <- native provider's counter
# You planned 3 tests, but ran 2
```

Both types now match `Numeric` / `Real` / `Cool`.

## `.abs`, which the type relation immediately needed

Adding the relation alone made `instants-and-durations.t` *worse* (38 assertions
down to 3): the right `is-approx` candidate was finally selected, and it died on
`No such method 'abs' for invocant of type 'Instant'`, because `.abs` came from
`Real` and mutsu's `Real` methods never reached these types.

`Real.abs` is `self < 0 ?? -self !! self` on the value itself, so it **keeps the
type** — `Instant.abs` is an `Instant`, `Duration.abs` a `Duration`. Both store
their seconds as a Real `value` attribute (the same shape the existing `.Rat` /
`.Int` arms coerce), so the implementation recurses into that and rebuilds the
instance.

`roast/S28-named-variables/init-instant.t` (3) and
`roast/S02-types/instants-and-durations.t` (36) now pass under
`MUTSU_REAL_TEST=1`. `roast/S32-num/real-bridge.t` moves from 195 to 195-of-201
with a different remaining gap.

Pin: `t/instant-duration-do-real.t` — without the change it fails 5 of the 6
assertions it reaches and then dies on `.abs`; all 12 pass under `raku`.

**Measure the whole file, not the first failure, when widening a type
relation.** The count going 38 → 3 was the signal that the relation had unmasked
a missing method rather than fixed anything; a "first `not ok`" check would have
shown nothing, because the file aborted before asserting.
