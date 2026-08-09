# X::OutOfRange exceptions now carry structured attributes

`DateTime` and `Date` validation failures (`month out of range`, `day out of range`,
`hour out of range`, `minute out of range`, `minutes of timezone out of range`) now
raise a properly structured `X::OutOfRange` exception with `.what`, `.got`, `.range`,
and `.message` attributes, matching Raku's documented `X::OutOfRange` type.

Previously these were plain `RuntimeError::new(msg)` strings, so `throws-like` calls
that destructure the exception (e.g. `throws-like { ... }, X::OutOfRange, what => rx{minute}`)
failed because the attributes were absent.

## Effect

- `roast/S32-temporal/DateTime.t` under `MUTSU_REAL_TEST=1`: 314/314 (was 310/314 —
  four `throws-like` tests checking `.what` on range errors failed)
