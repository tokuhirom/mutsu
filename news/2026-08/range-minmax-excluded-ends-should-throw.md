# `Range.minmax` now rejects an excluded end it cannot name

`Range.minmax` folds an excluded endpoint into the value it returns — but only
when the Range is `is-int`, where "excluded" is just a ±1 adjustment. For any
other Range the excluded bound has no nameable concrete value, and Rakudo fails
with `X::AdHoc: Cannot return minmax on Range with excluded ends`. mutsu silently
returned the raw endpoints instead, so `(1.1..^5.2).minmax` answered `(1.1 5.2)`
— a pair that claims `5.2` is *in* the range when it is exactly the value the
`^` excludes.

`minmax` now shares `src/builtins/range_bounds_int.rs` with `int-bounds`:

* `is-int` (both endpoints genuine `Int`s, and not the `i64::MIN`/`i64::MAX`
  open-end sentinel): `(min + excludes-min, max - excludes-max)`, so `1..^5` is
  `(1, 4)` and `1^..^5` is `(2, 4)` as before;
* otherwise, an excluded end at either side is the `X::AdHoc` error —
  `1.1..^5.2`, `1.0..^5.0`, `'a'..^'z'` and `1..^Inf` all reach it;
* otherwise the plain endpoints.

The old `GenericRange` arm's narrower "Cannot determine minmax with excluded
infinite endpoints" error is subsumed by that middle case.

One adjacent bug fell out of the shared helper: an *inclusive* open-ended Range
answered its raw sentinel, so `(1..Inf).minmax` was
`(1 9223372036854775807)`. It now reports `(1, Inf)`, matching both raku and
mutsu's own `Range.bounds`. `int64.Range.minmax` — where both extremes are
present and the sentinel is the real bound — still answers the concrete pair.

mutsu raises this as an error rather than returning a `Failure` the way Rakudo
does; that is mutsu's general convention for these Range errors, and `try` /
`CATCH` / `dies-ok` behave identically either way. `.min` and `.max` are
deliberately untouched: they keep the raw endpoints even when excluded
(`(1..^5).max` is `5`), as raku has it.

Pinned by `t/range-bounds-and-rotor.t`.
