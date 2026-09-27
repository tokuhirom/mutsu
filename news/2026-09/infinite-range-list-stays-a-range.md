# `(1..Inf).List` returned the Range itself instead of a lazy List

`Range.List` handled a finite range by materializing its integers into a real
`List`, but for an infinite range (`b == i64::MAX`, reached via `1..Inf`,
`1..*` or `1..∞`) it just cloned the invocant and handed back the `Range`
unchanged: `(1..Inf).List.^name` read `Range` instead of `List`, and `.gist`
printed `1..Inf` instead of rakudo's `(...)`.

`.List` on such a range (`src/builtins/methods_0arg/coercion.rs`) now builds a
genuinely lazy `List`: a `LazyList` carrying the same `Arithmetic` sequence
spec that already backs `1, 2, 3 ... *`, seeded at the range's start with
step 1 and tagged with list context. That makes `.^name` report `List`,
`.gist` render `(...)`, and `.is-lazy` stay `True`, while indexing
(`.List[^5]`) still reifies elements on demand. `RangeExcl`, `RangeExclStart`
and `RangeExclBoth` get the same treatment for consistency; a finite range's
`.List` is unchanged.

Pinned by `t/collections/range-pair/range-infinite-list.t`. Closes #9783.
