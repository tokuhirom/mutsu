# An Int range that really ends at `i64::MAX` is no longer read as an open range

The compact integer ranges (`Range`, `RangeExcl`, ...) store an open end (`1..*`, `1..Inf`) as the
sentinel `i64::MAX` (an open start as `i64::MIN`), so `1..9223372036854775807` answered
`.infinite` as `True`, `.is-int` as `False`, and `.elems` died with "Cannot .elems a lazy list".
A range written with genuine `Int` endpoints now goes through one constructor,
`builtins::arith::range::int_range`, which keeps the compact kind except when a bound really is
`i64::MAX` / `i64::MIN`: that range is a `GenericRange` of two `Int`s, the shape `1..2**70` already
has, so every reader of the compact kinds keeps meaning what the bounds say and no sentinel read
had to change. `1..*`, `1..Inf`, `-Inf..5` and `int64.Range` are untouched.

Expanding such a range exposed an `i + 1` overflow in the numeric range walk
(`value::to_list_range`): it panicked in a debug build and wrapped past the end in a release build.
The step now promotes to a big integer, so the end test stops the walk. Pinned in
`t/collections/range-pair/range-i64-bounds.t`
([#12055](https://github.com/tokuhirom/mutsu/issues/12055)).
