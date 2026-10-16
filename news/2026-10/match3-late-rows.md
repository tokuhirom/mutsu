# slice, toggle, rotate, join and friends are late rows; slice follows Rakudo

`slice`, `toggle`, `tree` with a depth spec, `is-lazy`, `rotate` on a `List` or `Seq`, `Thread.join` and `Any.join`
lost their name-matching arms in the interpreter's method cascade. They are late rows of the method table, which only
the MRO walk reaches (ADR-11276 §9.136). The twins and late rows are now a group registered after every other group.

`slice` had two implementations, one of them unreachable. The one that remains matches Rakudo:
- its indices are flattened, so `(1, 2, 3, 4).slice((0, 2))` is `(1 3)`;
- it ends at the first index past the end;
- an index that does not ascend dies with `Provided index 0, which is lower than 3`.

`.join` with two separators dies with Rakudo's `Too many positionals passed`.
