# Native integer coercions numify aggregates to their element count

`[1, 2].int8`, `%(a => 1).int64` and `(1, 2).byte` answered 0 because the
native integer coercions fell back to a string parse of the receiver. As in
Rakudo (`Cool.int8` is `self.Numeric.int8`), a List, Array or Hash now numifies
to its element count before the coercion wraps it.
