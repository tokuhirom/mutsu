# A write through `$OUTER::x` that names nothing dies

`OUTER::` names exactly one scope. When that scope does not declare the
name, `$OUTER::x` is `Nil`, and a read of it already answered `Nil`. A write
through it was silently dropped: mutsu stored the value under the literal
spelling `OUTER::x`. Now it dies as a write to `Nil` does, as on rakudo:

- `$OUTER::x = ...`, also in expression position, dies with "Cannot modify
  an immutable Nil value".
- `$OUTER::x++` and `++$OUTER::x` die with the `X::Multi::NoMatch` that
  `Nil++` raises.

A name the outer scope does declare is written as before.
