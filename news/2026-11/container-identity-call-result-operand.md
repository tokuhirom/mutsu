# `&[=:=]` through a code object: a variable against a call result is not identical

`&with($first, &as($next))` (Iter::Able's `is-all-same(:with(&[=:=]))`) compared the values
of a named scalar and a call result and answered True. The call-site rule now treats a named
scalar against an unnamed operand like the operator form does: a Scalar is never the operand's
container when both hold plain values.

An itemized element (`[$(3), $(3)]`) counts as a plain value there too, so Iter::Able
`t/18-is-all-same` and `t/19-is-all-different` now match rakudo.
