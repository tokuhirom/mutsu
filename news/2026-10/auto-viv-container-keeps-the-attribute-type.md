# The attribute container from `auto_viv_container` carries the attribute's type

The Scalar `Attribute.auto_viv_container` answers for a `$` attribute held only a
default value, so AttrX::Mooish's `$!val = $value` into a clone of it accepted any
value. It now records the attribute's type (`has Str $.s` refuses `5` with
`X::TypeCheck::Assignment`), and `nqp::clone_nd` of a Scalar keeps that type on the
copy. A `where` clause is not carried yet (#14391). Part of #14266.
