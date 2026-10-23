# An `is Type` attribute FETCHes a Proxy initial value

`C.new(:c($proxy))` for `has @.c is Tuple` handed the `Proxy` itself to the container
type's `.new`, giving `Tuple.new(Tuple.new(1,2,3))`. `coerce_value_to_is_type` now
FETCHes it first (ADR-0040's store boundary). ValueClass's `clone` hits this, as it passes
the `get_value` of attributes it holds behind a `Proxy`: `t/01-basic.rakutest` goes 52 ->
55 ok of 56 on #14588. Pinned in `t/oo/attribute/is-type-attribute-fetches-proxy-initial.t`.
