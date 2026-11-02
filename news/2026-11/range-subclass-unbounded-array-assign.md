# `my @a = $unbounded-range-subclass` stays lazy over the range

`class R is Range {}; my @a = R.new(0, Inf)` filled the array with `(0 (Any) (Any))` instead of the
range's elements, because only a bare unbounded range took the reify-on-demand route. An instance of a
`Range` subclass whose range payload is unbounded now takes it too, unless the class overrides
`iterator` or `list` (#15480). Follow-ups: #16013 (`list` override honoured by `@`-assignment), and
#16014 (a `STORE` role on an attribute container).
