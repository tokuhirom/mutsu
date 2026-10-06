# `Range.rand` fails on an unordered range

`(3..1).rand`, `(1..^1).rand`, `(1^..1).rand`, `(1..1).rand` and the matching `GenericRange`
forms now answer a `Failure` wrapping `X::Range::Rand::InvalidEndpoints` (with `min`, `max` and
rakudo's message) instead of a random number or `Nil`, as rakudo does (#12053).
