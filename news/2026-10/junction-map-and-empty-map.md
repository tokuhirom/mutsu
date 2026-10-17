# Junction mapping and an empty `.map` call

`Junction.map` now maps the Junction as one item instead of autothreading the
method over its eigenstates. Calling `.map()` without a mapper now throws
`X::Cannot::Map` with the invocant type and missing mapper in its `what` and
`using` attributes, matching Rakudo.
