# `(* += 1)($x)` writes back to its argument

A single-star compound assignment such as `(* += 1)($x)` now binds the argument's
container, so a direct call updates `$x` like Rakudo. The placeholder scan treats
`_ = ...` as a mutation again, which makes the topic parameter `is raw`.

Two map paths had to learn the same thing so `@a.map(* += 1)` keeps writing back:
the native rw map classifier now accepts an assignment expression, and the shared
rw map loop no longer rebinds a raw `_` parameter by value over its container
(which broke shaped-array `map(* *= 2)` when the parameter became raw).
