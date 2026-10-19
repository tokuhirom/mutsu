# Lazy sequences refuse counting, ordering and sampling the way Rakudo does

Rakudo checks `is-lazy` before it iterates. On an infinite or `lazy`-marked sequence, its
counting, ordering and sampling methods never run the sequence:

- `sum`, `reverse`, `pick`, `roll`, `elems`, `Numeric`, `Int` and `Seq.end` answer a `Failure`
  of `X::Cannot::Lazy`. Its message names the method and the type it would reify onto, as in
  `Cannot .sum a lazy list onto a Seq`.
- `tail`, a counted `roll`, and `List.end` die with it.

mutsu got these wrong in two ways:

- On an infinite sequence it died outright, so `//` could not recover.
- On a `lazy`-marked finite list it answered, so `(lazy 1,2,3).sum` was `6`.

Both now match Rakudo.

`(1, 2 ... *).values` now stays lazy. Before, it answered `()`.

The answer no longer depends on how the call compiles. Both of the VM's method lanes (an
expression receiver and a variable receiver) now ask the lazy step's pulls before their own
lazy handling, and their duplicate placeholder and `cache` code is gone (ADR-13891, #11276).
