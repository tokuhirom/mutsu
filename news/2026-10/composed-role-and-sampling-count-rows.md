# Composed built-in roles and the sampling count answer through rows

`Positional.of`, `Associative.of`/`keyof` and `Iterable`'s `hyper`/`race`/`lazy`
of a user class composing those roles are rows of the roles, and the
`Callable` count of `pick`/`roll`/`pickpairs`/`grab` (`$bag.pick(* - 1)`) is
resolved by the sampling rows themselves (ADR-11276 §9.165); two more by-name
gates of the interpreter's method entry are gone.
