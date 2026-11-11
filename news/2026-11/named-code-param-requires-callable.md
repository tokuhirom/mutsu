# A named `:&x` parameter requires a Callable

An untyped `&`-sigiled positional parameter already rejected a non-Callable
argument, but its named form did not: `sub f(:&e) { }; f(e => { })` bound the
empty Hash that `{ }` is, and `f(e => 42)` bound the Int. Both now fail with
rakudo's `Type check failed in binding to parameter '&e'; expected Callable
but got ...`, as `:@x` already did for `Positional`.
