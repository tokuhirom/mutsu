# A `for` loop's type capture ends with the loop

`sub f(::T) { for @types -> ::T { ... }; T }` returned the loop's last type
instead of the routine's `T`. The loop binds its capture like a `my \T`, which
since ADR-12441 is a term stored under its own key; the compiler kept treating
the bare `T` as that term after the loop. It now forgets every term the loop
bound once the loop ends, as it already did for sigilless loop parameters.
Found on the Deps distribution, whose `store` method walks `Type.^mro` and
then `Type.^roles` with two such loops.
