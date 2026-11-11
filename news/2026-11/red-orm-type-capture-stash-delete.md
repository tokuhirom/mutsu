# Red ORM: type captures, bound stash entries, accessor `:delete`

Several interpreter gaps surfaced by the Red ORM test suite are fixed:

- A routine's `::T` type capture is no longer clobbered when the routine's
  callee captures a type under the same name, and a sigilless argument named
  like a type capture is no longer written back to the caller.
- `OP=` on a method that returns a `Proxy` fetches the current value first.
- A coercion type (`Int(Str)`) ranks as its target type when a user infix
  candidate competes with a core one.
- A class that composes a parameterised role through the MOP
  (`.^add_role: R[T]`) smartmatches `R[T]` and answers `.^does(R[T])`.
- Assigning to a stash entry that was bound (`Pkg.WHO<x> := ...`) goes
  through a bound `Proxy`'s STORE, and refuses a bound type object, as rakudo
  does.
- `$obj.accessor<k>:delete` deletes from the attribute itself, and an absent
  key answers the container default.
- `%h.AT-KEY("k") = v` keeps an untyped hash untyped, so it still `eqv`s an
  equal plain `Hash`.
