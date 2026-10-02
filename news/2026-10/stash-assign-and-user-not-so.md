# Runtime-key stash writes visible from every frame; user `.not`/`.so` beat the Bool fast path

Taken from the Logic::Ternary ecosystem distribution (`t/05-coerce-type` and `t/02-operators` now pass, and
`t/01-basic` is down to two `//` assertions).

- `Pkg::{$k} = v` with an unsigiled runtime key now also stores the value in the `our` store, so a sub or method
  reads it back instead of only the assigning frame.
- `.not` / `.so` call a user-defined method of that name even when the class also defines `Bool`.
