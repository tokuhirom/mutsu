# `&pick` / `&roll` are Routines, `pick($n, a, b, c)` works, `Duration.rand`

Three gaps surfaced by `Data::Generators`, which picks its sampler with
`&method = $size.isa(Whatever) ?? &pick !! &roll` and later calls
`&method($size, @indexes)`:

- `&pick`, `&roll`, `&head`, `&tail`, `&first`, `&classify`, `&reduce` and the
  other list-shaped core subs evaluated to `Nil` as `&`-references, because
  their bare calls are dispatched outside `BUILTIN_FUNCTION_NAMES`. They now
  resolve to first-class Routine values (calling one goes through the same
  by-name dispatch as a bare call), without moving the names into
  `BUILTIN_FUNCTION_NAMES`, which would change how their calls dispatch.
- The sub forms `pick($n, +values)` / `roll($n, +values)` only handled the
  single-list form (`pick(2, @list)`); `pick(*, 1, 2, 3)` died with
  "Unknown function: pick". Every argument past the count is now one element.
- `Duration.rand` / `Instant.rand` (from `Real.rand`, `self.Bridge.rand`)
  were missing; they return a `Num` below the stored seconds.
