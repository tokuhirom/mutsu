# Configuration: `::T $x` role parameters bind `$x`, `duckmap` walks a gathered list

Running the `Configuration` distribution's own suite exposed two general gaps:

- `role R[::T $x]` binds both the type capture `T` and the variable `$x` to the
  argument. Only `T` was bound, so `$x` read as `Any` inside the role body (and
  `block $x` called `.any` on `Any` as a junction). The variable half is now
  recorded with the role's other parameter bindings, for roles composed into a
  class and for punned/parametrised role type objects.
- `.duckmap`, `.deepmap` and `.nodemap` on a lazy list (`gather {..}.grep(..)`)
  handed the list itself to the block instead of its elements. They now force the
  lazy receiver and walk its elements.

`Configuration` t/01-config.rakutest now prints the same as `raku`.
