# An `is rw` parameter's writes follow the caller container's type, not its own

A `$`-sigiled `is rw` / `is raw` parameter binds the caller's container itself,
so Raku checks the parameter's declared type once, when the argument binds, and
checks every later write against the *caller container's* constraint. mutsu
registered the parameter's own type as the assignment constraint, so
`sub g(Str:D $s is rw) { $s = 5 }; my $z = "a"; g($z)` died with "Type check
failed in assignment to $s" where rakudo leaves `$z` an Int (#10146).
`ParamDef::assignment_type_constraint` now returns `None` for such a parameter,
exactly as it already did for a sigilless `\p`; the binder keeps carrying the
source variable's own constraint over.

Two neighbouring holes closed with it:

- The positional-light call path (ADR-0109) promotes an untyped `$s is rw`
  argument to a shared cell but never gave that cell the caller's `of`, so
  `sub q($s is rw) { $s = 7 }; my Str $t = "a"; q($t)` silently stored an Int
  into a `Str` variable. The cell now carries the caller's constraint.
- `$x.&f(args)` compiled to a dynamic method call that only saw `$x`'s value,
  so an `is rw` first parameter could not write back and a multi skipped its
  rw candidate. It now compiles as the plain call `f($x, args)` it is, with a
  literal colonpair invocant kept positional.

Together these take `String::Fields`' `t/01-basic.rakutest` from 12/16 to
16/16 (`$foo.&apply-fields($sf)` into a `Str:D $string is rw` multi).
