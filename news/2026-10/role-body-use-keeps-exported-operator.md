# A role body's `use` no longer hides the role's own exported operator

CSS::Units declares two candidates of one role in the same file:

- `role CSS::Units[\dimension, \units]` declares `multi sub infix:<+>(...)
  is default is export(:ops)`.
- `role CSS::Units { use Color; ... }` imports Color, which exports its own
  `multi infix:<+>` candidates.

In mutsu, `1pt + 2pt` after `use CSS::Units :pt, :ops` answered a plain
`Int`.

A role body's `use` imports into the role's package, the same package the
other candidate's exported `infix:<+>` family is registered under. Color's
`infix:<+>` comes with a proto, and an imported proto hides the family
visible at its target, which is meant for an enclosing scope's family. At
module top level nothing restores what it hides, so the role's own operator
was dropped, and `use CSS::Units :ops` delivered Color's candidates.

An import now leaves alone a family that the package it imports into declared
itself. Only routines from an outer scope are hidden.
