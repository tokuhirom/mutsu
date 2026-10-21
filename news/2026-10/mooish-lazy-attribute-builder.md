# AttrX::Mooish lazy attributes run their builder

With the BUILDPLAN protocol of ADR-14162 (#14268) in place, `has $.bar is mooish(:lazy)` still read
`(Any)`: AttrX::Mooish's `AttrProxy`, `ClassHOW` mixin and attribute code leaned on several more
pieces. Each is a general fix (#14162):

- a `class is Proxy` subclass runs its own methods, public and private, on the value `.VAR` returns;
  an untyped `$` attribute of such a class starts as `Any`, so `cas` against a copy compares equal;
- `nqp::decont` and `nqp::istype_nd` hand a Proxy back without running its FETCH, and a `Mu`
  parameter no longer FETCHes the Proxy bound to it;
- `Attribute.auto_viv_container`, and `nqp::isconcrete_nd` / `nqp::clone_nd` on a Scalar container;
- two enum values joined by `|` (`pvmForce | pvmNever`) are the junction, not `pvmForce(|pvmNever)`;
- rebinding only `@!BUILDPLAN` leaves the `@!BUILDALLPLAN` that `create_BUILDPLAN` derived alone, as
  Rakudo does.

`Foo.new.bar` now gives the builder's value. The rest of the AttrX::Mooish suite is tracked in #14266.
