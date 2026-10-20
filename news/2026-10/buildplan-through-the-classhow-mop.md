# A class's BUILDPLAN is readable and rewritable through the ClassHOW MOP

Following ADR-14162, `Metamodel::ClassHOW.create_BUILDPLAN` and `publish_method_cache` are real
metamethods that run when a class composes, and a `ClassHOW` mixin can override them the way
AttrX::Mooish does. `nqp::getattr($how, Metamodel::ClassHOW, '@!BUILDPLAN' | '@!BUILDALLPLAN')`
returns the plan as a raw list of tasks in Rakudo's encoding (`0`/`1`/`2`/`3` init, `400` default,
`800` required, `1000` leave, `BUILD`/`TWEAK` submethods), and `nqp::bindattr` with those names
replaces it. The derivation is checked against Rakudo in `t/oo/construct/buildplan-matches-rakudo.t`.
Construction stays native; the `Code` tasks a hook added to `@!BUILDALLPLAN` run after the attribute
initializers and before `TWEAK`, with the instance and the constructor's named arguments.

AttrX::Mooish needed more than the plan, and each piece is a general fix:

- a `class is Proxy` subclass runs its own methods, public and private, on the value `.VAR` returns;
  `callwith` in its `new` reaches `Proxy.new` and keeps the Proxy unfetched;
- `nqp::decont` and `nqp::istype_nd` hand a Proxy back without running its FETCH, and a `Mu`
  parameter no longer FETCHes the Proxy bound to it;
- `Attribute.auto_viv_container`, and `nqp::isconcrete_nd` / `nqp::clone_nd` on a Scalar container;
- two enum values joined by `|` (`pvmForce | pvmNever`) are the junction, not `pvmForce(|pvmNever)`.

`has $.bar is mooish(:lazy)` now reads its builder's value (#14162). The rest of the AttrX::Mooish
suite is tracked in follow-up issues.
