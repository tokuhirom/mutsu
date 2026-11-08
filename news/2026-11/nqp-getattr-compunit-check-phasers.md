# `$!check-phasers` of the compilation unit is a pushable list

`nqp::getattr($*CU, RakuAST::CompUnit, '$!check-phasers')` used to answer `Nil`, so
Qwiratry's `BodyRewrite.install` died with `nqp::push: expected a list ... got Nil`.
It now answers the unit's raw list of CHECK closures: whatever a module's `sub EXPORT`
pushes onto it runs at the importing unit's CHECK phase, after the unit's own CHECK
phasers and the `add-check-code` queue and before `INIT`, as on rakudo. Covered by
`t/modules/compunit/compunit-check-phasers-attr.t`.
