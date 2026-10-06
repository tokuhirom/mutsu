# A parent class's method stub satisfies a role requirement

`class C is P does R` now composes when `P` declares `method m {...}` for a method `R` requires,
matching rakudo (found via the W3C::DOM distribution's `t/basic.t`).
