# An `is rw` parameter's store is not constrained by a caller's same-named parameter

A store through an `is rw`, `is raw` or sigilless parameter asked the type record
of its name through the whole call chain, so a *caller's* `Pair $v` (or a subset
type) constrained the callee's `$v is rw`: `Color`'s `clip-to($min, $v is rw, $max)`
died with "expected CSS::Properties::Util::ColorAST but got Rat". Such a parameter
is now recorded as a slot the routine declares (ADR-13757), so a store asks only
its own frame for the record, where the binder puts the bound container's
constraint. A typed caller variable still constrains the store.

CSS::Properties `t/colors.t` goes from 16 to 33 ok (rakudo 33).
