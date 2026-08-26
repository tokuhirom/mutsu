# Role type objects pretend to be `Cool`, and `.HOW.pretending_to_be` answers

`role Role { }; say Role ~~ Cool` answered `False` (raku: `True`), and
`Role.HOW.pretending_to_be` died with
`No such method 'pretending_to_be' for invocant of type 'Perl6::Metamodel::ParametricRoleGroupHOW'`.

## Root cause

Rakudo mixes `Metamodel::TypePretense` into the three role metaclasses
(`ParametricRoleGroupHOW`, `ParametricRoleHOW`, `CurriedRoleHOW`), which makes an
un-composed role type object type-check against the `Cool`/`Any`/`Mu` chain even though a
role has no MRO of its own. mutsu answered `Mu` and `Any` only by accident: those are the
two universal arms of `Interpreter::type_matches` (`Mu` matches everything, `Any` matches
everything but `Junction`/`Mu`), so nothing role-specific was ever consulted. `Cool` has
no such universal arm, so it fell through to the ordinary class-hierarchy walk and
answered `False`. Nothing implemented `pretending_to_be` at all.

A curried role (`role R[::T] {}; R[Int]`) was worse: it arrives as a `ParametricRole`
value, and the `ParametricRole ~~ Package` arm of `smart_match` short-circuits the generic
Package handling with an explicit `false`, so `R[Int] ~~ Mu` and `R[Int] ~~ Any` were
`False` too.

## Fix

The pretended chain is now one constant, `ROLE_PRETENDS_TO_BE` (`["Cool", "Any", "Mu"]`,
`src/runtime/types/type_registry.rs`), read by all three consumers:

- `type_matches_value` asserts `Cool` for a role type object, whether it arrives as a
  `Package` (the role group, or a natively-modelled core role) or as a `ParametricRole`
  (a curried role). Only the type OBJECT pretends — an instance of a class that composes
  the role is still judged by that class's own MRO.
- `smart_match`'s `ParametricRole ~~ Package` arm answers the pretended chain before its
  role-subtyping walk, so a curried role smartmatches `Mu`/`Any`/`Cool` like the group.
- `.^pretending_to_be` / `.HOW.pretending_to_be` is a new metamethod in
  `dispatch_classhow_dispatch`, gated on the receiver naming a role type
  (`Interpreter::is_role_type_name`) so a `ClassHOW`/`EnumHOW`/`SubsetHOW` receiver keeps
  throwing `X::Method::NotFound`, exactly as raku does.

Note that raku's `pretending_to_be` takes no invocant-type parameter, so
`Role.^pretending_to_be` is a "too many positionals" error there; `Role.HOW.pretending_to_be`
is the spelling the documentation and the test use.

Pinned by `t/role-composition-gaps.t`.
