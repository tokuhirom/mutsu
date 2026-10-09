# A role method's trait sees the role's ParametricRoleHOW

Inside a custom `trait_mod:<is>` applied to a method declared in a role, `$m.package.HOW` now
answers `Perl6::Metamodel::ParametricRoleHOW` (as Rakudo does) instead of `ClassHOW`, so modules such
as Method::Protected can tell roles from classes. The role is marked as "in trait dispatch" on the
registry for the duration of the trait call, and `.HOW` honours that marker.
