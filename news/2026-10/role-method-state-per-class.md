# `state` in a role method is per composing class

A `state` variable declared in a role method was one container shared by every
class composing the role, so `class A does R` and `class B does R` saw each
other's value. Rakudo clones the method per composition; mutsu now scopes the
state by the receiver class for role-origin methods. This fixes META6's
`t/050-assoc.t`, where the `AutoAssoc` role's `state %lookup` served the
`META6` attribute table to `META6::Support`.
