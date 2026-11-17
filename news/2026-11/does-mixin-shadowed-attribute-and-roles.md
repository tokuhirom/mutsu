# A mixed-in role's attribute named like the class's own is its own attribute

`$obj does R` reblesses the object into the mixin type `C+{R}`. When `R`
declares an attribute with the same name as one `C` (or a lower mixin level)
declares, the two were one slot: the role's default never reached it, so
`role HasX { has $.x = 'role-x' }; class V { has $.x = 'class-x' }` answered
`class-x` for the mixed-in object's `.x`. Now, as for a subclass shadowing its
parent's attribute, each declaring class keeps its own copy (`Class\0name`),
and the mixin type's accessor and role methods read the role's (#17890).

`.^roles` of such an object also lists the mixed-in roles first, most recent
first, without deduping a role the class already does (`A,A`), as the `but`
wrapper already did.

Still open before every `but` can rebless: a closure a method returns reads a
shadowed attribute from the most-derived class (#17936), a renamed mixin's
`.^mro.gist` head, and the copy's `DESTROY` at teardown.
