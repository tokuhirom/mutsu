# An anonymous role mixed in with `.^mixin` exposes its attributes and runs their traits

`my $r = role { has $!b is mooish(:lazy) }` evaluates to one candidate of an anonymous role
group, whose type object carries a `\0<id>` site key. `.^attributes`, `.^methods` and the
attribute table did not map that key back to the registered role, so they answered an empty list;
a `but`-mixed object lost the role's attributes in `^attributes`, because the mixin's roles were
read through their display names (`<anon|N>`) instead of their registry keys.

`Class.^mixin(R)` on a class type object also made no mixin class, so the role's attribute traits
never ran until an instance was made (AttrX::Mooish `t/020-role`'s `Foo.^mixin: role { ... }`).
It now composes the mixin class, as `class C does R` does. Pinned by
`t/oo/role/role-expression-mixin-attributes.t`.
