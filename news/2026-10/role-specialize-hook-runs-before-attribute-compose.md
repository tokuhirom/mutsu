# A user `specialize` on a role's HOW runs before the role's attributes compose

Rakudo drives `$role.HOW.specialize($role, $class)` when a class composes a role, before the
role's attributes are composed into the class, so a hook mixed into the role's HOW can prepare
the class HOW that an attribute's `compose` relies on. AttrX::Mooish does this from an
attribute trait: on a role attribute it mixes `AttrX::Mooish::ParametricRoleHOW` into the role's
HOW, whose `specialize` mixes `AttrX::Mooish::ClassHOW` into the class HOW; the attribute's
`compose` then calls `type.^setup-attr-helpers`.

Three things were wrong in mutsu:

- the `specialize` call was queued only if the role HOW already carried the hook when the class
  started composing, but an attribute trait mixes it in while the class composes the role's
  attributes. It is now queued for every composed role and the hook is looked up when it is driven;
- it was driven after the class's attribute `compose` hooks; it now runs before them;
- inside the trait, `$*PACKAGE.HOW` did not see the mixin the trait had just applied (the
  individual role's cached metaobject was stale against the role group's), and a lexical
  `my role` is keyed by a mangled name while the mixin is stored under the visible one.

`t/oo/role/role-attribute-trait-specialize-hook.t` pins the order for a package and a lexical role
(it also passes under `raku`). On AttrX::Mooish, `t/020-role` and `t/080-aliasing` now get past
`No such method 'setup-attr-helpers'` and stop on the HOW-owned `@!BUILDPLAN` (#13264).
