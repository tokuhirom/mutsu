# A role HOW's `specialize` hook reaches `.^mixin` types, anonymous roles and `.new` on a mixin type object

An attribute trait that mixes a meta-role into the role's HOW (`$*PACKAGE.HOW does RHow`, as
AttrX::Mooish does) now works in the cases Rakudo supports besides `class C does R`:

- an anonymous role's `HOW does ...` is kept (the minted `ParametricRoleHOW` named its
  `__mutsu_how_target` by display name, `<anon|N>`, not by the key the HOW is looked up under);
- `.HOW` of the type object `Class.^mixin(R)` is the metaobject `specialize` mixed the hook into
  (it was the plain `ClassHOW`), so `$mixin.^hook-method` finds it;
- `(Class.^mixin(R)).new` reblesses the new object into the mixin class, which runs the mixin
  class's own BUILDPLAN (it composed the roles onto a copy, so a plan a HOW replaced never ran);
- the name of a mixin class with an anonymous role shows as `Foo+{<anon|1>}` in `.^name`.

AttrX::Mooish `t/020-role.rakutest` passes with this. Pinned by
`t/oo/role/role-how-specialize-mixin-type-object.t`.
