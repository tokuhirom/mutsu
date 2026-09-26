# `.^add_role` accepts a role declaration expression

```raku
class D { }
D.^add_role(role :: { method hello { "hi" } });
D.^compose;
say D.new.hello;   # raku: hi -- mutsu: Unknown role: __ANON_ROLE_0__ 1
```

A role declaration used as an expression evaluates to the *individual*
parametric role it declares, which mutsu names with a declaration-site key
(`<group>\0<role_id>`, `src/runtime/types/role_candidate.rs`). That module's
contract is that every consumer of a role type object -- `but`, `does`,
composition, type matching -- normalises the site key back to the role group
first. `^add_role` was a consumer that did not: it rendered the site key to text
(the `\0` separator came out as a space, hence `__ANON_ROLE_0__ 1`) and looked
that up as a role name. It now normalises like the others, so an anonymous
role, one held in a variable, and a named `role R { }` expression all compose.

Red's `MetamodelX::Red::Model.compose` adds exactly such a role
(`self.add_role: type, role :: { method TWEAK(|c) { ... } }`), so every Red
model died there once #9497 and #9498 had cleared the way; the models now
compose their columns and reach the next independent blockers. Pinned by
`t/oo/role/add-role-declaration-expression.t` (#9520, part of #7988).
