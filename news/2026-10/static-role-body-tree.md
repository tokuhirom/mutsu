# The unit tree of a `role`, as `static` walks it

For `static` 0.0.1 (#15610), measured against rakudo 2026.09: in the `$*CU`
tree a `Role`'s children are its name and its `RoleBody`, which holds the
role's name and signature (an empty one for a plain role), is a routine
(`RoleBody.compile-time-value` is a `Sub`) and brackets its statements with the
implicit `Role::ResolveInstantiations` and `nqp::list(Role::MetaObject,
nqp::curlexpad)` ones (`Nqp.op` / `Nqp.args` answer). A private method declared
in a role has a `Method` value. `static` 0.0.1 `t/03-calls` and
`t/12-dynamic-coverage` go to 0 `not ok` (rakudo: 0).
