# A parametric role's attribute default survives an intermediate role

`role Kg does U["g"] {}` followed by `class D does Kg {}` (or `5 does Kg`)
lost `U`'s bound type parameter: `D.new.sym` and `(5 does Kg).sym` came back
`Any` instead of `"g"` (#9834). Composing `U["g"]` directly, either into a
class (`class C does U["x"]`) or as a mixin (`5 but U["y"]`), already worked.

`role_body_does_decl` records a role's resolved type-parameter bindings in
`Registry::class_role_param_bindings`, keyed by the *declaring* role's own
name ("Kg" gets `unit -> "g"`). That entry was only ever read back for
methods stamped with their own per-candidate binding; nothing merged it
forward when a class or mixin later composed "Kg" itself, so the binding sat
orphaned and the attribute default for `$.sym` evaluated its `$unit`
reference against nothing.

Three composition sites needed to inherit their parent role's own bindings,
not just the ones resolved for the immediate `does`/`but` clause:
`compose_role_into_class` (`registration_class_compose.rs`, the class-body
`does` path), `role_body_does_decl` itself (`registration_role_body.rs`, so
the chain survives more than one level of role-composes-role), and
`compose_role_on_value` (`types/roles.rs`, the runtime `does`/`but` mixin
path). Each now merges `class_role_param_bindings.get(<parent role>)` into
its own accumulating binding set before evaluating attribute defaults,
falling back to the directly-resolved bindings on any name collision.

Pinned by `t/oo/role/role-does-parametric-role-attr-default.t`.
