# A role method's deferred param-type check is re-validated at composition

`registration_role_method.rs`'s per-parameter type-constraint check accepts
a role method's parameter type optimistically whenever the role body has
an unloaded `use` — the not-yet-loaded module might supply the type. But
nothing ever re-validated that guess once the module actually loaded, so a
genuinely mistyped parameter type was accepted forever and its method
silently vanished once the role was composed, instead of raising
`X::Parameter::InvalidType` the way an immediately-unresolvable name
already does.

Each such deferred check is now recorded on the `RoleDef` (propagated when
one role composes another) and re-run in `compose_role_into_class` once the
role's deferred body — which runs its `use` statements — has executed,
reusing `resolve_declared_type_name`'s indirect (env/stash) lookup so a
type that legitimately resolves through the module's exports (#8023,
#7993) is still accepted.

See [#8083](https://github.com/tokuhirom/mutsu/issues/8083) and the
regression test `t/oo/role/role-composed-method-param-typo.t`.
