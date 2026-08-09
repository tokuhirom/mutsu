# `our $.attr` inside a role body is not rejected (X::Declaration::OurScopeInRole)

Discovered incidentally while verifying ADR-0019 D9 (dropping
`CompiledRoleDeclPlan::legacy_body`) — unrelated to that change, a
pre-existing divergence.

Real Raku rejects an `our`-scoped declaration inside a role body at compile
time:

```
role R {
    our $.shared = "our-attr";
}
```

```
===SORRY!=== Error while compiling ...
Cannot declare our-scoped variable inside of a role
(the scope inside of a role is generic, so there is no unambiguous
package to install the symbol in)
at ...:2
------>     our $.shared<HERE> = "our-attr";
```

mutsu accepts it and treats it as a role-level (class-level-on-composition)
attribute instead of raising `X::Declaration::OurScopeInRole`, e.g.:

```
role R {
    our $.shared = "our-attr";
}
class A does R {}
say A.shared;   # mutsu prints "our-attr"; raku never gets here
```

`register_role_decl`/`walk_role_body` already has an `our_scope_violation`
mechanism (ADR-0019 D7-1/D9-1, `CompiledRoleDeclPlan::our_scope_violation`,
`role_body_our_scope_violation`) that correctly rejects `our sub`, `our
class`, etc. inside a role — see `check_role_body_our_scoped_decls`/
`role_body_our_scope_violation` in `src/opcode.rs` and
`src/runtime/registration_role_decl.rs`. It just does not currently flag an
`our`-scoped `has` (attribute) declaration the same way. `class_body_has_decl`
/`role_body_has_decl` (`src/runtime/registration_role_body.rs`) both treat
`decl.is_our || decl.is_my` as a valid class-level-attribute form without
checking `is_our` against the role-specific restriction.

Fix likely belongs in whatever builds `our_scope_violation` at plan-lowering
time (`role_body_our_scope_violation` in `src/opcode.rs`) — it should also
scan for `Stmt::HasDecl { is_our: true, .. }` at the top level of the role
body, the same way it presumably already scans for `our sub`/`our class`.
Needs a `raku`-verified case table before implementing (per this repo's
"measure before naming the fix" convention) — in particular, confirm
whether `my $.attr` (accepted by both raku and mutsu today) is the only
non-rejected class-level-attribute form inside a role, and whether the
rejection message/exception type is exactly `X::Declaration::OurScopeInRole`
for the attribute case too.
