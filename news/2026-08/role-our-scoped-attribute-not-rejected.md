# `our $.attr` inside a role body is now rejected (X::Declaration::OurScopeInRole)

Real Raku rejects an `our`-scoped declaration inside a role body at compile
time — "Cannot declare our-scoped variable inside of a role (the scope
inside of a role is generic, so there is no unambiguous package to install
the symbol in)". mutsu already had this check for `our sub`, `our class`,
`our role`, etc. (`role_body_our_scope_violation` in `src/opcode.rs`), but
not for an `our`-scoped attribute declaration (`our $.attr` / `our @.attr` /
`our %.attr`), which it silently accepted and treated as a role-level
(class-level-on-composition) attribute:

```raku
role R {
    our $.shared = "our-attr";
}
class A does R {}
say A.shared;   # was: "our-attr"; raku never gets here — dies at compile time
```

Verified against `raku` (case table): `our $.attr` / `our @.attr` /
`our %.attr` are all rejected identically to `our sub`/`our class`, with the
same generic "variable" message (not an attribute-specific wording).
`my $.attr` and a plain `has $.attr` remain legal — only the `our`-scoped
form is forbidden.

Fixed by adding a `Stmt::HasDecl { is_our: true, .. } => Some("variable")`
arm to `role_body_our_scope_violation`, mirroring the existing `our sub`/
`our class`/`our role` checks in the same function.

Regression test: `t/role-our-scoped-attribute.t`.
