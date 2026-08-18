# EVAL's undeclared-variable pre-check now knows methods get an implicit `*%_`/`*@_`

```raku
EVAL(q[class D { method m { %_.elems } }; say D.new.m(a=>1,b=>2)])
```

Before this fix, `mutsu` raised `X::Undeclared: Variable '%_' is not declared`
here, even though calling the identical method directly (not through `EVAL`)
worked correctly. `raku` has no such discrepancy — `%_` in a method body works
identically whether reached via `EVAL` or not.

## Root cause

`check_eval_undeclared_vars` (`src/runtime/system_eval_vars.rs`) is a static
pre-check `EVAL`/`throws-like`'s string form runs over the parsed AST before
actual execution, meant to raise `X::Undeclared` for a genuinely undeclared
variable. It walked each `Stmt::MethodDecl`'s `params`/`param_defs` — the
*user-written* signature only — to seed the "declared" set, with no knowledge
that a signature-less method body legitimately gets an implicit `*%_` (unless
the class is `is hidden` or the signature already names an explicit named
slurpy) or, if it reads a bare `@_` directly, an implicit `*@_` that binds any
call arity before a runtime die — the same decisions
`method_signature_shared::effective_method_param_defs`/
`needs_direct_positional_placeholder_die*` make at compile/registration time.

## Fix

Replaced the combined `Stmt::ClassDecl | Stmt::RoleDecl` arm with a new
`check_class_or_role_body_undeclared` helper that special-cases a
`MethodDecl` inside: it seeds the method's scope from
`method_signature_shared::effective_method_param_defs` (the same single
source of truth the real registration/compile path uses, gated on the
class's own `is_hidden` — roles have no such AST-level concept and always get
the implicit `*%_`), plus a direct-`@_`-read check mirroring
`needs_direct_positional_placeholder_die`.

Regression test: `t/eval-method-implicit-slurpy-not-undeclared.t` (5
assertions, verified against real `raku`), covering a class method, a role
method, `%_` combined with an attribute read, a direct bare `@_` read (still
dies via its own specific placeholder check, not `X::Undeclared`), and a
sanity check that a genuinely undeclared variable is still caught.
