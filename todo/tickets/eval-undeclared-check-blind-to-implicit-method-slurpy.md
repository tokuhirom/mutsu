# EVAL's undeclared-variable pre-check doesn't know methods get an implicit `*%_`/`*@_`

Found while writing a regression test for
`todo/tickets/method-direct-at-underscore-should-be-rejected.md` (now
`news/2026-08/`).

`check_eval_undeclared_vars` (`src/runtime/system_eval_vars.rs`) is a static
pre-check `EVAL`/`throws-like`'s string form run over the parsed AST before
actual execution, meant to raise `X::Undeclared` for a genuinely undeclared
variable. It walks each `Stmt::MethodDecl`'s `params`/`param_defs` — the
*user-written* signature only — to seed the "declared" set
(`add_routine_locals`, around line 382). It has no knowledge that a
signature-less method body legitimately gets an implicit `*%_` (always) or
`*@_` (when the body reads it directly, before/regardless of this ticket's
fix), the way `method_signature_shared::effective_method_param_defs`/
`needs_direct_positional_placeholder_die*` compute at compile/registration
time.

## Repro

```
$ mutsu -e 'EVAL(q[class D { method m { %_.elems } }; say D.new.m(a=>1,b=>2)])'
Runtime error: X::Undeclared: Variable '%_' is not declared.
```

vs. running the identical code directly (not through `EVAL`), which works
correctly:

```
$ mutsu -e 'class D { method m { %_.elems } }; say D.new.m(a=>1,b=>2)'
2
```

`raku` does not have this discrepancy — `%_` in a method body works
identically whether reached via `EVAL` or not.

## Why this is not a one-liner

`add_routine_locals`/`check_eval_undeclared_vars` operates purely on the raw
AST (`Stmt::MethodDecl.params`), with no access to the class-body-walker
context (`is_hidden`, whether this is a role vs. class method, etc.) that
`effective_method_param_defs` needs to decide whether `%_` is even legal
here. A correct fix needs `find_undeclared_var_in_stmt`'s `MethodDecl` arm
to mirror that same decision (implicit `*%_` unless `is hidden`; implicit
`*@_` only for a class-body method whose body reads it directly, not a role
method) rather than just checking `params` — duplicating (or factoring out
and sharing) logic that currently lives in `method_signature_shared.rs`.

## Why this is worth tracking (but not urgent)

Every `t/`/roast test discovered so far that exercises `%_`/`@_` in a method
calls the method directly, not through `EVAL`/`throws-like`'s string form,
so this has not been observed to cause a real test failure — it surfaced
only while writing a new test that happened to reach for `throws-like`'s
string form (worked around by using the block form,
`throws-like { ... }, Type, desc`, which does not go through this
EVAL-specific pre-check at all). But it is a genuine, user-visible `EVAL`
compatibility gap for any code that legitimately uses `%_`/`@_` inside a
method body evaluated via `EVAL`/`throws-like` string form.

## Repro (regression test once fixed)

None yet. Once fixed, add a case to a `t/eval-*.t` or `t/method-*.t` file
asserting `EVAL(q[class D { method m { %_.elems } }; D.new.m(a=>1)])`
succeeds instead of raising `X::Undeclared`.
