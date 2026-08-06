# An assignment as a method-call argument no longer evaluates to a Pair

`@r.push($x += 5)` used to push the Pair `x => 5` instead of the assigned
value `5`:

```raku
my @r; my $x; @r.push($x += 5); say @r;
# raku:  [5]
# mutsu (before): [x => 5]
```

## Root cause

`compile_method_arg_with_escape` (`src/compiler/helpers_call_args.rs`)
special-cased a method-call argument that is an `Expr::AssignExpr` as a
*named-argument* sugar — `foo(arg = 1)` treated as `:arg(1)` — for any
`AssignExpr` whose `name` field did not start with `$`/`@`/`%`/`&`. The idea
was that a sigiled target (`$x = ...`, `@x = ...`, `%x = ...`) is a real
assignment expression, while a sigilless bareword name is the "named
argument" shape.

That check could never actually distinguish the two, because
`AssignExpr.name` **never carries the `$` sigil for a genuine scalar
target** — only `@`/`%` targets get one prepended when the AST node is
built (`Expr::Var(name) => AssignExpr{name, ...}` vs. `Expr::ArrayVar(name)
=> AssignExpr{name: format!("@{}", name), ...}`). So `$x += 5` and the
hypothetical `arg = 1` sugar produced the exact same AST shape
(`AssignExpr{name: "x", ...}` / `AssignExpr{name: "arg", ...}`), and the
sigil check silently misfired on every real scalar assignment used as a
method-call argument, converting it into a named-argument Pair.

Investigation also turned up that the "named argument via bareword
assignment" shape this check was meant to support isn't valid Raku syntax at
all — `raku -e 'foo(arg = 1)'` is a parse error ("Preceding context expects a
term, but found infix = instead"), since a bareword is not an assignable
lvalue in Raku. A grep of `t/` and `roast/` turned up no test relying on it
either. So the fix removes the special case outright: an `AssignExpr` method
argument is now always compiled as a genuine expression (mirroring how
`compile_call_arg_with_escape` already handled it for plain function-call
arguments, which were never affected by this bug), evaluating to the
assigned value.

## Verification

`t/compound-assign-as-call-argument.t` pins the plain and compound scalar
assignment shapes (including the anonymous per-routine-call `$` state
spelling that originally surfaced this while pinning
`news/2026-08/anon-state-per-routine-call.md`), plus a regression check that
genuine fat-arrow/colonpair named arguments on a method call still build a
Pair. The full `t/` suite (2906 files) and the 23 whitelisted roast files
covering `assign`/`pair` (820 tests) pass unchanged.
