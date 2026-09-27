# Reading an undeclared dynamic variable is now a Failure, not `Nil`

Raku raises `X::Dynamic::NotFound` ("Dynamic variable ... not found") when a
`$*x` / `@*x` / `%*x` is read and was never declared anywhere in the dynamic
scope (`my $*x` in the current or a live caller frame, or a built-in like
`$*OUT`). mutsu already threw this on *assignment* to such a variable
(`OpCode::CheckDynamicVarDeclared`), but a *read* quietly returned `Nil`
instead — so `sub i() { say $*x }; { my $x = 41; i() }` printed `Nil` and
exited 0, where raku dies with `Dynamic variable $*x not found` (issue
#9771).

The fix lands in each read opcode's existing "nothing found anywhere" tail —
`OpCode::GetGlobal` for `$*x`, `OpCode::GetArrayVar` for `@*x`,
`OpCode::GetHashVar` for `%*x` — which is reached only after every real store
(env, a caller's `my $*x`, `PROCESS::`, ...) has already missed. It now hands
back a lazy, unhandled `Failure` wrapping `X::Dynamic::NotFound`, the same
mechanism every other failed operation already uses: `.^name` and `.defined`
still answer without exploding it (`$*nope.^name` is `Failure`,
`$*nope.defined` is `False`), and only a context that actually sinks the
value — `say`, string interpolation, `.Str`, ... — raises the exception.

The built-in-dynamic whitelist that already gated the assignment-side check
and the compiler's `X::Dynamic::Postdeclaration` check
(`is_builtin_dynamic_var`) moved to `runtime::utils` so the read-side check
shares the exact same list rather than risking a second copy drifting from
it — a name on it (`$*OUT`, `$*TOLERANCE`, ...) is always "declared" by the
setting, whether or not mutsu has actually seeded a runtime value for it yet
(some, like `$*RAT-OVERFLOW`, are their own separate ticket).

Pinned by `t/vm/scope/dynamic-var-read-not-found.t`. Two existing tests
encoded the old (wrong) Nil-on-read behavior and were updated to match:
`t/vm/scope/dynamic-var-not-found.t` (a `lives-ok { my $v = ...; }` idiom,
which now needs a trailing statement so the block's return value isn't the
raw Failure — the same idiom already explodes for any other Failure-producing
expression, unrelated to this fix) and `t/vm/scope/dynamic-var-start-leak.t`
(a "did the dynamic leak past its declaring sub" probe now checks `.^name eq
'Failure'` instead of the exact `Nil` rendering).
