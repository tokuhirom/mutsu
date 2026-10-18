# A block's or an `EVAL`'s `sub h` shadows an outer `my &h`

`my &h = sub { 'outer' }; { sub h { 'inner' }; say h() }` printed `outer`, and
`EVAL q[sub k { 'eval' }; k()]` called the caller's `my &k` (#13855). Since
#13850's follow-up the compiler already compiled such a call as a routine call,
but its run-time resolution still took the visible `&h` binding, a slot of the
enclosing code or a captured `&h`, as a lexical override of the routine. The
compiler now records the call names a nearer `sub` shadows
(`CompiledCode::routine_shadowed_calls`), the caller's `&k` an `EVAL` sees
included, and the call does not consult the outer binding for them.

Two registration bugs came with it: a `sub h` in an `if` body, or in a bare
block that is the unit's last statement, died with "Redeclaration of routine
'h'" because the registration only treated blocks run under `BlockScope` as a
lexical scope. A declaration the compiler finds in a nested scope now carries
`NESTED_SCOPE_DECL_MARKER`, and the registration lets it shadow the outer
binding.
