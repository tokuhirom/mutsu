# A user sub that shadows a CORE routine no longer hands a mismatched call back to CORE

`sub pick(::T $x, T $y) { $y }` followed by `dies-ok { pick(1, 'two') }` reported the block as
having *lived*, while `try { pick(1, 'two') }` caught the
`X::TypeCheck::Binding::Parameter` the call really does raise (GH #8064). The type capture was a
red herring: the same shape with a plainly typed signature (`sub pick(Int $x, Int $y)`) failed
identically, and a sub whose name CORE does not use (`sub choose-it(::T $x, T $y)`) was fine all
along.

What actually happened is that the call reached CORE's `&pick` and returned its answer. Both
dispatch chains decide whether a user sub shadows a same-named builtin by asking whether that sub
*accepts these arguments*:

* the VM's `dispatch_func_call_inner` consults `user_function_matches_call`, and on `false` goes
  straight to `try_native_function`;
* the interpreter's `call_function_fallback` computes `user_shadows_builtin` and, when it is
  `false`, consults the native arity tables.

For a `multi` that is the right answer — rakudo merges the setting's candidates into the same
candidate list, so `multi sub min(Int $a) {...}; min(3, 4)` legitimately reaches CORE's `min`. For
an `only` sub it is not: a lexical or package `sub pick` hides `&CORE::pick` entirely, so
`pick(1, "two")` is a binding failure and nothing else. Answering it from CORE did not merely
return a wrong value, it made the failure *disappear*, which is why a `dies-ok` around it inverted.

The reason this survived the earlier builtin-shadow work (pinned by
`t/routines/dispatch/builtin-shadow-dispatch.t`) is that the interpreter's gate was written as
`is_builtin_function(name) && ...`, and `BUILTIN_FUNCTION_NAMES` is far narrower than the native
surface: `pick` is answered by `builtins::native_function`'s arity tables without appearing in that
list at all, so the shadow test said "not a builtin, nothing to shadow" and the native table then
answered the call anyway.

Both sites now ask one new predicate, `Interpreter::user_only_sub_hides_builtin`: does an `only`
sub declared under this name resolve for this call, whether or not its signature accepts the
arguments? It opens with the two cached registry lookups every dispatch already performs, so a
program that declares no sub under the name pays nothing, and it deliberately excludes `multi`
names so the candidate-merging behaviour above is untouched. When it answers yes, the call goes to
the interpreter terminal, which resolves the routine and raises the binding or arity error the
declared signature calls for.

The `dies-ok` path was only the loudest symptom; the same fallback silently answered any
sufficiently mismatched call to a shadowed CORE name. Pinned by
`t/routines/dispatch/builtin-shadow-dispatch-refuses-bad-args.t`, which covers the type-capture
repro from the issue, a plainly typed shadow, an arity mismatch, the calls that *do* fit (which
must still reach the user sub), and the `multi` case that must still fall through to CORE.

Closes #8064.
