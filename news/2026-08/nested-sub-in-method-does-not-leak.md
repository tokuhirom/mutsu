# A `sub` declared inside a method body no longer leaks to the global scope

```raku
class Scoped {
    method secret-holder() {
        sub secret() { 42 }
        secret();
    }
}
say Scoped.new.secret-holder();  # 42
my $leaked = try { secret() };
say $leaked.defined;             # False now (was: True — 42 was callable at top level)
```

## Root cause

A `sub` nested directly inside a routine body is lexical to that body. The
sub/closure call paths (`call_compiled_function_fast`,
`call_compiled_function_named_inner`) already implement this: before running
the body they `snapshot_routine_registry()`, and on return they
`restore_routine_registry()` (removing whatever the body registered) unless
the return value itself is the escaping routine (`return &innerSub`,
verified by `return_value_escapes_routine`). Method dispatch
(`call_compiled_method` / `call_compiled_method_fast`,
`src/vm/vm_method_dispatch.rs`) never had this snapshot/restore pair at all —
a `sub` declared inside a method body registered under the same global
function-registry key (`GLOBAL::secret`) as a top-level sub would, and
nothing ever removed it, so it stayed callable from anywhere after the
method returned.

## Fix

Added the identical snapshot/restore pair to both method-dispatch call
paths, gated on a new `CompiledCode::declares_inner_routines()` — the same
predicate `CompiledFunction::detect_inner_subs` already computed and cached
for the sub/closure paths, factored out so method dispatch (which has no
`CompiledFunction` wrapper to cache a flag on) can reuse it on demand. Both
functions already funnel every exit (normal return, explicit `return`,
`fail`, error) through a single `final_result` computed just before the
function's tail, so the restore sits in exactly one place per function, right
before the final result is consumed.

Verified the closure-escape case (`return &adder` from a method) still works
after the restore, and that two calls to the same method (or two instances)
each capture their own per-call nested-sub value without cross-leaking.

Pinned by `t/method-nested-sub-registry-scoped.t` and an added assertion in
`t/nested-sub-in-method-compiled.t`. Note: raku itself rejects the repro
above at *compile time* (`Undeclared routine: secret used`) — mutsu does not
yet do that stricter lexical-scope analysis for nested subs in general (a
separate, larger gap), so this fix closes the *runtime* leak only, matching
the `t/nested-sub-reregistration.t` behavior an ordinary (non-method) nested
sub already had.
