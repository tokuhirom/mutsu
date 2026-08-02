# A statement-level call with a named argument no longer severs the caller's container cell

A whole-container assignment could be silently lost:

```raku
my @arr;
my sub push-one() { @arr.push('x') }   # closure capture -> shared container cell
push-one();

takes-container @arr, ['x'], 'reason', opt => 1;   # statement call, named arg
@arr = ();                                          # <- silently a no-op
say @arr.raku;                                      # ["x"]  (raku: [])
```

Every condition was needed: the caller's variable had to be a closure-captured
lexical (so it lives in a shared `ContainerRef` cell), it had to be passed to the
routine, and the call had to be a *statement* carrying a *named* argument. Drop
any one of them — call it in expression position, pass positionals only, or take
the closure away — and the assignment worked.

## Why

A statement-level `Stmt::Call` whose argument list contains a named argument
compiles to `OpCode::ExecCallPairs`, which dispatches through the interpreter's
`exec_call` rather than the VM's `CallFunc` path. On return `exec_call` writes
each aliased container parameter back to the caller via
`apply_rw_bindings_to_env`.

That writeback already knew not to *replace* a caller entry holding a live
`ContainerRef` — it writes the value through the cell so aliases survive — but the
guard only fired when the incoming value was a plain container
(`&& !updated.is_container_ref()`). An aliased container argument (compiled as
`WrapVarRef`) binds the parameter through a **fresh cell of its own**, so the
incoming value *was* a `ContainerRef`, the guard declined, and the callee's cell
was stored into the caller's env entry.

The caller's local slot still held the original cell. From that point the name had
two cells: `@arr = ()` emptied the slot's cell, while `GetArrayVar` — which reads
env before locals — found the callee's copy, still holding the pre-call contents.
Any read that re-synced the two (a method call on the variable, for instance) hid
the problem, which is why inserting a `say @arr` before the assignment made it
disappear.

## The fix

`apply_rw_bindings_to_env` now unwraps an incoming cell and writes its value
*through* the caller's cell, keeping the caller's cell identity in every case
(and short-circuiting when both sides are already the same cell). The rule is now
unconditional: the caller's binding cell is never replaced, only written through.

Pinned by `t/named-arg-stmt-call-keeps-caller-cell.t` (with
`t/lib/NamedArgStmtCall.rakumod`), which covers the array and hash shapes plus
the positional-only statement call that always worked.

Found while running `t/` against the vendored upstream `Test.rakumod`
(`todo/tickets/vendor-real-test-module.md`): the real module's `is-deeply` is a
module routine, so `is-deeply @events, [...]` is exactly this call shape, and
`t/leave-in-if-branch.t`'s `@events = ()` between assertions never took effect.
That file now passes under `MUTSU_REAL_TEST=1`.
