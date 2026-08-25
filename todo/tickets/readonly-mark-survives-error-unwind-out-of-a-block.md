# A readonly marking survives an error unwinding out of a bare block, poisoning a later same-named variable

Found while writing `t/readonly-assign-exception-taxonomy.t`
(`news/2026-08/readonly-assign-exception-taxonomy.md`). Pre-existing — it is not
caused by the readonly-kind split, only made easier to see.

## Repro

```
$ raku -e 'try { my $zz := 42; $zz = 23 }; my $y = 1; my $zz := $y; $zz = 23; say $y'
23
$ target/debug/mutsu -e 'try { my $zz := 42; $zz = 23 }; my $y = 1; my $zz := $y; $zz = 23; say $y'
Cannot assign to an immutable value
```

The same shape through `Test`'s native `dies-ok` / `throws-like` reproduces it
too, which is how it was found:

```
$ target/debug/mutsu -e 'use Test; dies-ok { my $zz := 42; $zz = 23 }, "a"; my $y = 1; my $zz := $y; $zz = 23; say $y; done-testing'
ok 1 - a
Cannot assign to an immutable value
```

It is specifically the **error unwind** that leaks: the same block *without* a
throw is fine, and so is the same code inside a `sub` (whose frame rolls the
marking back):

```
$ target/debug/mutsu -e '{ my $zz := 42; }; my $y = 1; my $zz := $y; $zz = 23; say $y'
23
$ target/debug/mutsu -e 'sub s1() { my $zz := 42; }; s1(); my $y = 1; my $zz := $y; $zz = 23; say $y'
23
```

## Root cause hypothesis

`my $zz := 42` emits `OpCode::MarkVarReadonly`, which inserts the *bare* name
into `Interpreter::readonly_vars`. That map is name-keyed and global; it is
rolled back only by `exit_readonly_frame` / `ReadonlyFrameGuard`, and a readonly
frame is opened only for `is_routine` calls (see the `cf.code.is_routine` gate in
`src/vm/vm_call_named_inner.rs`). A bare block therefore has no frame of its
own — it relies on the enclosing routine's frame — and when an exception unwinds
out of the block the marking stays behind until that routine returns, which at
file scope is never.

A later `my $zz := $y` does not clear it: the declaration path in
`src/vm/vm_var_assign_set_local.rs` deliberately **excludes** `:=` binds from its
`unmark_readonly` call, because a literal-bound scalar's marking is set as part
of the bind and unmarking there would let a subsequent `$zz = 6` slip through.

## Why it is more than a one-liner

The obvious fixes each have a cost worth thinking about before picking one:

- open a readonly frame for every block, not only routines — correct, but that
  is a push/pop on a hot path that was deliberately narrowed for performance
  (ADR-0004 J4d replaced a whole-set snapshot with this journal precisely
  because it showed up in profiles);
- scope the marking to the declaring block via the existing `BlockLocalScope`
  machinery — narrower, but needs the block scope and the readonly journal to
  agree on entry/exit, including the abandoned-scope rebalancing
  `replay_readonly_undo` already does for error unwinds;
- key `readonly_vars` by something finer than the bare name (the real fix for a
  whole family of name-collision bugs, and much larger).

## Affected files

- `src/runtime/types/mod.rs` — `enter_readonly_frame` / `exit_readonly_frame`.
- `src/runtime/mod.rs` — `replay_readonly_undo`, `ReadonlyUndo`.
- `src/vm/vm_call_named_inner.rs` — the `is_routine` gate on opening a frame.
- `src/vm/vm_var_assign_set_local.rs` — the `:=`-excluded `unmark_readonly`.
