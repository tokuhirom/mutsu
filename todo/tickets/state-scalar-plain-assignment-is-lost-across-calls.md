# A `state` scalar written with `=` (not `++`) loses the write between calls

`state $n = 0; $n = $n + 1;` does not accumulate. Every call sees the
initializer value again, so a `state` counter written with plain assignment is
silently stuck at its first value:

```
$ raku  -e 'sub f() { state $n = 0; $n = $n + 1; $n }; say f(); say f(); say f();'
1
2
3
$ mutsu -e 'sub f() { state $n = 0; $n = $n + 1; $n }; say f(); say f(); say f();'
1
1
1
```

`$n += 1` behaves the same way. `++$n` and `$n++` are correct, and so is a
`state` aggregate (`state @a; @a.push(1)`), which is why the existing coverage
misses this: every `state` case in `t/` — `t/state-per-clone-named-subs.t`,
`t/state-in-loop-body-accumulates.t`, `t/module-state-sub-shared-cell.t`,
`t/anon-state-per-routine-call.t` — increments with `++`.

Found while measuring ADR-0019 C6d-1, not caused by it: it reproduces on
`main` and predates that work. The routine runs on the ordinary compiled path
(`MUTSU_VM_STATS` reports 0 function fallbacks), so this is a compiled-path
bug, not a fallback one. Notably the *interpreter* entry got it right: the
same operator body invoked through `call_function_def` accumulated correctly
(pinned now by `t/user-operator-compiled-body.t`'s `infix:<ss>` cases).

## What distinguishes the working shapes

The write reaches the state cell only when something *else* in the body reads
the variable in a way that forces a cell-direct read afterwards. Assignment as
an *expression* is also fine — only the statement form followed by a plain read
loses it:

| body | raku | mutsu |
| --- | --- | --- |
| `state $n = 0; $n = $n + 1; $n` | 1, 2 | 1, **1** |
| `state $n = 0; $n += 1; $n` | 1, 2 | 1, **1** |
| `state $n = 0; $n = $n + 1;` (implicit return of the assignment) | 1, 2 | 1, **1** |
| `state $n = 0; $n = $n + 1; return $n` | 1, 2 | 1, **1** |
| `state $n = 0; $n = $n + 1; my $r = $n; $r` | 1, 2 | 1, **1** |
| `state $n = 0; $n = $n + 1; say "in:$n"; $n` | 1, 2 | 1, 2 |
| `state $n = 0; my $x = ($n = $n + 1); $x` | 1, 2 | 1, 2 |
| `state $n = 0; $n++; $n` | 1, 2 | 1, 2 |
| `state $s = ""; $s ~= "x"; $s` | x, xx | x, **x** |

So the assignment writes the routine's VM local slot, and only some reads
flush the slot into the state cell — the interpolating read does, the trailing
bare read does not. The `++` forms presumably mutate the cell in place. That
matches the dual-store shape recorded in
`memory/project-mustache-remaining-two-files.md` ("a slot is only filled by a
cell-direct read").

A bare block reached through a statement modifier is broken the same way even
with the interpolating read, so the loop-body clone path shares the defect:

```
$ raku  -e '{ state $n = 0; $n = $n + 1; say $n; } for 1..3;'   # 1 2 3
$ mutsu -e '{ state $n = 0; $n = $n + 1; say $n; } for 1..3;'   # 1 1 1
```

## Why this is not a quick fix

The bug is in which side of the slot/cell dual store owns a `state` variable's
value at each read and write, so the fix has to establish one rule for all
four of: the assignment opcode's target, the plain read, the interpolating
read, and routine exit. Picking the wrong one re-introduces the per-clone
identity behavior that `t/state-per-clone-named-subs.t` and
`t/concurrent-state-var.t` pin (a nested named sub must re-initialize its
`state` per enclosing call, while a top-level sub must not), so it needs the
same care as the ADR-0018 slot/env work rather than a local patch at the
assignment site.

## Where to look

- `src/vm/vm_var_ops.rs`, `src/vm/vm_misc_scope.rs` — state-cell read/write ops.
- `src/vm/vm_call_named_inner.rs`, `src/vm/vm_call_fast.rs` — the per-call
  state-cell seeding and the routine-exit flush.
- `src/vm/vm_closure_dispatch.rs` — the block-clone variant of the same.
