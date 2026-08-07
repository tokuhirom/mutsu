# A multi-parameter `for` loop no longer clobbers the outer scalar it shadows

```raku
my $v = "outer";
for (1, 2) -> $k, $v { say "in loop $k $v" }   # in loop 1 2
say "after: $v";                               # raku: outer / mutsu (before): (uninitialized Any)
```

A multi-parameter `for` loop (`-> $k, $v`) binds its parameters with plain
assignments emitted into the body prefix (`build_for_bind_stmts`), reusing
whatever local slot the name already occupies in the enclosing scope rather
than allocating a fresh shadow slot. `exec_for_loop_body`
(`src/vm/vm_for_loop_body.rs`) already saved and restored each multi-param
name's `env` entry after the loop, but a compiler-scoped `my $v` lives in a
local slot and is not necessarily mirrored into `env` (the single-store
default), so the save read `None`, the restore took the "remove the env
key" branch, and the local slot kept the loop's last-iteration value.

Fixed by baking each multi-param name's local slot into the compiled
`ForLoopSpec` (`multi_param_locals`, computed at the same compile point as the
existing single-param `param_local`) and snapshotting/restoring that slot's
value directly — independent of whether `env` ever had an entry for the name
— mirroring how the single named parameter's slot is already restored via
`RestoreForParam`. This also fixes the case where an inner loop reusing the
outer loop's own parameter name (`for 1..2 -> $i { for (...) -> $a, $i { } }`)
silently corrupted the outer `$i`.

Covers both sigil'd scalar (`$v`) and sigilless (`\value`) multi-param names.
The `@`/`%`-sigil case (an array/hash multi-param shadowing an outer
same-named array/hash) has a different root cause — the shared local slot
holds a mutable container, not a plain value, so a snapshot/restore of the
container handle alone does not undo the loop's in-place content mutation —
and is tracked separately as
`todo/tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md`.

Pinned in `t/for-multi-param-type-constraint.t` (untyped/typed/sigilless
outer scalar, and the nested-reuse case).
