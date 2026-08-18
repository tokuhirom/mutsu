# A dynamic-variable write from inside any bare block no longer reverts on block exit

```raku
my $*x = 1;
{ $*x = 99; }
say $*x;   # raku: 99   mutsu (before): 1
```

Originally filed as a narrower "a `LEAVE`-driven `PROCESS::` write is lost
specifically inside `for`/`while` loop bodies" ticket while investigating
`Log::Timeline`. Investigation found the real bug is much more general: **any
plain reassignment of an existing dynamic variable (`$*x = ...`, or a
`PROCESS::<$x> = ...` pseudo-stash write, which stores through the identical
`*x`-prefixed env key) made from inside ANY bare block** — not just a `LEAVE`
phaser, and not just inside a loop — was lost once that block exited:

```raku
PROCESS::<$X> = 42;
{ PROCESS::<$X> = 99 }
say PROCESS::<$X>;   # raku: 99   mutsu (before): 42
```

## Root cause

`exec_block_scope_op` (`src/vm/vm_misc_scope.rs`), the runtime handler for
`OpCode::BlockScope`'s env restoration on exit, treated **any** env key
starting with `*` (the internal spelling for a dynamic variable — `*x`,
`@*x`, `%*x`) as unconditionally block-scoped: reverted to its block-entry
value on exit, regardless of whether it was genuinely *redeclared* fresh in
that block (`my $*x = ...`, which real Raku does scope to the block) or was
just a plain *write-through* to an already-existing outer dynamic (which
real Raku does not scope — it mutates the existing container, visible after
the block exits). Ordinary (non-dynamic) variables already had the correct,
precise distinction via `block_declared` (a per-block set of names actually
`my`-declared there); dynamic-var keys had their own blanket special case
that bypassed that check entirely, predating the more precise mechanism.

## Fix

Removed the blanket `*`-prefix special case and let dynamic-var keys fall
through to the same `block_declared`-based ownership check every other
variable already uses. A genuine `my $*x` redeclaration is still correctly
scoped (it populates `block_declared` with the `*x` key exactly like an
ordinary `my $x`); a plain write-through to an existing dynamic now
correctly propagates out.

Regression test: `t/dynamic-var-write-through-block-persists.t` (6
assertions, all verified against real `raku`), covering a `PROCESS::` write
from a plain bare block, a `for`-loop `LEAVE`, a `while`-loop `LEAVE`, a
plain `$*x` reassignment, and a guard that a genuine `my $*x` redeclaration
still correctly reverts on block exit.

## Found but out of scope

A separate, narrower, pre-existing bug in the same area: `PROCESS::<$x> =
Nil` stores a literal `Nil` instead of decaying to the `Any` type object the
way an ordinary `my $x = Nil` scalar assignment does — filed as
`todo/tickets/process-dynamic-write-nil-not-decayed-to-any.md`.
