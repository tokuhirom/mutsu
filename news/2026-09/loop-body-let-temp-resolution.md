# A loop body now resolves its own `let`/`temp` saves, once per iteration

`let` and `temp` are resolved at the end of the enclosing **block** — `temp`
always restores the saved value, `let` restores it unless the block succeeded.
A loop body is a Raku block, so it owes that resolution to every one of its
iterations. mutsu gave it to none of them:

```raku
my $k = 1;
for 1..2 { temp $k = 2; }
say $k;                     # raku: 1   mutsu (before): 2
```

The save was not resolved *per iteration* either, so the second iteration
observed the first one's temporized value:

```raku
my $j = 1;
for 1..2 { say "in: $j"; temp $j = 2; }
# raku:            in: 1 / in: 1
# mutsu (before):  in: 1 / in: 2
```

`while`, `until`, C-style `loop` and `repeat` diverged identically, and so did
`let`:

```raku
my $n = 1;
for 1..2 { let $n = 2; Nil }
say $n;                     # raku: 1   mutsu (before): 2
```

## Root cause

Only three lowerings implemented the resolution: the statement-position bare
block and the value-position `do` block, both through `OpCode::LetBlock`
(GH-7635), and routine/method/closure frame teardown together with the
`try`/implicit-`CATCH` region (GH-7646). A loop body is compiled by none of
them — `OpCode::ForLoop` / `WhileLoop` / `CStyleLoop` / `RepeatLoop` run their
body range directly, taking no `let_saves` mark — so a save made inside one
survived until some outer block happened to resolve it, or forever.

## The fix

The save frame is emitted **inside the loop's own body range**, at its head.
That placement is what makes it per-iteration for free: the loop opcodes
re-run the range on every pass, so `OpCode::LetBlock` re-executes, re-marks,
and resolves what that one iteration recorded. No loop opcode changed, and a
body with no `let`/`temp` in it emits nothing at all, so the overwhelming
majority of loops pay nothing.

The ticket (GH-7677) framed the hard part correctly: a real `let` decides from
the block's own value whether to roll back, and a loop body is compiled in sink
context, so it has no value to offer. The two existing lowerings deliver that
value in different ways — the statement block routes its tail statement through
the topic, the `do` block leaves it on the value stack — and only the second is
available here: a loop body's topic **is** the loop variable, so writing it
would clobber the binding the iteration runs under. So a body containing a real
`let` has its tail statement compiled for value (`value_on_stack: true`) and the
frame is followed by a `Pop` that keeps the body range stack-balanced. A
`temp`-only body needs no value at all — `temp` restores unconditionally — and
keeps its ordinary sink lowering. The value-collecting `for` expression already
leaves the iteration's value on the stack, so there the frame is a bracket and
nothing else.

An iteration that exits early through `next`, `last` or a `return` out of the
enclosing routine is an unsuccessful block exit: the frame's error path restores
both kinds of save, which is what Rakudo does (measured — a `let` rolled back by
each of the three).

## Tests

`t/loop-body-let-resolution.t` (28 assertions) pins the whole matrix: `temp`
restored by all five loop forms, the per-iteration property spelled out as the
sequence of values successive iterations observe, `let` rolling back on an
undefined iteration value and committing on a defined one, the three early-exit
routes, a `temp` nested in an `if` branch, the loop variable staying undisturbed
by the tail-value routing, and the collecting `for` yielding the temporized
value while still restoring it. All 28 pass under Rakudo v2026.07 as well as
mutsu. `t/do-block-let-resolution.t` and `t/routine-body-let-resolution.t` pin
the two lowerings that were already correct and stay green.

## Left open

`my $g = 1; my @v = do for 1..2 { temp $g = 9; $g }` — a collecting `for` whose
tail is the bare temporized variable — yields `[1 1]` under Rakudo and `[9 9]`
under mutsu. That is a container-identity divergence (Rakudo collects the
`Scalar` container and reads it after the restore; mutsu collects the value),
independent of this fix and unchanged by it: with the tail decontainerized
(`$g + 0`) both give `[9 9]`. Filed as GH-7718.
