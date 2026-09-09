# A collecting `for` gathers containers, not snapshots

A Raku block's value is not decontainerized, so a value-collecting `for` whose
body ends in `$g` gathers the Scalar container `$g` denotes. Every collected
slot then reads that one container when the list is built — after the loop, so
after the last iteration's `temp` restore. mutsu pushed each iteration's *value*
instead, and issue [#7718](https://github.com/tokuhirom/mutsu/issues/7718) had
the three lines that show it:

```raku
my $g = 1; my @v = do for 1..2 { temp $g = 9; $g };     # raku [1 1], mutsu was [9 9]
my $g = 1; my @v = do for 1..2 { $g = $g + 1; $g };     # raku [3 3], mutsu was [2 3]
my $g = 1; my @v = do for 1..2 { temp $g = 9; $g + 0 }; # raku [9 9], mutsu [9 9]
```

The third is the control: `$g + 0` is a value, so the snapshot is right there.

## The mechanism was already half-built

`compile_expr_assign` has emitted `OpCode::TagContainerRef` after an
expression-position assignment for a while, and `exec_for_loop_body` records the
tagged slot and re-reads it once the loop is over. That is why
`do for 1..3 { $s += $_ }` was already `(6 6 6)` — pinned by
`t/for-collect-assign-container.t`. What had no tag was the *other* way a
container reaches tail position: a bare variable read. Adding it is a handful of
lines in `src/compiler/control_for.rs`.

Two things then had to be settled that the issue did not raise.

**Which names count.** Only a container that outlives the iteration. The loop
parameter, the topic and a body-local `my` are each rebound per iteration, so
`do for 1..3 -> $i { $i }` stays `(1 2 3)` and
`do for 1..3 { my $x = $_ * 2; $x }` stays `(2 4 6)`. `state` is the exception
that proves the rule is about storage rather than about where the name was
written — one cell for the whole loop, so
`do for 1..3 { state $s = 0; $s = $s + $_; $s }` is `(6 6 6)`, where mutsu had
been answering `(1 3 6)`.

**Where the re-read reads from.** The existing one went to the env only, and a
plain `my $g` keeps its live value in its local slot with the env mirror
suppressed — so it came back `Any` for every `my` lexical. Nobody had noticed,
because the one shape that reached it, a tail assignment, refreshes the env on
its way through. It now goes slot-first through `gate_local_slot_at` and falls
back to the env, which is the §1.5 order the rest of the VM uses.

## The other bug in the same repro

With the tag in place the first line still read `[9 9]`, for a reason that had
nothing to do with collection: a `temp` in a loop body was never restored, so
the container this change correctly collects was still holding 9 when the loop
ended. That is [#7677](https://github.com/tokuhirom/mutsu/issues/7677), and
[PR #7722](https://github.com/tokuhirom/mutsu/pull/7722) landed its fix — a
per-iteration `LetBlock` frame, including the stack-routed value a real `let`
needs — while this work was in flight. The two together are what make the first
line `[1 1]`, so `t/for-collect-container-tail.t` leans on
`t/loop-body-let-resolution.t` for that half rather than re-pinning it.

The same `temp` gap is still open one branch over:
`if 1 { temp $g = 9 }` leaves 9 where raku leaves 1, and so do `unless` and
`with`. Filed as [#7720](https://github.com/tokuhirom/mutsu/issues/7720).

## What is still divergent

A collected list does not survive as containers *past* the loop:

```raku
my $g = 1; my $s = do for 1..2 { $g }; $g = 5; say $s;   # raku (5 5), mutsu (1 1)
```

That needs real `ContainerRef` cells in an ordinary Array, whose blast radius is
every consumer of a collected list. The deferred re-read is the mechanism this
repository already chose for the assignment shape, and this change extends it
rather than standing a second one up beside it. The reasoning, and what would
justify revisiting it, is in
[ADR-0082](../../docs/adr/0082-a-collecting-for-gathers-containers-not-snapshots.md).
