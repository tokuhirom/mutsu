# First `Interpreter` subsystem extracted: the `.raku` cycle guards

The first subsystem extraction under ADR-10779 (#10779) moved the four fields
that keep a `.raku`/`.gist` render of a self-referencing structure from
looping into their own type, `RakuCycleGuards`
(`src/runtime/raku_cycle_guards.rs`).

`rakuseen_active`/`rakuseen_cycle_hit` (behind `Mu.rakuseen`) and
`raku_leaf_active`/`raku_leaf_cycle_hit` (behind the native instance `.raku`)
were the same mechanism written out twice. Each pair is now one generic
`CycleGuard<K>` with three operations:

- `revisit`: has this id been met again while it is still being rendered?
- `enter`: start rendering an id.
- `leave`: finish rendering an id, and report whether the result needs the
  `(my \NAME = ...)` wrapper.

A spawned thread starts with empty guards; the type itself states that in its
`fork_for_thread`. `Interpreter` went from 439 to 436 direct fields.
