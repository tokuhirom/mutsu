# Control-flow and lifecycle state leaves `Interpreter`

The seventh subsystem extraction under ADR-10779 (#10779) moved 25 fields into
`ControlState` (`src/runtime/control_state.rs`):

- the CONTROL/CATCH handler stacks;
- `let`/`temp` saves;
- END/LEAVE/CHECK/BEGIN phaser bookkeeping;
- `once` blocks;
- the program's halt and exit status;
- uncaught-exception reporting.

Code reaches them as `self.control.<field>`.

A spawned thread still starts with nothing in progress, but two things are not
plain defaults. The thread shares the parent's `once` store, so a `once` runs
once across all threads, and it continues the parent's `once` scope ids. The
main interpreter's ids start at 1. `ControlState::new` and
`ControlState::fork_for_thread` now state these rules, together with the
comment that justifies sharing the store.

`pending_dispatch_error` stayed on `Interpreter`. It is set by one call and
taken by the next, which makes it a side channel, and ADR-10779 D3 turns those
into explicit parameters instead of struct fields. Its subsystem rule now
classifies it as such. `Interpreter` went from 319 to 295 direct fields. Only
fields moved; the behaviour is unchanged.
