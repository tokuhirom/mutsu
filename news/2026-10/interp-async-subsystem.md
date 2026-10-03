# `gather`/supply/react state leaves `Interpreter`

The fourth subsystem extraction under ADR-10779 (#10779) moved the 23 fields
that hold the interpreter's in-flight asynchronous and lazy state into
`AsyncState` (`src/runtime/async_state.rs`):

- `gather`/`take` items, limits and suspend/resume points;
- the lazy-pull entry depths;
- supply emit buffers and stream consumers;
- `react` subscriptions and the react waker;
- the routine invocation-id block.

Code reaches these fields as `self.async_state.<field>`. A spawned thread starts
with all of this state empty, as `clone_for_thread` already did field by field.
`AsyncState::fork_for_thread` now states that policy in one place.
`Interpreter` went from 375 to 353 direct fields. This step only moves fields;
behaviour is unchanged.
