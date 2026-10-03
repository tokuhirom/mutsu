# Dispatch state leaves `Interpreter`

The eighth subsystem extraction under ADR-10779 (#10779) moved 30 fields into
`DispatchState` (`src/runtime/dispatch_state.rs`). These are the parts of sub
and method dispatch that are not caches:

- the frame stacks for multi, proto, method, metamodel, wrap and `samewith`;
- the `.wrap` chains and their handles;
- the user-declared operator tables;
- the function-key indexes the resolver walks;
- the per-call dispatch flags.

Code now reaches these fields as `self.dispatch.<field>`.

A spawned thread starts with no dispatch in progress, but it still carries over
the program's operator tables, its `.wrap` chains, and the registered stub
declaration sites, so a wrapped routine stays wrapped on every thread.
`DispatchState::fork_for_thread` states that policy in one place, together with
the comment explaining why stub sites travel with the thread.

This step only moves fields; it changes no behaviour.
