# A `race for` / `hyper for` body shares its scalars through one cell

`my $n = 0; race for ^8 { $l.protect: { $n++ } }` left `$n` at 2 instead of 8: each batch thread
copied the frame's locals, incremented its own copy of the scalar, and the merge kept one batch's
value. Before the batches clone the frame, `exec_threaded_for_loop` now boxes every plain scalar
lexical the body names (directly or through a closure it creates) into a shared cell
(`box_threaded_body_scalars`, over `CompiledCode::threaded_body_shared_slots`, which the
compile-time escape analysis shares). The second repro of the issue, a debug-build panic on the
ADR-0097 env/slot invariant, no longer occurs. Test:
`t/concurrency/thread-lock/race-for-lock-thread-scalar-updates.t`.
