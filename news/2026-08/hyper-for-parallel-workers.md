# `hyper for` in expression position actually runs on worker threads

`my @a = hyper for LIST { BODY }` (and the same shape with a postfix, as in
`mandelbrot.raku`'s `.rotor`) used to parse as `.hyper(do for …)`: the loop
ran sequentially on the main thread and `.hyper` only wrapped the already
computed list. Statement-level `hyper for` did spawn a thread, but only one,
and that thread ran the whole loop — enough for `$*THREAD.id` to differ, not
enough to use more than one core.

Expression-position `hyper for` / `race for` now parse like `lazy for`
(`ForMode::Hyper` / `Race` on a `DoStmt`), `compile_do_for_expr` honours that
mode, and the VM batches iterations across `available_parallelism` workers,
concatenating collected results in input order.

Pinned by `t/hyper-for-parallel.t`.
