# reduce/produce steps dispatch compiled-first — t/ripemd.t 295s -> 119s

`reduce_items` (and `eval_produce_over_items`) called every step through
`call_sub_value`, whose body execution is the `eval_block_value` AST
carrier — it re-compiles `data.body` on every invocation. For
`Digest::RIPEMD` this meant the 80-round reduce lambda (including its
`BEGIN`-array's five anon subs) was fully recompiled per step, per task:
gdb showed `compile_routine_closure_body` firing hundreds of times in a
2k-input run, and perf attributed ~10% of the whole rmd160 run to the
compiler plus its malloc traffic.

A `Sub` carrying bytecode (`compiled_code` or `compiled_routine`) now
dispatches through the VM closure path (`vm_call_on_value`, made
`pub(crate)`); Subs without bytecode keep the interpreter carrier.
`last`/`next` loop control propagates identically through both paths.
This retires one more route through the tree-walk-era AST carrier
(reduce in #5942, produce in the follow-up PR).

The companion slice #5941 slimmed the closure-call setup itself:
`&?BLOCK`/block_stack reuse the caller's `Gc<SubData>` (refcount bump)
instead of a full per-call `SubData` clone, the well-known env inserts
(`self`, `&?BLOCK`, `__mutsu_callable_id`, `!`, `_`) are symbol-keyed,
and `sanitize_call_args_owned` passes the caller's owned args `Vec`
through untouched when no callsite-line marker is present (applied to
the six dispatch paths that own their args). Closure-call microbench:
4.30s -> 3.7s.

Measured effect (release, local): `rmd160("a" x 100_000)` 28s -> 12.0s;
the full upstream libdigest `t/ripemd.t` 295s -> 119s (9/9 pass) —
inside the batteries gate's 120s per-file budget for the first time,
though it stays un-whitelisted until another lever provides margin for
slower CI runners (the gate is a hard `timeout 120`). Remaining levers
are recorded in `todo/tickets/digest-ripemd-start-per-block-overhead.md`
status update 4.
