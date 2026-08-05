# A routine code object runs its compiled body in the interpreter carrier

ADR-0019 C6d-4 (#5948). After C6c, `call_sub_value`'s
`eval_block_value(&data.body)` was the one dispatch path left that still
executed a routine code object's AST — reached when a `.wrap` chain routes
the original sub's direct run (the `callsame`/`nextcallee` legs) through
the interpreter carrier, and by the other carriers that call a routine
value through that entry (230 of the site's 9,574 hits in a fresh `t/`
survey; the rest are blocks and closures, which carry `compiled_code`, not
`compiled_routine`, and keep the carrier). The fork mirrors
`vm_call_on_value`'s C6c dispatch: run `SubData::compiled_routine`'s
bytecode via `call_compiled_closure`.

The fork is gated off for a routine with a scalar `is rw`/`is raw`
parameter: mutsu's rw writeback is a value copy-back, not a shared
container, and a wrap chain's rw relay currently survives only through the
interpreter carrier's same-name blanket merge — the different-name relay is
broken on every path, on `main` too, with a raku baseline recorded in
`todo/tickets/rw-writeback-through-wrap-chain-needs-shared-cells.md`. The
cell-based fix filed there also removes this gate.

The slice also fixed a pre-existing rw bug on the C6c value-dispatch path
(`my &b = &bump; b($c)` left `$c` unchanged; raku mutates it):
`call_compiled_closure`'s exit read the rw param's value from env for the
caller writeback, but a scalar rw param is bound to a slot-only local, so
the body's write never reached env and the writeback copied the stale
bind-time value. The rw-param slot flush that
`call_compiled_function_named_inner` already performs is now ported there,
storing through a shared cell when the binder installed one so aliases
survive.

Pinned by `t/wrap-original-runs-compiled-body.t` (10 assertions,
raku-first).
