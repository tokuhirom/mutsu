# Compiled-eligible fallback calls run the routine's bytecode

ADR-0019 C6d-5 (#5950) — a def-execution site the C6d survey missed: it
counted six `&def.body` execution sites, but `call_function_fallback`'s def
arm was a seventh (and `call_proxy_callback`'s `run_block(&data.body)` an
eighth, which gets 2 anonymous-block hits across `t/` and stays out of C6d
scope). The arm is the interpreter terminal reached when a name resolves
but the VM's OTF dispatch declined it — a proto name, a multi-candidate
name, or a gate-rejected single — and it compiled the routine's AST body
on every call via `eval_block_value_with_pre_post`: 410 hits across `t/`
in a fresh instrumented survey, dominated by multi dispatch and
`trait_mod:<is>`, with `def.compiled` already attached in essentially
every hit.

The arm now runs `call_routine_def` when the def passes the same
signature/body assessment the OTF dispatch uses
(`def_module_single_sig_body_ok_ignoring_state`), keeping only the
multi/samewith frames (for `callsame`/`nextsame`) and the is-raw/rw Proxy
tail caller-side. A def the gate rejects keeps the interpreter arm
unchanged — that arm is load-bearing semantics for those shapes, not a
missed optimization: a sigilless-scalar param's caller-alias writeback
across an EVAL boundary (`t/sigilless-params.t`'s `EVAL 'swap($a, $b)'`)
only works through the interpreter's sigilless-alias merge, which is
exactly why the OTF gate excluded that shape in the first place. The
first, unconditional version of this fold broke that test, which is how
the gate's purpose was rediscovered.

`state` passes the gate deliberately: `call_routine_def` runs one stable
body identity (the plan's compiled routine, or one memoized OTF compile),
so cells are not severed the way the per-call recompile this replaces
would sever them — verified against `raku` on a state-bearing multi
dispatched through this arm.
