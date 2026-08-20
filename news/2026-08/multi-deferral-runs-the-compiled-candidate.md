# A multi deferral runs the next candidate's compiled body

ADR-0019 C6d-1, first slice. `nextsame`/`callsame`/`nextwith`/`callwith` advance a
multi-dispatch chain by picking the next matching candidate and invoking it. That
invocation went through the interpreter entry `call_function_def`, whose body run
is `run_block(&def.body)` -> `run_block_raw` -> `compile_block_raw`: **the
candidate's AST body was compiled afresh on every deferral.** The deferral now
runs the bytecode the declaration plan already attached to that candidate
(`FunctionDef::compiled`), falling back to one memoized on-the-fly compile per
candidate when the plan attached none.

Measured effect: `t/multi-where-otf-dispatch.t` went from 103 recompiles at
`calls.rs`'s `run_block(&def.body)` to 0. Those 103 were the single largest
contributor to that site across the whole `t/` suite (144 hits total — see
`news/2026-08/c6d-interpreter-body-sites-are-mostly-token-bodies.md` for the survey
this slice came out of).

## The entry point this must *not* use

The obvious rewrite — call `compile_and_call_function_def`, the general VM routine
entry that most callers use — **overflows the stack**. That entry does

```
self.push_samewith_context(&name, None);
let pushed_dispatch = push_multi_dispatch_frame(&name, &args);
```

before running the body, and a deferral chain *owns* the multi-dispatch frame it
just advanced. Pushing a fresh frame for the same name resets the candidate list,
so the next `nextsame` inside the deferred candidate defers to the same candidate
again, forever. `t/multi-where-otf-dispatch.t` aborted with
`fatal runtime error: stack overflow` at its `proto sub seq($) {*}` chain.

The correct entry is the one below that setup — `call_compiled_function_named` —
which runs the routine body without touching the samewith or multi-dispatch
stacks, exactly matching the interpreter entry it replaces (`call_function_def`
pushes neither). It and `otf_compile_function_def` widened from `pub(super)` to
`pub(crate)` so the re-dispatch code in `runtime/` can reach them.

## Pinned

`t/multi-redispatch-compiled-candidate.t` (18 assertions, verified against `raku`
first). The load-bearing cases are the deep chains — a five-candidate `nextsame`
chain and a 20-iteration repeated chain — because a frame-ownership regression
recurses until the stack overflows rather than failing an assertion. It also
covers `callsame` value threading, `nextwith`/`callwith` argument replacement, a
candidate that is itself recursive while deferring, `is rw` writeback through the
chain, named arguments and the deferred candidate's own defaults, a `state`
variable in a deferred candidate, and method `callsame` to a parent class.

Note while writing it: `nextsame` **tail-calls** — it returns the next
candidate's value directly and discards the current frame — so
`'derived ' ~ nextsame()` never concatenates. Value threading needs `callsame`.
