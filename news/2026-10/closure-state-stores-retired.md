# The two per-closure state stores are gone (ADR-0055 slice 5)

A closure's state across calls now lives in exactly one place: the
`ContainerRef` cells it captured. The interpreter used to keep two more:

- `closure_env_overrides`, a per-closure-id copy of the whole captured env that
  the tree-walk carrier saved after every call and restored before the next;
- `closure_captured_state`, the compiled closure path's per-instance replay of
  every free variable the body wrote.

Both are deleted, together with the code that kept them consistent: the
wrap-chain writebacks that copied a wrapper's saved env into the caller, the
`cas` write-back that fished a block's object out of the saved env, the
Proxy FETCH/STORE env sync, the react drive loop's per-callback clear and the
nested-`whenever` exemption it needed, the GC root visits, the thread-clone
copies and the two `.clear()` calls.

Slice 4 had already forked every closure with bytecode onto
`call_compiled_closure`, so only Rust-synthesized `Sub` values without bytecode
(the `substr-rw` Proxy pair, method-object carriers and similar) still reach the
carrier. A census over all of `t/` showed the carrier store never carried live
state for them, and the wrap/`cas`/Proxy readers never fired. The compiled
store only replayed compiler temporaries and `$_`. The `substr-rw` pair already
shares its window length through a cell.

The carrier stays as the runner for those synthesized subs, but it is
stateless now (ADR-0055 §8). A 2026-06 attempt to delete
`closure_captured_state` failed on a closure serialized at `BEGIN` time
(`S10-packages/precompilation.t`, GH2897); that case now counts `0,1,2`
through its captured cell.

Pin: `t/routines/closure/closure-state-lives-in-captured-cells.t`, covering
factory instances with independent typed state, a nested `whenever`
accumulator, the `substr-rw` window, a wrapper counter, a `cas` block's
object write and per-iteration loop closures.
