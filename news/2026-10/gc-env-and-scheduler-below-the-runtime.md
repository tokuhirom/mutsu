# GC, Env and the wasm scheduler stop naming the runtime

The fourth slice of the layer split (#10779) took the `make check-layer-deps` count of upward
references from the lower layers from 135 to 112. The AST, `Env` and the GC's counters are now
free of them, and only two remain in the GC (the worker pool's blocking guard).

- `scope_scan` (the shared "own scope" boundary of the AST analyses, ADR-0137) is pure AST code;
  it moved from `src/compiler/` to `src/ast/scope_scan.rs`.
- The `MUTSU_VM_STATS` counters a lower layer bumps now live beside what they count, like the
  regex-capture counters before them: the GC's in `src/gc/stats.rs`, the env copy-on-write
  counter in `src/env/stats.rs`, and the `ContainerRef` cell count at the NaN-box encode
  chokepoint. `vm_stats::dump` reads each through a snapshot, so the report is unchanged.
- `thread_compat` and the wasm32 cooperative scheduler `wasm_sched` moved to the crate root and
  joined the lower layers. The scheduler used to call up into the interval-timer heap to fire the
  next timer; the heap now registers that step with the scheduler when its first timer is armed,
  so until then `pump` correctly has no timer to fire.
- The numeric-subclass payload (`__mutsu_int_value` / `__mutsu_num_value`) readers moved to
  `src/value/numeric_payload.rs`, and `to_int` / `to_float_value` to
  `src/value/numeric_coerce.rs`; `builtins::numeric_subclass` and `runtime::utils` re-export
  them.

Nothing changes in behavior.
