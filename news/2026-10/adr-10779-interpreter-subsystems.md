# Proposed ADR-10779: `Interpreter` subsystems and traits for upward calls

ADR-10779 proposes phase 3 of the layer decomposition (#10779), based on the
phase-2 state map:

- `Interpreter` keeps only the VM frame core. Each of the other 14 subsystems
  becomes its own type that owns its fields and states its own thread-clone
  policy. They are extracted one per PR, starting with the small ones (`guards`,
  then the 51 derived `caches`).
- The 39 `pending_*` "handoff" fields become explicit parameters, not a struct.
- A ratchet will stop the direct field count of `Interpreter` from growing
  again.
- A lower layer that has to call the runtime declares a narrow trait for it,
  and the runtime implements it. The trait is a parameter when the service
  needs a specific interpreter (as with `SubsetBases`). It is a registration
  made once at startup when the service is process-global: the parser's
  compile-time host (slang activation and module export probing) and the
  scheduler that the promise path and the GC use.
- The crate split stays a separate, measured decision.

Writing the ADR also corrected the state map. `clone_for_thread` builds the
child interpreter in one struct literal, so the compiler already catches a
forgotten field. The GC root enumeration that the map cited is test-only.
