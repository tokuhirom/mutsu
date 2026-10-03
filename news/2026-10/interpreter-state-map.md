# A map of the `Interpreter` state

Phase 2 of the layer decomposition (#10779) maps which parts of the source use
each of `struct Interpreter`'s 438 fields. `scripts/interp-field-matrix.py`
counts the accesses per file (field reads plus calls to the 352 one-field
accessor methods), and `docs/interpreter-state-map.md` records the results.

Only 12 fields are touched from 30 or more files, led by `env` (334 files),
`registry` (184), `stack` (115) and `locals` (91). Over half of the fields are
touched from three files or fewer. Every field falls into one of 15 proposed
subsystems: frame, handoff, topic, control, io, module, types, lexicals,
threads, dispatch, caches, async, regex, eval and guards. The doc includes a
coupling matrix between them.

Two findings shape the extraction ADR. The 51 cache fields are derived state
touched from 44 files, which makes them the easiest large extraction. The 39
`pending_*`-style "handoff" fields are arguments passed through shared mutable
state, so they should become explicit parameters instead of being moved into a
struct.
