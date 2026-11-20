# ADR-18393 proposes call frames without an Env overlay

ADR-18393 (Proposed) proposes a new shape for a call frame. A frame whose code never needs a name
view keeps only:

- its slot window;
- a slim call-frame record;
- the routine frame's fixtures.

It builds no name-keyed `Env` overlay, opens no readonly scope, and recomputes no per-(code,
class) fact on each call. Admission is a closed list of ops, so an op nobody classified can only
keep a frame on today's path. A runtime request for the frame's names (`CALLER::`, `callframe`) is
answered from the record and the slots, as ADR-0035 already does for calling frames.

The measurements behind it were taken on `main` @ `41be14d5`, as callgrind slopes:

- **Empty method call `$o.m0`.** It costs ~8.9k Ir. About 3.8k of that is the call-site layers
  and 4.8k is the frame. Overlay, readonly scope, record and per-call facts are about 3.3k of the
  frame.
- **A stage 1 prototype** (`perf/18393-slot-frame-prototype`, behind `MUTSU_SLOT_FRAME`). For
  leaf methods it takes `$o.m0` from 10,029 to 7,727 Ir (−23%) and `$o.m3(2)` from 11,626 to
  8,742 Ir (−25%).
- **The workloads do not move yet.** The AttrX::Mooish t/090 probe (4.63M Ir per round) and
  MCP::Server::Tool::Web `html-to-text` (Δ 3.15G Ir) run their hot methods through the general
  fast binder, not the lean frame. Stage 2, which covers calls, declarations and the fast
  binder's frame, is the first stage that can move them.
- **The estimate.** With closures and class-body statics in later stages, the ADR estimates
  −10 to −12% per Mooish round and −9 to −10% on Web.
