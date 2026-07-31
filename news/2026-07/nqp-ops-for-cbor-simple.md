# The nqp op family CBOR::Simple needs — `use Cro::HTTP::Router` now loads

Resolved 2026-07-31 (Cro campaign slice 2; filed the same day by the
web-framework survey follow-up). `Log::Timeline` — a hard dep of Cro::HTTP —
eagerly loads its `CBOR::Simple` output backend, an `nqp_ops_only` dist, so
`use Cro::HTTP::Router` died at `nqp::bitor_i`.

Three pieces, extending the existing narrow nqp subset rather than adding a
guts layer:

- **`compiler/nqp_forms.rs`** — the CONTROL-FLOW ops (`nqp::if`,
  `nqp::unless`, `nqp::while`, `nqp::until`, `nqp::stmts`) are special
  forms, not calls: operands evaluate lazily/repeatedly, so they compile to
  jumps. `nqp::const::BINARY_*` size/endian flags fold to integer literals
  (values match MoarVM — pinned by running the test under raku).
- **`runtime/nqp_ops.rs`** — the VALUE ops, dispatched from the
  unsupported-op guard so unknown ops still fail loudly: int/bit/comparison
  families, `add_I`, `istype`, `p6box_*`, `elems`/`chars`, `decode`,
  `atpos_*`/`bindpos_*`, `slice` (end-inclusive), `splice`, and the sized
  binary `readuint`/`readint`/`readnum` / `writeuint`/`writeint`/`writenum`
  over Buf/Blob (reusing the `buf_write_int` / `read_int_value` machinery).
  `nqp::setelems` extended to presize plain/native arrays.
- **`vm_register_ops.rs` closure-capture fix** — CBOR's encoder exposed a
  real coherence bug: a **value-typed scalar** (`my int $pos`, or an
  `Int:D $pos is rw` parameter) captured and `++`d by a stored closure was
  snapshotted instead of cell-boxed (the constrained-scalar boxing skip),
  so the owner's own reassignment diverged into two stores — every encoded
  string byte landed one position off. Native and builtin value types
  (`Int`/`UInt`/`Num`/`Str`/`Rat` + natives) now box like `Mu`; the cas
  concern that motivated the skip targets class-typed scalars, which keep
  it (S17-lowlevel/cas.t stays green).

Result: `use CBOR::Simple`, `use Log::Timeline`, **`use Cro::HTTP::Router`**
all load, `route { get -> { ... } }` builds a
`Cro::HTTP::Router::RouteSet`, and `cbor-encode` produces byte-correct CBOR
(`{a => 1, b => [1,2,3]}` → `A2 61 61 01 61 62 83 01 02 03`). `cbor-decode`
has one remaining blocker — a forward-captured `&decode` snapshotted as Nil
— filed as `todo/tickets/forward-captured-code-var-snapshot.md` (it gates
CBOR functionality only, not the Cro load path). Pin: `t/nqp-cbor-ops.t`
(17/17 under raku too).
