# ADR-0019 E9-pre: the raku ground-truth campaign for dispatch-deferral semantics

Ran the mandatory pre-E9 verification campaign (ADR-0019 Phase E, design decision 3 in
`news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md`) as its own dedicated session: every
`samewith`/`nextsame`/`callsame`/`nextwith`/`lastcall`/wrap/proto chain-order scenario was
probed against real raku (Rakudo v2026.06) before any conclusion was drawn, matching behaviors
were pinned, divergences were ticketed, and no cursor implementation code was written.

Deliverables:

- **12 new `t/` pins (38 assertions)**, each verified to pass under BOTH `prove -e raku` and
  `prove -e target/debug/mutsu` — so the pins provably encode raku's answer, not mutsu's:
  `defer-multi-single-class`, `defer-inherited-chain`, `defer-multi-plain-cross-level`,
  `grammar-parse-override-defer`, `method-wrap-callsame-order`, `wrap-multi-candidate-scope`,
  `wrap-mid-mro-callsame`, `lastcall-then-nextsame`, `samewith-restart-from-top`,
  `callwith-rw-passthrough`, `proto-star-cross-mro-candidates`, `build-callsame-nil`.
- **8 divergence findings filed as tickets** (1 deep + 7 tickets), the headline being
  `todo/deep/defer-chain-ranked-multi-order.md`: when multi candidates span MRO levels, raku
  defers along the specificity-ranked MERGED candidate list (an implicit proto clones the
  nearest MRO proto; a plain method in a middle MRO level is a later outer-chain entry whose
  own deferral re-enters lower protos — a parent candidate can legitimately run twice), while
  mutsu walks `(MRO level, declaration order)`. This falsified the E9 cursor design's stated
  assumption that today's "remaining = signature-matching candidates in MRO order" semantics
  were the target — design decision 2 is amended in place, and E9a is blocked on re-drawing the
  cursor's sequence layout against the raku model.
- The other tickets: class-overridden `does`-role methods must not be in the chain (this also
  re-opens E8a's "real walker is authoritative" attribution of its 58 accepted shadow
  mismatches), an explicit child proto must not assume parent candidates, the `is Array` native
  push fallback appends nothing, method-wrap `unwrap`/`restore` are no-ops, `lastcall` inside a
  wrapper kills the dispatcher scope, callsame from gist/Str/raku/new overrides never reaches
  the native Mu implementations, and a cosmetic `Signature.gist` invocant-format difference.

The full scenario-by-scenario table (a-m plus bonus probes, verdict and artifact per row) lives
in the E9-pre section of `news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md`, and the
ADR's E9 checkbox carries the progress note. Next up in Phase E: the decision-2 re-draw, then
E9a (method frames → cursor).
