# ADR-0019 E9: deferral-cursor design redrawn as a flat expansion, confirmed by prediction

Follow-up to the E9-pre campaign (same day): the two probes run after the campaign PR
confirmed a concrete replacement for the falsified part of E9's design decision 2, and the
re-draw is now written into `news/2026-08/adr0019-e8-e11-candidate-sequence-semantics.md`
("E9 design decision 2 — REDRAWN") with the ADR's E9 checkbox carrying the progress note.

The model: the deferral sequence is a FLAT expansion — concat over MRO classes of, per class,
either its plain method or its proto's specificity-ranked candidate block, where an implicit
proto clones the nearest MRO proto and merges its own candidates (an explicit proto stands
alone), and the same candidate appearing in several blocks is correct re-visit semantics, not
a dedup bug. Both confirming probes were predicted before running raku and hit exactly:
a parent candidate legitimately runs twice in one call (block re-visit), and a three-level
implicit-clone chain runs the same inherited candidate three times while skipping non-matching
candidates per-call. A flat cursor index therefore suffices for E9a/b/c — only the sequence
builder changes, not the cursor mechanics.

Probe 2 also surfaced one more real divergence, filed as
`todo/tickets/multi-matcher-admits-int-for-num.md`: mutsu's multi candidate matcher admits an
Int argument for a `Num $x` parameter and then dies in the binder
(X::TypeCheck::Binding::Parameter) where raku rejects at dispatch (X::Multi::NoMatch) — a
matcher/binder inconsistency that would kill the redrawn cursor's advance filter mid-chain,
so it is listed as a prerequisite/co-requisite for E9a.
