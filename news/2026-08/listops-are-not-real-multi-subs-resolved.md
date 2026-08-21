# Resolved: core listops are not real multi-subs

The finding originally tracked as `todo/deep/listops-are-not-real-multi-subs.md`
(local/imported `multi splice`/`push`/etc. destroying the core array
candidate instead of adding to it, and `&push`/`&splice` being uncallable or
silently doing nothing) is fixed by ADR-0044 D1 — see
[adr-0044-d1-listop-callable-routines.md](adr-0044-d1-listop-callable-routines.md)
for the fix and
[docs/adr/0044-listops-are-routines-not-a-syntactic-rewrite.md](../../docs/adr/0044-listops-are-routines-not-a-syntactic-rewrite.md)
for the design.

The `String::Splice` repro that originally surfaced this
(`todo/tickets/dist-test-suite-failures-batch.md`'s real-dist compat sweep) is
covered by the shape pinned in `t/listop-imported-multi-extends-core.t`.

The one piece this file's "why this is deep" section anticipated that is
still open is narrowness ranking between core and user/imported candidates
(the ADR's alternative B) and the accessor/subscript first-argument shapes
under a competing multi (D3) — both are recorded, accepted non-goals in the
ADR, not open bugs.
