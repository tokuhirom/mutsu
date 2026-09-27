# ADR status lines refreshed after a full ADR survey

A survey of all 130 ADRs on 2026-09-27 filed every untracked remaining work item as its own
GitHub issue (#9892–#9940), and found nine ADRs whose recorded state had drifted from what
shipped. Their status lines and `docs/adr/README.md` index rows now match the code:

- ADR-0030: the closing "This ADR is `Proposed`" note now says it is Accepted and implemented.
- ADR-0043: Accepted; Decision 1 shipped, Decision 2 stays deferred behind its trigger (#9932).
- ADR-0047: P3/P4 are recorded as obsolete (the `subtest` registry rollback they targeted is
  gone); the lexical `role`/`subset` remainder is #9894.
- ADR-0070, ADR-0081, ADR-0101, ADR-0120, ADR-0124: Proposed → Accepted, as implemented.
- ADR-0099 §8: Stage 0 is recorded as implemented (PRs #8277–#8281); the Stage 2 decision is #9916.
