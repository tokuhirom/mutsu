# ANALYSIS.md rev11 and an ADR-ledger progress review

`ANALYSIS.md` was last re-verified at rev10 (2026-07-19). 1096 commits later its
picture of the architecture had drifted far enough to mislead: it still listed the
`gc_contents_mut` provenance UB as the #2 roadmap item (fixed since), and it described
neither of the two subsystems that had become load-bearing in the meantime. rev11
re-verifies every claim against HEAD (`c65835e13`) and re-derives the roadmap.

## What changed in the picture

- **ADR-0013 landed.** `GcBox` stores its payload in an `UnsafeCell`
  (`src/gc/gc_ptr.rs:166`), so the ~59 deliberate aliased container writes derive a `&mut`
  with valid interior-mutable provenance instead of casting a `*const`. rev10's soundness
  item is mechanically closed; what remains is that **no Miri job exists**, so the claim
  rests on an argument rather than a check.
- **Two new subsystems are described for the first time** (§1.8, §1.9): the bundled-module
  layer (`modules/`, 22 vendored upstream dists gated by their own upstream suites) and the
  user-facing MOP (`EXPORTHOW::DECLARE` declarator registry, HOW-driven class registration).
  Both matter architecturally because both push load onto the one subsystem that still
  walks the AST — declaration registration.
- **ADR-0016 completed** (all five phases, 2026-07-28…31), leaving one unenforced invariant:
  a `view()`-based variant probe materializes a lazy `Match`.
- **ADR-0015 did not** — `Buf`/`CArray` are native-backed, `array[T]` is not.

## The re-prioritization

The roadmap is now derived from debt shape and dependency rather than profile share (per
PLAN's 2026-07-16 reset, performance is polish). The headline change is that the
lexical-slot / env-writeback campaign moves to #1 **as a correctness item, not a perf one**:
§2.4 collects at least seven open `todo/` findings — closure captures shadowed by callee
parameters, captured-outer `Pair` snapshots, three `whenever`/supply lexical leaks, a
forward-captured `&`-lexical read as `Nil`, and PLAN §6's `start`-block stale writeback —
that all reduce to locals being mirrored into a name-keyed env and written back wholesale.
Each was individually triaged as "not a small slice", which is the signature of a shared
mechanism; fixing them separately also makes the eventual campaign harder, because every
local fix adds another consumer of the mirror.

## The ADR ledger

Reviewing all 17 ADRs found no *wrong* decision, but a systematic drift: implementation
progress gets reported in `news/` and PLAN.md and never folded back into the ADR that owns
the decision. Six ADRs were updated in place — 0001 (an outcome section; its "Track B is
fused with GC, do not start it standalone" rule is superseded by ADR-0013 §7), 0007 (a
forward pointer to its successor), 0011 (Phase 1 → Phases 1–5 landed), 0013 (an
implementation-status section separating the shipped primitive from the missing Miri gate),
0015 (P0–P3a landed, P3b/P3c open), and 0016 (`Proposed` → `Accepted`; it had shipped
completely while still labelled a proposal, which invites a reader to re-litigate an
executed decision). The index in `docs/adr/README.md` carried its own drift and now records
per-ADR progress, and the conventions section states the rule that was missing: **record
implementation progress inside the ADR** — `news/` is where the work is reported, not a
substitute.

ANALYSIS §8 also names the two decisions that are being made by default and deserve ADRs of
their own: the shared worker pool (PLAN §6 has specified its content for weeks while 20
spawn sites × 256 MiB accumulate) and the batteries adoption policy (rung 2 vs. the banned
rung 3, currently recorded only in `BATTERIES.md` and CLAUDE.md).
