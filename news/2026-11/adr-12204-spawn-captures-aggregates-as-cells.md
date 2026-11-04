# Proposed: a spawned block captures its `@`/`%` lexicals as shared cells (ADR-12204)

The name-keyed cross-thread store has needed the same guard in three call paths in one
week (#15156, #16345, #16395) for the same "callee's `my %pars` vs the caller's" defect.
ADR-12204 records the structural fix ADR-0010 names as its destination: aggregates that a
spawned block references are captured as shared cells like plain scalars already are, per
name, so the lane (and its masks) shrinks to an opaque remainder and can then be deleted.
It lays out four slices; slice S1 (an oracle that lists the shapes the capture analysis must
learn first) is filed as #16404. Part of #12204.
