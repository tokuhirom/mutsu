# Ecosystem dashboard and dist lock board retired

The ecosystem dashboard issue (#15488) and the distribution lock board (#17702)
are retired. With dist parity at 99.9%, the free queue down to sweep fallout,
and regressions triaged straight from the ledger, the queue/lock/dashboard
machinery no longer paid for itself: a separate `Locking:` protocol with its
own rotation skill, and a nightly job holding `issues: write` to rewrite one
issue body.

- `scripts/ecosystem-dashboard.py` and the sweep's `dashboard` job are gone,
  and so is the `ecosystem-lock-board-rotation` skill (with its
  `live-locks.py`).
- The one part worth keeping, the "Regressed since the last sweep" list, moved
  into the generated rollup: `ecosystem/summary.md` now has a "Regressed since
  the previous ledger" section, comparing the records on disk against the
  ledger commit they replace (`ecosystem/.ledger-ref`, or `--previous-ref`).
  `regressions()` and the git reader now live in `ecosystem_common.py`, with
  self-tests, and the sweep's run report and ledger commit message carry the
  section too.
- A distribution is now claimed through the open `ecosystem` issue that tracks
  it (an `eco-dist:` marker line), with the ordinary `Claiming:` /
  `Releasing:` comments; one with no tracking issue gets a short issue filed
  first. `ecosystem-dist-queue`, `ecosystem-dist-fix`,
  `ecosystem-cluster-filing` and `docs/issue-workflow.md` say so.
