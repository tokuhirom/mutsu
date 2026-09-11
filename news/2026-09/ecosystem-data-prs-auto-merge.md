# The ecosystem sweep's records auto-merge: there is nothing in them to review

`.github/workflows/ecosystem-sweep.yml` shipped opening its pull request and
stopping there, deliberately — `docs/ecosystem-parity.md` §8 says to check that
`measured.host` is uniform before landing a sweep, and that read as "a human
looks at it first". That was wrong, and the workflow now enables auto-merge
(merge, never squash) on the pull request it opens.

The records are **thousands of machine-generated measurements**. A reviewer
cannot tell a right number from a wrong one by reading the diff: `"ok": 12` for
one test file of one distribution carries no signal a person can check. So the
review step decided nothing, while costing the one thing the workflow exists to
remove — a human in the loop between "the numbers changed" and "the numbers are
published".

Everything that actually protects the numbers runs **before the diff exists**,
and all of it is already automatic:

- the vendored-`Test.rakumod` probe, so both sides count `ok` lines emitted by
  the same harness (ADR-0085 D8);
- a working `bwrap`, or the job fails rather than measuring unsandboxed;
- one release binary, one pinned rakudo and one ecosystem-index snapshot shared
  by every shard, so nothing inside a single number was measured against a
  different denominator;
- `scripts/ecosystem-ci.py provenance` — which is §8's host-uniformity check,
  automated; its verdict is what gates the `history.tsv` row;
- the guards that withhold a `summary.json` or a history row from a partial or
  mixed sweep.

CI is therefore the gate, and the pull request is the **audit trail and the
revert handle** rather than a review queue — which is also what CLAUDE.md's PR
workflow step 4 has always said to do with a pull request in this repository.
`publish: branch` stays available for a sweep someone does want to inspect
before it lands.

Recorded as an amendment in ADR-0085 D9 rather than silently: the ADR's D9
amendment now says the pull request auto-merges, and why the first version's
review step was a misreading of §8.
