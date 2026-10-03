# The ecosystem sweep is landed by a Claude Code routine, not a GitHub App

The nightly ecosystem sweep used to open its records pull request itself. A pull
request opened with the default `GITHUB_TOKEN` never starts CI, so the `collect`
job minted a token from the release GitHub App. That App is a bypass actor on the
`main` ruleset, and its key sat in the job that handles third-party test output.

The job now only pushes `ecosystem/sweep-<date>-<run>` with `GITHUB_TOKEN` and
holds no secret. A new skill, `ecosystem-sweep-landing`, does the rest, and runs
nightly as a scheduled Claude Code routine:

- It verifies the branch:
  - exactly one commit on a `main` ancestor;
  - only `ecosystem/` paths;
  - valid JSON.
- It opens the pull request under the maintainer's account, so CI runs, and
  enables auto-merge.
- It rebuilds a conflicted branch with the same "newer mutsu commit wins" rule.
- It files issues for root-cause clusters the tracker does not have yet, at most
  three a night.
- It deletes the landed branches.

Sweeps dispatched with `publish: branch` or from a feature ref push
`ecosystem/hold-*`, which the routine never lands. ADR-0085 gains a third
amendment to D9 recording the change.
