---
name: mutsu-ticket-flow
description: Implement up to five mutsu todo tickets end-to-end, including deep-ticket triage, PR publication, and verified merges. Use when asked to fix or work through todo/tickets/.
metadata:
  short-description: Deliver up to five mutsu tickets through merge
---

# Mutsu Ticket Flow

Use this skill for requests to fix or process `todo/tickets/` items. A processed ticket ends as
either a correctly filed deep item or a PR whose merge is verified on GitHub and in `origin/main`.

Process at most **five tickets in one user-triggered run**. Count a ticket when its documentation
or implementation PR has merged. After the fifth verified merge, report the next actionable
filename but do not start it. A later user request starts a new run and resets this limit.

## Triage before implementation

1. Read the selected ticket completely. Reproduce its stated behavior when practical, inspect every
   linked ADR/design record, re-check each ADR's current status, and inspect affected code/tests.
2. Keep it in `todo/tickets/` only when evidence supports a small, self-contained implementation
   without a new cross-cutting design.
3. Move it to `todo/deep/` when the fix needs a new or unimplemented architectural decision, a broad
   invariant across execution layers, a prerequisite campaign, or cannot be bounded as one PR. Use
   `git mv`, preserve repro/root-cause evidence, and add a concise dated note naming the owning
   ADR/campaign. Publish that documentation change through the same merge workflow.

Do not special-case one method or test where the ticket establishes a general mechanism.

## Implement and validate a ticket-sized fix

Before starting **every** ticket, return to `main` and update it from the remote:

```sh
git switch main
git pull --ff-only origin main
```

Then create a fresh focused branch from that updated `main`, without overwriting unrelated changes.
Follow the Parser -> Compiler -> VM architecture, add focused regressions, and run targeted tests
while iterating.

Before publishing, run `cargo fmt --all`, `cargo clippy -- -D warnings`, `make test`, and
`make roast` once each. Inspect `tmp/make-test.log` and `tmp/make-roast.log` instead of rerunning a
full suite. Do not publish until both full suites succeed.

## Publish, monitor, and verify merge

Commit the focused change, push it, and create a non-draft PR. Enable auto-merge using merge or
rebase, then verify immediately:

```sh
gh pr view <number> --json isDraft,autoMergeRequest,mergeStateStatus,state
gh pr view <number> --json mergeStateStatus,state -q '.state + " / " + .mergeStateStatus'
```

If it is `DIRTY`, rebase on `origin/main`, resolve it, and force-push with lease. Monitor checks:

```sh
gh pr checks <number> --watch --fail-fast
```

Fix failures forward on the same branch. Continue until GitHub reports `MERGED`, then verify its
merge commit is in `origin/main`:

```sh
merge_oid=$(gh pr view <number> --json mergeCommit -q '.mergeCommit.oid')
git fetch origin main
git merge-base --is-ancestor "$merge_oid" origin/main
```

## Continue the queue

After each verified merge, choose the lexicographically next actionable `todo/tickets/` filename
after the completed item, wrapping to the first filename as needed. Skip deliberate non-divergence
records, blocked tickets, and items whose current evidence makes them deep; record or move the
latter through this workflow. Never start a dependent ticket before its prerequisite merge is
verified. Stop after five processed tickets in this run.
