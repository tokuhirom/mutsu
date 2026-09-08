---
name: mutsu-ticket-flow
description: Implement up to five mutsu backlog issues end-to-end, including deep-ticket triage, PR publication, and verified merges. Use when asked to fix or work through the todo:ticket issue queue.
metadata:
  short-description: Deliver up to five mutsu tickets through merge
---

# Mutsu Ticket Flow

Use this skill for requests to fix or process `todo:ticket` issues on `tokuhirom/mutsu`. A
processed ticket ends as either a correctly relabelled deep item or a PR whose merge is verified on
GitHub and in `origin/main`.

Never file, label, comment on or close an issue in any repository other than `tokuhirom/mutsu`.

The `gh` commands below are the local-dev-box form. A remote container has no `gh` — translate them
with the mapping table in [docs/agent-environments.md](../../../docs/agent-environments.md) and use
the GitHub MCP tools instead. Every step of the flow is available in both; only the command surface
differs.

## Claim the issue before you start

Agents run in parallel, and every one of them posts as the same GitHub user, so a claim has to name
itself. Before any investigation:

1. Read the issue's comments. A live claim by a different branch means it is taken — move on.
2. Post a comment whose first line is exactly `Claiming: <the branch you will push>`.
3. Read the comments again. The live claim with the **lowest comment id** wins (they come back
   oldest-first; ids increase, `created_at` can tie). If that is not yours, post
   `Releasing: <your branch>` and take another issue.
4. Only then add the `working` label and start.

When you are done — merged, stopped, or blocked — post `Releasing: <your branch>` and remove the
`working` label. Read `docs/issue-workflow.md` for the full label scheme and why the comment log,
not the label, is the record.

Process at most **five tickets in one user-triggered run**, and only continue beyond the first
when the user explicitly asks to process multiple tickets or the queue. Count a ticket when its
re-triage or implementation PR has merged. For a single-ticket request, report the next actionable
issue number after its verified merge but do not start it. After the fifth verified merge, report
the next actionable issue number but do not start it. A later user request starts a new run and
resets this limit.

## Triage before implementation

1. Read the selected issue completely, including its comment thread. Reproduce its stated behavior
   when practical, inspect every linked ADR/design record, re-check each ADR's current status, and
   inspect affected code/tests.
2. Keep the `todo:ticket` label only when evidence supports a small, self-contained implementation
   without a new cross-cutting design.
3. Relabel it `todo:deep` when the fix needs a new or unimplemented architectural decision, a broad
   invariant across execution layers, a prerequisite campaign, or cannot be bounded as one PR. Post
   a comment recording the repro/root-cause evidence you gathered and naming the owning
   ADR/campaign, so the re-triage is not a bare label change.

Re-triage touches no repository files, so it needs no `cargo fmt`, `cargo clippy`, `make test`, or
`make roast` — but it is not "done" until the `working` label is removed and the comment explaining
the relabel is posted.

Never overlap full-suite runs. They share Cargo locks, temporary logs, and
test-harness state; wait for one to finish before rerunning it for evidence.

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

Before publishing an implementation PR, run `cargo fmt --all`,
`cargo clippy -- -D warnings`, `make test`, and `make roast` once each. Inspect
`tmp/make-test.log` and `tmp/make-roast.log`. Do not publish an implementation
PR until both full suites succeed.

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

After each verified merge, close the issue (the PR body's `Closes #NNNN` does this; verify it
actually closed), remove any lingering `working` label, and write the accomplishment up as
`news/YYYY-MM/<slug>.md`.

Then choose the next actionable open `todo:ticket` issue — oldest first, **skipping every issue
labelled `working`**, plus deliberate non-divergence records, blocked tickets, and items whose
current evidence makes them deep (relabel the latter through this workflow). Never start a dependent
ticket before its prerequisite merge is verified. For a single-ticket request, report that issue
number and stop. Continue only when the user explicitly requested multiple tickets or queue
processing, and stop after five processed tickets in that run.
