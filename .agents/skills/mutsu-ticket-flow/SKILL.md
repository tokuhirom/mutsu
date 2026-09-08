---
name: mutsu-ticket-flow
description: Implement up to five mutsu backlog issues end-to-end, including deep-ticket triage, PR publication, and verified merges. Use when asked to fix, process or work through todo:ticket issues — the whole queue or a named slice of it, such as the tier:N tickets, the ones with no tier label yet, or "keep opening PRs for them".
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

## The request is already complete — do not ask what this file settles

A request to work a slice of the queue ("the `todo:ticket` issues with no `tier:*` yet, oldest
first, open the PRs", "process the `tier:N` tickets and keep the PRs coming") is a standing
instruction. Everything about *how* is decided here, so asking it back — "one PR per issue?", "shall
I continue with the next one?", "should I open the PR now?" — costs a round-trip and answers
nothing. The settled defaults:

| Question you might be tempted to ask | The standing answer |
| --- | --- |
| One PR per issue, or one for the batch? | **One issue, one PR.** Never bundle, never stack. |
| Which issues are in scope? | The filter the user named, oldest-first, skipping `working` / live claims. No `tier:*` is a workable state, not a blocker. |
| Should I claim it / add `working`? | Yes — the protocol below, every time. |
| Add a test? Write `news/`? `Closes #NNNN`? | Yes to all three, on every code fix. |
| May I open the PR / enable auto-merge? | Yes. The request already said so; use the merge method, then watch CI and fix forward. |
| Shall I continue to the next ticket? | Yes, straight on, up to the five-ticket run cap below. |
| This one turns out to be deep / already fixed — is that OK? | Yes. Re-triage to `todo:deep` or close it with the evidence; both are legitimate outcomes. |

Ask only when the answer is genuinely the user's — a decision `CLAUDE.md` reserves for them (a
rung-3 native provider, a new or superseding ADR, weakening a CI gate, dropping a whitelisted test),
or two readings of the issue that give materially different implementations and cannot be settled
from its own evidence. Even then: park that one issue, finish the rest of the batch, and raise the
question in the final report instead of idling the queue.

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
when the user explicitly asks to process multiple tickets or the queue. Any request that names a
*set* — "the `tier:N` tickets", "the ones with no tier", "the queue", "one after another", "keep the
PRs coming" — **is** that explicit ask; do not treat it as a single-ticket request and do not ask
for confirmation before the second one. Count a ticket when its
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

Before publishing an implementation PR, run `cargo fmt --all`, `make lint`, `make test`, and
`make roast` once each (`make lint` rather than a bare `cargo clippy` — it adds the three
configurations CI's `lint-configs` job gates on and the default clippy is blind to). Inspect
`tmp/make-test.log` and `tmp/make-roast.log` with the Grep tool rather than rerunning a suite for
its output. Do not publish an implementation PR until both full suites succeed.

In a remote container `make roast` has a fixed set of three environment-only failures it cannot
avoid (`uid 0` breaks two `chmod`-based file-test files; the network sandbox times out one socket
file). Confirm the failing set is a subset of the table in
[docs/agent-environments.md](../../../docs/agent-environments.md) **by name** before treating a red
roast as publishable — a fourth file, or a different subtest range inside those three, is your
change.

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

Then choose the next actionable open issue **from the slice the user named** (the whole
`todo:ticket` queue, or the `tier:N` / no-tier subset they asked for) — oldest first, **skipping
every issue labelled `working`** or carrying a live claim, plus deliberate non-divergence records,
blocked tickets, and items whose
current evidence makes them deep (relabel the latter through this workflow). Never start a dependent
ticket before its prerequisite merge is verified. For a single-ticket request, report that issue
number and stop. Continue only when the user explicitly requested multiple tickets or queue
processing, and stop after five processed tickets in that run.
