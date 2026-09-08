---
name: mutsu-ticket-flow
description: Implement up to five mutsu backlog issues end-to-end, including slices of a deep campaign issue, deep-ticket triage, PR publication, and verified merges. Use when asked to fix, process or work through todo:ticket or todo:deep issues — the whole queue or a named slice of it, such as the tier:N tickets, the ones with no tier label yet, or "keep opening PRs for them".
metadata:
  short-description: Deliver up to five mutsu tickets through merge
---

# Mutsu Ticket Flow

Use this skill for requests to fix or process `todo:ticket` or `todo:deep` issues on
`tokuhirom/mutsu`. A processed ticket ends as either a correctly relabelled deep item or a PR whose
merge is verified on GitHub and in `origin/main`. An issue that already carries `todo:deep` has its
own section below — it is worked one named remainder at a time, not deferred.

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
5. **Re-read the comments again at two later checkpoints** — before the pre-publication
   `make test` + `make roast` run, and immediately before you open the PR (see "Publish"
   below). Steps 1-3 settle only the claims that exist in the first few seconds; they cannot
   see an agent who claims later and declines to yield, nor a sibling PR that lands while your
   suites are running.

**The lowest comment id is the whole tiebreaker.** It is exclusive by design — an ordered
append-only log is the only thing every agent reads identically, so any criterion needing
judgement reintroduces the race. "The earlier branch has nothing pushed yet" (a claim exists to
cover exactly that window), "the user pointed me at this issue" (so, routinely, was the other
agent — that is *why* two arrived), and "I am further along" are not overrides. An earlier
claimant who has gone quiet is released only by a matching `Releasing:` comment. (Got wrong on
[#7569](https://github.com/tokuhirom/mutsu/issues/7569): the later claim noted the earlier one,
judged it forfeit for being unpushed, and proceeded; both agents ran the full suites and opened
a PR for the same work.)

When you are done — merged, stopped, or blocked — post `Releasing: <your branch>` and remove the
`working` label. Read `docs/issue-workflow.md` for the full label scheme, why the comment log is
the record, and what to salvage when a checkpoint shows you lost.

**`working` is a lock, so there is no exception for a long-lived issue.** Taking one slice of a
campaign issue that will stay open for many more slices still means claiming it and labelling it for
the duration of that slice — two agents inside the same ADR collide even when their slices sound
unrelated. (This was got wrong on [#7543](https://github.com/tokuhirom/mutsu/issues/7543): three
slices were worked with no claim and no label, on the reasoning that the issue was not being "taken".
That reasoning is wrong — the lock is over the *work*, not over the issue's lifetime.)

Process at most **five tickets in one user-triggered run**, and only continue beyond the first
when the user explicitly asks to process multiple tickets or the queue. Any request that names a
*set* — "the `tier:N` tickets", "the ones with no tier", "the queue", "one after another", "keep the
PRs coming" — **is** that explicit ask; do not treat it as a single-ticket request and do not ask
for confirmation before the second one. Count a ticket when its
re-triage or implementation PR has merged, and count **one slice of a campaign issue as one ticket**
(see the next section). For a single-ticket request, report the next actionable
issue number after its verified merge but do not start it. After the fifth verified merge, report
the next actionable issue number but do not start it. A later user request starts a new run and
resets this limit.

**The cap bounds the session's context length, not its output.** A long session degrades however
much it has shipped, which is why a slice that merged no `src/` change at all still spends one of
the five: it cost the same reading, measuring and reporting as any other. Do not argue your way past
the cap on the grounds that a slice was "small" — start a fresh run instead.

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

## Working an issue that is ALREADY `todo:deep`

The step above treats `todo:deep` as a *destination* — a label you move a ticket to. But
`CLAUDE.md`'s task-selection order works the two queues in parallel, so you will also be handed an
issue that already carries it. Two shapes hide under the one label and they want different first
moves:

- **A campaign issue** names an owning ADR and lists its own open remainders.
  [#7543](https://github.com/tokuhirom/mutsu/issues/7543) is the archetype: "ADR-0068 §4 step 3",
  followed by a bulleted list of the specific routes still unclassified. It is workable *now*, and
  **the unit of work is one named remainder, not the issue.**
- **A single deep problem** has no design yet — one bug or feature too big for a PR. Here the first
  deliverable is usually a `Proposed` ADR, or a narrower issue recording what you learned, **not
  code**. Shipping an undersized fix to look productive is the failure mode; `CLAUDE.md`'s "Working
  on complex features" governs once a design exists.

### The slice loop for a campaign issue

Claim and label it per the section above — for the slice, not for the issue's lifetime — then:

1. **Take one named remainder.** Not two, and not "the campaign".
2. **Re-verify the owning ADR's own recipes before you trust them.** A campaign document's
   measurement recipe rots as the code it points at moves. ADR-0068 §1.2's `rust-gdb` oracle told
   the reader to break on one line and read a hit there as "exposed"; a later slice had put a scope
   guard *around* that line, so the recipe had silently become a false-positive generator. Re-derive
   it against current source, and **fix the recipe in the same PR** when it is wrong — a stale
   recipe costs every future slice, not just yours.
3. **Produce the acceptance the owning ADR asks for**, not a weaker one you find convenient.
4. **Pin any invariant the slice establishes**, and say in the test what it pins — a concurrency pin
   often encodes *mutsu's* guarantee rather than a Raku one, and a reader needs to know which.
5. **Record it as a numbered section in the ADR**, and post a comment on the campaign issue saying
   what is now settled and what is left.

**A slice that changes no `src/` line is a complete slice, not a failure.** Two of the three
[#7543](https://github.com/tokuhirom/mutsu/issues/7543) slices shipped zero interpreter change and
were the most valuable of the three, because they removed wrong entries from the campaign's ledger:
one route turned out to be already covered, and one long-tracked crash turned out not to belong to
the campaign at all. Do not manufacture a code change to make a slice feel finished, and do not stop
because you found nothing to fix.

**Do not widen the slice.** Findings outside it get their own issue with the right label — that run
produced a `todo:ticket`, a `todo:deep` and a `todo:perf` this way
([#7604](https://github.com/tokuhirom/mutsu/issues/7604),
[#7609](https://github.com/tokuhirom/mutsu/issues/7609),
[#7613](https://github.com/tokuhirom/mutsu/issues/7613)) — and get named in the ADR section and the
issue comment so they are not lost.

### Closing a campaign issue

Close it when **the remainders it names** are resolved, having first filed anything genuinely still
open as its own issue. Say in the PR body (`Closes #NNNN`) and in a final comment which remainders
were resolved by which PR, and where the residue went. That is the whole test: not whether the
underlying subject is "finished" in some larger sense, and not whether you are tired of it. A
campaign issue left open as a graveyard for one follow-up is as wrong as closing one with named
remainders still unaddressed.

## Implement and validate a ticket-sized fix

Everything from here on applies to a campaign slice too — only the *unit of work* differs, and the
section above settles that. The branch hygiene, the pre-publication gate, the publish/monitor/verify
steps and the queue rules below are the same either way.

Before starting **every** ticket, return to `main` and update it from the remote:

```sh
git switch main
git pull --ff-only origin main
```

Then create a fresh focused branch from that updated `main`, without overwriting unrelated changes.
Follow the Parser -> Compiler -> VM architecture, add focused regressions, and run targeted tests
while iterating.

### When the session pins you to ONE branch

Some sessions (Claude Code on the web, and any run started with a *designated branch*) hand you a
single branch name and forbid pushing anywhere else. That is a **session** setting, not a repository
rule — nothing here can lift it, and you must not push to a different branch to work around it.

It does not change one-ticket-one-PR. You satisfy both by **reusing** the one branch across tickets,
never by stacking two tickets into one PR. Substitute this loop for the fresh-branch step above:

```sh
# 1. Land the current ticket's PR, then PROVE it is in main before touching the branch.
git fetch origin main
git merge-base --is-ancestor <your last commit> origin/main   # must succeed

# 2. Drop the remote-tracking ref for the branch GitHub just deleted on merge.
git remote prune origin

# 3. Restart the SAME branch name from the merged main, and take the next ticket.
git checkout -B <the designated branch> origin/main
```

**Step 2 is not optional, and skipping it fails in two confusing ways.** GitHub deletes the head
branch when a PR merges, but your local `refs/remotes/origin/<branch>` keeps pointing at your old
commit. From then on:

- `git push --force-with-lease` is refused with `! [rejected] … (stale info)`, because the lease is
  checked against a ref that no longer exists upstream;
- anything that counts "unpushed commits" against the tracking ref miscounts **main's own new
  commits as yours** — a stop hook reported "9 unpushed commit(s)" in a session where
  `git rev-list --count origin/main..HEAD` was `0` and everything was merged.

Two rules that go with the loop:

- **Never `checkout -B` the branch while its PR is still open** — that discards the commit the PR
  points at. Step 1's `--is-ancestor` check is what makes the reset safe; if it fails, the work is
  not merged and you must not reset.
- **Expect the run to be strictly serial.** You cannot push ticket N+1 while ticket N's PR is
  waiting on CI (15-20 minutes per PR). Use that wait to read the next issue, reproduce it, and
  measure — just keep the result uncommitted (or stashed) until the current PR merges, then reset
  and commit onto the fresh branch. Do not open a second PR from the same branch.

If a ticket genuinely needs two branches in flight, stop and ask the user rather than pushing
elsewhere.

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

**Before you open the PR, re-read the issue's comments one last time** and check whether a PR
already closes it (`issue_read` `get` reports `closed_by_pull_requests`; an agent that skipped the
claim protocol leaves no comment but does leave a PR). One tool call, and it is the checkpoint that
saves a wasted PR — the claim you posted hours ago has not been looked at since.

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
`news/YYYY-MM/<slug>.md`. If the session pinned you to one branch, this is also where you run the
prune-and-reset loop above before starting the next ticket.

Then choose the next actionable open issue **from the slice the user named** (the whole
`todo:ticket` queue, or the `tier:N` / no-tier subset they asked for) — oldest first, **skipping
every issue labelled `working`** or carrying a live claim, plus deliberate non-divergence records,
blocked tickets, and items whose
current evidence makes them deep (relabel the latter through this workflow). Never start a dependent
ticket before its prerequisite merge is verified. For a single-ticket request, report that issue
number and stop. Continue only when the user explicitly requested multiple tickets or queue
processing, and stop after five processed tickets in that run.
