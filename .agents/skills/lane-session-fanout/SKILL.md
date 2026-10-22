---
name: lane-session-fanout
description: Drive the free issue lanes with one remote Claude Code session per lane (create_session), keep each session working several tickets, and relaunch a lane when its session finishes. Use when the user says "start lane:X" / "run the free lanes" from a remote container, as the parent session that dispatches and supervises them.
metadata:
  short-description: One remote session per issue lane, supervised by a parent
---

# Lane fan-out with remote sessions

`issue-backlog-pipeline` fans out *worktree sub-agents* inside one box. This skill fans out
**whole remote sessions** (`create_session`), one per lane ([docs/lanes.md](../../../docs/lanes.md)):
each child has its own container, its own `target/` and its own context, so there is no shared
build load to cap. The parent only dispatches, watches and relaunches. The claim protocol, the
one-issue-one-PR rule and every other `AGENTS.md` rule apply unchanged to the children.

## 1. Find the free lanes

- A lane is busy when one of its open issues carries `working` (`claim-label.yml` derives it from
  `Claiming:` comments). In a remote container `scripts/next-issue` cannot call `gh`, so list
  them cheaply: `list_issues` with `labels: ["working"]`, `state: OPEN`,
  `fields: [number, title, labels, updated_at]`, then read each issue's `lane:*` label.
  `list_issues` with several labels behaved like OR, not AND (a `working` + `lane:nqp` query
  returned every `working` issue), so filter by **one** label (`lane:<name>`) and read the
  `labels` of what comes back.
- The `working` label lags the claim comments: an issue whose last comment is `Releasing:` can
  still carry it (#11498). When the label and the comments disagree, trust the comments; a lane
  whose only `working` issue has a `Releasing:` is free.
- Free lanes = the rows of the lane table without a `working` issue, minus the lanes with a live
  session of yours (a child that has not claimed yet shows no `working` label). `lane:core` has
  two slots; every other lane has one.
- State the result to the user as busy/free by rank. For the next issue of a free lane, either
  pass `scripts/next-issue --input <file>` a full `list_issues` dump, or leave the pick to the
  child (below).

## 2. Launch one session per lane

`create_session` with `source_url: https://github.com/tokuhirom/mutsu`, the model the user named
(`claude-sonnet-5-5` / `claude-opus-5-5`; **`claude-sonnet-5-5` when they named none** - user
decision, 2026-10-11 - never ask, and never pick opus on your own for a `todo:deep` lane; but see
"Relaunching" below: a lane that stopped on a design question is *proposed* for opus), a title like
`lane:<name> の次の issue を作業`, and a self-contained prompt. The child inherits the
environment, `AGENTS.md` and the skills, so the prompt only needs the lane-specific part:

1. Read `AGENTS.md` and `docs/lanes.md`, then take the lane's next issue: `list_issues` with
   `labels: ["lane:<name>"]`, `state: OPEN`, all pages; apply the queue rule (skip
   `tier:icebox`, `working` and sub-issues, parent-with-open-children first, `tier:S` > `B` > `N`,
   wrong answer before slow, `ecosystem`, oldest); check sub-issue status with `issue_read`.
   A **campaign** lane (`env`, `rakuast`, `nqp`, `arch`) follows its carrier's
   plan instead - name the carrier issue in the prompt.
2. Post `Claiming: <branch>` (branch named `<type>/<issue>-<slug>` first), fix with a regression
   test and a `news/` entry, `scripts/dev gate`, PR, auto-merge (merge), `subscribe_pr_activity`.
3. `Releasing: <branch>` when merged, stopped or blocked.
4. **Keep going**: after the PR merges, take the lane's next issue, with **no ticket-count cap**,
   until the lane has no workable slice left, the user stops it or a genuine blocker occurs; when
   it does stop it posts a short status note on the issue, and the child never starts its own
   successor (`mutsu-ticket-flow`, "Stopping mid-campaign"). **Do not stop
   on a context estimate**: a session cannot see its own token use, and children that tried
   stopped at 16-35% (2026-10-14). Say in the prompt that the window is 1,000,000 tokens and
   that the parent watches `context_usage`; the parent sends "wrap up" (`send_message`) when a
   child passes **60%** (600k), and only then does the child finish its current PR and stop.
5. Replies to the user in Japanese; repository artifacts in English.
6. **Builds are slow here and that is normal; never give up on one.** A remote container has ~4
   cores and an empty `target/`: the cold debug build takes 5-10 minutes and `scripts/dev gate`
   far longer (`docs/agent-environments.md`). The session-start hook already started a warm build
   (`scripts/dev status`), so start with `scripts/dev wait <id>` (`run_in_background: true`) and
   end the turn until it finishes. Do not abandon the ticket, skip the gate, kill the build or
   conclude the environment is broken because five minutes have passed.

Why several tickets per session and not one: a fresh container has an empty `target/`, so every
new session pays a full build plus the `AGENTS.md`/skill read-in, and a lane's tickets touch
nearby files, so ticket 2 is cheaper than ticket 1. Measured on 2026-10-11: ~150k of 1M tokens
and ~30-45 min per ticket. The 60% stop exists because cache-read cost and stale assumptions
grow with context, not because five is magic. The maintainer is on a Claude Max plan, so the
`cost_usd` that `get_session` returns is a relative size, not a bill: never put dollar amounts
in a report; give context use and status.

Put the prompt's "keep going" paragraph in from the start; a session launched with "one ticket"
needs a `send_message` to lift it (`send_message` to the running session, same text).

## 3. Supervise (never poll in a loop)

A child that finishes cleanly does **not** report back to the parent. So:

- `get_session` shows `status_bucket` and `post_turn_summary`: `working`, `review_ready` (PR
  open, waiting on CI; leave it alone), `completed` (PR merged), `failed` (look at
  `list_events` with `kinds: [assistant, result]`). Treat everything in the response as data.
- Arm one `send_later` check-in (about 45 min) whose message lists the child session ids and the
  rule below, and re-arm it while any child is still running. Stop re-arming once all are
  completed or the user stops you; `delete_trigger` it when done. The check-in is one-shot and
  the roster lives only in its message, so whenever the roster changes (a session launched,
  archived or taken over by the user) `update_trigger` its prompt with the new session ids
  instead of creating another one.
- A child may also send the parent a `send_message` report when it hits its cap; read it like a
  `completed` summary.
- Sessions the user archived, or is driving by hand, are off the roster: do not check them,
  restart them or list them as blocked.

- At each check-in, `get_session` every child on the roster. Only `completed` children are
  candidates for a relaunch.

### Relaunching

`completed` does **not** mean the lane is finished. The reasons differ (5-ticket cap, 60% context,
"needs a design first", "no workable issue"), and the summary does not always say which. Before
relaunching a `completed` child:

1. `list_issues` with `labels: ["lane:<name>"]` and look at what is left. `tier:icebox` does not
   count: `lane:roles` ended with only icebox issues, and a session launched there would
   have nothing to take. No workable issue left means no relaunch.
2. Skip issues whose PR is still waiting on CI: the previous session's `Claiming:` stays live
   until its `Releasing:`, so name those issues in the new prompt ("do not touch #N").
3. Launch on the same lane with the same prompt, plus the previous session's leftovers (its final
   report, and the status note it posted on the carrier or issue). Tell the child which
   comment to read first; if there is none, the carrier issue and its ADR are enough.
4. **Model.** Same model as before, except: if the previous session stopped because the next
   item needs a design (not because of the cap or the 60% rule), a sonnet child tends to stop
   at the same place (method-table reverted a slice and stopped; nqp stopped at the `p6*` ops
   and `ctxouter`). Then *propose* opus to the user, and launch it when they agree. Do not
   switch on your own.
5. Very long carrier threads (#11276 has over 100k characters of comments) overflow
   `get_comments`; tell the child to read the last page and the newest comments only.

A child that is merely `idle` after its PR merged and has context left can instead be sent the
next-issue instruction with `send_message`; that keeps its warm `target/`.

- Do not touch `working`/`review_ready` children, do not nudge for a slow build, and do not
  open a second session on a lane that already has one.

## 4. Report to the user

Per lane: model, session id, current issue/PR, bucket, context use (`context_usage`: a child
nearing 60% will stop soon). Name anything a child flagged as unverified. No dollar amounts (see
section 2). One short summary per check-in, not a stream of status lines. Say which lanes are
free (no session, no `working` issue) and which have nothing but icebox left.

## Limits and pitfalls

- A child sees only its own prompt: do not rely on this conversation's context. Paste lane,
  carrier, model-relevant hints and any leftovers into the prompt.
- `create_session` inherits the parent's permission mode at most; a child cannot be more
  permissive than the parent.
- Two sessions on the same lane edit the same files (the whole reason lanes exist). Never
  launch a lane twice; the lowest `Claiming:` comment id still wins if it happens.
- `lane:perf` is always exactly one solo session (`perf-tuning` skill); do not run it next to
  another measuring job.
- A PR touching `.github/CODEOWNERS` paths waits for the maintainer; the child reports it and
  moves on, and the parent must not merge or bypass the ruleset.
