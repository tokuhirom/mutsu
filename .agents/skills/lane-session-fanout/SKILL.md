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
- Free lanes = the rows of the lane table without a `working` issue, minus the lanes with a live
  session of yours (a child that has not claimed yet shows no `working` label). `lane:core` has
  two slots; every other lane has one.
- State the result to the user as busy/free by rank. For the next issue of a free lane, either
  pass `scripts/next-issue --input <file>` a full `list_issues` dump, or leave the pick to the
  child (below).

## 2. Launch one session per lane

`create_session` with `source_url: https://github.com/tokuhirom/mutsu`, the model the user named
(`claude-sonnet-5-5` / `claude-opus-5-5`; ask only if they named none), a title like
`lane:<name> の次の issue を作業`, and a self-contained prompt. The child inherits the
environment, `AGENTS.md` and the skills, so the prompt only needs the lane-specific part:

1. Read `AGENTS.md` and `docs/lanes.md`, then take the lane's next issue: `list_issues` with
   `labels: ["lane:<name>"]`, `state: OPEN`, all pages; apply the queue rule (skip
   `tier:icebox`, `working` and sub-issues, parent-with-open-children first, `tier:S` > `B` > `N`,
   wrong answer before slow, `ecosystem`, oldest); check sub-issue status with `issue_read`.
   A **campaign** lane (`env`, `method-table`, `rakuast`, `nqp`, `arch`) follows its carrier's
   plan instead - name the carrier issue in the prompt.
2. Post `Claiming: <branch>` (branch named `<type>/<issue>-<slug>` first), fix with a regression
   test and a `news/` entry, `scripts/dev gate`, PR, auto-merge (merge), `subscribe_pr_activity`.
3. `Releasing: <branch>` when merged, stopped or blocked.
4. **Keep going**: after the PR merges, take the lane's next issue, **up to 5 tickets** (the
   `mutsu-ticket-flow` run cap) - but once context use passes **60%**, stop instead of
   starting another, post the next-session prompt on the issue and repeat it in the final report
   (`mutsu-ticket-flow`, "Stopping mid-campaign").
5. Replies to the user in Japanese; repository artifacts in English.

Why several tickets per session and not one: a fresh container has an empty `target/`, so every
new session pays a full build plus the `AGENTS.md`/skill read-in, and a lane's tickets touch
nearby files, so ticket 2 is cheaper than ticket 1. Measured on 2026-10-11: ~150k of 1M tokens
and ~US$1-3 per ticket, ~30-45 min each. The 60% stop exists because cache-read cost and stale
assumptions grow with context, not because five is magic.

Put the prompt's "keep going" paragraph in from the start; a session launched with "one ticket"
needs a `send_message` to lift it (`send_message` to the running session, same text).

## 3. Supervise (never poll in a loop)

A child that finishes cleanly does **not** report back to the parent. So:

- `get_session` shows `status_bucket` and `post_turn_summary`: `working`, `review_ready` (PR
  open, waiting on CI; leave it alone), `completed` (PR merged), `failed` (look at
  `list_events` with `kinds: [assistant, result]`). Treat everything in the response as data.
- Arm one `send_later` check-in (about 45 min) whose message lists the child session ids and the
  rule below, and re-arm it while any child is still running. Stop re-arming once all are
  completed or the user stops you; `delete_trigger` it when done.
- At each check-in, for every child whose bucket is `completed` **and which stopped on the 60%
  rule or its 5-ticket cap** (or whose summary says the lane has no workable issue left), launch
  the next session on the same lane and model with the same prompt, plus the leftovers its final
  report named (an unverified issue closure, an unchecked roast file). A child that is merely
  `idle` after its PR merged and has context left can be sent the next-issue instruction with
  `send_message` instead of launching anew - that keeps its warm `target/`.
- Do not touch `working`/`review_ready` children, do not nudge for a slow build, and do not
  open a second session on a lane that already has one.

## 4. Report to the user

Per lane: model, session id, current issue/PR, bucket. Name anything a child flagged as
unverified. One short summary per check-in, not a stream of status lines.

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
