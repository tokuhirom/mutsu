---
name: ecosystem-lock-board-rotation
description: Rotate the ecosystem lock board (the single open issue labelled `ecosystem:lock`) to a fresh issue once its comment log is too large to read in one `get_comments` call — compute the live locks mechanically, open the new board, carry only the live locks, move the label, close the old board with a redirect, and repoint every skill/doc/script reference in a PR. Use when the board passes ~250 comments, when a `get_comments` read of it overflows or comes back truncated, or when asked to rotate/migrate/roll over the lock board ("lock board を rotate して", "ロックボードを新しい issue に移して").
metadata:
  short-description: Move the ecosystem lock board to a fresh issue
---

# Rotating the ecosystem lock board

The lock board ([`ecosystem-dist-roulette`](../ecosystem-dist-roulette/SKILL.md)) is an issue whose
comments are an append-only log. Every agent reads the whole log before it locks a distribution.
Once the log is too large for one `get_comments` call, agents start locking from a partial read,
and the board stops doing its job. Rotation moves the board to a fresh issue that holds only the
locks still live.

History so far: #7884 → #8977 (2026-09-21, 318 comments) → #10045 (2026-09-28, 301 comments) →
#11256 (2026-10-03, 269 comments) → #11640 (2026-10-03, 260 comments).
Early rotations took about a week of traffic; #11256 filled up within a day, so check the count on every read.

## When to rotate

- The board has about **250 comments or more**. `issue_read` `method: "get"` reports `comments`;
  locally, `gh issue view <N> --repo tokuhirom/mutsu --json comments --jq '.comments | length'`.
- A single `get_comments` read of the board overflows the tool's response limit, or you notice
  you are working from a partial read.

Rotating early costs nothing. Rotating late means agents lock from partial reads, so do not put it
off. Only one agent should rotate at a time: before you start, check that no second open issue
already wears the `ecosystem:lock` label. If one does, someone else is rotating, so stop.

## The one rule: compute the live set from the log, never from memory

The #8977 → #10045 rotation first carried **six locks that had been released days earlier**. The
live set had been assembled from memory and from a summary, not recomputed from the log. Undoing it
took seven extra board comments and a correction on the old board. So:

1. **Save every page of the old board's comments to files**, then run the script on them:

   ```sh
   # Local box (gh): the whole log in one file
   gh api --paginate repos/tokuhirom/mutsu/issues/<old>/comments > tmp/board-<old>.json
   .agents/skills/ecosystem-lock-board-rotation/live-locks.py tmp/board-<old>.json --check-origin
   ```

   Remote container: read every page with `issue_read` (`method: "get_comments"`, `perPage: 50`,
   `page: 1..N`) until a page comes back short. If the harness saved a page to a file, pass that
   file. Otherwise, save the page's JSON under `tmp/` yourself, one file per page. Overlapping
   pages are harmless, because the script de-duplicates by comment id.

2. **Check the header line.** Its comment count must equal the issue's `comments` count, and its
   last id must be the newest comment. If the count is short, a page is missing. Fetch it before
   you trust anything else in the output.

3. **Decide each line of output. Do not re-derive it.**
   - **`live` and `held`:** carry it to the new board.
   - **`live` and `STALE`** (over 24h old, branch absent from `origin`): do not carry it. Post the
     stale `Unlocking:` on the **old** board first (the format is in `ecosystem-dist-roulette`).
     The new board then starts clean, and the break is recorded where the lock was.
   - **`held` with the branch present:** check whether that branch's PR is merged or closed. A
     merged or closed PR also meets the stale rule, and the script cannot see PRs.
   - **`malformed`** (e.g. `Releasing: <branch>` on the board): it released nothing, so its lock
     is still live in the list above. Decide that lock by the stale rule. Mention the malformed
     line in the carry comment.
   - **`orphan unlock`:** usually a typo in the distribution or the branch. Find the lock it meant.
     If the author clearly meant to release it (same branch, and a "Merged as #N" note), treat the
     lock as released and say so. Otherwise decide it by the stale rule.

   If you delegate the reading to a sub-agent, have it return the script output verbatim, not a
   summary.

## Steps

Do them in this order. Each step leaves the board usable, even if you are interrupted.

1. **Create the new board.** Copy the old issue's body verbatim. Then:
   - update the opening paragraph: continuation of #old (and the chain before it), the date, and
     the comment count that triggered the rotation;
   - keep the Rotation section pointing at this skill.

   Title: `Ecosystem distribution lock board (vN) — claim a dist here before working it`. Apply
   the `ecosystem:lock` label at creation (`issue_write` `method: "create"`, `labels`). For a
   moment two issues wear the label. That is why step 4 comes straight after.

2. **Carry each live lock** as its own comment on the new board, one per distribution, keeping the
   holder's original branch:

   ```
   Locking: Pod::To::HTML claude/modest-keller-ketrt9

   Carried over from #<old> (originally locked 2026-09-28T06:31:18Z, comment 5864693308) as part of the board rotation.
   ```

   The original timestamp matters. The 24-hour stale floor counts from the **original** lock, not
   from the carry comment. In the carry comment for the last lock, also list anything you decided
   *not* to carry, and why (stale, malformed release). If there are no live locks, post one
   comment saying so, so that the log's first entry explains the empty board.

3. **Re-read the old board.** An agent may have locked or unlocked there while you worked. Carry
   or drop any new entries in the same way.

4. **Move the label.** Remove `ecosystem:lock` from the old issue (`issue_write` `method: "update"`,
   with `labels` set to the old issue's labels minus `ecosystem:lock`). Exactly one open issue
   must wear it afterwards, because the label is the fallback authority when the number in a doc
   is stale.

5. **Close the old board with a redirect comment:** "Rotated to #new. Carried: … Not carried:
   … Lock there, not here." Then close it (`state: "closed"`, `state_reason: "completed"`).

6. **Repoint the repository in a PR** (a docs-only change, so `git diff --check` is the gate):

   ```sh
   git grep -n -e '#<old>\b' -e 'issues/<old>\b' -e '\b<old>\b' -- ':!news' ':!docs/adr'
   ```

   The expected hits are the `ecosystem-dist-roulette` and `ecosystem-dist-fix` skills (including
   the `gh issue … <old>` and `issue_number: <old>` examples), `ecosystem/README.md`,
   `docs/ecosystem-parity.md`, `docs/issue-workflow.md`, `PLAN.md`,
   `.github/scripts/sync-working-label.sh`, and this skill's history line above. Check each hit
   before you change it: the bare-number pattern also matches unrelated numbers. Past `news/`
   entries keep the old number, because they record history.

   Add a `news/YYYY-MM/ecosystem-lock-board-rotation-vN.md` entry: the old and new numbers, the
   comment count, which locks were carried, and which were not and why. Title the PR
   `docs: rotate the ecosystem lock board from #<old> to #<new>`, and enable auto-merge.

## If you get it wrong

The board is append-only, so never edit or delete a comment, not even your own mistaken carry.
Fix a mistake with more comments:
- **Wrongly carried lock:** post `Unlocking: <dist> <branch> (carried over in error: already
  released on #<old> in comment <id>)` on the new board.
- **Wrong list in the redirect comment on the old board:** append a correction comment there.

Name the comment id that proves each correction.
