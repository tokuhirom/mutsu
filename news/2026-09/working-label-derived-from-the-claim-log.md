# The `working` label is derived from the claim log, not added by hand

`docs/issue-workflow.md` has always made an issue's comment thread the record of
who is working on it, and the `working` label a fast filter over that record.
Keeping the two in step was left to the agent, as step 4 of the claim protocol:
post `Claiming: <branch>`, re-read, *then* add the label.

Step 4 was the one that got skipped. A survey of the queue on 2026-09-12 found
the failure in both directions:

- [#8033](https://github.com/tokuhirom/mutsu/issues/8033) carried a live claim
  from 04:08 to 10:49 — a six-hour execution-tree-lowering slice — and never
  carried the label;
- [#8094](https://github.com/tokuhirom/mutsu/issues/8094) was claimed at 09:40,
  worked, and released at 10:47, also with no label at any point;
- [#7989](https://github.com/tokuhirom/mutsu/issues/7989) carried the label 11
  hours after a claim whose session had plainly ended, with no `Releasing:`
  comment and nothing pushed under its branch name.

The first two cost a build slot: an issue somebody is on reads as free, which is
precisely what the label exists to prevent. The third is the mirror image, an
issue nobody is on reading as taken. And the person looking at the queue cannot
tell which of the two any given row is.

## Stop synchronizing two things by hand

`.github/workflows/claim-label.yml` now derives the label from the log instead.
It replays an issue's comments on every new one — and every three hours across
the whole open queue — and makes the label agree: any live claim and the issue
carries `working`, none and it does not. Adding or removing it by hand is
harmless, since the run is idempotent, but there is no longer a reason to.

`.github/scripts/sync-working-label.sh` holds the logic. Its claim-log parser is
the fragile part, because it has to survive the prose agents append after the
keyword (`Releasing: br — fixed in #8114`, a paragraph of findings, a Claude Code
attribution footer), so it carries a `--self-test` of fourteen cases — including
the exact logs of #8033 and #8094 — which the workflow runs before it touches
anything.

Two consequences worth knowing:

- **The comment format is now load-bearing.** The keyword must be the comment's
  first line, and the branch name in `Releasing:` must match the one in
  `Claiming:` exactly; that pairing is what both the sync and every other agent
  read. Everything after the branch on that line, and every line below it, is
  free.
- **A claim whose session died expires.** The scheduled run applies the
  evidence-based test the ecosystem lock board already uses — older than 24
  hours *and* no such branch on `origin` — and posts a real `Releasing:` comment
  rather than silently dropping the label, because the log is the record. A
  session that is still alive re-claims.

The MCP path gets a second, smaller win. Claiming through the GitHub MCP tools
now needs one `add_issue_comment` call: `issue_write` with `method: "update"`
*replaces* the whole label set, so adding `working` there meant reading the
issue's labels and passing them back, and getting it wrong silently dropped its
kind and tier.

`CLAUDE.md`, `AGENTS.md`, `docs/issue-workflow.md` and the `mutsu-ticket-flow`
skill were updated to match: claim by comment, never set the label.
