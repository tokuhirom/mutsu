# A queue request is a standing instruction, not the start of a negotiation

Requests like "work the `todo:ticket` issues that have no `tier:*` label yet, one after another, and
open the PRs" or "process the `tier:N` tickets, keep the PRs coming" were being answered with a
round of confirmation questions — *one PR per issue or one for the batch? shall I continue to the
next one? may I open the PR?* — every one of which the repository had already answered. The cost is
not just the round-trip: a queue run that pauses after each ticket is not a queue run.

The mechanics are now recorded as settled defaults that must not be asked back, in `CLAUDE.md`
("A queue request is a standing instruction"), in `.agents/skills/mutsu-ticket-flow/SKILL.md` as a
table of the tempting question against its standing answer, and in one paragraph of `AGENTS.md`:

- one issue, one PR — never bundled, never stacked;
- selection is the slice the user named (a label, a tier, "no tier yet", "the queue"), oldest-first
  within it, skipping anything carrying `working` or a live claim — a missing `tier:*` is a workable
  state, not a reason to stop and triage tiers first;
- claim and release every issue by the `Claiming:` / re-read / `Releasing:` protocol;
- every code fix carries a focused regression test, a `news/YYYY-MM/<slug>.md` entry and
  `Closes #NNNN`;
- open the PR, enable auto-merge with the merge method, watch CI and fix forward — "open the PRs" is
  already the permission;
- go straight on to the next issue after each verified merge, up to the five-ticket run cap;
- re-triaging a ticket to `todo:deep` or closing it as already fixed is a legitimate outcome of the
  run, not something to seek approval for.

The skill's run cap needed the same treatment. It processes at most five tickets per run and
continues past the first "only when the user explicitly asks to process multiple tickets or the
queue" — which was being read too narrowly, so a request naming a *set* now explicitly counts as
that ask. Its "choose the next actionable issue" step also now says to stay inside the named slice
rather than falling back to the whole `todo:ticket` queue, and its description lists the filtered
phrasings so the skill actually triggers on them.

The rule is not "never ask". Two things still stop a run: a decision `CLAUDE.md` reserves for the
user (a rung-3 native provider, a new or superseding ADR, weakening a CI gate, dropping a
whitelisted test), and an issue whose own evidence cannot settle between two materially different
implementations. Both are handled by parking that one issue, finishing the rest of the batch, and
raising the question in the final report — never by idling the whole queue on it.
