# Architecture Decision Records (ADR)

This directory records mutsu's architectural decisions.

## Purpose

For design forks in the road (major mechanism selections, ordering decisions, judgments that could be reversed),
make it possible to trace **"why we decided that way" and "what we rejected"** after the fact.
The role of an ADR is to preserve the *context of the judgment* — something that cannot be read out of the code or PLAN.md.

## Conventions

- 1 decision = 1 file, named `<issue>-kebab-title.md` and headed `# ADR-<issue>: <title>`,
  where `<issue>` is the number of the GitHub issue that carries the decision (no zero
  padding). If there is no issue yet, file one first (usually `todo:deep`). GitHub hands out
  that number, so two concurrent PRs cannot pick the same one — "highest number + 1" collided
  at least six times, once only noticed after both PRs had merged. ADR-0001 … ADR-0138 keep
  their sequential numbers; that scheme is closed.
- **Status**: a `- **Status**: ...` line near the top — `Proposed` (under discussion / awaiting
  approval) / `Accepted` (final) / `Rejected` (declined; the file stays, with the reason) /
  `Superseded by ADR-XXXX` (updated). That line is the only
  place the status lives.
- When a decision changes, **do not rewrite the existing ADR** — supersede it with a new ADR and update the old ADR's Status.
- **An ADR records the decision, not implementation progress** (user decision, 2026-10-19):
  context, decision, consequences, alternatives, open questions. Progress goes in the PR body,
  a `news/YYYY-MM/<slug>.md` entry, git history and the campaign issue's Status comment. The
  only progress-like thing an ADR keeps is its one-line `- **Status**:`, optionally with a short
  suffix (e.g. "Accepted; slices 1-3 landed") updated only when the overall state changes, never
  per PR. Touch an ADR only when the decision itself changes (a new ADR, or supersede). Parallel
  PRs appending progress to one ADR made it a conflict hotspot and buried its decisions
  (ADR-11276 reached ~5,800 lines). Existing "Implementation status" / "Outcome" sections are
  frozen history: do not extend them.
- Written in English (repo-wide English-only documentation rule).

## Index

There is no hand-written index: a table every new ADR appended to and every landed slice
edited made sibling PRs conflict on this file, and its status column drifted from the ADRs'
own Status lines. Build it from the files instead:

```sh
make adr-index        # Markdown table of number, title and Status line
ls docs/adr/          # or just the file names
```

`make check-adr` (part of `make checks`, and also run by CI's always-on `changes` job so a
docs-only PR is covered) rejects a duplicate number, a new sequential number, and an
issue-numbered ADR without the `# ADR-<issue>: ` heading or the `- **Status**:` line.
