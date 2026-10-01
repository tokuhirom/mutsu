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
  approval) / `Accepted` (final) / `Superseded by ADR-XXXX` (updated). That line is the only
  place the status lives.
- When a decision changes, **do not rewrite the existing ADR** — supersede it with a new ADR and update the old ADR's Status.
- **Record implementation progress inside the ADR that owns the decision** — a Status suffix
  for a short state, or an "Outcome" / "Implementation status" section for a phased one.
  `news/` and PLAN.md are where the *work* is reported; they are not a substitute. An ADR
  whose recorded state has drifted from what shipped defeats its own purpose: a reader who
  starts from the ADR either re-litigates a decision that is already executed, or cannot tell
  which of its phases are done. (The 2026-08-02 ledger review in
  [ANALYSIS.md §8](../../ANALYSIS.md) found this drift on six of seventeen ADRs.)
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
