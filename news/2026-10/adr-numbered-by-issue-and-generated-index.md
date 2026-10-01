# ADRs are numbered by their GitHub issue, and the index is generated

Two hand-coordinated parts of `docs/adr/` kept breaking under parallel PRs.

**Number collisions.** A new ADR took "the highest number on my `main` + 1", so two concurrent
PRs took the same number. That happened at least six times (0054/0055, 0111/0112, 0112/0113,
0133, 0136, …), and the 0054 collision was only noticed after both PRs had merged, costing a
renumbering PR of its own. A new ADR is now named `<issue>-kebab-title.md` and headed
`# ADR-<issue>: <title>`, where `<issue>` is the GitHub issue that carries the decision. GitHub
hands out that number, so it cannot collide. ADR-0001 … ADR-0138 keep their numbers; the
sequential scheme is closed.

**The README index.** `docs/adr/README.md` held a hand-written table of every ADR and its
status. Every new ADR appended a row to the same last line, and every landed slice edited a
row, so sibling PRs conflicted on it constantly (166 commits touched it in two months). The
status column also drifted from the ADRs' own Status lines. The table is gone. `make adr-index`
builds it from the files, and each ADR's `- **Status**:` line is the only place its status
lives.

`scripts/adr.sh check` (`make check-adr`, part of `make checks`) rejects a duplicate number, a
new sequential number, and an issue-numbered ADR that lacks the heading or the Status line.
An ADR PR is usually docs-only, and a docs-only PR skips the job that runs `make checks`, so
CI's always-on `changes` job runs the check as well.
