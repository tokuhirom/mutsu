# CLAUDE.md: move task-triggered procedures out into skills

CLAUDE.md had grown to 627 lines / 70 KB, and a large share of that was
step-by-step procedure that only matters when a specific task is requested —
how to cut a release, how to run a "Test::Util workout", how to clear stale
agent worktrees and cargo caches, how to pick and investigate roast work. Every
session paid for all of it up front, and the file's actual job — the standing
rules that apply to *every* session — was buried among them.

Those four clusters are now skills under `.agents/skills/`, joining the existing
`mutsu-ticket-flow` and `rakuast-implementation` and the new `install-raku`:

- **`cut-release`** — version choice by semver judgment over what merged since
  the last tag, the `gh workflow run tag-release.yml -f version=X.Y.Z` trigger,
  a job-by-job account of what `tag-release.yml` and `release.yml` actually do
  (including *why* the GitHub App token matters twice: bypass actor on the `main`
  ruleset, and a tag pushed with the default `GITHUB_TOKEN` would not start
  dependent workflows), explicit verification commands for the four tarballs,
  the npm publish and the GitHub Release, the label-driven release-note grouping,
  and the one-time infra prerequisites.
- **`roast-triage`** — the PLAN.md → BLOCKERS.md task-selection order, the
  `scripts/roast-history.sh` diagnostic categories, and the raku-first
  investigation order for a single failing file.
- **`test-util-workout`** — the one-function-at-a-time workflow, with the
  reminder that `Test::Util` is a roast test-helper module and never a core
  builtin.
- **`reclaim-disk`** — worktree removal, `cargo sweep`, nuking
  `target/*/incremental` (the dominant offender), and the optional mold + sccache
  setup.

Each skill is written to stand alone, because a subagent starts with no context:
where a skill needs a rule that also lives in CLAUDE.md (never special-case a
roast test, keep the whitelist sorted, `--squash` is rejected by this
repository), it restates it rather than pointing back.

CLAUDE.md keeps a short section for each cluster holding only the always-on
rule — the release trigger and the `type:` title convention, "clean worktrees at
least once per hour", "task selection is PLAN.md → BLOCKERS.md driven" — plus a
pointer to the skill. The section *headings* were deliberately preserved, since
`docs/maintenance.md`, `docs/batteries/testsuite-gate.md`, ADR-0014 and
`todo/perf/yaml-parse-throughput.md` all cite them by name. Two subsections that
had been filed under "Delegate the full roast run to CI" but are really about
build profiles (`MUTSU_VM_STATS` counters on debug; benchmark numbers coming
from the bench CI) were promoted to their own section. A new table at the top of
CLAUDE.md indexes all seven skills with the trigger for each, and AGENTS.md
gained the same pointer for Codex.

Net: CLAUDE.md 627 → 534 lines, with no content dropped — every removed line
landed in a skill.
