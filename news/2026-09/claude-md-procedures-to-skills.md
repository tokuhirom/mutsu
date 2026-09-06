# CLAUDE.md: move task-triggered procedures out into skills

CLAUDE.md had grown to 627 lines / 70 KB, and a large share of that was
step-by-step procedure that only matters when a specific task is requested —
how to cut a release, how to clear stale agent worktrees and cargo caches, how
to pick and investigate roast work. Every session paid for all of it up front,
and the file's actual job — the standing rules that apply to *every* session —
was buried among them.

Those three clusters are now skills under `.agents/skills/`, joining the existing
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
CLAUDE.md indexes all six skills with the trigger for each, and AGENTS.md gained
the same pointer for Codex.

## The "Test::Util function workout" section was retired, not extracted

A fourth cluster, "Test::Util function workout", was first extracted alongside
the others and then deleted outright, because auditing it for the extraction
showed its central instruction had gone stale. It told the reader that "the
function implementation lives in `src/runtime/test_functions.rs` (not as a
builtin)" — but `news/2026-08/retire-native-test-util-overrides.md` records that
mutsu's native `Test::Util` handlers were **deleted in their entirety**
(`src/runtime/test_functions/subprocess.rs` and `util.rs`, plus the individual
functions in `comparison.rs`, `tap_subtest.rs`, `throws_like.rs` and
`eval_exception.rs`). `use Test::Util` now loads the real
`roast/packages/Test-Helpers/lib/Test/Util.rakumod` and runs it, so making a
helper work means fixing a general interpreter bug — exactly what that
retirement slice did, and exactly what ordinary roast work already covers. A
workflow whose distinguishing step is "add a native implementation here" no
longer describes anything anyone should do.

The coverage picture agrees that there is no enumerated backlog left to work
through. Of the 17 names `Test/Util.rakumod` exports, eight already have a
dedicated `t/` file (`is-path`, `is-deeply-junction`, `test-iter-opt`, `is_run`
across two files, `make-temp-path`/`-file`/`-dir`, `throws-like-any`) and the
remaining nine — `group-of`, `is-eqv`, `get_out`, `doesn't-hang`, `warns-like`,
`doesn't-warn`, `no-fatal-throws-like`, `run-with-tty`, `make-test-dist` — are
exercised through roast rather than being known-broken. A probe covering seven
of those nine (all but `run-with-tty`, which needs a tty, and `make-test-dist`,
which needs a `JSON::Fast` the reference rakudo here lacks) passes 8/8 under
rakudo, confirming the probe itself is well-formed.

That probe was **not** run under mutsu: this container's rustc is 1.94.1 while
the repo pins 1.96.1, so `cargo build` fails on an `if let` guard
(`src/vm/vm_data_ops.rs:134`, E0658). The case for deleting the skill does not
depend on that run — it rests on the instruction being false and the backlog
being unenumerated — but if one of those seven helpers does turn out to be
broken under mutsu, it is an ordinary interpreter bug with an ordinary fix, not
a reason to restore the workflow.

The durable rule survives where it belongs: CLAUDE.md's very first working
agreement still says these helpers come from the roast module and **not** from
Raku core, and that you must check what a test `use`s before implementing
anything as a builtin.

Net: CLAUDE.md 627 → 529 lines. Every removed line either landed in a skill or
was retired as stale.
