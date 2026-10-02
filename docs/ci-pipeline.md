# CI pipeline (`.github/workflows/ci.yml`)

How the CI workflow is laid out and why a docs-only PR skips most of it. The rules that depend on
this — run both full suites before publishing, trust `main`, fix forward — are in `AGENTS.md`.

## Job layout

CI does not invoke `make test`; it runs the steps individually, and **one `build` job compiles the release binary once for the whole workflow** — `test-suites` downloads that artifact instead of compiling its own (it installs no toolchain at all). `test-suites` runs **both** the TAP suite (`prove t/`) and `make roast` on it (`MUTSU_BIN=target/release/mutsu`); local `make test` matches (see `docs/adr/0075-make-test-runs-tap-on-release-binary.md`, superseding ADR-0014). `test-check` runs fmt, clippy and the unit tests (`cargo test --test-threads=1` with `MUTSU_GC=on`, the configuration that ships). The **`debug-tap` job runs `prove t/` on its own debug binary** with default runtime settings — that is where the `debug_assert!`s in `src/` get their suite-wide pass, so do not "align" it onto release. `test` is an **aggregator job**: it runs nothing and exists only to keep the required-status-check name the `main` ruleset asks for, failing when `build`, `test-check`, `test-suites` or `debug-tap` did. `cargo test` is debug everywhere. A *debug* run of a heavy file is ~3.3x slower than release, so a local timeout on one does not by itself indicate a real failure — confirm on `target/release/mutsu` before assuming a regression.

## Stress runs (`.github/workflows/stress.yml`)

The GC-stress (`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`) and JIT-stress (`MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2`) configurations are **not on the PR gate**. They run in `stress.yml` — `gc-stress-tap`, `gc-stress-roast`, `jit-stress-tap`, `jit-stress-roast` — nightly on `main`, and on demand via `workflow_dispatch` against any branch ("Use workflow from"). A failed nightly run opens, or comments on, the open issue labelled `ci:stress`. A PR that changes the cycle collector, the JIT or the concurrency runtime should dispatch it on its branch before merging and link the run. Why they moved — 28 stress-only PR failures in three weeks, none of them a GC or JIT defect, at ~31 of each run's ~65 runner-minutes — is in [ADR-10738](adr/10738-stress-runs-leave-the-pr-gate.md); the history of the jobs themselves is in `news/2026-09/ci-stress-jobs-stop-paying-for-a-serial-tap-run.md` and `news/2026-09/ci-builds-the-release-binary-once.md`.

## Required checks

The required status checks live in the **repository ruleset** for `main`, not in classic branch protection: `test`, `wasm-e2e`, `lint-configs`, `miri`, `changes`. `gh api repos/tokuhirom/mutsu/rules/branches/main` lists them (the classic `branches/main/protection` endpoint is not where they are). A required check whose job is renamed or removed is never created, which leaves every PR pending forever with no error, so change the ruleset in the same step as the workflow.

## Documentation-only PRs

**A documentation-only PR skips the build jobs on purpose.** `ci.yml`'s `changes` job classifies the diff (`scripts/ci-docs-only.sh`); when every changed path is on that script's allowlist, the build jobs (`build`, `test-check`, `test-suites`, `debug-tap`, `lint-configs`, `wasm-e2e`) report `skipped`, which counts as success for branch protection, and the aggregator job that carries the required check name `test` passes on seeing that classification. So a checks listing showing those as skipped on a docs PR is **correct**, not a stuck CI — the PR is mergeable. The allowlist is not only prose: it is everything **no build job reads** — `docs/`, `news/`, `TODO_roast/`, `old-design-docs/`, `raku-doc/`, `.claude/`, `.agents/`, `ecosystem/` (the zef parity ledger), `.github/**` *except* `ci.yml`, `scripts/*.py` *except* the ones a build job runs (`migrate-t-layout.py`, the `make checks` ratchets — listed in `is_doc_path`), and top-level `*.md` / `*.tsv` / `*.svg`. Anything else runs the full suite, including any nested `README.md`, all of `site/` (the wasm-e2e job loads `site/ecosystem.html` and checks it against `site/content/ecosystem.json`), every shell/`.mjs` script under `scripts/`, and `.github/workflows/ci.yml` itself — that file *defines* those jobs, so skipping them would merge an edit to the test pipeline that never ran once.

If you add a new documentation directory, add it to the allowlist in that script, extend its `--self-test` cases, and add it to `bench.yml`'s `paths-ignore`. The reverse direction is enforced for you: `scripts/ci-docs-only.sh --check-inputs` (a `changes`-job step) scans the Makefile and `ci.yml` for every repository path they name and fails if any of them is on the allowlist, so wiring an allowlisted file into the build can no longer make an untested change look like documentation.

## Runner labels are pinned

Every workflow names an explicit runner image (`ubuntu-24.04`, `ubuntu-24.04-arm`, `macos-26`), never a floating `ubuntu-latest` / `macos-latest`. GitHub re-points a `-latest` alias to a new OS image on its own schedule, which changes the toolchain, the glibc the release tarballs link against, the preinstalled apt packages and the core count under an unchanged commit; a red run then looks like a regression in whatever PR was open. Moving to a newer image is a deliberate edit of the pin, reviewed and run through CI like any other change. `make check-runner-pins` (`scripts/check-runner-pins.sh`) rejects a `-latest` label; it runs in the always-on `changes` job, because a PR that edits only a workflow other than `ci.yml` is docs-only and skips `test-check`.

## Cancelled runs show up as red aggregators

A push to a PR branch cancels the previous commit's in-flight run. The aggregator job then sees
its halves as `cancelled` and reports **failure** on the *old* head (`debug-tap: cancelled`
... `did not succeed`). That red is not a test failure and needs no action; judge the PR by the run
on its current head SHA.
