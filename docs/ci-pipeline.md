# CI pipeline (`.github/workflows/ci.yml`)

How the CI workflow is laid out and why a docs-only PR skips most of it. The rules that depend on
this — run both full suites before publishing, trust `main`, fix forward — are in `AGENTS.md`.

## Job layout

CI does not invoke `make test`; it runs the steps individually, and **one `build` job compiles the release binary once for the whole workflow** — `test-suites`, `gc-stress-roast` and `jit-stress-roast` download that artifact instead of each compiling their own (they install no toolchain at all). `test-suites` runs **both** the TAP suite (`prove t/`) and `make roast` on it (`MUTSU_BIN=target/release/mutsu`); local `make test` matches (see `docs/adr/0075-make-test-runs-tap-on-release-binary.md`, superseding ADR-0014). The **`gc-stress-tap` and `jit-stress-tap` jobs still run `prove t/` on their own debug binary** — that is where the 75 `debug_assert!`s in `src/` get their suite-wide pass, so do not "align" those jobs onto release. `test`, `gc-stress` and `jit-stress` are now **aggregator jobs**: they run nothing and exist only to keep the required-status-check names branch protection asks for, failing when any half of theirs did. They run it at `-j4` like every other prove step (since 2026-09-11; running it serially was costing those jobs 12m12s and 11m47s of pure wall clock and nothing else — `gc-stress` went 27m06s -> 11m17s and `jit-stress` 28m30s -> 15m49s, see `news/2026-09/ci-stress-jobs-stop-paying-for-a-serial-tap-run.md`; the halves and the shared build landed on top of that, see `news/2026-09/ci-builds-the-release-binary-once.md`). `cargo test` is debug everywhere. A *debug* run of a heavy file is ~3.3x slower than release, so a local timeout on one does not by itself indicate a real failure — confirm on `target/release/mutsu` before assuming a regression.

## Documentation-only PRs

**A documentation-only PR skips the build jobs on purpose.** `ci.yml`'s `changes` job classifies the diff (`scripts/ci-docs-only.sh`); when every changed path is on that script's allowlist, the build jobs (`build`, `test-check`, `test-suites`, `lint-configs`, `wasm-e2e`, `gc-stress-tap`, `gc-stress-roast`, `jit-stress-tap`, `jit-stress-roast`) report `skipped`, which counts as success for branch protection, and the three aggregator jobs that carry the required check names (`test`, `gc-stress`, `jit-stress`) pass on seeing that classification. So a checks listing showing those as skipped on a docs PR is **correct**, not a stuck CI — the PR is mergeable. The allowlist is not only prose: it is everything **no build job reads** — `docs/`, `news/`, `TODO_roast/`, `old-design-docs/`, `raku-doc/`, `.claude/`, `.agents/`, `ecosystem/` (the zef parity ledger), `.github/**` *except* `ci.yml`, `scripts/*.py` *except* `migrate-t-layout.py`, and top-level `*.md` / `*.tsv` / `*.svg`. Anything else runs the full suite, including any nested `README.md`, all of `site/` (the wasm-e2e job loads `site/ecosystem.html` and checks it against `site/content/ecosystem.json`), every shell/`.mjs` script under `scripts/`, and `.github/workflows/ci.yml` itself — that file *defines* those jobs, so skipping them would merge an edit to the test pipeline that never ran once.

If you add a new documentation directory, add it to the allowlist in that script, extend its `--self-test` cases, and add it to `bench.yml`'s `paths-ignore`. The reverse direction is enforced for you: `scripts/ci-docs-only.sh --check-inputs` (a `changes`-job step) scans the Makefile and `ci.yml` for every repository path they name and fails if any of them is on the allowlist, so wiring an allowlisted file into the build can no longer make an untested change look like documentation.

## Cancelled runs show up as red aggregators

A push to a PR branch cancels the previous commit's in-flight run. The aggregator jobs then see
their halves as `cancelled` and report **failure** on the *old* head (`jit-stress-tap: cancelled`
... `did not succeed`). That red is not a test failure and needs no action; judge the PR by the run
on its current head SHA.
