# The docs-only CI gate now covers every path no build job reads

`scripts/ci-docs-only.sh` decides whether a change can skip the five build jobs
(`test`, `lint-configs`, `wasm-e2e`, `gc-stress`, `jit-stress`) — roughly 25-30
minutes of runner time, two cargo builds and three roast sweeps. Its allowlist
was written as "documentation", and it had drifted into meaning *prose*: `docs/`,
`news/`, `TODO_roast/`, `old-design-docs/`, `raku-doc/`, the agent trees, and
top-level `*.md`. Everything else paid the full suite, including several trees
that no job in `ci.yml` has ever opened.

The rule was restated to what it always should have been — **a path is skippable
iff no build job reads it** — and the allowlist grew to match:

- **The `ecosystem/` tree**, the zef-distribution parity ledger. It is a measurement
  *of* mutsu, never an input to it: `.github/workflows/ecosystem-sweep.yml`
  writes it and `pages.yml` (with its own `paths:` trigger on the same tree)
  reads it. A 250-file re-measurement sweep was paying for two cargo builds and
  three roast runs to confirm that recording what mutsu did does not change what
  mutsu does.
- **All of `.github/` except `ci.yml`.** Issue templates, the release-note config and
  the other six workflows are read by GitHub, not by any job here, and each is
  exercised by its own run — which the five build jobs say nothing about.
- **`scripts/*.py`, except `scripts/migrate-t-layout.py`.** The Python under
  `scripts/` is reporting and campaign tooling (ecosystem sweeps, roast/bench/
  backlog plots, manifest generation, surveys); only that one file is on the
  build path, via `make check-t-layout`. The shell and `.mjs` scripts stay off
  the allowlist — CI runs those.
- **Top-level `*.tsv` / `*.svg`.** `HISTORY.tsv` and `HISTORY-pass.svg`, the
  roast-history record and its chart, appended by `scripts/roast-history.sh`.

Two paths stay off the allowlist on purpose, and both are the interesting part
of the change. `.github/workflows/ci.yml` *defines* the five jobs, so a change
to it is precisely the change they exist to demonstrate; skipping them would
merge an edit to the test pipeline that had never once been executed. And all
of `site/` stays off even for the generated ecosystem projection, because
`site/e2e.test.mjs` in the `wasm-e2e` job loads `site/ecosystem.html` and
cross-checks it against `site/content/ecosystem.json` — those really are build
inputs, so an ecosystem sweep that regenerates the site manifest still runs
everything.

## The claim is now derived, not trusted

Every allowlist entry is a claim about consumers: "no job reads this tree". Such
a claim rots silently and in the dangerous direction — wire a new
`scripts/*.py` into `make test` the way `migrate-t-layout.py` already is, and a
change to it starts reading as documentation, skipping the very suite that runs
it. Nothing would have said so.

`scripts/ci-docs-only.sh --check-inputs` closes that. It strips comment lines
from the `Makefile` and `ci.yml` (both cite documentation in prose, and a
citation is not an input), extracts every repository path they name, and fails
if any of them is on the allowlist. It runs as its own step in the `changes`
job — separate from `--self-test` so that an incomplete sparse checkout fails
loudly instead of quietly skipping the guard — and from `--self-test` locally
whenever those two files are present. Deleting the `migrate-t-layout.py` denial
makes it fail, which is what makes it worth having.

Only one hop is scanned: a path reached *through* a shell script CI runs is not
covered, because scanning those drowns the signal (`run-roast-test.sh` and its
neighbours cite `docs/` and `news/` in running text). When you add that kind of
indirection, name the file in the `Makefile` or `ci.yml` too, or deny it in
`is_doc_path` by hand.

`bench.yml`'s `paths-ignore` was widened in step, with two deliberate
differences in the direction of ignoring more: `.github/**` is listed whole
(ci.yml included) and `scripts/*.py` is listed whole
(`migrate-t-layout.py` included). Gating which CI jobs run is a real reason to
run the test suite and no reason at all to re-measure a benchmark.
