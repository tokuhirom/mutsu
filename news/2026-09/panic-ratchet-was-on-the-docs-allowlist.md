# The panic ratchet could have edited itself without running CI

`scripts/check-panic-surface.py` — the #8186 ratchet that keeps the
panic-family and `#[allow(` surface in `src/` from growing — was wired into
`make test` and into `ci.yml` when it landed, but was never taken off
`ci-docs-only.sh`'s documentation allowlist. The allowlist covers `scripts/*.py`
with one hand-maintained exception (`migrate-t-layout.py`), and the new script
was not added to it.

Two consequences, one loud and one quiet.

The loud one: `scripts/ci-docs-only.sh --self-test` and `--check-inputs` are both
steps of the `changes` job, so the guard failed on **every** pull request, not
only documentation ones:

```
not ok - scripts/check-panic-surface.py is read by the build but is on the documentation allowlist
ci-docs-only --check-inputs: 1 build input(s) classified as documentation
```

The quiet one is the reason that guard exists. Until this was fixed, a pull
request whose only change was `scripts/check-panic-surface.py` classified as
documentation, so `build`, `test-suites`, `lint-configs` and the rest reported
`skipped` — which counts as success for branch protection. The ratchet's
baseline could have been raised, or its `--self-test` weakened, by a change that
never once ran the suite that enforces it.

## The fix

One `return 1` line in `is_doc_path`, beside the `migrate-t-layout.py` exception
it mirrors, plus a self-test case (`check false 'the panic ratchet'`) so the deny
is pinned rather than re-derivable. That is the cost the guard's own comment
predicts for a genuinely-new build input: "one `return 1` line here rather than a
silently-untested merge".

## Why `--check-inputs` caught it and nothing else did

`--check-inputs` does not trust the allowlist; it derives the claim. It scans the
Makefile and `ci.yml` for every repository path they name — comments stripped,
non-existent paths dropped — and fails if any of those paths is on the allowlist.
That is exactly the rot it was built for, and it worked: the ratchet commit's
only mistake was not acting on the red it produced.

Its documented limit is that it scans **one hop** — a path reached *through* a
script CI runs is not covered. Both ratchet baselines are safe from that gap for
a different reason: `scripts/panic-surface-baseline.txt` and
`scripts/magic-keys-baseline.tsv` are nested paths, and `is_doc_path`'s `*/*`
case denies every nested path that no earlier case allowed, so only `*.py` under
`scripts/` was ever at risk. A sweep of the Makefile and `ci.yml` confirms
`check-panic-surface.py` and `migrate-t-layout.py` are the only two Python files
the build names, and that the other four `make test` prerequisites
(`check-value-wall`, `check-flaky-list`, `check-t-layout`, `check-magic-keys`)
reach no further Python.

Closes #8222.
