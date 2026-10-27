# CI runs the legacy-frontend opt-out after merge

`MUTSU_LEGACY=1`, the opt-out to the parser's own tree kept for one release after the Stage 3 flip
(ADR-10723 section 6.122), was exercised by nothing in CI. `test-suites` now runs
`scripts/rakuast-frontend.sh legacy` (every `t/` file under `MUTSU_LEGACY=1`) on the release binary it
already built, on `main`, dispatch and release-gate runs only, like `debug-tap`.
