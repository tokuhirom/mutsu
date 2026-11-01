# A nightly Docker build, and a tracking issue when `main`'s CI goes red

The v0.26.0 release was blocked twice by checks that run only at release time:
`wasm-e2e` failed on the tag build, and `docker.yml` (tag-only) failed because
`.dockerignore` lacked `crates/`. Both were broken for a while before the tag.
`wasm-e2e` in particular had already failed in the hourly run on `main` three
hours earlier; nobody was told.

- `.github/workflows/nightly.yml` builds the Docker image (amd64, nothing
  pushed) every night, so a broken Dockerfile or build context is found the
  next morning instead of during a release.
- `ci.yml` gets a `report` job. When a scheduled (hourly) run on `main` fails, it
  opens the `ci:main` tracking issue, and comments on it only when the set of
  failed jobs is new, so a persistent failure does not add a comment per hour.
- Both follow `stress.yml`'s pattern for `ci:stress`.

`wasm-e2e` is not added to the nightly: it already runs on every hourly `main`
run (ADR-11581), which is more often than nightly would be.
