# README refresh: Rust version, install examples and compatibility figures

`README.md` had drifted from the repository it describes.

- **Requirements** said `Rust 1.94.0+`; `Cargo.toml` declares `rust-version = "1.98.1"` (and the CI
  workflows and the `Dockerfile` builder pin the same toolchain), so the README now says `1.98.1+`.
- **Install examples** pinned `0.7.0` for both `mise use` and the GHCR image tag; they now show the
  current `0.24.0`. The README also advertised a `:main` image tag, but `docker.yml` has built
  images from release tags only since the rolling tag was dropped, so that claim is gone.
- **Roast** read `1,433 out of 1,464`; the figure is now `1,426 out of 1,454`, counted the way
  `pages.yml` counts it for the site (lines of `roast-whitelist.txt` over `roast/**/*.t`) after the
  2026-09-29 re-vendor of `roast/`.
- **Ecosystem parity** quoted the first sweep (41.2% / 53.6% / 62.4% over 1,624 distributions). It
  now quotes the latest recorded one (`ecosystem/summary.json`, 2026-09-30): 69.1% of the 1,202
  gradable distributions, 77.0% of test files and 89.3% of assertions, out of 1,638 in the index.
  The old wording also read as if the percentage were over every distribution; its denominator is
  the gradable ones, and the sentence now says so.
- **Building** described `make test` as `prove t/`; it runs the cargo tests and then the TAP suite
  on the release binary. The architecture pointer moved from `AGENTS.md` to `docs/architecture.md`,
  where the module map now lives.
