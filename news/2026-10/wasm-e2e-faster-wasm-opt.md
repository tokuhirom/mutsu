# wasm-e2e: a current wasm-opt cuts the browser build by minutes

The `wasm-e2e` CI job spent about 10 minutes in "Build npm package", and only
3m46s of that was `cargo build`: the other ~6 minutes were wasm-pack's
`wasm-opt -O` pass. wasm-pack (0.13.1, and still 0.15.0) downloads binaryen
version_117 for it. Measured on a 4-core container against the same 34.5 MB
unoptimised `mutsu_bg.wasm`:

| wasm-opt               | time   | output   |
| ---------------------- | ------ | -------- |
| binaryen 117, `-O`     | 897 s  | 27.10 MB |
| binaryen 129, `-O`     | 147 s  | 27.00 MB |

`.github/scripts/install-wasm-pack.sh` now also installs a pinned,
checksum-verified binaryen 129 `wasm-opt` next to wasm-pack; wasm-pack picks
up a `wasm-opt` on `PATH` and runs it with the same `-O`, so the npm release
job builds the package exactly as CI tests it. wasm-pack itself moves from
0.13.1 to 0.15.0. The optimised module passes `site/concurrency.test.mjs` and
the whole `site/e2e.test.mjs` suite.

The job's Playwright step also stops downloading the full Chrome build: the
e2e test launches headless, which uses only `chrome-headless-shell`, so it
installs `--only-shell`.
