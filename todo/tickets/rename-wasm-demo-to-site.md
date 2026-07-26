# `wasm-demo/` is the whole site now — rename the directory

The directory is called `wasm-demo/` because that is what it started as: a page
that ran the WebAssembly build in a browser. It is now the project's entire
public site — landing page, manual, tutorial, playground, REPL, bundled-library
listing and benchmark dashboard — and the WASM playground is one page of it. The
name misleads anyone looking for the site, and it misleads about what a change in
there affects.

Rename it to `site/` (or `www/`).

## Why this is its own change

The name is referenced from more places than it looks, and every one has to move
in the same commit or the deploy breaks:

- `.github/workflows/pages.yml` — the `paths:` trigger, the `wasm-pack build`
  output move (`mv pkg wasm-demo/pkg`), `gen-batteries-manifest.py`'s output
  path, the stats/bench-trend writes, and `upload-pages-artifact`'s `path:`.
- `.github/workflows/ci.yml` — the `wasm-e2e` job.
- `scripts/gen-batteries-manifest.py`, `scripts/check-site-snippets.mjs`,
  `scripts/bench-visualize.py` (the `--site-chrome` output path).
- `wasm-demo/e2e.test.mjs` and `concurrency.test.mjs` — the served directory and
  the `existsSync('wasm-demo/pkg/mutsu.js')` guard.
- `.gitignore` (`wasm-demo/pkg`), `README.md`, `CLAUDE.md`, `docs/*.md`.

Do it as a pure `git mv` plus a mechanical reference sweep, with no content
changes mixed in, so the diff stays reviewable and a bisect over it is boring.
Verify with a local `node <dir>/e2e.test.mjs` run and one manual
`gh workflow run pages.yml` before considering it done — the Pages deploy is not
exercised by ordinary CI.
