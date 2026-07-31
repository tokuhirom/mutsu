# `wasm-demo/` is now `site/`

The directory was called `wasm-demo/` because that is what it started as: a page
that ran the WebAssembly build in a browser. It had become the project's entire
public site — landing page, manual, tutorial, playground, REPL, bundled-library
listing and benchmark dashboard — with the WASM playground as one page of it.
The name misled anyone looking for the site, and it misled about what a change
in there affects.

It is `site/` now: a `git mv` plus a mechanical sweep of the 71 references, with
no content changes mixed in, so the diff stays reviewable and a bisect over it
is boring. Everything that names the directory moved in the same commit, because
a straggler breaks the deploy rather than failing a test:

- `.github/workflows/pages.yml` — the `paths:` trigger, the `wasm-pack` output
  move, the batteries-manifest and stats writes, the bench-trend render target
  and `upload-pages-artifact`'s `path:`.
- `.github/workflows/ci.yml` — the `wasm-e2e` job.
- `scripts/gen-batteries-manifest.py`, `scripts/check-site-snippets.mjs`,
  `scripts/bench-visualize.py`, `scripts/ci-docs-only.sh`.
- `site/e2e.test.mjs` and `site/concurrency.test.mjs` — the served directory and
  the `existsSync('site/pkg/mutsu.js')` guard.
- `.gitignore`, `README.md`, `BATTERIES.md`, `docs/user-guide.md`,
  `todo/deep/wasm-start-and-channel-trap.md`.

`news/` entries and the generated `docs/doc-diff-sweep/` report snapshots keep
the old name on purpose: they record what the tree looked like when they were
written.

The classifier that lets a documentation-only change skip the heavy CI jobs
(`scripts/ci-docs-only.sh`) works off a positive allowlist, so `site/` forces
the full suite exactly as `wasm-demo/` did — its self-test still passes and a
`site/README.md` change still classifies as `false`.

## Verified

The Pages deploy is not exercised by ordinary CI, so its moving parts were run
by hand against the renamed tree: `scripts/gen-batteries-manifest.py` (wrote
`site/content/batteries.json`, 16 libraries), `scripts/check-site-snippets.mjs`
(63/63 snippets), `node site/concurrency.test.mjs` (28/28) and
`node site/e2e.test.mjs` (107/107 in a real browser).
