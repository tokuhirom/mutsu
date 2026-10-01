# Site refresh: install versions, known gaps and the committed snapshots

The README was brought back in line with the repository earlier today; the static site under
`site/` carried the same drift, and some of it was older still.

- **Toolchain.** The landing page said `Rust 1.92+` and the manual `Rust 1.94`, in both languages;
  `Cargo.toml` declares `rust-version = "1.98.1"`. All four now say `1.98.1`.
- **Pinned versions.** The install snippets pinned `@0.7.0` (landing) and `@0.18.0` (manual,
  both languages), and the embed demo loaded the CDN build of `0.23.0`. All now show `0.24.0`,
  which is the current release and is published on npm.
- **Known gaps.** The manual (both languages) still said multi-line feeds do not parse and that
  `X::` exception types are broadly incomplete. A fresh build prints what `raku` prints for
  multi-line `==>` and `<==` chains, and only three rare `X::` roles are missing (`X::Await::Died`,
  `X::HyperRace::Died`, `X::Wrapper`). The feed bullet is gone and the exception bullet names the
  three roles.
- **Roast fallback.** The baked-in `STATS` that `index.html` and `manual.html` show when
  `stats.json` is absent (a local checkout) read 1433/1464 and 1432/1464. Both now read 1426/1454,
  the figure `pages.yml` counts for a deploy.
- **Ecosystem snapshot.** `site/content/ecosystem.json` was still the 2026-09-11 projection
  (41.1% dist parity, mutsu 0.23.0). It was regenerated with `scripts/gen-ecosystem-manifest.py`
  and now shows 834 of 1,202 gradable distributions green, 69.4%. `ecosystem/summary.json` and
  `summary.md` had stopped at the 2026-09-30 sweep (830 green, 69.1%) while per-distribution fix
  PRs kept turning records green, so `scripts/ecosystem-sweep.py --rollup` was re-run (it measures
  nothing) to bring them to the same 834, and the README quotes that.

Not changed: `site/content/batteries.json` is also a stale snapshot. Regenerating it adds a
`JSON::Fast` entry with an empty `slot` and `record`, so the bundled module has no entry in the
sidecar map of `scripts/gen-batteries-manifest.py`; that is its own change.
