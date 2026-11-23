# Roast compatibility figure

How mutsu counts its headline Roast figure, the one README, the site and
`HISTORY.tsv` report (#19036). One generator, `scripts/roast-compat.py`,
produces it for all three, so they cannot drift apart.

## Definition

- **The file list** is `roast/spectest.data`: the spec files Rakudo runs ("a
  list of all spec tests that are expected to pass"). An entry is the path before
  any `#` marker (`# slow`, `# stress`, `# moar`), relative to `roast/`; blank
  lines and comment lines are ignored. At the vendored Roast revision this is
  1,425 files, including `6.c/`, `6.d/` and `integration/` entries.
- **The fudge** is applied: each file runs with `MUTSU_FUDGE=1`, so the
  `#?rakudo` `skip` / `todo` / `emit` directives take effect as they do for
  Rakudo.
- **A file passes** when it runs to completion with exit code 0, prints its
  plan, and every assertion it does not skip passes; a `# TODO` assertion
  counts as passing (`scripts/run-roast-test.sh`, `scripts/roast-history.sh`).
- **The figure** is the number of passing files out of all spectest.data files.

## Where it comes from

| Figure | Source of "passing" | Command |
|---|---|---|
| Site `stats.json` (landing page, manual) | `roast-whitelist.txt`: every whitelisted file passes `make roast` in CI | `scripts/roast-compat.py stats-json`, run by `pages.yml` at deploy |
| README "Status" paragraph | the same whitelist, plus the last `HISTORY.tsv` row's subtest count | `scripts/roast-compat.py readme` (also run by `scripts/roast-history.sh`) |
| `HISTORY.tsv` `spec_files` / `spec_pass`, `HISTORY-pass.svg` | a full run of the suite | `scripts/roast-history.sh`, through `scripts/roast-compat.py count --pass-list tmp/roast-pass.txt` |

`scripts/roast-compat.py list` prints the file list itself.

## The legacy series in `HISTORY.tsv`

Until #19036, `HISTORY.tsv` counted a different set: `find roast -maxdepth 2
-name '*.t'` minus `6.c/`, `6.d/`, `integration/`, `3rdparty/`, `APPENDICES/`,
`MISC/`, `packages/`, `docs/` and `t/` (about 1,288 files). Its columns
(`files`, `pass`, `fail`, `error`, `panic`, `timeout`, `subtests_pass`,
`subtests_total`) keep counting that set, so every old row stays comparable
with the new ones. The two `spectest.data` columns were appended after them;
rows written before the switch leave them empty. `HISTORY-pass.svg` draws both
series and marks the row where the `spectest.data` series starts.

A full run executes each file of the two sets' union once, and each column
counts only its own set.

## Comparing with other implementations

[Raku++ (rakupp)](https://github.com/ash/rakupp) reports its Roast figure the
same way: against `spectest.data`, with the `#?rakudo` fudge applied
([its COUNTING.md](https://github.com/ash/rakupp/blob/main/docs/status/COUNTING.md)).
The two figures can be compared directly, provided both use the same Roast
revision.

Files mutsu fails only because Rakudo fails them too are not in
`spectest.data`, so they do not lower the figure. They are still tracked in
`TODO_roast/BLOCKERS.md`, as are all files that are not whitelisted.
