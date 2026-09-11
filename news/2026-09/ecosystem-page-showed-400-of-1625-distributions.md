# The ecosystem page showed 400 of 1625 distributions and claimed all of them

`site/ecosystem.html` rendered `matches.slice(0, 400)` — a cap added when the
corpus sweep had measured a couple of hundred distributions and the table was
never expected to reach it. The sweep has since measured the whole corpus (1625
records under `ecosystem/dists/`), so the cap became the page's dominant
behaviour, and a bad one, because the rows are sorted worst-first
(`STATUS_ORDER` in `scripts/gen-ecosystem-manifest.py`: red, partial,
blocked_load, blocked_dep, green, no_baseline, skipped).

With today's ledger that puts the cut 94 rows into `partial`:

| status | records | rows rendered before |
| --- | --- | --- |
| red | 306 | 306 |
| partial | 271 | 94 |
| blocked_load | 360 | 0 |
| blocked_dep | 43 | 0 |
| green | 403 | 0 |
| no_baseline | 235 | 0 |
| skipped | 7 | 0 |

So the page that exists to answer "does my module work on mutsu?" could not
display a single working module in its default view — every one of the 403 green
distributions fell past the cap — while the count above the table read
`1625 件中 1625 件`, because it counted matches rather than rendered rows. The
status filter and the search box both still reached the hidden rows, which is
why this survived: any query narrow enough to matter came back correct.

The cap is gone; the table renders every match. A full 1625-row render measures
~1.1s to `body[data-ready]` on a cold load and ~90ms to re-render on a keystroke
in the search box, so nothing needed chunking or virtualizing to pay for it.

Two things now keep it fixed. `site/e2e.test.mjs` asserts the row count equals
the manifest's distribution count outright instead of `min(count, 400)`, and
asserts that filtering yields exactly the matching rows rather than "at most the
full count" — an assertion a truncating table also satisfied. And the committed
`site/content/ecosystem.json` snapshot was regenerated: it had been left at the
251-row shard-C measurement from 2026-09-10 while `ecosystem/dists/` grew to
1625, so the e2e suite had been exercising a corpus far too small to ever reach
the cap. (The deployed page never read that snapshot — `pages.yml` regenerates
it at deploy time — which is exactly why the staleness went unnoticed.)

The same regeneration flipped the manifest's `has_chart` to true, so the e2e
suite now stages `ecosystem/history.svg` into `site/` the way `pages.yml` does
for the deploy, and asserts the chart's visibility follows `has_chart`. That
wiring had no coverage before.
