# bench-trend.html renders the deterministic series too

`news/2026-09/deterministic-instruction-count-bench-series.md` added
`bench-det-history.tsv` on the `bench-data` branch, and stopped there. Nothing
published it: `pages.yml` fetched only `bench-history.tsv` and handed only that
to `scripts/bench-visualize.py`, so the series that can resolve a 0.1% change was
readable only by `git show origin/bench-data:bench-det-history.tsv`. That is a
metric nobody would look at.

## One page, two series

`bench-visualize.py` grew an optional `--det` argument. The deterministic rows
join the **same commit axis** and the same per-benchmark cards as a third metric
button beside `mutsu seconds` and `ratio vs raku`, rather than a page of their
own — because the whole point is to read them together. The deterministic series
localizes a change; the wall-clock series says whether it mattered. Splitting
them across two pages would have made the pairing the reader's job.

A point is now `[commitIdx, seconds, ratio, instructions]`, with `instructions`
null for every commit recorded before the series existed — which is all 2748 of
them at the time of writing. Every consumer (axis bounds, the delta chip, the
hover tooltip, the table) filters on a `defined` predicate, so the older stretch
of the chart simply has no line under that metric instead of collapsing the
y-axis to zero.

Three smaller things the shape forced:

- **Instruction counts need their own formatter.** They run 1e9-1e13, where the
  seconds formatter's fixed decimals are unreadable; they render as `12.74G`,
  `269.4M`.
- **A deterministic row with no wall-clock twin still gets a point.** It happens
  already: `bench-yaml-parse` has no rakudo baseline, so its `NA` ratio drops it
  from the wall-clock model entirely — and it would have vanished from the page
  under a metric that does not need rakudo at all.
- **The button is hidden unless there is data behind it** (`hasDet` in the
  model), so a checkout whose `bench-data` has no deterministic history yet does
  not offer a metric that renders nothing. `--det` pointing at a missing file
  prints a note and renders wall clock only, which is exactly the state of the
  world between this change and the first `bench.yml` run that records a
  deterministic row.

## Verified in a browser, not just in the generator

The change is mostly client-side JavaScript inside a Python string, which no
Rust test can reach. It was checked headlessly against the real 123,499-row
history (Chromium via Playwright): no page errors either with or without the
deterministic series; the button hidden in the first case and visible in the
second; switching to `instructions` draws 48 series paths across 24 cards with
SI-formatted readouts; and switching back to seconds and then to the table view
still renders 24 rows. Both branches of the new `pages.yml` step were run as
written.
