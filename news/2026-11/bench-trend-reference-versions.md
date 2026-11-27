# bench-trend names the rakudo and rakupp its ratios are measured against

The bench dashboard (`bench-trend.html`) showed mutsu's time divided by rakudo's and
rakupp's, but not which rakudo or rakupp. The bench history has recorded both versions on
every row since the columns were added, and the page now reads them:

- The header names the current reference versions, for example
  "vs Rakudo 2026.11 · Raku++ v5.9.0". Its tooltip lists each reference's version history.
- A ratio chart draws a dashed vertical line where a reference version changed. A new rakudo
  steps every rakudo ratio at once without mutsu changing, and the marker keeps that step
  from being read as a mutsu regression or win. The history already has one: the runner
  moved from Rakudo 2022.12 to 2026.07 on 2026-08-25.
- The ten-commit ratio medians, on the cards, in the table and in `--report`, no longer
  read points from before the reference's latest version change. Right after a bump, a
  ratio reads fewer points and says so with `n=`.

The card, its ratio line and the hover tooltip are now built as DOM nodes rather than
parsed markup, so a version string or a benchmark name from the history is only ever text.
