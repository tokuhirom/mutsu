# The bench dashboard marks losses in place instead of in a banner

The "mutsu is slower here" section that #19235 put at the top of `bench-trend.html` was louder
than the page needed. It is gone. The same information now sits in the ordinary views:

- each card's "vs rakudo / vs rakupp" line shows the 10-commit median ratios;
- a losing ratio is red, and lighter red when it is marginal;
- a ratio that is not engine against engine carries a † with the caveat as its tooltip;
- a losing card ends that line with small links to its tracking issues from
  `benchmarks/bench-tracking.toml`.

Wins are no longer coloured, and the card no longer gets a red edge. The table keeps its
"vs rakudo" and "vs rakupp" columns with the same colouring. `--report` still prints the full
ranked list of losing rows for a status report.
