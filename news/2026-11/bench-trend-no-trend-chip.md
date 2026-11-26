# The bench dashboard drops its trend chip

`bench-trend.html` no longer shows a change figure on its cards ("+26.6% slower", "no change")
or a trend column in its table. #19235 had replaced the single-commit "Δ latest" with a trend that
compared two 10-commit medians against a noise band. It still read run-to-run motion on the CI
runners as a change in mutsu, and that motion is mostly which instance the run landed on. A card
now shows the median of its last 10 points, its ratios against rakudo and rakupp, and the chart.
