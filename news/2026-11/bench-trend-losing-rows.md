# The bench dashboard leads with where mutsu loses

`bench-trend.html` used to put a "Δ latest" chip on every card: the latest commit against the one
before it. On a runner pool where wall clock moves 16-33% between runs of one binary, that number
was mostly noise, and it answered the wrong question.

The page now opens with a **"mutsu is slower here"** section. It lists every benchmark whose
ratio against rakudo or rakupp is above 1.0x, where the ratio is the median of the last 10
commits of the `+jit` series rather than the latest point, worst first, and links each row to the
issues that track it. The mapping lives in `benchmarks/bench-tracking.toml`, and a losing row with
no entry is shown as untracked. Rows that lose only to rakupp's native JSON parser are listed in
their own group, labelled, instead of ranking first on a number that does not compare engines.

Losing and winning ratios are coloured wherever they appear: on the cards, in the table and in
the new section. The per-commit delta is replaced by a trend that compares the median of the
last 10 points with the 10 before. It is shown only when the step is larger than the series'
own noise band. `--report` prints the list as Markdown.

At the first render, 21 of 54 benchmarks lose. Against rakudo, `bench-multi-dispatch@section`
(1.40x), `bench-yaml-parse-big@section` (1.33x) and `bench-json-fast-spdx@section` (1.27x) lose.
The rest lose only to rakupp, led by the grammar rows (1.93x / 1.68x) and startup (1.67x). The
one losing row without an issue, `bench-multi-dispatch@section`, is now #19232.
