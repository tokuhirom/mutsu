# The bench trend dashboard names Rakudo, not "raku"

The benchmark trend dashboard (`bench-trend.html`) labelled its baseline metric
**ratio vs raku**, labelled the 1× reference line `raku`, and said "faster than
raku" in the footer. But *Raku* is the language; what the bench job actually
measures against is **Rakudo**, the implementation, on the same runner. The
wording was also ambiguous now that the page carries a second reference
implementation, Raku++ (`ratio vs rakupp`): both of them are "raku".

The metric button now reads **ratio vs rakudo**, the reference line is labelled
`rakudo`, and the footer defines the metric by name ("below 1× = faster than
Rakudo"). The step summary the bench workflow writes for each run uses the same
column names (`rakudo median (s)`, `ratio vs rakudo`).

The stored data is unchanged: the `raku_median_s` / `ratio_mutsu_over_raku`
columns of `bench-history.tsv` on the `bench-data` branch keep their names
(renaming them would orphan the whole history), and the `#metric=ratio` URL
state keeps working, so existing links to the dashboard still open on the same
view.
