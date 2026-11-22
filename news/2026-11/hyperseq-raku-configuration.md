# `HyperSeq.raku` renders the configuration, not the elements

In Rakudo, a `HyperSeq` or `RaceSeq` holds only its configuration, so its
`.raku` is `Mu.raku` over that configuration:
`(hyper 1, 2).raku` is
`HyperSeq.new(configuration => HyperConfiguration.new(batch => 64, degree => 3))`.
mutsu rendered the elements as a list, `(1, 2)`. On an infinite source such as
`(1..*).hyper.raku`, it pulled elements without end (#18690).

The rendering is now one value-layer helper, `value::hyper_config::seq_raku`.
It is used by the `HyperSeq`/`RaceSeq` `raku` rows, by the lazy hyper list's
`raku`, and by the nested rendering inside a list. `.gist` and `.Str` still
show the elements, as Rakudo's do.
