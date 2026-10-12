# `min`, `max` and `minmax` refuse a lazy list

`(1..3).lazy.min` answered the lazy `Seq` itself (and, briefly, `1`), and
an infinite `(1, 2 ... *).min` relied on a later path to refuse it. The
`Any.min`/`Any.max`/`Any.minmax` implementations now raise
`X::Cannot::Lazy` for any receiver that `.is-lazy` reports lazy, as Rakudo
does, while a `Range` keeps answering from its bounds (`(1..*).min` is
`1`) (#12971).
