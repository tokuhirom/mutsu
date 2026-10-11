# `Cool.Complex` is a method-table row; `Range`/`Seq` reach `Complex`, `UInt` and `Version`

`(1, 2, 3).Complex`, `[1, 2].Complex` and `{a => 1}.Complex` answered `0+0i`; they are now the element
count as a `Complex` (`3+0i`, `2+0i`, `1+0i`), as in Rakudo, where `Cool.Complex` is
`self.Numeric.Complex`. `Cool.Complex` is a method-table row shared with the cascade arm
(ADR-11276 §9.72). `(1..5).Version` (`v1.2.3.4.5`) and `(1, 2, 3).Seq.UInt` (`3`) no longer die
with `X::Method::NotFound`.

A `Range` with a fractional endpoint also numifies correctly now: `(1..^5.5).sqrt` counted four
elements instead of five.
