# Cool's numeric methods now work on a Range or Seq receiver

`(1..3).sin`, `.abs`, `.log2`, `(1,2,3).Seq.sin` and the rest of `Cool`'s numeric
family (the math functions, `abs`, `sign`, `floor`/`ceiling`/`round`/`truncate`,
`is-prime`, `conj`, `chr`, `rand` and the native integer coercions) used to die with
"No such method". `Range` and `Seq` are closed dispatch shapes, so `Cool` rows never
reached them. They now reach exactly the audited names listed in
`DispatchShape::reaches_audited_cool`, so the text, iteration and search rows `Cool`
also declares stay unreachable. A numeric `Range` numifies to its element count
(`Inf` for an endless one).

`log2` is now `log(x) / log(2)` like Rakudo, so `3.log2` prints
`1.5849625007211563` instead of `1.584962500721156`.
