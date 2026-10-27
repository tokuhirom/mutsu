# A Junction argument to an imported multi is autothreaded

A call of a multi that a `use` bound lexically (a captured candidate family)
passed a Junction argument straight to the candidates, so
`compare(any($a, $b), $c)` with `proto compare($, $)` and typed candidates died
"Cannot resolve caller compare(Junction:D, ...)"; the same multi declared in the
file threaded. `call_captured_candidates` now threads a Junction no candidate
takes whole, one dispatch per eigenstate. Red's `Optimizer::AND.has-condition`
(`where compare(any($.left, $.right), $cond)`) needs it, so
`(C.a >= 1) AND (... AND (C.a < 1))` folds to `False` (`t/06-optimizer`, `t/47`).
