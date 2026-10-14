# `+$mix` sums weights exactly, like `.total`

`+$mix` summed the weights in `f64`, so a Rat total could come back as a Num. The
weight addition now lives once on `MixWeight::add`/`sum` (`src/value/mix_weight.rs`):
`(+)`, duplicate-key accumulation, `.total` and the numeric coercion all use it, and
`value` no longer needs to name `builtins` for it (#12736).
