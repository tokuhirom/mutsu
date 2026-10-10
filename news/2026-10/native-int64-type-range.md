# `int.Range` / `int64.Range` report the real 64-bit bounds

`int.Range`, `int64.Range` and the other 64-bit signed aliases (`long`, `longlong`,
`ssize_t`, `atomicint`) returned `-Inf..Inf` because the fast `Value::range` form reads
`i64::MIN` / `i64::MAX` as infinity sentinels. They now return the generic integer range
`-9223372036854775808..9223372036854775807`, so `int.Range.max` is an `Int` as in Rakudo.
Found via the Audio::Icecast suite (`t/020-stats-basic.t`, now at parity).
