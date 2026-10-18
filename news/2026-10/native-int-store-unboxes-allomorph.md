# A native `int` slot accepts an allomorph (`IntStr`, `$*USER`)

`my int32 $n = $*USER` died with `Cannot convert value to native integer type 'int32'`: the
store only recognised plain numerics. An `IntStr` now stores its `Int` part. P5getpriority's
`getpriority(2, $*USER)` (`Int() $who` leaves an `IntStr` alone) works; `t/getpriority.t`
matches Rakudo apart from the live PID values.
