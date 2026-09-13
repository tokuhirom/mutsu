# `return |...` normalizes its returned values

`return |EXPR` now applies Raku's return flattening before the caller sees the
result: one value is returned directly and multiple values become a `List`.
Ordinary `.Slip` and `slip(...)` returns remain first-class `Slip` values.

This fixes URI v0.1.4's `URI::Escape::uri-unescape`, whose one-element return
was previously left as a `Slip` and caused duplicate query parameters to be
lost by `URI::split-query`. The behavior is pinned by
`t/routines/call/return-pipe-scalar-context.t`.
