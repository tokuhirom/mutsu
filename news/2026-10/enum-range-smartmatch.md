# Enum values numify inside Range smartmatch

`DEBUG ~~ TRACE..INFO` returned False because `Value::to_f64` treated an enum value as 0, so a
Range with enum endpoints collapsed to `0..0`. Enum values now numify to their underlying value.
Found via Log::Async `t/04-filter.rakutest` (now 10/10 under mutsu).
