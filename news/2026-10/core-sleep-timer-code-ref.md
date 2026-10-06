# `&CORE::sleep-timer` resolves

`&CORE::sleep-timer(...)` died with "Could not find symbol '&sleep-timer' in
'GLOBAL::CORE'" because only `sleep`, `now` and `time` were exposed as core
code references. `sleep-timer` now resolves the same way, which
makes the ecosystem distribution `P5sleep` pass its test file.
