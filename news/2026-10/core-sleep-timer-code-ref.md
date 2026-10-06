# `&CORE::sleep-timer` and `&CORE::sleep-till` resolve

`&CORE::sleep-timer(...)` died with "Could not find symbol '&sleep-timer' in
'GLOBAL::CORE'" because only `sleep`, `now` and `time` were exposed as core
code references. `sleep-timer` and `sleep-till` now resolve the same way, which
makes the ecosystem distribution `P5sleep` pass its test file.
