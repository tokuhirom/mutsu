# Core type barewords resolve through the builtin type catalog

`Macro` and `PROCESS` used as terms fell through to the name as a `Str`, because the
bareword resolver kept its own hand-written list of core type names. `is_builtin_type` now
also consults the builtin type catalog, so every catalog row (and `PROCESS`) yields its type
object or package (#11164).
