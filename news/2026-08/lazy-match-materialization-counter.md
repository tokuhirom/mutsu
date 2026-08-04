# Lazy Match materialization is now observable

ADR-0016 relies on tag probes and Match accessors avoiding `Value::view()`: viewing a
lazy Match forces its Instance-shaped attribute map and quietly defeats the lazy
representation. `MUTSU_VM_STATS=1` now reports `match_materializations` in the
`regex-captures` line. The counter increments once when each lazy Match node is first
forced, making accidental materialization visible in grammar and regex diagnostics.
