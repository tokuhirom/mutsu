# The native base candidates of a deferral chain are enumerated in one place

The ten `native_*_next_candidate` bridges that end a `callsame`/`nextsame` chain in a builtin were listed by
hand at three sites of `dispatch_next_candidate`, each in its own order. They are now a `NativeBase` enum with
three named orders and one walker, the first step of ADR-11276's resolver cutover (slice 4). No behaviour change.
