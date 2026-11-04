# A value slipped in by a computed enum body is a declared term

`enum Slipped (:30SLA, slip <SLB>); say SLB.value` died with `Undeclared name:
SLB` unless the unit started with a `use` that made the compile-time check skip
it (`use v6;` did, so `t/rakuast/rakuast-enum-term.t` passed by accident). The
undeclared-name and undeclared-routine scans read an enum's values from its raw
variants, whose only entry for a computed body is `__DYNAMIC__`; they now use
`parser::enum_value_names`, the helper the executing finalization already used.
Found by the `EVAL($ast)` ratchet triage (#16356), where the unit has no `use`.
