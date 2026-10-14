# RakuAST: named parameters with a trailing sub-signature

`:x($r) ($a, $b)` (an alias followed by a signature that destructures the
named value) now round-trips: one `RakuAST::Parameter` with `names => ("x",)`,
`target => $r` and the trailing signature as its `sub-signature`, as rakudo
renders it. Lowering puts the signature back on the parser's
`outer_sub_signature`. The anonymous alias target `$` is now rendered as
`ParameterTarget::Var(name => "$")` instead of the parser's placeholder name.
Two more `t/` files pass under `MUTSU_RAKUAST=1` (ADR-10723 S10).
