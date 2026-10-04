# `Metamodel::SubsetHOW.refinement`

`S.^refinement` (and `UInt.^refinement`) now answers the subset's `where` predicate as a
callable, `Mu` for a subset without one, matching rakudo. The compiler builds the predicate
value at every subset declaration (a non-code predicate such as `where /a/` is wrapped as
`{ $_ ~~ PRED }`) and the registry keeps it separately from `predicate_closure`, so the type
check path is unchanged. This unblocks Grok's `Grok::Moppet.methods` (#11698).
