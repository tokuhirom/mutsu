# `.protect` no longer reads a stale sigilless binding from the shared-variable store

`$lock.protect: { ... }` runs its block inline and first pulls the block's captured names from the
cross-thread shared-variable store into the env. The store is keyed by bare name, so for a sigilless
binding (`\type`) it held a snapshot of whichever frame last bound that name, and the pull replaced
the running frame's own value with it. Red's `MetamodelX::Red::Model.alias` (`|c (Red::Model:U \type, ...)`
with `$!alias-cache-lock.protect: { ... type ... }`) therefore saw `type` as `Mu` from its second call on
and died with "No matching candidate found for the parametric role" (#14867).

A sigilless name is bound once and never assigned, so no thread can have updated it; the sync now skips
those names. Red's `t/16-create-related-pars` gets past the alias creation. The regression is a
`#[test]` in `runtime_shared_vars_tests.rs`: I could not shrink the failure to a script, since it needs
a stale store entry left by a thread started in another frame.
