# A thread spawn seeds no user lexical; the name-lane masks are gone

ADR-16595 slice 4. `clone_for_thread` used to publish every live lexical of the
spawning frame into the name-keyed `shared_vars` lane, and a stack of masks
(`thread_redeclared_vars`, `thread_decl_in_flight`, `thread_param_shadow_vars`,
`ThreadParamMaskGuard`, `frame_remask_armed`, `container_name_is_redeclared`,
`block_captured_scalars`, `param_bound_aggregates`, the ADR-0039 section 8.6
transient withdrawal and ADR-0129's retired-binding box) existed only to keep
that publication from hijacking a same-named binding in an unrelated frame.

Since slices 1-3 every escaping-captured `$`/`@`/`%` lexical is a cell from its
declaration, and a plain user `@`/`%` never uses the lane. The spawn now
publishes only the names code on another thread reaches without a capture (the
opaque set: lexicals a registered class/role method writes) and the `@*x`/`%*x`
dynamics. With nothing user-lexical on the lane, every mask has nothing to mask
and is deleted.

One behavioural widening: a captured scalar holding a `Seq`, `HyperSeq`,
`RaceSeq` or `Slip` now gets a declaration cell like any other value, so its
reassignment on a worker reaches the spawning frame without the lane.

`MUTSU_VM_STATS=1` reports `spawn-seeding ... inserts=0` on the concurrency
tests that spawn with only plain lexicals in scope.
