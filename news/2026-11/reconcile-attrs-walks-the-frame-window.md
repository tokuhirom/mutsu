# The method-exit ContainerRef scan walks the frame window directly

`frame_container_refs`, run by `reconcile_attrs` on every non-pure method exit,
looked up each local slot through `Locals::get` (a base add and a bounds check
per slot) before testing its tag. It now zips the compiled local names with the
frame window slice, so the common exit (no ContainerRef in scope) pays one
bounds check for the whole walk. Semantics are unchanged; `t/oo/attribute/attr-bind-container-ref-method-exit.t`
pins that a `:=`-bound attribute is still recovered. Part of ADR-14633; measured
with `callgrind` on `$o.z()` (see the PR body).
