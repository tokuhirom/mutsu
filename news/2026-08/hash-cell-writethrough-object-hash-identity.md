# Whole-container hash reassignment through a shared cell now keeps object-hash identity

A `for`-loop pointy-block parameter aliased to an outer object hash
(`for (%ao,) -> %a { ... }`) lost that hash's `key_type`/`value_type`
identity — and silently detached from `%ao` — the moment `%a` was passed
into a `Mu $x`-style parameter (the existing "scalar-container-share"
promotion boxes the local into a shared `ContainerRef` cell) and then
whole-reassigned. The reassignment fell into the `ContainerRef` branch of
`exec_set_local_op_inner`, which had no hash analogue of the array
write-through path: it stored the (already-demoted) plain-keyed value
straight into the cell via `Value::store_through_cell`, replacing the cell's
pointer wholesale and orphaning the `Gc<HashData>` node `%ao` still held.

Fixed by adding `hash_container_writethrough_value` (`vm/vm_var_assign_typed.rs`),
the `%`-sigil counterpart of the existing `array_container_writethrough_value`:
it re-applies the variable's declared type constraint, or — for a bound/aliased
name with no constraint of its own — inherits the container type metadata
already embedded in the cell's current value, retagging (including the
object-hash `.WHICH`-key invariant) via the existing `tag_container_metadata`.
The `ContainerRef` branch in `vm_var_assign_set_local.rs` now calls this
helper for `%` names alongside the array helper for `@` names, and stores
the result through `cell_store_preserving_container_identity` (writing into
the *existing* backing `Gc` when both sides are the same container kind)
instead of the old unconditional pointer-swapping `store_through_cell` —
mirroring the mechanism the array side already used.

This fixes `roast/S03-metaops/infix.t`'s largest failure cluster: under the
vendored real `Test.rakumod` (`MUTSU_REAL_TEST=1`), the file previously
aborted after 2086/5076 tests (171 failing) once a hash-`for` loop's `%a`
alias was boxed by the file's own `is-deeply` calls; it now runs
5076/5076 clean under both the native and real-Test providers.

Pinned by the new `t/hash-cell-writethrough-object-hash.t` (11 assertions,
each independently checked against `raku`'s actual output): a boxed
object-hash loop param writing through to its outer alias, a boxed
value-typed (non-object) hash (`Hash[Cool]`), a boxed plain hash staying
plain (unchanged behavior), and the roast file's own
`<<[&metaop]>>`-writeback shape. No regressions across the related
container-identity, hyper-hash, and for-loop-param-writethrough test
surface (30 files, 512 tests).
