# Element stores reconcile the slot and the env through their place

The named element-store bodies keep a declared local's container in two
places, the env and the slot, and copy it between them around each store.
The four shapes of that copying are now `StorePlace` methods (ADR-14632):
`place_pull_slot` (mirror the slot into an empty env binding before the
store), `place_sync_slot` (copy the env's container back after it),
`place_write_slot` (write a value into the slot) and
`place_release_slot_copy` (drop the slot's copy so a chained store mutates
the node in place). Each does what the inlined code did. When a `Slot` place
starts using the slot itself, these become no-ops for it in one place
instead of a dozen.
