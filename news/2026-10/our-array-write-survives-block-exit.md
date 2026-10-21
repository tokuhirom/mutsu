# A write to a captured unit-level `our @a` survives a bare block

An `our @a` / `our %h` that a sub captured was written through its shared cell,
but the package store the block-exit refresh of `our` aliases reads kept the
declaration's empty container. After any bare block the stale container was put
back over the live one, so a BEGIN-time effect (or a user trait callback run in
the declaration phase) that pushed into it appeared to be undone. The cell
write-through now publishes the container to the package store too.
