# Loop kernels get `%h{$int}` and `@grid[$i][$j]` helpers

After #19365 wired array element reads and stores into loop kernels, the two
container shapes left on `bench-index-read` were a hash keyed by an `Int` and a
nested array read. Campaign B (#19294) added the helpers for both in
`src/vm/vm_kernel_hash.rs`. They follow the contract the array helpers
already keep: an access either succeeds or declines, a declined access reruns
its iteration generically, and a store checks everything before it writes.

- `kernel_hash_read_{int,num}` look the key up by its decimal text, formatted
  on the stack as the interpreter's `%h{$int}` fast path does (#19308).
- `kernel_hash_store_{int,num}` follow the lean `%h{$k} = $v` store's rules
  (#19330). A new key allocates its Rust string, never a GC object.
- `kernel_hash_entry_ok`, `kernel_hash_node_id` and `kernel_hash_kind_hint`
  give the kernel its per-entry check, the identity for the alias rule and
  the kind to compile reads for.
- `kernel_elem2_read_{int,num}` read `@grid[$i][$j]` when the row is a plain
  `Array` held directly in the outer element.

The array helpers' entry checks are now shared: `local_read_unspoiled_for`
and `process_latches_clear` serve both modules. Campaign A wires the new
helpers into kernel typing and emission in a follow-up PR.
