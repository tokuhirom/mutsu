# Loop kernels get array element read and store helpers

Campaign B (#19294) added `src/vm/vm_kernel_elem.rs`, the helper interface
agreed with campaign A that lets a native loop kernel (ADR-19288) read and
write `@a[$i]` without declining the whole loop.

- `kernel_elem_slot_ok` is the static check, made at kernel compile time.
- `kernel_elem_entry_ok` runs once per kernel entry. It covers the process
  latches and the variable's by-name records, which cannot change while a
  kernel runs because a kernel runs no user code.
- `kernel_elem_read_{int,num}` and `kernel_elem_store_{int,num}` do the
  per-access work. Each re-checks only what the kernel's own stores can
  change: the slot's array, its length and the element.

An access either succeeds or declines. A decline reruns the iteration
generically, and a store that declines writes nothing.

Campaign A wires the helpers into kernel typing and emission in a follow-up
PR. The loop rows (bench-index-read, bench-index-store and
bench-threads-serial) move only after that.
