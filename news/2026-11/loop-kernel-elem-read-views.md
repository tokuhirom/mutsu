# A loop kernel resolves the arrays it only reads once per entry

A loop kernel's `@a[$i]` read called its helper with the interpreter and the
slot. On every access the helper read the slot's word, took the `:=` binding
cell's lock, decoded the cell's array view, checked the node's metadata, and
then found the element. For bench-threads-serial's celled reads that was
~140 instructions per read even after #19443.

Now the kernel entry resolves each array the region only *reads* to an element
view: the slot plus a pointer to the element slice and its length. The read
helper takes those views in the interpreter's place, finds the slot's view,
bounds-checks the index and probes the element (`as_int` / `as_num`). It
declines exactly as before for an out-of-range index, a hole or a non-number.

The view stays valid for the whole run, for these reasons:

- the slot or its binding cell holds the node;
- the region stores into no array it reads under any name (the entry already
  compares node identities);
- a kernel runs no user code, and the sub kernels it calls touch no array;
- no second mutator thread exists;
- a poll only reaches the cycle collector, which neither moves nor mutates a
  reachable node.

Two-level grid reads and hash reads keep their helpers.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-threads-serial.raku` | 151,425,397 Ir | 111,610,844 Ir (-26%) |
| `benchmarks/bench-index-read.raku` | 130,467,236 Ir | 99,703,372 Ir (-24%) |
