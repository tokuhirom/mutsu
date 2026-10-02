# Buf mutators write through a shared lexical cell, so a nested sub's reassignment survives the block

A bare block that mutated a captured `Buf` (`.push`, `.pop`, `.splice`, `.write-*`, ...)
and then called a sub that *reassigned* the same lexical lost the reassignment
(issue #10390). `Terminal::ANSIParser`'s `finish-sequence` does exactly this
(`$sequence.push($byte); ...; $sequence = $seq-buf-type.new`), so after the first
sequence the reset was lost and the next ESC emitted a spurious `Incomplete`.

```raku
my sub reset() { $seq = buf8.new }
my &b := { $seq.push(1); reset() };   # rakudo: $seq.elems == 0 afterwards
```

**Root cause.** A lexical that a closure captures and something writes lives in a
shared `ContainerRef` cell. Every Buf mutator re-seated the receiver binding with a
plain `env.insert(name, updated)`, which replaced that cell with a bare value in the
running block's own overlay. The nested sub's `$seq = buf8.new` then wrote the cell
the block no longer read, and the block's exit rejoin
(`call_compiled_closure_in_unit`, the `pending_caller_var_writeback` loop) stored the
stale overlay Buf back over it. The closure-exit machinery was doing what it is
designed to do — the stale value was planted by the writer.

**Fix.** The writers now use `Env::insert_through`, the existing helper for "assign to
the container this name already denotes": `buf_mutate_method`,
`buf_pop_shift_splice`, the `reallocate` writer (`runtime/methods_mut_substr_buf.rs`),
the `write-num*`/`write-int*` writers (`runtime/methods_mut_dispatch.rs`), the native
`try_native_buf_mut`, and the three delegate writers that rebuild an `is Array`,
`is Hash` or `is Baggy` subclass instance (`write_back_array_storage_instance`,
`vm_hash_subclass_delegate.rs`, `vm_baggy_subclass_delegate.rs`). An `is Array`
subclass hit the identical bug (`mutsu 1`, `rakudo 0`).

Pinned by `t/routines/closure/closure-exit-buf-reassigned-by-nested-sub.t`, which runs one
factory per mutator so each writer is exercised on its own.

Found while doing this, filed separately: a mainline-block `my sub` is shadowed by a
same-named `my sub` declared in a routine (#10391), and `BagHash.add` is missing on a
user `is BagHash` subclass (#10392).
