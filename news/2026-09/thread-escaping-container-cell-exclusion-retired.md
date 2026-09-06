# The thread-escaping container capture is a lexical again

`CompiledCode::compute_free_vars` used to subtract `thread_escaping_captures`
from `needs_cell_unvouched_containers`: every name a thread-escaping nested
closure captured was *excluded* from the declaration-site container cell, so it
kept the name-keyed `__mutsu_atomic_arr::` / `__mutsu_atomic_hash::` lane
instead. The comment on the subtraction said why in as many words — boxing one
"turns `start { @a[$i] = ... }` into a data race".

That premise stopped being true when
[ADR-0068](../../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md)
landed. An element write that reaches its container through a shared
`ContainerRef` cell now excludes concurrent threads on both the write and the
read side, keyed on the cell, through three guarded funnels
(`vm_var_assign_index_named.rs`, `value_methods_a.rs`, `builtins_multidim_assign.rs`
plus the mutating-method guard in `methods_call_dispatch.rs`). The celled path
is synchronized, so the mitigation was protecting against a race that no longer
exists — while still charging its full correctness cost.

## What the mitigation cost

The lane is keyed by **name**, so a container lexical handed to a thread kept
the ADR-0055 hijack that the rest of the capture-cell family lost in slice 1b
(`da8e94252`):

```raku
my @a = 1, 2;
@a.push(3);
my $f = -> { start { @a.elems } };
sub collide() { my @a = 9; await $f.() }
say collide();          # raku: 3, mutsu (before): 1
```

The closure's `@a` resolved to whatever same-named binding the *calling* frame
happened to have. This was the last hole in the family, and it was held open
only by the subtraction.

## What changed

The subtraction is gone, so a thread-shared container lexical takes the
declaration-site cell like every other capture. That removed the last consumer
of `CompiledCode::thread_escaping`, which is deleted along with its compiler
plumbing (`Compiler::thread_escaping_position`, `with_thread_escape`, and the
`start`-keyed `thread_esc` locals in `expr_call.rs` / `expr_method.rs`) — the
flag's own doc comment described a `box_captured_lexicals` gate that had already
been replaced by the shared `type_constrained_unboxable` predicate, so nothing
else read it.

The `todo/deep/` ticket warned that the two mechanisms are not interchangeable,
because the lane also performs the dirty-marking and `sync_shared_vars_to_env`
writeback that merges a worker's env back into the spawning frame. Measured, the
cell carries that shape too — the cell *is* the sharing, so a worker's
`@b.push(30)` is visible to the declaring frame afterwards with no writeback
step at all.

## Gates

`t/thread-escaping-container-capture-is-lexical.t` pins the four shapes
(`@`-capture, `%`-capture, worker push visible on both sides, inner frame
shadowing the name). `t/concurrent-array-index-assign.t`,
`t/concurrent-hash-assign.t`, `t/concurrent-celled-container-store.t`,
`t/concurrent-attribute-element-store.t` and `t/lock.t` all still pass, and the
ADR-0068 stress probes (`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024
MUTSU_GC_VERIFY=1`, 240 processes at 24-way) report 0 failures on every probe
both before and after the removal.
