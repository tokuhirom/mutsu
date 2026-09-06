# A thread-escaping container is kept off the cell path, and loses its own binding

**The race this file was originally about is fixed** (2026-09-06,
`news/2026-09/celled-container-cross-thread-store-exclusion.md`,
[ADR-0068](../../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md)
§4 steps 1-2). An element write that reaches its container through a shared
`ContainerRef` cell now excludes concurrent threads, on both the write and the
read side, keyed on the cell. What survives is the **mitigation that was put in
place while it was unsafe**, and the correctness cost that mitigation carries.

## The mitigation

`CompiledCode::compute_free_vars` (`src/opcode.rs`) subtracts
`thread_escaping_captures` from `needs_cell_unvouched_containers`: every name a
thread-escaping nested closure captures is *excluded* from the decl-site
container cell, so it keeps the name-keyed `__mutsu_atomic_arr::` lane instead.
Its comment says why in as many words — "boxing one turns
`start { @a[$i] = ... }` into a data race".

That premise is now false. The celled path is synchronized.

## What the mitigation costs

A container lexical handed to a thread keeps the lane, and therefore keeps the
ADR-0055 hijack the rest of the capture-cell family no longer has:

```raku
my @a = 1, 2;
@a.push(3);
my $f = -> { start { @a.elems } };
sub collide() { my @a = 9; await $f.() }
say collide();          # raku: 3, mutsu: 1
```

The closure's `@a` resolves to whatever same-named binding the *calling* frame
happens to have, because the lane is keyed by name. Every non-thread-escaping
container capture was fixed by ADR-0055 slice 1b (`da8e94252`); this is the last
hole, and it is held open only by the subtraction.

## Why it is still deep, not a one-line deletion

Deleting the subtraction moves thread-shared containers from the lane onto the
cell path, and the two are not interchangeable in what they carry:

- the lane performs the dirty-marking and `sync_shared_vars_to_env` writeback
  that merges a worker's env back into the spawning frame; the cell path does
  not, because the cell *is* the sharing;
- the lane's `assign_array_elem_to_shared_var` / `assign_hash_elem_to_shared_var`
  bail-outs and `container_name_is_redeclared` masking exist for shapes
  (re-declared names, attribute containers) that the cell path spells
  differently.

So the unit of work is "retire the lane for containers that have a cell", not
"drop the filter". The pins that decide it are `t/concurrent-array-index-assign.t`,
`t/concurrent-hash-assign.t`, `t/concurrent-celled-container-store.t` and the
S17 whitelist; the reward is the repro above answering `3`, and one mechanism
instead of two for a container more than one thread can see.

Adjacent, and probably the same slice: ADR-0068 §4 step 3 still has to widen the
store-side exclusion to mutating *methods* (`push`/`pop`/`splice`/`:delete`),
which today rely on the lane for exactly the containers this filter keeps there.
See `todo/deep/gc-contents-mut-cross-thread-aliased-writes.md`.
