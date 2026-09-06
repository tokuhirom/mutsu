# An element write through a celled `@`/`%` races when threads share it

Measured 2026-09-06 while landing the container half of ADR-0055's capture cell
dichotomy (`news/2026-09/container-captures-join-the-cell-dichotomy.md`). The
container-cell path and the cross-thread atomic lane are two mechanisms for the
same job, and the hand-off between them is not sound: an array or hash that has
been boxed into a `ContainerRef` cell loses the lane's locking without gaining
any locking of its own.

## Root cause

`Interpreter::assign_array_elem_to_shared_var` (and its hash twin
`assign_hash_elem_to_shared_var`, `src/runtime/runtime_shared_vars.rs`) route a
`@a[$i] = $v` performed while `shared_vars_active` through
`shared_array_elem_set`, which holds the `__mutsu_atomic_arr::` store's write
lock across the whole read-modify-write. That is what makes twenty concurrent
`start { @a[$i] = 1 }` blocks all land instead of clobbering each other.

Both functions bail out early when the name is already a `ContainerRef`:

```rust
// An array already boxed into a shared `ContainerRef` cell is already shared
// through the Mutex; let the general assignment path write through it.
if matches!(self.env.get(key).map(|v| v.view()), Some(ValueView::ContainerRef(_))) {
    return None;
}
```

The comment is half true. The cell's `Mutex` protects *rebinding* the name — the
`*cell.lock() = updated` writes elsewhere in that file. It does not protect the
container's **contents**: the general assignment path
(`exec_index_assign_expr_named_op` → `vm_var_assign_index_named.rs`) derefs the
cell, reaches the `Array`, and mutates its `ArrayData` in place via
`crate::value::gc_contents_mut` with no lock held at all. Two threads doing that
concurrently is a data race on a `Vec<Value>`.

The bail-out itself is load-bearing for a different scenario and must not simply
be deleted: it was added because the `__mutsu_atomic_hash::` lane only understands
a bare `ValueView::Hash`, so it treated a celled hash as absent and reinstalled a
plain unboxed `Hash` into `env`, permanently un-sharing the cell (see
`todo/deep/nested-whenever-registration-clobbers-sibling-event-aggregate-writes.md`).

## Minimal repro

Any path that gets a container celled *and* then written from several threads.
The one that surfaced it: temporarily drop the `thread_escaping_captures`
subtraction from `needs_cell_unvouched_containers` (`CompiledCode::compute_free_vars`,
`src/opcode.rs`), then

```raku
my @a;
await (^20).map: -> $t { start { for ^50 -> $k { @a[$t * 50 + $k] = 1 } } };
say @a.grep(*.defined).elems;   # want 1000
```

aborts within a run or two:

```
thread 'pool' panicked at src/value/view.rs:849:18:
internal error: entered unreachable code: with_array_mut probed an Array
double free or corruption (out)
```

— a TOCTOU inside `with_array_mut` (`self.view()` saw an `Array`, the following
`with_repr_mut` did not, because another thread had rewritten the same `Value`)
followed by a heap corruption in glibc. It is real UB, not a logic bug.

`t/concurrent-array-index-assign.t` test 7 and `t/concurrent-hash-assign.t`
test 7 are the ready-made reproductions.

## Why this is large

The fix is a decision about which mechanism owns a shared container, not a patch:

- **Make the celled write path thread-safe.** Every general-path element write
  that reaches a container through a `ContainerRef` would have to take that
  cell's lock for the duration of the `ArrayData`/`HashData` mutation. The write
  sites are numerous (`vm_var_assign_index_named.rs`,
  `vm_var_assign_element.rs`, the `try_native_array_mut` /
  `try_native_hash_mut_bound` mutating-method paths, `env_root_descended_mut`)
  and they are on the hot single-threaded path, so a blanket lock is a
  pessimization that would need measuring.
- **Or retire the atomic lane for celled containers properly**, teaching
  `shared_array_elem_set` / `shared_hash_elem_set` to write *through* the cell
  under the cell's own lock, and then deleting the bail-out. That keeps one
  mechanism, but the lane also carries the dirty-marking and
  `sync_shared_vars_to_env` writeback that the cell path does not, so the two
  have to be unified rather than one dropped.

Either way it is an ADR-sized call about the relationship between the
`ContainerRef` cell (ADR-0013/ADR-0039/ADR-0055) and the `shared_vars` lane.

## What it currently costs

`needs_cell_unvouched_containers` subtracts every name a thread-escaping nested
closure captures, so a container lexical handed to a thread keeps the atomic
lane — and keeps the ADR-0055 hijack the rest of the family no longer has:

```raku
my @a = 1, 2;
@a.push(3);
my $f = -> { start { @a.elems } };
sub collide() { my @a = 9; await $f.() }
say collide();          # raku: 3
```

That is the whole residue; every non-thread-escaping container capture is fixed.
