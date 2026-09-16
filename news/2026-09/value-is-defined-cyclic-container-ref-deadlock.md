# `value_is_defined` no longer deadlocks on a cyclic `ContainerRef` cell

`value_is_defined` (`src/runtime/types/mod.rs`) reads through a `ContainerRef`
cell to test the definedness of what it actually holds — `$x // …` and
`take-rw …[i] // next` need the *inner* value's definedness, not the wrapper's.
Its `ContainerRef` arm did that by locking the cell's `Mutex` and recursing
into the locked value while still holding the guard:

```rust
ValueView::ContainerRef(arc) => value_is_defined(&arc.lock().unwrap()),
```

`std::sync::Mutex` is not reentrant. If a cell's own content is (eventually) a
`ContainerRef` back to the same cell — a cyclic cell, which no legitimate Raku
container can be but which mutsu's container machinery apparently managed to
construct anyway — the recursive call tries to lock the very `Mutex` this
thread already holds and hangs forever, with no diagnostic. This was found
([#8507](https://github.com/tokuhirom/mutsu/issues/8507)) via a real zef
distribution, `Game::Entities` 0.1.6: its `t/sorting.t` does a
read-two-elements-then-write-them-back-swapped multi-dim slice assignment that,
somewhere in its fuller context, leaves a cell pointing at itself.

The fix follows the same pattern `gist_value`'s `ContainerRef` arm
(`runtime/utils/gist.rs`) already uses for exactly this hazard: clone the
cell's contents out and drop the lock *before* recursing, and track which
cells are currently being unwound on this thread so a cycle is detected
rather than deadlocked on. A cyclic cell is treated as defined — it is a real,
allocated container, not a type object — which turns an infinite hang into an
instant, correct-enough answer instead of a crash.

A survey of every other `ContainerCell::lock()` call site (`arc.lock().unwrap()`
across `src/`) found this was the only place that recurses while still holding
the guard; every other site either locks-then-clones-then-drops before doing
anything further, or doesn't recurse at all, so none of them share this
deadlock hazard.

The root cause — how mutsu's multi-dim slice-swap assignment machinery can
produce a self-referential cell in the first place — is still open and needs a
minimal repro to isolate; that half of #8507 remains unresolved. This PR is
the crash-safety backstop only.
