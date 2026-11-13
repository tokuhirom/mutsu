# IterationBuffer methods edit in place; `is raw` routines take the light call path

Found while profiling ParaSeq's `squish(:with)` (#17337), whose consumer appends every batch to one
`IterationBuffer` and whose `:as` default is `sub identity(\value) is raw { value }`.

- **`IterationBuffer`'s `push`, `append`, `unshift`, `prepend`, `BIND-POS`, `clear`, `elems` and
  `AT-POS` no longer copy the whole buffer.** Each of them copied the element vector out, edited
  the copy and stored a new array back, so `$buffer.append($batch)` in a loop was quadratic, and
  `elems`/`AT-POS` were O(n). They now edit the buffer's storage in place, as `nqp::push` does.
  `append`, `push`, `elems` and `AT-POS` are O(1) amortized, as on Rakudo. A `.List` taken from
  the buffer earlier now sees later edits, because on Rakudo the buffer is that `List`'s storage.
- **An `is raw` routine takes the positional-light call path.** Its raw return is already
  compiled into the body, and the call site decides the Proxy FETCH for both paths, so the
  general binder had nothing to add. A sigilless parameter already binds the caller's container
  on the light path. `sub identity(\value) is raw { value }` went from ~5 µs to ~1.7 µs per call.
  `is rw` routines still take the full path.
- **`&[==]` looks through a scalar container for its Int/Num fast path.** An `is raw` routine's
  result arrives in its container, and `==` used to send every such pair through the junction and
  numeric-bridge machinery (~4k instructions) to compare two Ints.
- **A spawn skips its IO-handle walk when there is nothing to find.** Every fork descended three
  object levels into every env value, looking for handles the worker must be handed a clone of.
  When every open handle is already among the ones the built-in dynamics (`$*OUT`, `$*ERR`,
  `$*IN`) reference, the walk can add nothing. That is the usual case, and it saves ~80k
  instructions per cued batch on ParaSeq's batcher thread.
- **A cue boxes its forked interpreter immediately**, so moving it into the pool's task closures
  copies a pointer instead of the whole `Interpreter` struct.

ParaSeq's `t/50-squish` went from 22.5 s to 17.6 s. Most of what is left is per-batch: the fork
of every cued task (`spawn_clone`, ~260k instructions on the batcher thread) and the
consumer-side bookkeeping, tracked on #17337.
