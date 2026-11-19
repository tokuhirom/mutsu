# A lazy list stops being lazy once it is read to its end

Rakudo's `List.is-lazy` drops the list's unreified tail once a pull has
reached the end of its source, so a `lazy gather { ... }` that `.raku`,
an index past its end, or a `.head(5).eager` on its `.cache` has read whole
answers `.is-lazy` `False` from then on, and renders without the `.lazy`
suffix. mutsu answered from the list's static shape (the `lazy` marker, a
pipe over a source it could not prove finite) and stayed lazy forever.

A `LazyList` now records that a pull reached the end of its source: a full
force, or a window pull that produced fewer elements than it asked for. The
arms of `is_genuinely_lazy` that a finite source can satisfy (the `lazy`
marker and a lazy pipe) yield to that fact; an infinite sequence, closure
sequence, scan or `xx *` stays lazy (#17908).
