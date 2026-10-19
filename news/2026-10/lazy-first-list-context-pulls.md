# `first` and a gather's `list` are pulls of the lazy step

On a lazy sequence, `first` stops pulling at the element that matches. `list` and `List` of a live
`gather` hand back the same pullable sequence, reporting `List`, so a `for` over it sees the
elements already taken before a later `die`.

Each of the VM's two method lanes used to keep its own copy of both behaviours. The interpreter's
dispatch, which a call through a method object reaches, had neither: it reified the receiver first.
Both are now pulls of the lazy step (ADR-13891, #11276), so every way of making the call answers
alike, and the VM's copies are gone.
