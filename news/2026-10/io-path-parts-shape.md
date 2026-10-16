# `IO::Path::Parts` answers through method-table rows

`IO::Path::Parts` (what `IO::Path.parts` and `IO::Spec.split` return) is now a
receiver kind of the method table (ADR-11276 §9.114): its `AT-POS`, `AT-KEY` and
`flat` are rows, and its other list methods are `Any`'s, which read it as one
item, as Rakudo does. `.Slip`, `.cache` and `.eager` now answer the parts object
itself instead of its three pairs, and `.hash`, `.Hash` and `.Map` read the parts
by name.
