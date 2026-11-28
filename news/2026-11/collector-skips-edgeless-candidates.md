# The cycle collector skips edgeless candidates

A loop that constructs objects whose attributes are all strings and numbers
(`Dog.new(name => "Rex", legs => 4)`) survivor-drops each new instance and each
named-argument `Pair` at least once, so every one was buffered as a cycle-root
candidate, and every few thousand constructions the buffer armed a cycle
collect that trial-deleted all of them and reclaimed nothing — about 940
instructions per construction in `gc_safepoint_armed_slow`.

- A `Pair` of edgeless values (`Trace::currently_acyclic`) is no longer
  buffered at its survivor-drop: the check is two tag tests.
- An instance is asked at collect time instead (the new
  `Trace::acyclic_at_collect`): a drained candidate whose attributes are all
  edgeless, with no RakuAST base and no pending lazy attributes, is left out
  of trial deletion. Asking it at every drop cost a reused instance ~2,000
  instructions per method call, because it lost the already-buffered one-load
  fast path; at collect time it costs nothing between collects.

Both rest on Bacon-Rajan's argument, already documented on
`currently_acyclic`: a node with no outgoing edge cannot be on a cycle now, and
one that gains an edge is offered again at its next survivor-drop.

`Dog.new(name => "Rex", legs => 4, breed => "Labrador")`: 17,370 → 16,302
instructions; bench-class's warm workload 388.6M → 375.1M (-3.5%; profiling
build, callgrind).
