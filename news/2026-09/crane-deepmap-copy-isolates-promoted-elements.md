# Crane deep copies isolate elements promoted by rw accessors

`deepmap({ .clone })` now descends through a `ContainerRef` when that cell holds
an aggregate. Previously the cell was mistaken for a scalar leaf, so cloning it
copied only the cell handle and left the aggregate shared with the source.

The rw writeback path also resolves an argument's sigilless alias chain to its
root and keeps method-parameter alias metadata local to the method frame. This
prevents a nested static rw method from redirecting a caller's alias to the
copied container's immediate parameter name.

Pinned by `t/deepmap-containerref-copy.t`; the test covers both promoted
aggregate cloning and a nested static rw method, and matches `raku`.
