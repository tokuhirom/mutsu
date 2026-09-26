# Non-numeric Str arithmetic is a Failure; shared entry refs no longer corrupt GC counts

`"a" / "b"`, `1 - "b"` and the rest of the arithmetic operators (`+ - * / % **`,
`×`, `÷`, the `&infix:<+>` routine form and the `+=` metaop) used to throw
`X::Str::Numeric` the moment an operand failed to numify. Rakudo instead
evaluates the operation to a lazy `Failure` that only throws once it is sunk or
used, so a module whose mainline merely *stores* such a value loads fine. That
difference kept `WWW::HorizonsEphemerisSystem` from loading at all: its
property tables contain entries like `'AngularDegrees' / 'Seconds'`. The
arithmetic opcodes now return the same `X::Str::Numeric` Failure `.Numeric`
already produced.

Loading the module then surfaced a cycle-collector corruption: a deferred
hash-entry reference (`%h{$missing}` taken as a pair value inside a `.map`
closure) keeps its root hash's `Gc` handle in one shared `Arc` box, but the
tracer yielded that edge once per *holder* of the box. Once the pair value was
copied into a merged hash, trial deletion decremented the root hash's count
below zero (a debug-build panic at program end). `HashEntryRef` now joins the
other shared-box kinds behind the uniqueness gate, so only a sole holder claims
the edge.

WWW::HorizonsEphemerisSystem 0.0.2 moves from `blocked_load` to green.
