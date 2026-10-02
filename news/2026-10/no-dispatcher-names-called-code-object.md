# X::NoDispatcher names the innermost called code object

When a `{*}` finds no dispatcher in its call chain, the `X::NoDispatcher`
message now names what rakudo names: the innermost code object that was
actually called. A closure or pointy block called from a routine, and a `try`
block, are anonymous (` is not in the dynamic scope of a dispatcher`), while an
inlined `if` body, bare block or loop body is no frame of its own and the
enclosing routine's name is kept. `RoutineFrame` records the difference as
`is_inlined_block`, set by the frames of statement-level bare blocks (#10786).
