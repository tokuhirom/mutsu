# A `sub` in a `supply` block under `EVAL(.AST)` is callable from a whenever callback

A block-scoped `sub` declared inside a `supply` block is kept callable after the block exits
through an escape-hatch copy. That copy was skipped for anything running inside an `EVAL`, so under
`EVAL($ast)` (the whole program is one EVAL unit) a `whenever` callback on another thread died with
`Unknown function`. Only the EVAL unit's own top level now skips the copy; nested blocks and
routines behave as outside an `EVAL` (#16198).
