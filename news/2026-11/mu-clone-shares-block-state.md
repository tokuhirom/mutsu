# `Mu.clone` of a Block shares its `state` scope

`Mu.^lookup('clone')($block)` now runs Mu's own `clone` row instead of the
invocant class's, and the copy it makes is a distinct code object that keeps
the original's `state` variables (`0`, `1`, `2` as on Rakudo). The ordinary
`Block.clone` still starts independent state, and now reports its own scope's
disposal. (#15897)
