# A typed array argument no longer constrains the caller's same-named variable

Calling `sub ins(@a, $v)` with a typed array (`my Int @typed`) through the
positional light bind registered the parameter's element type before the
parameter was in scope, so the registration tagged whatever the caller's frame
bound as `@a`. When that was a custom `Positional` instance (`my @a is P`),
whose tag lives in a process-wide side table, every later `@a.push` in the
caller was checked against `Int`. Concurrent::PriorityQueue's `t/01-basic`
died this way at test 5, after Array::Sorted::Util's `insert(@a, $pos, \value)`
ran on a role's `Int @!keys`.

The light bind now registers the constraint without tagging: the bound
container already carries it.
