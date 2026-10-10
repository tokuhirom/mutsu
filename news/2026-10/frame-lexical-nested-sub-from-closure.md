# A sub with a nested sub now works when called from a closure

A `my sub` that declares its own inner `sub` died with "frame-lexical routine has no
compiled body" when called from inside a closure such as `$lock.protect: { ... }`,
because the call resolved the inner declaration against the closure's function table.
The call now uses the callee's own table. This takes P5pack's t/02-t/05 and spec-pack
files from dying to passing.
