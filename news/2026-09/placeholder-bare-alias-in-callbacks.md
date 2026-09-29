# A bare `$x` after `$^x` names the placeholder in map/grep/`...` callbacks

In `{ $^x + $x }` the bare `$x` is the placeholder parameter. When the block ran as a
`.map`/`.grep` callback or a `...` generator, mutsu resolved `$x` by name at run time and read the
caller's `$x` (or `Any`) instead. The fresh compilers behind those paths now know which placeholders
the caller has already bound, and the compiler resolves a bare name that matches a bound `^name` to
that placeholder. Fixes #9964.
