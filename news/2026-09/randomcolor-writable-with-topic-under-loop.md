# `with` can write its scalar topic inside a readonly loop

`raku-RandomColor` v0.12 died in its seeded-color path with “Cannot assign to
an immutable value”. Its `BUILD` routine assigns through `$_` inside a writable
`with $!seed` topicalizer nested in a `for ^count` loop.

The loop's implicit topic is an immutable range item, and mutsu let that
readonly mark leak into the nested `with`. Writable scalar `given` / `with`
topics now temporarily clear an inherited `$_` mark and restore it after the
topicalizer, including when the body exits with an error.

Pinned by `t/control/with-writable-topic-under-loop.t`. The distribution moves
from `red` (37/46 assertions before dying) to `green` (46/46), measured with
the release binary under the standard sandbox.
