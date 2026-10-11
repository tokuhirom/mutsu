# An assignment inside a conditional binds a native `is rw` parameter

`f($c ?? $p !! $p = $p + 1)` parses as an assignment to the conditional
`($c ?? $p !! $p)`, and an assignment yields the container it wrote. When both
arms are native `is rw` parameters the compiler now runs the assignment inside
the chosen arm and passes that parameter's reference to the callee, instead of
handing it a bare value ("expects a writable container").

Found by working JSON::Fast off the ecosystem queue: its comment scanner
(`nom-comment`) uses exactly this shape, so `t/14-comments.t` (`:allow-jsonc`)
died. With the fix the file passes under mutsu like rakudo, and all 14 test
files match rakudo (t/01-parse.t's two failures also fail under rakudo here).
Pinned by `t/vm/binding/rw-arg-ternary-assign.t`.
