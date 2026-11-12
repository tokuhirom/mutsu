# CATCH runs inside its block's LEAVE scope, and returns from the right routine

A block (or routine body) with both a `CATCH`/`CONTROL` handler and
`ENTER`/`LEAVE`-family phasers compiled the handler region *around* the phaser
scope. An exception unwinding out of the body therefore left the scope first —
running `LEAVE` and restoring the block's locals — and only then reached the
handler, which read the block's `my` variables as `Nil`, and ran after the
`LEAVE` instead of before it. The handler region now nests inside the phaser
scope, as in Rakudo.

Two related `return`-from-`CATCH` fixes: a handler installed directly in a
routine body now targets that routine even when the routine was entered through
a light call path that does not bind its callable id in the env, and the fast
call path no longer swallows a `return` stamped for another routine (the
handler of a deep throw ran at the thrower and its `return` stopped there).

Together these made EEL's CLI exit 0 on every configuration error
(`t/12-cli.t` #30, #45).
