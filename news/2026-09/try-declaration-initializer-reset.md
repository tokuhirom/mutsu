# Failed `try` declaration initializers start from a fresh binding

An ordinary `my` declaration whose initializer fails inside `try` now leaves
`Any`, as in Rakudo. Previously, an expression-position declaration in a loop
could retain the previous iteration's value, causing Env::File's missing secret
file path to populate `TEST_UNKNOWN` with the prior successful file contents.

The declaration opcode now resets the appropriate local or environment binding
before the initializer runs, while preserving `state`, `our`, and constant
bindings. The behavior is pinned by
`t/exceptions/try-declaration-initializer-reset.t` and Env::File 0.0.1 moves
from 5/7 to 7/7 assertions.
