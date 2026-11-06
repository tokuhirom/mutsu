# `nqp::while` / `nqp::until` with a post operand

`nqp::until(cond, body, post)` (QAST's third loop child, run after each iteration's body) was
not lowered, so ecosystem code using it (Iter::Able's `min-max`, `assign-at`, `remove-at`,
`split-at`) misbehaved: an empty input answered a list instead of a Failure. The sunk form now
compiles to a C-style loop whose step is the post operand (#16939).
