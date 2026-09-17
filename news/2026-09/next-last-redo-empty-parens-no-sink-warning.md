# `next()`/`last()`/`redo()` no longer trigger a spurious sink-context warning

`next()`, `last()`, and `redo()` — the explicit empty-argument-list call form of the loop-control
statements — used to make mutsu print a false-positive "Useless use of () in sink context" warning,
even though the program's output was correct on both sides.

The root cause was in the parser, not the sink-warning pass itself: `next_stmt`/`last_stmt`/`redo_stmt`
consumed the `next`/`last`/`redo` keyword (and an optional label) but left the trailing `()` entirely
unconsumed. The statement-list parser then re-entered at the leftover `();`, parsed it as a completely
separate statement — a bare empty-list literal (`Expr::ArrayLiteral([])`) — and appended it right after
the `Stmt::Next`/`Stmt::Last`/`Stmt::Redo` node. The sink-context warning pass walks every statement in a
block statically, so it flagged that phantom statement even though `next()`'s control transfer means it
never actually executes.

Found while investigating `mzef install`: the vendored zef's `Zef::Client.rakumod` writes `next();` in
sink position inside a `for` loop, so the warning fired on every `mzef` invocation that went through that
code path.

Fixed by having `next_stmt`/`last_stmt`/`redo_stmt` consume a literal empty argument list (`()`, optionally
with inner whitespace) right after the keyword/label, the same way `return_stmt` already special-cases
`return()`. A non-empty parenthesized form (e.g. `last(LABEL)`, a real call with a `Label` argument) is
left untouched.

Regression coverage: `t/control/sink-warning.t` gained cases for `next()`, `last()`, and `redo()` in sink
position inside a `for` loop, asserting both the correct output and the absence of the warning.

Closes #8610.
