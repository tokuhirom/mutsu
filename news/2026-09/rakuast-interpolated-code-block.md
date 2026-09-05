# RakuAST interpolated code blocks

`"a{ $x }b"` now renders and lowers. It was a `.AST` boundary, and it is on the
campaign file's own lowering list ("code-block interpolation").

raku renders a `{ ... }` segment of an interpolated string as a plain
`RakuAST::Block`, alongside the `StrLiteral` runs. mutsu wraps the block in a
`DoStmt` — that is how its parser makes a block an expression — which has no
RakuAST counterpart, so the segment converter refused the whole string.
Unwrapping it in `interp_segment` is the entire read-side change; measured
against rakudo 2026.07, the rendered gists are byte-for-byte identical.

## The write direction needed the mirror

A `Block` *segment* is evaluated, not a closure value. Lowering it through the
ordinary expression path built a closure and interpolated its stringification,
so `"a{ $x }b"` came out as `ab` — silently losing the interpolated value rather
than erroring. The lowerer now spells a `Block` segment `DoStmt(Block)`, the
same shape the parser produces.

That divergence was found by running the same snippet twice — directly, and
through `EVAL(Q{...}.AST)` — against both mutsu and rakudo 2026.07. The read
direction alone looked complete.

## Coverage

`t/rakuast-interp-code-block.t` (10 assertions) pins the `Block` segment and the
absence of any `DoStmt` wrapper in the gist, the literal runs around it, that a
plain `$x` segment is still a `Var` and not a `Block`, and four `EVAL` round
trips including a string that is only a code block and a method call inside one.
It is a dual-oracle test: it passes verbatim under both mutsu and rakudo 2026.07.
