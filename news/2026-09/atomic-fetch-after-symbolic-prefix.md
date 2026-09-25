# `+⚛$x` / `-⚛$x`: atomic fetch after another prefix operator

`⚛$x` parsed on its own, but not behind another symbolic prefix: `+⚛$x`,
`-⚛$x` and `~⚛$x` died with the generic "Confused. expected statement:
expected '.' or digits or generic radix literal or ..." dump. That blocked
FFmpegProgressBar from loading its own module (`return +⚛$!force-exit-code;`),
one of the parse gaps split out of the #7988 cluster (#9324).

The cause was in `parse_prefix_unary_op`: `+`, `-` and `~` are only taken as
prefix operators when what follows can start an operand — a term-start
character, a hyper marker, a user-declared prefix, or another prefix operator
that the same function recognises. The atomic-fetch prefix is not one of
those: `prefix_expr` parses `⚛` itself (it rewrites `⚛$x` into the atomic
fetch call), so the lookahead never saw it, and the leading `+`/`-` fell
through to numeric-literal parsing. `!` and `?` carry no lookahead, which is
why `!⚛$x` already worked.

The "another prefix follows" lookahead now also accepts `⚛`. The operand is
still parsed by `prefix_expr`, so `+⚛$!attr` reaches the same atomic fetch as
a bare `⚛$!attr`. Pinned by `t/lang/operators/atomic-fetch-after-prefix.t`.
