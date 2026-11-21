# `$?LINE` in an anonymous token's code block keeps its RakuAST spelling

`Q[my $t = token { a { @seen.push($?LINE) } }].AST.DEPARSE` read back `@seen.push(1)`: an
anonymous `token`/`regex`/`rule` term folds `$?LINE` in its code blocks to the line number before
the body is stored for matching (#16603), and its RakuAST source tree was parsed from that folded
text, so the view showed the literal instead of `Var::Compiler::Line`. The source tree is now
parsed from the body as written, at its place in the unit (so its `$?LINE` still reads the right
line), and only the pattern used for matching is folded (#18291).
