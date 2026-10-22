# Regex aliases to a variable cross the RakuAST boundary

`m/a $foo=[<two>] d/` aliases an atom to a plain variable rather than to a
capture. It was the last construct keeping `roast/S05-capture/alias.t` from
round-tripping under `MUTSU_RAKUAST=1`: the regex source tree had no node for
it, so the whole regex had no tree.

Rakudo 2026.09 parses the alias but throws the atom away. Its `.AST` is only
`RakuAST::Regex::Interpolation.new(sequential => False, var =>
RakuAST::Var::Lexical.new("$foo"))`, and it matches as if `=[<two>]` were
not written. mutsu now renders exactly that node, so a `.AST` consumer sees
rakudo's shape. The aliased atom travels in a hidden `source-alias` field that
the renderer leaves out, and `EVAL` of the tree puts the alias back. The
round-tripped regex therefore matches what the written one does.

What the written one should match is a separate question (#14628): mutsu's
matcher reads the `=` as a literal character, rakudo drops the atom, and S05
wants the match bound to `$foo`.
