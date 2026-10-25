RakuAST: `is built(:bind)` and a `.=` declaration under a postfix call under `MUTSU_RAKUAST=1`

Two cases that failed only under the RakuAST frontend (found by the Stage 3 survey): the attribute trait `is built(:bind)`
lost its `:bind` argument in the conversion, so the lowered class no longer bound the named argument; and
`(my Int $e .= new(4)).^name` lowered the declaration, whose parentheses rakudo drops, as the statement-level
expansion, which answered a `List`. Both now match the ordinary frontend.

The sink-context warning analysis also reads a declaration with a tail (`my $x = 1, 123`) that a `.AST` parse keeps as a
`DeclTail` node, so `roast/S32-exceptions/misc.t` no longer loses its "Useless use" warning under `MUTSU_RAKUAST=1`.
