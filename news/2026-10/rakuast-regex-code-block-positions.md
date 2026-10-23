# RakuAST statements in regex code blocks carry the unit's positions

The body of a regex code block (`/ a { $c++ } b /`) is parsed from a copy of
its text, and the positions that parse recorded were offsets into that copy.
`.AST` read them as offsets into the unit, so a statement inside a code block
showed a range near the start of the file (`0..2`, `1..15`).

A parse of such a copy now records the unit's positions: the regex parser
passes the copy's unit offset (found from the source buffer, where it used to
be found only when the unit recorded `use trace` statement attempts), and the
copy's parse adds it to every position it records. A copy whose place in the
unit is not known records none.
