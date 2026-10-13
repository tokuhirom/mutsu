# RakuAST keeps the import list of `require`

`require Test <&plan &is>` was refused by the RakuAST conversion and rendered
a bare `Str` when the list was dropped. The list is now the
`Statement::Require` `argument` (a `words` `QuotedString`, as in Rakudo
2026.09) and lowers back to the call's second operand. An `EVAL` string run
from inside a module also treats every loaded module's package as a known
global name. ADR-10723 section 6.52.
