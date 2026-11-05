# RakuAST: verbatim Pod blocks

`=begin code`, `input`, `output` and `comment` blocks between the statements of a unit are
`RakuAST::Doc::Block` statements of `.AST`: a delimited one is a single verbatim string of every
line up to its `=end` (directives, indentation and inner blank lines kept, leading blank lines
dropped), `code`/`input`/`output` still read formatting codes and `comment` never does, and the
abbreviated and `=for` forms follow the paragraph rules (#14880).
