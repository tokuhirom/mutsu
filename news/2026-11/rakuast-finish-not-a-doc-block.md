# RakuAST: the text after `=finish` is not a Pod block

`.AST` of a source with a `=finish` line used to end with a `RakuAST::Doc::Block` (`type => "finish"`)
holding the rest of the file; rakudo keeps that text as `$=finish` (here `CompUnit.finish-content`) and
leaves it out of the statement list. It is left out now, and a `=finish` on the very first line is split
off like any other (the unit has no statements and no code after it runs). Nineteen other Pod forms
were measured against rakudo 2026.09 and already match (#14880).
