The RakuAST frontend now preserves inline proto declarations passed to `use`
and the statement metadata needed by `use trace`. A proto remains an embedded
`Sub` node in the public import AST, and trace hooks retain their exact source
text, line and statement number through the round trip. All ten previously
divergent language tests now pass in frontend mode.
