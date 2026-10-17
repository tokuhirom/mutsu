RakuAST round-trips zip assignment through Rakudo's
`ApplyListInfix(MetaInfix::Zip(Assignment(:item)))` shape. Ordinary assignments,
declaration assignments, and shaped native array initialization lower through
the existing zip assignment path. Regression coverage pins tail preservation,
fresh-array behavior, and row-major shaped filling.
