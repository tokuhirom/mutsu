RakuAST now preserves enum declarations across `.AST` and `.AST.EVAL`, including
word, quote-word, and pair-list enum forms. `RakuAST::Type::Enum` and enum
variant-term processors are supported for these declarations.
