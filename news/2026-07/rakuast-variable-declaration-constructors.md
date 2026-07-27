# Construct plain RakuAST variable declarations

RakuAST Phase 4 now constructs plain variable declarations through
`RakuAST::VarDeclaration::Simple.new` and their assignment initializers through
`RakuAST::Initializer::Assign.new`. The model validates child node kinds, exposes the declaration
and initializer fields through accessors and introspection, and renders the same constructor form
as Rakudo.

Constructed declarations lower back to mutsu's internal AST through the existing RakuAST lowering
path, so a statement list assembled entirely from model nodes can declare, initialize, and read a
lexical variable under `EVAL`. The regression test is also run against Rakudo as the behavioral
oracle.
