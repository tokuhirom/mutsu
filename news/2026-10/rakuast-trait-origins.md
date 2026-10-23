# RakuAST traits carry their origin

`.AST` now gives the traits of a routine (`returns Int`, `of Str`,
`is export`, `is DEPRECATED`) and of a parameter (`is copy`, `is raw`) the
source positions Rakudo does: the `Trait::*` node covers the whole trait, and
the name or type it names (the `Name` of a `Trait::Is`, the type of a
`Trait::Returns` / `Trait::Of`) covers just that word.

The parser records them as it reads the traits: a routine's in the
`RoutineSpans::traits` list, one per trait the `.AST` parse keeps as a source
record (`ast::routine_trait`), and a parameter's in `ParamSpans::traits`, one
per `ParamDef::traits` entry.
