# RakuAST declarations keep their `WHY` documentation

`RakuAST::Type::Subset.new(name => ..., WHY => $doc)` (and `Sub`, `Method`, `Submethod`, `Class`,
`Role`, `Package`, `Grammar`, `Block`, `Parameter`, ... every class that does
`RakuAST::Doc::DeclaratorTarget`) accepted `WHY` and dropped it. The `Doc::Declarator` is now kept in
a hidden field: `.WHY` answers it (the `Doc::Declarator` type object when there is none), `set-WHY`
replaces it, `.raku` prints its lines as rakudo's `.declarator-docs(leading => ..., trailing => ...)`
call, and a `WHY` that is not a `Doc::Declarator` is refused with the bind's type-check message.
The `Type::Subset` entry of the accepted-and-dropped list is gone (#16773).

Not done here: `.WHY` of a *parsed* declaration (`#| doc` before `sub f {}`) still answers undefined,
and `set-WHY` does not point the documentation's `WHEREFORE` back at the declaration.
