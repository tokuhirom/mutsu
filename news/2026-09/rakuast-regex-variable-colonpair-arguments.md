# RakuAST regex arguments retain variable colonpairs

Constructed RakuAST regexes now preserve variable colonpair arguments such as
`:$expected` through the existing match-time subrule argument path, including
lexical value reassignment and sigil-specific AST shapes.
