# A slang's package declarator ends with the block that `use`d it

A package declarator a slang adds from its `EXPORT` (EEL 0.3's `event`,
#16888) used to stay registered for the rest of the compilation unit, so
`{ use EEL; event E { } }; event F { }` compiled where rakudo rejects the
second `event`, and a class could no longer be named after the keyword. The
parser now records each declarator a `use` introduces on the lexical scope
holding it and removes it when that scope ends, as rakudo's lexically scoped
`$*LANG` does; a keyword an outer scope already had stays with that scope.
