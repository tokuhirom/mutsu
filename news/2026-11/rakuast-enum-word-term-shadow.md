RakuAST unit lowering now recognizes every word in a word-quoted enum term as
a declared name. Enum members such as `e`, `pi`, and `tau` correctly shadow the
setting's math terms when the unit is evaluated through its AST.
