# `use`d modules run as their RakuAST round trip

Every compilation unit now goes `parse -> RakuAST -> lower`: the main program, `EVAL` strings and, since
this change, every `use`d or `require`d module. The `Unit` kinds and the parser's second, unspelled
entry point for executing a unit are gone. Making `Test`, `JSON::Fast`, Cro and the roast packages
round-trip needed three more fixes: a type named `Pkg::D` or `Pkg::U` is not the `:D` / `:U` smiley, the
operand of `require Module` stays a module name when an imported term made it an `ImportedTerm`
bareword (#15074), and the four gaps listed in the previous entry.
