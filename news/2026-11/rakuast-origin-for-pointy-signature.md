# RakuAST: origins of a `for` loop's pointy signature and of `\x` parameters

The parameters of `for @a -> $x, $y { }` were parsed without recording where they were written,
so the loop's pointy block answered no `Signature` / `Parameter` / `Var` ranges; they are now
recorded like the parameters of every other pointy block. A pointy block with a sigilless
parameter (`-> \x { }`) also gets its ranges (the `Parameter` spans the backslash, the `Term`
and its `Name` the name alone), as in rakudo (#15780).
