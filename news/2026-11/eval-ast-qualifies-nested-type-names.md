# EVAL($ast) qualifies nested type names

`EVAL($ast)` lowered a tree without the type-qualification pass an ordinary
parse runs, so `my Inner $i = Inner.new` inside `class Outer { class Inner {} }`
checked the value (`Outer::Inner`) against the bare name `Inner` and died with
a type-check failure. The lowered unit now goes through `qualify_type_names`
too. Four more `t/` files (and 394 files that already passed but were missing
from the list) join the `EVAL(slurp($file).AST)` ratchet (#16356).
