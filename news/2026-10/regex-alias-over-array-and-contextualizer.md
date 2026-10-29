# Regex `<x=@w>`, `<x=@(EXPR)>` and `<x=$(EXPR)>` aliases capture

An alias over an array variable or a contextualized expression fell through the
named-rule lookup and never matched. `<x=@w>` now reuses the `<@w>` alternation
under the alias, and `<x=@(EXPR)>` / `<x=$(EXPR)>` run like `<x={ EXPR }>`, whose
array result is now an alternation of its elements (so `<x={ @w }>` works too).
Found with MUGS::Games' `CommandParser` grammar (#15509).
