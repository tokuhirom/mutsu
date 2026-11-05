# RakuAST: `=defn` and delimited `=formula` Pod blocks

`=defn` blocks (all three forms) and `=begin formula` / `=for formula` blocks
were not converted, so they were missing from `.AST`. A `defn` now starts with
its term, the first line as a plain string (never read for formatting codes),
followed by the paragraphs of its definition; a `formula` is an ordinary
paragraph block. An abbreviated `=formula` keeps only its first line in rakudo
and is still not converted, nor is `table` (a `Doc::LegacyRow` node of its
own); see #14880.
