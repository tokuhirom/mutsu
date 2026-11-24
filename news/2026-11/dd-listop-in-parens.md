# `(dd 1, 2)` passes both arguments to `dd`

`dd` is a list operator like `note`: with no parentheses of its own, it takes
the whole comma list that follows. The parser's list-operator table had no
`dd`, so inside parentheses `(dd 1, 2)` parsed as `(dd(1), 2)`. `dd` printed
only `1`, and the list gained a stray element (#19082). A statement-level
`dd 1, 2` was already right. `dd` is now in the table next to `note`.
