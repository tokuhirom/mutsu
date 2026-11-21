# RakuAST: origins of a parenthesized element assignment called on

Rakudo drops the parentheses around an element assignment that a method is
called on (`(@a[0] = 7).say`, `(%h<b> = 7).raku`), and mutsu left the
assignment and the call without `.origin` (#15780). The assignment is now
placed from a fragment parse of the text inside the parentheses, and a
fragment copy also places the statements nested inside it (a subscript's
`Statement::Expression`), so the call after it is placed too.
