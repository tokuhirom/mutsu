# A statement modifier after `once`, `quietly`, `eager` & co. is inside the prefix

A statement prefix takes a statement, so a modifier written after it on the
same line belongs to that statement: `once say 1 if $c` is `once (say 1 if $c)`.
mutsu did this for `try`, `do`, `gather` and `start` but attached the modifier
to the enclosing statement for `once`, `quietly`, `sink`, `eager`, `lazy`,
`hyper` and `race`. That changed answers: in `for 0, 1, 1 { once say $_ if $_ }`
rakudo's `once` is spent on the first iteration and prints nothing, while
mutsu printed `1`; `once say 1 for 1, 2` looped outside the phaser; and
`$x = quietly 3 if 0` skipped the assignment instead of assigning `Empty`. The
parser now folds the modifier into the prefix's statement, and the RakuAST view
shows it there (`StatementPrefix::Quietly(Statement::Expression(...,
condition-modifier => ...))`), with `.DEPARSE` and `EVAL` round-tripping
(#18510).
