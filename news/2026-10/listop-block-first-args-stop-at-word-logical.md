# `name { ... }, args and more` ends the listop's arguments at the word-logical

The arguments after a leading block argument (`lives-ok { ... }, "name" and is $x, 1`) were parsed as full expressions, so the loose `and`/`or`/`xor` was swallowed into the last argument: `is` ran before the block did and saw an unset `$x`. They are parsed at list-prefix precedence now, like every other listop's arguments. CSS::Writer's `t/node-doco.t` (116 assertions) depends on it.
