# A sigilless term followed by ` | ` is the junction infix

`-> \g, \e { g | e }` died with "Unknown function: g": the compiler guessed
that any lowercase `bareword | bareword` was a listop call slipping a capture
(`g(|e)`), even when the left bareword named an in-scope sigilless parameter,
a `my \x` binding or a `constant`. The guess now skips words that name a
lexical term, so `g | e` builds `any(g, e)` as in Rakudo, while `foo |c` for a
(possibly post-declared) sub still slips its capture. This unblocks the test
helper every Iter::Able test file starts with (issue #9364).
