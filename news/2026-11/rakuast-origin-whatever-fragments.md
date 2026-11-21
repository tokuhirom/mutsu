# RakuAST: origins of a curry with no position of its own

A curry such as `* + *` is turned into a closure by the parser, which keeps
no position for it, so `.origin` was missing on it wherever no surrounding
statement or parentheses gave it a range: `my $f = * + *`, `say(* + *)`,
`say 1, * + *`, `my @a = * + *, 2` (#15780). The text of such an initializer,
call argument or comma-list operand is now parsed again on its own and its
ranges copied onto the curry (exact or absent), and the initializer runs
from its `=` / `:=` to the end of it. A primed `*` that starts or ends a
list-associative application (`* max *`, `1 min *`) is found in the source
too, so its operator is placed.
