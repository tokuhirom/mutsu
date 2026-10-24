# RakuAST: an infix keeps its Unicode spelling

`1 × 2`, `$a ≤ $b`, `1 … 3`, `$x ∈ $s`, `f ∘ g` and the other Unicode
spellings of an infix now stay in the tree as written (`Infix.new("×")`),
as on rakudo 2026.09. mutsu used to show the ASCII operator the parser maps
them to (`*`, `<=`, `...`, `(elem)`, `o`), which a deparse writes back
differently.

The spelling is read from the source once the application is placed between
its operands (`infix_origins` / `list_origins`), and only when the text there
is one of the operator's measured aliases: exact or absent. `EVAL` lowers the
spellings mutsu has no operator for (`−`, `≤`, `…`, `∘`, ...) to the ASCII
operator. `×`, `÷` and the set relations still dispatch by their own name.
Closes #14887's second half.
