# A lazy `gather do { ... }` whose block declares a sub resumes inside the block

A bounded pull of a lazy `gather` suspends its body at a `take` and resumes it on the next pull.
When the body was a `do { ... }` block that declares a routine (`gather do { .take for i($n);
sub i($m) { ... } }`), the block runs as its own `DoBlockExpr` op, which did not take part in
that suspension: the pull left the op as if the block had finished, so the rest of the block
never ran and its later `take`s were lost -- the routine's second call took nothing
(Math::Sequences, #18259). The `do` block op now parks a continuation like a `try` region or a
loop does, and on resume re-registers the block's hoisted routines and continues after the
`take`.
