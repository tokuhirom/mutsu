RakuAST now round-trips signature-literal binding. The parser keeps the written
`:(...) := RHS` source form, while the existing signature binder still performs
type, arity, and named-argument checks. The right-hand side is evaluated once.
