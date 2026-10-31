# The `substr-rw` / `subbuf-rw` Proxy aliases its source and runs as bytecode

The Proxy `substr-rw($s, ...)`, `$s.substr-rw(...)` and `subbuf-rw(...)` hand back outside an
assignment used two tree-walk closures that read and wrote the source variable by name from the
caller's live scope, so every FETCH and STORE recompiled its AST through the `call_sub_value`
carrier. Through the VM the Proxy now captures the variable's shared cell (the one an `is rw`
parameter binds), the window start and length ride in the captured env, and both closures are
shared compiled bodies (`synthesized_closure_code`). A `substr-rw` reached from an interpreter
entry with no VM chunk keeps the by-name closures (#14572).
