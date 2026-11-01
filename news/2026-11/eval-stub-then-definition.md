# An EVAL'd unit accepts a forward-declared sub and its definition

The redeclaration check of an `EVAL` unit treated `sub f {...}` followed by `sub f { ... }` as a redeclaration. A `{...}` stub
is now completed by the definition that follows it, as in the unit's source outside `EVAL`. Two definitions are still refused.
The `EVAL($ast)` ratchet pins the 11 more files this lets through (`ci/rakuast-ast-eval-passing.txt`).
