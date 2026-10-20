# Declaration feeds preserve their AST and append through native methods

Declaration feeds in either direction retain their `ApplyListInfix` / `Feed`
operands through conversion and lowering, including scalar, array and hash
declarations, callable chains and statement modifiers. Hand-built
`ApplyListInfix` nodes accept their operand list and evaluate through the same
compiler path.

Variable sinks now append to their existing contents and return the original
source, so a later sink receives the new feed rather than the earlier sink's
old contents. Scalar-held aggregates are iterated for the append while the
expression result keeps its original value. Compiler temporaries evaluate the
source once; native `.append` handles autovivification, element constraints,
container identity and immutable sinks. The obsolete feed-assignment runtime
helper is removed, and callable/legacy append-feed expansion moves out of the
parser into the compiler.

Bare sink declarations under modifiers are installed outside the modifier;
only the append repeats or depends on its condition. This preserves typed
declarations under false conditions and accumulates loop input. Native array
append/prepend also reject a single infinite range with `X::Cannot::Lazy`.

Measured against Rakudo 2026.07; append feed syntax (`==>>`, `<<==`) remains
outside Rakudo's implemented RakuAST contract. See #14096 and ADR-10723.
