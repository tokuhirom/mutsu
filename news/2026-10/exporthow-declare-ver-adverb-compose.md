# A DECLARE class keeps its custom HOW when its name has `:ver`

`mydecl Versioned:ver<2.0.0> { ... }` kept its version metadata but skipped the
HOW attached by `EXPORTHOW::DECLARE`; the class lost the parent installed by
the HOW's `compose` hook (#13527). The `:ver` header wraps the class AST in a
synthetic block, so the declarator marker now attaches to the wrapped class
declaration itself. The custom compose hook and the version metadata both
survive.
