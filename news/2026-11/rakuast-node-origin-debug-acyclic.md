# A RakuAST node held as a literal no longer overflows the stack when hashed

A RakuAST node's origin carries its resolution and its memoized meta-object,
both of which point back into the tree (a lexical lookup resolved to its own
declaration). The derived `Debug` rendered them, and a literal holding a node is
hashed through its `Debug` form when routine fingerprints are built, so the
walk went round the cycle until the stack overflowed (SIGSEGV). EEL's CLI hit
it on every `--eval` program (`"use EEL; event Bla { ... }".AST`).

The origin now renders only which of its members are set, as its equality
already ignores them.
