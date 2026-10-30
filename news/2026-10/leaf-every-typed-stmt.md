# The executing round trip carries every typed statement as a leaf

ADR-15334 slice 9, first step. `lower_parsed_stmt` lowers every `ParsedStmt` from its typed
struct, so the purity test that decides what crosses the executing round trip as a
`ParsedLeaf` no longer lists the variants: any `Stmt::Ast` is carried and lowered where it
stands (a modifier lowers the signature declaration it governs itself, and a routine's written
traits are still read by the routine's own conversion).

The node scans (`shadowed_terms`, `declared_routines`) used to walk the converted tree four
times and could not see inside a leaf. They are now one walk that asks a leaf for the scope
facts it declares (sigilless terms, packages, routines). A converted tree also skips the
subclass-normalizing walk, which only a hand-built tree needs.

Frontend Ir against the frozen baseline (profiling build, callgrind): zef-cli 117.3% -> 116.1%,
s12-class-attributes 115.1% -> 114.2%, grammar-cursor-attributes 120.3% -> 118.9%; the other two
cases moved by under a point. #15657 stays open.
