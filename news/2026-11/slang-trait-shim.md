# A slang token can call the core `<trait>` rule

A slang token that calls `<trait>` now gets the one trait the Rust parser reads at that point as a
`RakuAST::Trait::*` node (ADR-15762 §2.3, #13370). `parse_sub_traits` takes a trait-count limit for
this; the host grammar gained the `trait` shim beside `block`, `signature` and `statementlist`.
