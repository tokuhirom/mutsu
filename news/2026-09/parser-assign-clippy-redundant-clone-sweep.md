# Removed wasted `.clone()`/`.to_string()` calls in assignment-statement parsing

`clippy::redundant_clone` flagged 198 hits (99 unique call sites, doubled by
`--all-targets` compiling the lib and its test target separately) across 34
files under `src/parser/`, concentrated in the assignment-statement parsing
code (`src/parser/stmt/assign/try_assign.rs` alone accounted for 34 of them).
Each hit was the same shape: a value cloned (via `.clone()` or, for an
already-owned `String`, `.to_string()`) into a place that turned out to be
its last use, so the clone/allocation was pure waste.

Fixed by moving the original value instead of cloning it at each of the 99
call sites. A few of these moves left `field: field` struct-literal
initializers behind (once the value stopped needing a `.clone()` to satisfy
the borrow checker), which `clippy::redundant_field_names` in turn flagged
under `make lint`'s default configuration; those were collapsed to the
struct-update shorthand (`field,`).

No behavior change — this is Rust-internal code-quality cleanup, verified by
the existing `make test` suite and a clean `make lint` across all four
configurations `make lint` gates on.

Closes #8911.
