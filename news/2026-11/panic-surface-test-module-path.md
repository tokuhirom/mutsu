# check-panic-surface: the opcode test module declares its `#[path]`

`src/opcode/nested_decl_names_tests.rs` is declared with the `#[cfg(test)] #[path = "..."]
mod` form the panic-surface ratchet exempts, so its `.expect` calls no longer count
against the production budget (#18276).
