//! WhateverCode closure construction (ADR-0033).
//!
//! Historically mutsu built the `Lambda` / `AnonSubParams { is_whatever_code:
//! true, .. }` closure eagerly in the parser, at the same site that decided
//! *whether* to curry (`should_wrap_whatevercode` / `contains_whatever` in
//! `crate::parser::expr::whatever`). That destroyed the pre-curry expression
//! before any later consumer (RakuAST, `.DEPARSE`, error messages) could see
//! it.
//!
//! ADR-0033 splits this into two steps: the parser still decides the priming
//! *scope* (unchanged in Phase 1 — see `docs/adr/0033-whatever-priming-leaf-and-derived-scope.md`),
//! but instead of building the closure on the spot it wraps the un-curried
//! body in the marker node `Expr::WhateverCurry`. The actual closure
//! construction — everything that used to live in
//! `src/parser/expr/whatever_wrap.rs` / `whatever_replace.rs` — moves here,
//! parser-independent, and is invoked from exactly one place:
//! `Compiler::compile_expr`'s `Expr::WhateverCurry` arm. This is the module
//! `src/rakuast/lower.rs`'s future `plant()` re-deriver will also depend on
//! (Phase 3/4), which is why it lives at the crate root rather than under
//! `src/parser/`.
//!
//! Phase 1 (this module's initial shape) is a pure relocation: `build_closure`
//! is byte-for-byte the old `wrap_whatevercode`, and the parser's ~50 call
//! sites now construct `Expr::WhateverCurry` instead of calling it directly.
//! No runtime behaviour changes.

mod build;
pub(crate) mod mark;
mod replace;

pub(crate) use build::{build_closure, make_wc_param};
