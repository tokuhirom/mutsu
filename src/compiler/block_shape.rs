//! The single classification of a `{ ... }` block body's shape.
//!
//! A bare block is compiled by two passes — the `Stmt::Block` arm of
//! [`Compiler::compile_stmt`](crate::compiler::Compiler::compile_stmt) for
//! statement position, and
//! [`Compiler::compile_do_block_expr`](crate::compiler::Compiler::compile_do_block_expr)
//! for value position. They emit different opcodes for the scope itself
//! (`BlockScope` vs `DoBlockExpr`, see `docs/adr/0076-bare-block-keeps-two-opcodes-one-shape.md`
//! for why that stays), but the question "*what kind* of block is this" has one
//! answer, and each pass used to compute it separately — in a different order,
//! with a different set of cases. That is how the value form ended up with no
//! `let`/`temp` handling at all, and with its per-execution `state` reset
//! computed *after* the CATCH/phaser early-returns rather than before them
//! (`news/2026-09/bare-block-shape-classifier.md`).
//!
//! Both passes now dispatch on [`BlockShape`], so a body that is an implicit
//! `try` in one position is an implicit `try` in the other by construction.

use super::*;

/// What a `{ ... }` body needs wrapped around it, independent of whether the
/// block appears in statement or value position.
///
/// The variant order is the dispatch order, and it is significant: a body with
/// both a `CATCH` and a `use` is an implicit `try` (the import scope is not
/// applied), because that is what both passes have always done.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum BlockShape {
    /// Contains `CATCH`/`CONTROL`: the block is an implicit `try`.
    ImplicitTry,
    /// Contains `ENTER`/`LEAVE`/`KEEP`/`UNDO`: needs a real phaser block scope.
    PhaserScope,
    /// Contains `let`/`temp`: needs `LetBlock` save/restore.
    LetBlock,
    /// Contains `use`: imports are lexical to this block.
    ImportScope,
    /// Nothing special.
    Plain,
}

impl Compiler {
    /// Classify a block body. See [`BlockShape`] for why the order matters.
    pub(super) fn classify_block_shape(stmts: &[Stmt]) -> BlockShape {
        if Self::has_catch_or_control(stmts) {
            BlockShape::ImplicitTry
        } else if Self::has_block_enter_leave_phasers(stmts) {
            BlockShape::PhaserScope
        } else if Self::has_let_deep(stmts) {
            BlockShape::LetBlock
        } else if Self::has_use_stmt(stmts) {
            BlockShape::ImportScope
        } else {
            BlockShape::Plain
        }
    }
}
