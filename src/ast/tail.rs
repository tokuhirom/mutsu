//! "The last value statement of a block" -- the statement whose value a
//! block, a routine body or the program evaluates to -- shared by every
//! analysis that asks (#10468). Before this helper nine call sites each wrote
//! the backwards scan with one of four different skip rules.

use super::{PhaserKind, Stmt};
use std::borrow::Borrow;

/// Which non-value statements [`last_value_stmt`] looks past.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TailSkip {
    /// The statements that are never a value: `SetLine` line markers and the
    /// compile-time binding markers a declaration lowering appends after its
    /// `VarDecl` (`my \x = 1` is `SyntheticBlock[VarDecl, MarkSigillessReadonly]`,
    /// whose value is the declaration's). A trailing phaser IS the last
    /// statement, as in rakudo (`sub f { 42; ENTER { 7 } }` returns 7 and
    /// `sub g { 42; LEAVE { } }` returns Nil).
    Markers,
    /// [`TailSkip::Markers`] and phasers too: for the program's top-level
    /// statement list, where `run()` re-splices a `POST` phaser to the very
    /// end regardless of where it was written, so a trailing phaser says
    /// nothing about the source's real last statement.
    MarkersAndPhasers,
}

impl TailSkip {
    // Cost: O(1).
    fn skips(self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::SetLine(_)
            | Stmt::MarkBind
            | Stmt::MarkBoundContainer(_)
            | Stmt::MarkReadonly(..)
            | Stmt::MarkSigilless(_)
            | Stmt::MarkSigillessReadonly(_) => true,
            Stmt::Phaser { .. } => self == TailSkip::MarkersAndPhasers,
            _ => false,
        }
    }
}

/// The index of the last statement of `stmts` that `skip` does not look past.
/// Generic over the element so a compiler's filtered `Vec<&Stmt>` view of a
/// body is scanned by the same rule as the body itself.
// Cost: O(k), k = trailing statements skipped.
pub(crate) fn last_value_stmt_index<S: Borrow<Stmt>>(stmts: &[S], skip: TailSkip) -> Option<usize> {
    stmts.iter().rposition(|s| !skip.skips(s.borrow()))
}

/// Whether `stmt` is a phaser a block compiler lifts out of the statement
/// flow (`LEAVE`, `KEEP`, `UNDO`, `PRE`, `POST`) -- when it is a block's last
/// value statement the block evaluates to `Nil`, as in rakudo
/// (`do { 42; LEAVE { } }` is `Nil`, and a trailing `KEEP` therefore leaves
/// `UNDO` to run). A trailing `ENTER` is not one: its value is the block's.
// Cost: O(1).
pub(crate) fn is_nil_valued_tail_phaser(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Phaser {
            kind: PhaserKind::Leave
                | PhaserKind::Keep
                | PhaserKind::Undo
                | PhaserKind::Pre
                | PhaserKind::Post,
            ..
        }
    )
}

/// The last statement of `stmts` that `skip` does not look past -- the
/// statement whose value the block evaluates to.
// Cost: O(k), k = trailing statements skipped.
pub(crate) fn last_value_stmt(stmts: &[Stmt], skip: TailSkip) -> Option<&Stmt> {
    last_value_stmt_index(stmts, skip).map(|i| &stmts[i])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;

    fn phaser() -> Stmt {
        Stmt::Phaser {
            kind: PhaserKind::Leave,
            body: Vec::new(),
            condition: None,
            end_index: None,
        }
    }

    #[test]
    fn markers_are_never_the_value() {
        let decl = Stmt::Expr(Expr::Var("x".into()));
        let stmts = vec![
            Stmt::SetLine(1),
            decl.clone(),
            Stmt::MarkSigillessReadonly("x".into()),
            Stmt::SetLine(2),
        ];
        assert_eq!(last_value_stmt_index(&stmts, TailSkip::Markers), Some(1));
        assert!(last_value_stmt(&[Stmt::SetLine(1)], TailSkip::Markers).is_none());
    }

    #[test]
    fn a_trailing_phaser_is_skipped_only_on_request() {
        let stmts = vec![Stmt::Expr(Expr::Var("x".into())), phaser()];
        assert_eq!(last_value_stmt_index(&stmts, TailSkip::Markers), Some(1));
        assert_eq!(
            last_value_stmt_index(&stmts, TailSkip::MarkersAndPhasers),
            Some(0)
        );
        assert!(is_nil_valued_tail_phaser(&stmts[1]));
    }
}
