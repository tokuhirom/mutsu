//! Whether a routine body reads the legacy argument variables `@_` / `%_`.

use super::{NameKind, Visit, walk_stmts};
use crate::ast::Stmt;

/// Collects whether any `@_` (`Expr::ArrayVar("_")`) or `%_`
/// (`Expr::HashVar("_")`) read occurs anywhere in a subtree.
#[derive(Default)]
struct LegacyArgReads {
    positional: bool,
    named: bool,
}

impl<'ast> Visit<'ast> for LegacyArgReads {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if name == "_" {
            match kind {
                NameKind::ArrayVar => self.positional = true,
                NameKind::HashVar => self.named = true,
                _ => {}
            }
        }
    }
}

/// `(reads @_, reads %_)` for `body`, nested blocks and routines included.
///
/// This used to be answered by rendering the whole body with `{:?}` and
/// searching the text for `ArrayVar("_")`, which allocated a string the size
/// of the subtree for every empty-signature routine a module declares.
// Cost: O(n), n = size of the body's subtree.
pub(crate) fn legacy_arg_reads(body: &[Stmt]) -> (bool, bool) {
    let mut scan = LegacyArgReads::default();
    walk_stmts(&mut scan, body);
    (scan.positional, scan.named)
}
