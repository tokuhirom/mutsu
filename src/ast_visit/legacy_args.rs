//! Whether a routine body reads or writes the legacy argument variables
//! `@_` / `%_`.

use super::{NameKind, Visit, walk_stmts};
use crate::ast::Stmt;

/// Collects whether any `@_` (`Expr::ArrayVar("_")`) or `%_`
/// (`Expr::HashVar("_")`) read, or any sigiled `@_`/`%_` write target,
/// occurs anywhere in a subtree.
#[derive(Default)]
struct LegacyArgScan {
    positional: bool,
    named: bool,
    written: bool,
}

impl<'ast> Visit<'ast> for LegacyArgScan {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match (name, kind) {
            ("_", NameKind::ArrayVar) => self.positional = true,
            ("_", NameKind::HashVar) => self.named = true,
            // A write, `temp` or declaration names the variable with its sigil.
            (
                "@_" | "%_",
                NameKind::VarDecl | NameKind::AssignTarget | NameKind::TempTarget | NameKind::Param,
            ) => self.written = true,
            _ => {}
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
    let scan = scan(body);
    (scan.positional, scan.named)
}

/// Whether `body` assigns, `temp`s or declares a sigiled `@_` / `%_`
/// (a `name: "@_"` field: a variable declaration, an assignment target, a
/// `temp`/`let` target or a parameter), nested blocks and routines included.
// Cost: O(n), n = size of the body's subtree.
pub(crate) fn legacy_arg_writes(body: &[Stmt]) -> bool {
    scan(body).written
}

/// Whether `body` reads or writes `@_` / `%_` at all.
// Cost: O(n), n = size of the body's subtree.
pub(crate) fn legacy_arg_uses(body: &[Stmt]) -> bool {
    let scan = scan(body);
    scan.positional || scan.named || scan.written
}

fn scan(body: &[Stmt]) -> LegacyArgScan {
    let mut scan = LegacyArgScan::default();
    walk_stmts(&mut scan, body);
    scan
}

#[cfg(test)]
mod tests {
    use super::*;

    fn body(src: &str) -> Vec<Stmt> {
        crate::parser::parse_program(src).expect("parse").0
    }

    #[test]
    fn reads_are_found_in_nested_blocks() {
        assert_eq!(legacy_arg_reads(&body("if 1 { say @_[0] }")), (true, false));
        assert_eq!(
            legacy_arg_reads(&body("for 1 { say %_<k> }")),
            (false, true)
        );
        assert_eq!(legacy_arg_reads(&body("say $_; say 1")), (false, false));
    }

    #[test]
    fn a_string_spelling_is_not_a_use() {
        assert!(!legacy_arg_uses(&body(r#"say "@_ %_"; say <$_ @_ %_>"#)));
    }

    #[test]
    fn writes_need_a_sigiled_target() {
        assert!(legacy_arg_writes(&body("@_ = 1, 2")));
        assert!(legacy_arg_writes(&body("my %_ = a => 1")));
        assert!(legacy_arg_writes(&body("temp @_ = 3")));
        assert!(!legacy_arg_writes(&body("say @_")));
        assert!(legacy_arg_uses(&body("say @_")));
    }
}
