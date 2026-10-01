//! Lexical pragmas declared ahead of a lifted BEGIN (ADR-0134, slice 2;
//! #10472).
//!
//! A pragma in an inner scope (`sub f { use strict; BEGIN ... }`) is lexical:
//! it applies to the rest of its scope, the BEGIN included. The lifted body's
//! block for that scope repeats it, as it repeats the scope's imports
//! ([`super::decls`]). Whether repeating one is sound depends on how mutsu
//! applies it, so each pragma is classified here:
//!
//! - **Scoped to the block** ([`Repeat::Anywhere`]). `strict` and `newline`
//!   set interpreter modes that the block's `ImportScope` region saves and
//!   restores, and the others are no-ops in mutsu. Repeating one cannot leak
//!   past the lifted block, and it affects nothing but the code that runs in
//!   it.
//! - **Applied to what is compiled after it.** `use fatal` sets a mode the
//!   region restores too, but it also marks a later routine as declared under
//!   it ([`Repeat::BeforeRoutines`]). `use variables` changes the constraint
//!   of a later typed declaration, and `use dynamic-scope` makes a later
//!   declaration dynamic ([`Repeat::BeforeDeclarations`]). The compiler
//!   restores each on block exit. The lifted block puts its repeated pragmas
//!   ahead of the copied declarations, though, so a copy would come under a
//!   pragma that its original precedes. A [`Guard`] records what the scope
//!   held at the pragma, and a BEGIN that would copy any of it is not lifted.
//! - **Anything else** keeps the scope blocked. `use lib` and `use if` act
//!   beyond the block; `use attributes` is not restored on block exit; and a
//!   pragma mutsu does not implement (`use worries`, `use trace`) fails at
//!   run time, which repeating it would move to startup.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr};

/// Where a lifted body's block may repeat a pragma.
pub(super) enum Repeat {
    /// It only affects the code that runs inside the block.
    Anywhere,
    /// It affects the routines and types compiled after it, so no copied one
    /// may precede it in its scope.
    BeforeRoutines,
    /// It affects every declaration compiled after it, so no copied variable,
    /// routine or type may precede it in its scope.
    BeforeDeclarations,
}

/// The declarations of a scope that precede a repeated [`Repeat::BeforeRoutines`]
/// or [`Repeat::BeforeDeclarations`] pragma: the first `bindings`, `routines`
/// and `types` of its frame. A lifted body's block must not copy any of them.
pub(super) struct Guard {
    /// `None` when the pragma does not affect variable declarations.
    pub(super) bindings: Option<usize>,
    pub(super) routines: usize,
    pub(super) types: usize,
}

/// How a lifted body's block may repeat `stmt`, if it is a pragma that can be
/// repeated at all. `None` for any other statement.
pub(super) fn repeat_of(stmt: &Stmt) -> Option<Repeat> {
    match stmt {
        Stmt::Use {
            module,
            arg,
            condition: None,
            ..
        } if arg.as_ref().is_none_or(is_literal_arg) => match module.as_str() {
            "strict" | "newline" | "soft" | "nqp" | "isms" | "v6" | "oo" | "class"
            | "experimental" | "customtrait" | "warnings" => Some(Repeat::Anywhere),
            "fatal" => Some(Repeat::BeforeRoutines),
            "variables" | "dynamic-scope" => Some(Repeat::BeforeDeclarations),
            _ => None,
        },
        // `no` only switches off a mode `use` switched on (`strict`, `fatal`),
        // which the block restores, or is a no-op.
        Stmt::No { module, arg: None } => matches!(
            module.as_str(),
            "strict" | "fatal" | "isms" | "worries" | "precompilation" | "soft"
        )
        .then_some(Repeat::Anywhere),
        _ => None,
    }
}

/// Whether `stmt` is a lexical pragma: a `use` or `no` of a lowercase name.
pub(super) fn is_pragma(stmt: &Stmt) -> bool {
    let (Stmt::Use { module, .. } | Stmt::No { module, .. } | Stmt::Need { module }) = stmt else {
        return false;
    };
    module.starts_with(|c: char| c.is_ascii_lowercase())
}

/// A pragma argument the repeat can compile anywhere: it reads no variable
/// (`:crlf`, `:D`, `<$x>`, `6.d`). It is built from literals, pairs and lists.
fn is_literal_arg(arg: &Expr) -> bool {
    let mut check = LiteralArg(true);
    check.visit_expr(arg);
    check.0
}

/// Whether every expression it visits is a literal, a pair or a list of them.
struct LiteralArg(bool);

impl Visit for LiteralArg {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(_) | Expr::Binary { .. } | Expr::ArrayLiteral(_) => walk_expr(self, expr),
            _ => self.0 = false,
        }
    }
}
