//! The lean shape of a list-assigning declaration, `my ($a, $b) = EXPR`.
//!
//! The parser lowers such a declaration (`rakuast::signature_expand`) to a
//! synthetic block: the targets declared with their defaults, the staging
//! array `@__destructure_tmp__ = __mutsu_list_assign_rhs(EXPR)`, one
//! declaration per target reading its element, and finally the list of the
//! targets as the block's value. Two parts of that cost more than the work
//! they stand for:
//!
//! - the helper call went through the general by-name function dispatch
//!   (~11k instructions) to deitemize one value; a single positional argument
//!   now compiles to [`OpCode::ListAssignRhs`];
//! - in statement position the block's value (`($a, $b)`) was built and then
//!   sunk, which observes nothing: it is a List of variables just declared.
//!   [`Compiler::destructure_result_is_unobservable`] lets the sunk form skip
//!   it;
//! - the plain `$` targets are predeclared ahead of the right-hand side, so
//!   that the RHS sees the new variables (`my ($a, $b) = $b, $a` reads the
//!   new, still-undefined `$b`), and then declared again from their element.
//!   When the RHS names none of the targets, the predeclaration observes
//!   nothing: the statement form skips it
//!   ([`Compiler::destructure_elidable_predecls`]), and the second
//!   declaration is the only one, an ordinary declaring store.

use super::Compiler;
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{NameKind, Visit};
use crate::opcode::OpCode;
use crate::symbol::Symbol;

impl Compiler {
    /// Compile `__mutsu_list_assign_rhs(arg)` as `arg` followed by
    /// [`OpCode::ListAssignRhs`] when the call has one argument whose value
    /// the general call path would hand the helper unchanged: a call or a
    /// method call (not an accessor-shaped one, whose argument compile marks
    /// a container for `is rw` binding), or an array variable. Returns
    /// `false`, emitting nothing, for every other shape (a slip, a pair
    /// argument, an element or attribute read), which keep the call.
    // Cost: O(1).
    pub(super) fn try_compile_list_assign_rhs_op(&mut self, name: Symbol, args: &[Expr]) -> bool {
        let [arg] = args else {
            return false;
        };
        if !name.with_str(|n| n == "__mutsu_list_assign_rhs")
            || Self::is_named_arg_expr(arg)
            || Self::is_accessor_shaped_arg(arg)
        {
            return false;
        }
        let lean = match arg {
            Expr::Call { name, .. } => !name.with_str(|n| n.starts_with("__mutsu_")),
            Expr::MethodCall { .. } | Expr::ArrayVar(_) => true,
            _ => false,
        };
        if !lean {
            return false;
        }
        self.compile_expr(arg);
        // The call path turns a Pair argument into a positional one; the
        // helper then received (and returned) that positional pair.
        self.code.emit(OpCode::ContainerizePair);
        self.code.emit(OpCode::ListAssignRhs);
        true
    }

    /// Whether `stmt`, the last statement of a list-assigning declaration's
    /// synthetic block (`stmts`), is the block's result list made of plain
    /// reads of the declared variables, so that evaluating and sinking it is
    /// unobservable and the sunk form may omit it.
    // Cost: O(1) for any statement but a block's trailing list; O(n) for that
    // one, n = statements of the block plus the list's elements.
    pub(super) fn destructure_result_is_unobservable(stmts: &[Stmt], stmt: &Stmt) -> bool {
        // Cheapest first: the shape of `stmt`, then its position, then the
        // block's (a scan of its leading declarations).
        let Stmt::Expr(Expr::ArrayLiteral(items)) = stmt else {
            return false;
        };
        if !stmts.last().is_some_and(|last| std::ptr::eq(last, stmt))
            || !crate::ast::signature_decl::is_destructure_block(stmts)
        {
            return false;
        }
        items.iter().all(|item| match item {
            Expr::Var(name) => crate::value::attr_twigil_base(name).is_none(),
            Expr::ArrayVar(_) | Expr::HashVar(_) => true,
            _ => false,
        })
    }

    /// Which statements of `stmts`, the statement form of a list-assigning
    /// declaration's synthetic block, are predeclarations of plain `$`
    /// targets the statement form may skip: all of them when the right-hand
    /// side cannot name a target, none otherwise (an empty result). The real
    /// declaration of each target follows the staging temp, so skipping the
    /// predeclaration leaves exactly one declaring store per target.
    ///
    /// The RHS "cannot name a target" when no identifier in it is a target's
    /// name (closures in it included, so a capture is seen), it holds no
    /// source text parsed later, no symbolic or pseudo-package lookup, and no
    /// `EVAL`.
    // Cost: O(n), n = size of the block's statements.
    pub(super) fn destructure_elidable_predecls(stmts: &[Stmt]) -> Vec<bool> {
        let mut elide = vec![false; stmts.len()];
        if !crate::ast::signature_decl::is_destructure_block(stmts) {
            return elide;
        }
        let is_tmp = |s: &Stmt| {
            matches!(s, Stmt::VarDecl { name, .. } if name.contains("__destructure_tmp__"))
        };
        let Some(tmp_at) = stmts.iter().position(is_tmp) else {
            return elide;
        };
        let targets: Vec<&str> = stmts
            .iter()
            .filter_map(|s| match s {
                Stmt::VarDecl { name, .. } if !is_tmp(s) => {
                    Some(name.trim_start_matches(['$', '@', '%', '&']))
                }
                _ => None,
            })
            .collect();
        let Stmt::VarDecl { expr: rhs, .. } = &stmts[tmp_at] else {
            return elide;
        };
        let mut scan = NamesAnyTarget {
            targets: &targets,
            hit: false,
        };
        scan.visit_expr(rhs);
        if scan.hit {
            return elide;
        }
        for (i, stmt) in stmts[..tmp_at].iter().enumerate() {
            elide[i] = matches!(
                stmt,
                Stmt::VarDecl {
                    name,
                    expr: Expr::Literal(v),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    custom_traits,
                    bind: None,
                    constant: None,
                    where_constraint: None,
                    ..
                } if v.is_nil()
                    && custom_traits.is_empty()
                    && Self::is_plain_lexical_name(name)
            );
        }
        elide
    }
}

/// Whether an expression may name any of `targets` (see
/// [`Compiler::destructure_elidable_predecls`]).
struct NamesAnyTarget<'t> {
    targets: &'t [&'t str],
    hit: bool,
}

impl<'ast> Visit<'ast> for NamesAnyTarget<'_> {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if self.hit {
            return;
        }
        self.hit = match kind {
            NameKind::Source | NameKind::Symbolic => true,
            NameKind::Call | NameKind::UserRoutineCall => {
                matches!(name, "EVAL" | "EVALFILE")
            }
            NameKind::Var
            | NameKind::ArrayVar
            | NameKind::HashVar
            | NameKind::CodeVar
            | NameKind::CaptureVar
            | NameKind::AssignTarget
            | NameKind::VarDecl
            | NameKind::TempTarget
            | NameKind::MarkReadonly
            | NameKind::Regex
            | NameKind::Term => {
                name.contains("::")
                    || self
                        .targets
                        .contains(&name.trim_start_matches(['$', '@', '%', '&']))
            }
            _ => false,
        };
    }
}
