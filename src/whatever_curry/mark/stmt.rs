//! Statement-level recursion for the `*` leaf classifier (see `super`'s module
//! doc), on the mutable AST visitor (ADR-10499). The statement hooks only pick
//! which of a statement's expressions are *value* positions; every other
//! expression is classified by `expr.rs`, where the interesting table lives.

use super::{mark_opt_expr, mark_opt_value_leaf, mark_value_leaf};
use crate::ast::{CallArg, Expr, HandleSpec, ParamDef, Stmt};
use crate::ast_visit::{VisitMut, walk_param_mut, walk_stmt_mut};
use crate::regex_tree::RegexNode;

// Cost: O(n), n = size of `stmt`'s subtree.
pub(super) fn mark_stmt(stmt: &mut Stmt) {
    Marker.visit_stmt_mut(stmt);
}

/// A routine's or block's parameter (see [`Marker::visit_param_mut`]).
// Cost: O(n), n = size of the parameter's subtree.
pub(super) fn mark_param(param: &mut ParamDef) {
    Marker.visit_param_mut(param);
}

struct Marker;

impl VisitMut for Marker {
    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            // A bare `*` standing alone as a whole statement (`*;`, a proto's
            // `{*}` body) stays a value, as does an assignment's RHS.
            Stmt::Expr(e) | Stmt::Assign { expr: e, .. } => mark_value_leaf(e),
            Stmt::VarDecl {
                expr,
                custom_traits,
                where_constraint,
                ..
            } => {
                // `my $x = *` / `my $x := *` — assignment/bind RHS.
                mark_value_leaf(expr);
                for (_, arg) in custom_traits {
                    mark_opt_expr(arg);
                }
                if let Some(e) = where_constraint {
                    self.visit_expr_mut(e);
                }
            }
            Stmt::Say(args) | Stmt::Put(args) | Stmt::Print(args) | Stmt::Note(args) => {
                for a in args {
                    mark_value_leaf(a);
                }
            }
            Stmt::Call { args, name, .. } => {
                // ADR-0115's CORE type fold; see `parser::core_type_fold`.
                crate::parser::core_type_fold::fold_nqp_call_args(*name, args);
                for a in args {
                    mark_call_arg(a);
                }
            }
            Stmt::Let { index, value, .. } => {
                if let Some(index) = index {
                    self.visit_expr_mut(index);
                }
                if let Some(value) = value {
                    mark_value_leaf(value);
                }
            }
            Stmt::TempMethodAssign {
                method_args, value, ..
            } => {
                for a in method_args {
                    mark_value_leaf(a);
                }
                mark_value_leaf(value);
            }
            Stmt::HasDecl {
                default,
                is_default,
                where_constraint,
                unknown_traits,
                handles,
                ..
            } => {
                mark_opt_value_leaf(default);
                mark_opt_value_leaf(is_default);
                if let Some(e) = where_constraint {
                    self.visit_expr_mut(e);
                }
                for (_, _, arg) in unknown_traits {
                    mark_opt_expr(arg);
                }
                for h in handles {
                    if let HandleSpec::Expr(e) = h {
                        self.visit_expr_mut(e);
                    }
                }
            }
            // Everything else marks its expressions as arguments.
            _ => walk_stmt_mut(self, stmt),
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        super::expr::mark_expr(expr);
    }

    /// A parameter default is a value position (`$x = *`); every other
    /// expression of a parameter (a `where` constraint, a sub-signature's) is
    /// an argument. `walk_param_mut` also gives the parameter a fresh
    /// `ParamCode`: the compiled chunks (ADR-0133) describe the expressions as
    /// they were.
    fn visit_param_mut(&mut self, param: &mut ParamDef) {
        let default = param.default.take();
        walk_param_mut(self, param);
        param.default = default;
        mark_opt_value_leaf(&mut param.default);
    }

    // A regex's code blocks are not classified: `expr.rs` stops at regex
    // literals too, whose pattern the runtime also keeps as source text.
    fn visit_regex_node_mut(&mut self, _node: &mut RegexNode) {}
}

fn mark_call_arg(arg: &mut CallArg) {
    match arg {
        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => mark_value_leaf(e),
        CallArg::Named { value, .. } => mark_opt_value_leaf(value),
    }
}
