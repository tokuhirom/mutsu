//! The one lowering of the assignment family (ADR-10723 Stage 2, #14948): an
//! [`Assign`] the parser built, or one read off a hand-built
//! `ApplyInfix(Assignment)`, becomes the expression the compiler runs.
//!
//! Each target kind has its own executable form: a variable an `AssignExpr`,
//! a subscript an `IndexAssign`, a call or a parenthesised list a writeback
//! through its container, a method call the rw-accessor writeback. This is
//! the dispatch the parser used to do at the `=` it read.

use crate::ast::parsed::Assign;
use crate::ast::{AssignOp, Expr, Stmt};
use crate::symbol::Symbol;

/// Whether an assignment in expression position assigns to `target`, as
/// written. `$(EXPR) = v` is not one here: its item assignment is the
/// statement parser's (`EXPR.item = v` binds tighter than the comma).
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_expression_target(target: &Expr) -> bool {
    let mut target = target;
    while let Expr::Grouped(inner) = target {
        target = inner;
    }
    match target {
        Expr::MethodCall { name, args, .. } => !(name == "item" && args.is_empty()),
        Expr::Var(_)
        | Expr::ArrayVar(_)
        | Expr::HashVar(_)
        | Expr::Index { .. }
        | Expr::Call { .. }
        | Expr::MultiDimIndex { .. }
        | Expr::SymbolicDeref { .. }
        | Expr::IndirectTypeLookup(_)
        | Expr::BareWord(_)
        | Expr::CallOn { .. }
        | Expr::DynamicMethodCall { .. }
        | Expr::ArrayLiteral(_) => true,
        _ => false,
    }
}

/// Whether `target` is an lvalue expression the statement parser assigns
/// through at run time (`-$x = 5`, `c ?? $a !! $b = 1`, `$<c> = $0`,
/// `($a || $b) = 1`, `(my $v) = 1, 2`): the conditional, operator or
/// declaration is evaluated and its result container is written.
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_callable_lvalue_target(target: &Expr) -> bool {
    match target.peel_parens() {
        Expr::Unary { .. } | Expr::Ternary { .. } | Expr::CaptureVar(_) | Expr::Binary { .. } => {
            true
        }
        Expr::DoStmt(stmt) => matches!(stmt.as_ref(), Stmt::VarDecl { .. }),
        _ => false,
    }
}

/// Lower an assignment in expression position whose target
/// [`is_expression_target`] or [`is_callable_lvalue_target`].
// Cost: O(n), n = size of the target and source.
pub(crate) fn lower_expression(node: Assign) -> Expr {
    let Assign { target, source } = node;
    match crate::parser::unwrap_grouped_lvalue(target) {
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(source),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{name}"),
            expr: Box::new(source),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{name}"),
            expr: Box::new(source),
            is_bind: false,
        },
        // `&f = …, …` (a statement whose comma list follows the assignment).
        Expr::CodeVar(name) => Expr::AssignExpr {
            name: format!("&{name}"),
            expr: Box::new(source),
            is_bind: false,
        },
        // `(@a[0]:kv)[1] = rhs` writes through the live element container the
        // `:kv` list carries (ADR-0036 slice 2), so a subscript needs no
        // special case.
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
            ..
        } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(source),
            is_positional,
            spelling,
        },
        Expr::Call { name, args, .. } => {
            crate::parser::named_sub_lvalue_assign_expr(name.resolve(), args, source)
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value: Box::new(source),
            is_positional,
        },
        // `$::('name') = expr` (`flat $::('b') = l()`).
        Expr::SymbolicDeref { sigil, expr } => Expr::SymbolicDerefAssign {
            sigil,
            expr,
            value: Box::new(source),
        },
        Expr::IndirectTypeLookup(inner) => Expr::IndirectTypeLookupAssign {
            expr: inner,
            value: Box::new(source),
        },
        // A bareword target is a term, spelled by its term key (ADR-12441).
        Expr::BareWord(name) => Expr::AssignExpr {
            name: crate::term_names::bareword_target_key(&name),
            expr: Box::new(source),
            is_bind: false,
        },
        Expr::CallOn { target, args } => {
            if args.is_empty()
                && let Expr::ArrayLiteral(items) = target.as_ref()
                && let Some(expr) =
                    crate::parser::list_lvalue_assign_expr(items.clone(), source.clone())
            {
                return expr;
            }
            callable_lvalue(*target, args, source)
        }
        // A method-call lvalue (`$o.a = v`, `my $c = $o.a = 42`): the
        // rw-accessor writeback.
        target @ (Expr::MethodCall { .. } | Expr::DynamicMethodCall { .. }) => {
            crate::parser::assign_to_target_expr(target, source)
        }
        // A parenthesised list target (`say ($a, $b) = foo()`): a lone
        // `*`-slurpy target takes its element by position, any other list
        // assigns through the list container.
        Expr::ArrayLiteral(items) => {
            match crate::parser::single_target_list_lvalue_expr(items.clone(), source.clone()) {
                Some(expr) => expr,
                None => callable_lvalue(Expr::ArrayLiteral(items), Vec::new(), source),
            }
        }
        // `-$x = 5`, `c ?? $a !! $b = 1`, `$<c> = $0`, `($a || $b) = 1`,
        // `(my $v) = 1, 2`: write through the expression's result.
        target if is_callable_lvalue_target(&target) => callable_lvalue(target, Vec::new(), source),
        other => unreachable!("not an assignment target: {other:?}"),
    }
}

/// Lower an assignment written as a statement. A variable target is a
/// `Stmt::Assign`; any other target is the expression form as a statement.
// Cost: O(n), n = size of the target and source.
pub(crate) fn lower_statement(node: Assign) -> Stmt {
    match variable_name(&node.target) {
        Some(name) => Stmt::Assign {
            name,
            expr: node.source,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        },
        None => Stmt::Expr(lower_expression(node)),
    }
}

/// The storage name of a variable target, as `Stmt::Assign` spells it:
/// `$x` is `x`, `@a` is `@a`, `%h` is `%h`, `&f` is `&f`.
// Cost: O(1).
pub(super) fn variable_name(target: &Expr) -> Option<String> {
    match target {
        Expr::Var(name) => Some(name.clone()),
        Expr::ArrayVar(name) => Some(format!("@{name}")),
        Expr::HashVar(name) => Some(format!("%{name}")),
        Expr::CodeVar(name) => Some(format!("&{name}")),
        _ => None,
    }
}

/// The writeback through a callable or list container.
// Cost: O(1).
fn callable_lvalue(target: Expr, args: Vec<Expr>, source: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
        args: vec![target, Expr::ArrayLiteral(args), source],
        listop: false,
    }
}
