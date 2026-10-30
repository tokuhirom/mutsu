//! The one lowering of the assignment family (ADR-10723 Stage 2, #14948): an
//! [`Assign`] the parser built, or one read off a hand-built
//! `ApplyInfix(Assignment)`, becomes the expression the compiler runs.
//!
//! Each target kind has its own executable form: a variable an `AssignExpr`,
//! a subscript an `IndexAssign`, a call or a parenthesised list a writeback
//! through its container, a method call the rw-accessor writeback. This is
//! the dispatch the parser used to do at the `=` it read.

use crate::ast::parsed::{Assign, ParsedExpr};
use crate::ast::{AssignOp, Expr, Stmt};

/// Whether an assignment in expression position assigns to `target`, as
/// written. `$(EXPR) = v` is not one here: its item assignment is the
/// statement parser's (`EXPR.item = v` binds tighter than the comma).
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_expression_target(target: &Expr) -> bool {
    let mut target = target;
    loop {
        match target {
            Expr::Grouped(inner) => target = inner,
            Expr::Spelled(spelled) => target = &spelled.expr,
            _ => break,
        }
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
        // `@a[0]:v = 1`: the adverbs make the read a value, which refuses.
        Expr::Ast(node) => matches!(node.as_ref(), ParsedExpr::SubscriptAdverbs(_)),
        _ => false,
    }
}

/// Whether `target` is an lvalue expression the statement parser assigns
/// through at run time (`-$x = 5`, `c ?? $a !! $b = 1`, `$<c> = $0`,
/// `($a || $b) = 1`, `(my $v) = 1, 2`, `($x = 5) = 6`, `("a" R~ "b") = 1`): the conditional,
/// operator, declaration or assignment is evaluated and its result container
/// is written.
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_callable_lvalue_target(target: &Expr) -> bool {
    let mut target = target.peel_parens();
    while let Expr::Spelled(spelled) = target {
        target = spelled.expr.peel_parens();
    }
    match target {
        Expr::Unary { .. }
        | Expr::Ternary { .. }
        | Expr::CaptureVar(_)
        | Expr::Binary { .. }
        // `("a" R~ "b") = 1` (dies with `X::Assignment::RO`), `(key => $v) = 1`.
        | Expr::MetaOp { .. }
        // `@a[0;1]:!exists = 1` is an immutable `Bool`.
        | Expr::Exists { .. }
        | Expr::PositionalPair(_) => true,
        Expr::DoStmt(stmt) => matches!(stmt.as_ref(), Stmt::VarDecl { .. }),
        // `($x = 5) = 6`, `(@a[0] = 5) = 7`: an assignment yields its
        // target's container.
        Expr::Ast(node) => match node.as_ref() {
            ParsedExpr::Assign(_) => true,
            // `@a[0;1]:!exists = 1` is an immutable `Bool`.
            ParsedExpr::SubscriptAdverbs(adverbs) => adverbs.has_exists(),
            _ => false,
        },
        Expr::AssignExpr { is_bind, .. } => !is_bind,
        Expr::IndexAssign { .. } | Expr::MultiDimIndexAssign { .. } => true,
        _ => false,
    }
}

/// Whether `target` is an item contextualizer (`$(EXPR)`, `EXPR.item`), whose
/// assignment writes through to `EXPR` as an item assignment. The statement
/// parser owns it: `$(EXPR) = a, b` leaves the comma to the statement.
// Cost: O(d), d = levels of parentheses around the target.
pub(crate) fn is_item_target(target: &Expr) -> bool {
    match target.peel_parens() {
        Expr::Contextualizer { kind, .. } => *kind == crate::ast::ContextKind::Item,
        Expr::MethodCall { name, args, .. } => name == "item" && args.is_empty(),
        _ => false,
    }
}

/// Lower an assignment in expression position.
// Cost: O(n), n = size of the target and source.
pub(crate) fn lower_expression(node: Assign) -> Expr {
    let Assign {
        mut target, source, ..
    } = node;
    // How the target was spelled (`@a[0;1]:foo`'s adverbs) takes no part in
    // the write.
    while let Expr::Spelled(spelled) = target {
        target = spelled.expr;
    }
    match super::assign_target::unwrap_grouped_lvalue(target) {
        // `120 = 3`: an immutable literal, which dies once the source has run.
        Expr::Literal(value) => super::literal_assign_ro_expr(Expr::Literal(value), source),
        Expr::LiteralSrc(value, _) => super::literal_assign_ro_expr(Expr::Literal(value), source),
        // `&f = …, …` (a statement whose comma list follows the assignment).
        Expr::CodeVar(name) => Expr::AssignExpr {
            name: format!("&{name}"),
            expr: Box::new(source),
            is_bind: false,
        },
        // A bareword target is a term, spelled by its term key (ADR-12441).
        Expr::BareWord(name) => Expr::AssignExpr {
            name: crate::term_names::bareword_target_key(&name),
            expr: Box::new(source),
            is_bind: false,
        },
        // A parenthesised list target (`say ($a, $b) = foo()`): a lone
        // `*`-slurpy target takes its element by position, any other list
        // assigns through the list container.
        Expr::ArrayLiteral(items) => {
            match crate::parser::single_target_list_lvalue_expr(items.clone(), source.clone()) {
                Some(expr) => expr,
                None => super::assign_writeback::callable_lvalue_assign_expr(
                    Expr::ArrayLiteral(items),
                    Vec::new(),
                    source,
                ),
            }
        }
        // `-$x = 5`, `c ?? $a !! $b = 1`, `$<c> = $0`, `($a || $b) = 1`,
        // `(my $v) = 1, 2`: write through the expression's result.
        target if is_callable_lvalue_target(&target) => {
            super::assign_writeback::callable_lvalue_assign_expr(target, Vec::new(), source)
        }
        // Every other target is the one general dispatch: a variable, a
        // subscript, a call, a method call (the rw-accessor writeback, and
        // `$(EXPR) = v` through its `.item` call), a symbolic or type
        // lookup. An immutable one (`pi = 4`, a literal) dies with
        // `X::Assignment::RO` when the assignment runs.
        other => super::assign_target::assign_to_target_expr(other, source),
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
