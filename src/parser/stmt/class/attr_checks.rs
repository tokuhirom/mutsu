use crate::ast::{Expr, Stmt};
use crate::parser::parse_result::PError;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

pub(crate) fn expr_uses_attr_twigil(expr: &Expr) -> bool {
    match expr {
        Expr::Var(name) | Expr::ArrayVar(name) | Expr::HashVar(name) => {
            name.starts_with('.') || name.starts_with('!')
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            expr_uses_attr_twigil(target) || args.iter().any(expr_uses_attr_twigil)
        }
        Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            expr_uses_attr_twigil(target)
                || expr_uses_attr_twigil(name_expr)
                || args.iter().any(expr_uses_attr_twigil)
        }
        Expr::Call { args, .. }
        | Expr::ArrayLiteral(args)
        | Expr::BracketArray(args, _)
        | Expr::CaptureLiteral(args)
        | Expr::StringInterpolation(args) => args.iter().any(expr_uses_attr_twigil),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } | Expr::Reduction { expr, .. } => {
            expr_uses_attr_twigil(expr)
        }
        Expr::Binary { left, right, .. }
        | Expr::MetaOp { left, right, .. }
        | Expr::HyperOp { left, right, .. } => {
            expr_uses_attr_twigil(left) || expr_uses_attr_twigil(right)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            expr_uses_attr_twigil(cond)
                || expr_uses_attr_twigil(then_expr)
                || expr_uses_attr_twigil(else_expr)
        }
        Expr::Index { target, index, .. } => {
            expr_uses_attr_twigil(target) || expr_uses_attr_twigil(index)
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            expr_uses_attr_twigil(target)
                || expr_uses_attr_twigil(index)
                || expr_uses_attr_twigil(value)
        }
        Expr::AssignExpr { expr, .. } => expr_uses_attr_twigil(expr),
        Expr::DoBlock { body, .. }
        | Expr::Block(body)
        | Expr::Gather(body)
        | Expr::AnonSub { body, .. }
        | Expr::AnonSubParams { body, .. }
        | Expr::Lambda { body, .. } => body.iter().any(stmt_uses_attr_twigil),
        Expr::Try { body, catch } => {
            body.iter().any(stmt_uses_attr_twigil)
                || catch
                    .as_ref()
                    .is_some_and(|body| body.iter().any(stmt_uses_attr_twigil))
        }
        Expr::DoStmt(stmt) => stmt_uses_attr_twigil(stmt),
        Expr::CallOn { target, args } => {
            expr_uses_attr_twigil(target) || args.iter().any(expr_uses_attr_twigil)
        }
        Expr::InfixFunc { left, right, .. } => {
            expr_uses_attr_twigil(left) || right.iter().any(expr_uses_attr_twigil)
        }
        Expr::Exists { target, arg, .. } => {
            expr_uses_attr_twigil(target)
                || arg
                    .as_ref()
                    .is_some_and(|arg_expr| expr_uses_attr_twigil(arg_expr))
        }
        Expr::ZenSlice(inner) => expr_uses_attr_twigil(inner),
        _ => false,
    }
}

pub(crate) fn stmt_uses_attr_twigil(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr)
        | Stmt::Return(expr)
        | Stmt::Take(expr, _)
        | Stmt::Die(expr)
        | Stmt::Fail(expr) => expr_uses_attr_twigil(expr),
        Stmt::VarDecl {
            expr,
            where_constraint,
            ..
        } => {
            expr_uses_attr_twigil(expr)
                || where_constraint
                    .as_ref()
                    .is_some_and(|wc| expr_uses_attr_twigil(wc))
        }
        Stmt::Assign { expr, .. } => expr_uses_attr_twigil(expr),
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Package { body, .. }
        | Stmt::Catch(body)
        | Stmt::Control(body) => body.iter().any(stmt_uses_attr_twigil),
        Stmt::SubDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::TokenDecl { body, .. }
        | Stmt::RuleDecl { body, .. } => body.iter().any(stmt_uses_attr_twigil),
        Stmt::Label { stmt, .. } => stmt_uses_attr_twigil(stmt),
        _ => false,
    }
}

pub(crate) fn no_self_error() -> PError {
    let msg = "X::Syntax::NoSelf".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::NoSelf"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// `X::Syntax::Regex::NullRegex` — an empty `token`/`regex`/`rule` body (e.g.
/// `regex foo { }`) is a null regex, rejected by Raku at parse time.
pub(crate) fn null_regex_error() -> PError {
    let msg = "Null regex not allowed".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::NullRegex"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

pub(crate) fn expr_is_bare_ident(expr: &Expr, ident: &str) -> bool {
    matches!(expr, Expr::Var(name) | Expr::BareWord(name) if name == ident)
}

pub(crate) fn stmt_is_also_is_rw(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(Expr::InfixFunc {
            name, left, right, ..
        }) => {
            name == "is"
                && expr_is_bare_ident(left, "also")
                && right.len() == 1
                && expr_is_bare_ident(&right[0], "rw")
        }
        Stmt::Expr(Expr::Binary { left, op, right }) => {
            matches!(op, TokenKind::Ident(name) if name == "is")
                && expr_is_bare_ident(left, "also")
                && expr_is_bare_ident(right, "rw")
        }
        _ => false,
    }
}

/// Extract the parent class name from `also is <ClassName>` statements
/// (where ClassName is not `rw`).
pub(crate) fn stmt_also_is_parent(stmt: &Stmt) -> Option<String> {
    match stmt {
        Stmt::Expr(Expr::InfixFunc {
            name, left, right, ..
        }) if name == "is" && expr_is_bare_ident(left, "also") && right.len() == 1 => {
            if let Expr::BareWord(parent) = &right[0]
                && parent != "rw"
            {
                return Some(parent.clone());
            }
            None
        }
        Stmt::Expr(Expr::Binary { left, op, right }) => {
            if matches!(op, TokenKind::Ident(name) if name == "is")
                && expr_is_bare_ident(left, "also")
                && let Expr::BareWord(parent) = right.as_ref()
                && parent != "rw"
            {
                return Some(parent.clone());
            }
            None
        }
        _ => None,
    }
}

pub(crate) fn reject_no_self_in_subs(body: &[Stmt]) -> Result<(), PError> {
    for stmt in body {
        if let Stmt::SubDecl { body: sub_body, .. } = stmt
            && sub_body.iter().any(stmt_uses_attr_twigil)
        {
            return Err(no_self_error());
        }
    }
    Ok(())
}

/// Reject attribute-twigil references (`$!a`, `$.a`, ...) inside a `where`
/// constraint on an attribute declaration. The `where` clause is evaluated as a
/// thunk that has no `self`, so such a reference is an X::Syntax::NoSelf error.
/// Note: this only applies to the `where` constraint, not to the `= default`
/// initializer (where `$!a` is allowed, since defaults run with `self`).
pub(crate) fn reject_no_self_in_attr_where(body: &[Stmt]) -> Result<(), PError> {
    for stmt in body {
        if let Stmt::HasDecl {
            where_constraint: Some(wc),
            ..
        } = stmt
            && expr_uses_attr_twigil(wc)
        {
            return Err(no_self_error());
        }
    }
    Ok(())
}

/// Collect no-twigil attribute names from HasDecl statements in a class body.
pub(crate) fn collect_no_twigil_attr_names(body: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in body {
        if let Stmt::HasDecl {
            name,
            is_alias: true,
            ..
        } = stmt
        {
            names.push(name.to_string());
        }
    }
    names
}

/// Check if an expression references a variable matching one of the given names.
pub(crate) fn expr_uses_var_name(expr: &Expr, names: &[String]) -> bool {
    match expr {
        Expr::Var(name) => names.iter().any(|n| n == name),
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            expr_uses_var_name(target, names) || args.iter().any(|a| expr_uses_var_name(a, names))
        }
        Expr::Call { args, .. }
        | Expr::ArrayLiteral(args)
        | Expr::BracketArray(args, _)
        | Expr::CaptureLiteral(args)
        | Expr::StringInterpolation(args) => args.iter().any(|a| expr_uses_var_name(a, names)),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } | Expr::Reduction { expr, .. } => {
            expr_uses_var_name(expr, names)
        }
        Expr::Binary { left, right, .. } => {
            expr_uses_var_name(left, names) || expr_uses_var_name(right, names)
        }
        Expr::InfixFunc { left, right, .. } => {
            expr_uses_var_name(left, names) || right.iter().any(|a| expr_uses_var_name(a, names))
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            expr_uses_var_name(cond, names)
                || expr_uses_var_name(then_expr, names)
                || expr_uses_var_name(else_expr, names)
        }
        Expr::AssignExpr { expr, .. } => expr_uses_var_name(expr, names),
        _ => false,
    }
}

/// Check if a statement at class body level uses a no-twigil attribute variable.
pub(crate) fn stmt_uses_var_name_at_body_level(stmt: &Stmt, names: &[String]) -> bool {
    match stmt {
        // Skip method/sub/token/rule declarations — they have `self`
        Stmt::MethodDecl { .. }
        | Stmt::SubDecl { .. }
        | Stmt::TokenDecl { .. }
        | Stmt::RuleDecl { .. } => false,
        // Skip HasDecl — declaring the attr is fine
        Stmt::HasDecl { .. } => false,
        Stmt::Expr(expr) | Stmt::Return(expr) | Stmt::Fail(expr) => expr_uses_var_name(expr, names),
        Stmt::Say(args) | Stmt::Print(args) | Stmt::Note(args) | Stmt::Put(args) => {
            args.iter().any(|a| expr_uses_var_name(a, names))
        }
        _ => false,
    }
}

/// Reject no-twigil attribute variable usage at class body level (outside methods).
pub(crate) fn reject_no_twigil_attr_at_body_level(body: &[Stmt]) -> Result<(), PError> {
    let names = collect_no_twigil_attr_names(body);
    if names.is_empty() {
        return Ok(());
    }
    for stmt in body {
        if stmt_uses_var_name_at_body_level(stmt, &names) {
            return Err(no_self_error());
        }
    }
    Ok(())
}
