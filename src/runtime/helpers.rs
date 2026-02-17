use crate::ast::{Expr, Stmt};

/// Scan a statement list for placeholder variables (names starting with `^`,
/// e.g. `$^a`, `$^b`) and return their sorted, deduplicated names.
///
/// This is used to detect implicit block parameters so that constructs like
/// `{ $^a + $^b }` can automatically introduce `$^a` and `$^b` as parameters.
pub(crate) fn collect_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_ph_stmt(stmt, &mut names);
    }
    names.sort();
    names.dedup();
    names
}

/// Recursively walk a single statement, collecting placeholder variable names
/// into `out`. Dispatches into sub-statements and contained expressions.
fn collect_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Take(e) => collect_ph_expr(e, out),
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => collect_ph_expr(expr, out),
        Stmt::Say(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_ph_expr(cond, out);
            for s in then_branch {
                collect_ph_stmt(s, out);
            }
            for s in else_branch {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::For { iterable, body, .. } => {
            collect_ph_expr(iterable, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Loop { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::React { body } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Whenever { supply, body, .. } => {
            collect_ph_expr(supply, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Block(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Phaser { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Given { topic, body } => {
            collect_ph_expr(topic, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::When { cond, body } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::ProtoDecl { .. } => {}
        Stmt::DoesDecl { .. } => {}
        Stmt::SubsetDecl { predicate, .. } => {
            collect_ph_expr(predicate, out);
        }
        _ => {}
    }
}

/// Recursively walk a single expression, collecting placeholder variable names
/// (variables whose name starts with `^`) into `out`. Traverses all nested
/// sub-expressions and any embedded statement bodies (blocks, lambdas, try, etc.).
fn collect_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Var(name) if name.starts_with('^') => {
            if !out.contains(name) {
                out.push(name.clone());
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => collect_ph_expr(expr, out),
        Expr::MethodCall { target, args, .. } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::CallOn { target, args } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Index { target, index } => {
            collect_ph_expr(target, out);
            collect_ph_expr(index, out);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_ph_expr(cond, out);
            collect_ph_expr(then_expr, out);
            collect_ph_expr(else_expr, out);
        }
        Expr::AssignExpr { expr, .. } | Expr::Exists(expr) => collect_ph_expr(expr, out),
        Expr::ArrayLiteral(es) | Expr::StringInterpolation(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Expr::Block(stmts)
        | Expr::AnonSub(stmts)
        | Expr::AnonSubParams { body: stmts, .. }
        | Expr::Gather(stmts) => {
            for s in stmts {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoBlock { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoStmt(stmt) => {
            collect_ph_stmt(stmt, out);
        }
        Expr::Lambda { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::Try { body, catch } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
            if let Some(c) = catch {
                for s in c {
                    collect_ph_stmt(s, out);
                }
            }
        }
        Expr::CodeVar(_) => {}
        Expr::Reduction { expr, .. } => collect_ph_expr(expr, out),
        Expr::HyperOp { left, right, .. } | Expr::MetaOp { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::InfixFunc { left, right, .. } => {
            collect_ph_expr(left, out);
            for e in right {
                collect_ph_expr(e, out);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    collect_ph_expr(e, out);
                }
            }
        }
        _ => {}
    }
}
