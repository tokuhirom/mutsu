//! Compile-time "Useless use of ... in sink context" warning analysis.
//!
//! Raku warns when a pure, side-effect-free value or expression is evaluated in
//! sink (void) context, where its result is discarded. This pass walks the
//! top-level (mainline) statement list — where every statement is in sink
//! context — and recurses into the bodies of bare blocks and control-flow
//! constructs (which remain in sink context), but never into routine/package
//! bodies (where the final statement is a return value, not sunk).
//!
//! Only genuinely useless expressions are flagged: literals, plain variable
//! reads, pure operators applied to useless operands, pairs and empty parens.
//! Anything that may have a side effect (calls, method calls, assignments,
//! declarations, `say`/`print`, regex matches, ...) is never flagged.

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Entry point: emit sink-context warnings for the mainline statement list.
pub(super) fn add_sink_warnings(stmts: &[Stmt]) {
    walk_stmts(stmts, false);
}

/// Walk a sink-context statement list. `nil_hint` is true when these statements
/// are the body of a loop/topicalizer modifier, where Raku suggests using `Nil`
/// to suppress the warning.
fn walk_stmts(stmts: &[Stmt], nil_hint: bool) {
    for stmt in stmts {
        walk_stmt(stmt, nil_hint);
    }
}

fn walk_stmt(stmt: &Stmt, nil_hint: bool) {
    match stmt {
        Stmt::Expr(e) => warn_expr_sink(e, nil_hint),
        // Bare blocks and parser desugaring blocks stay in sink context.
        Stmt::Block(body) | Stmt::SyntheticBlock(body) => walk_stmts(body, false),
        // Loop / topicalizer bodies are sunk. The "(use Nil instead...)" hint is
        // only emitted for the statement-modifier form (`1 while 0`), not the
        // block form (`while 0 { 1 }`); the modifier desugaring leaves the body
        // without a leading SetLine marker.
        Stmt::For { body, .. }
        | Stmt::Given { body, .. }
        | Stmt::While { body, .. }
        | Stmt::Loop { body, .. } => walk_stmts(body, is_modifier_body(body)),
        // Conditionals are sunk but do not suggest `Nil`.
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            walk_stmts(then_branch, false);
            walk_stmts(else_branch, false);
        }
        Stmt::When { body, .. } | Stmt::Default(body) => walk_stmts(body, false),
        // Everything else (declarations, calls, say/print, assignments, and the
        // bodies of subs/classes/etc.) is not analysed for sink warnings.
        _ => {}
    }
}

/// A loop/topicalizer body produced by a statement modifier (`1 while 0`) is a
/// single statement with no leading `SetLine` marker, unlike a brace block.
fn is_modifier_body(body: &[Stmt]) -> bool {
    !matches!(body.first(), Some(Stmt::SetLine(_)))
}

/// Warn for an expression evaluated in sink context. Comma lists distribute the
/// sink to each element.
fn warn_expr_sink(expr: &Expr, nil_hint: bool) {
    match expr {
        Expr::Grouped(inner) => warn_expr_sink(inner, nil_hint),
        // A bare comma list `1, 2` distributes sink to each element. An empty
        // `()` is itself a useless value.
        Expr::ArrayLiteral(elems) => {
            if elems.is_empty() {
                emit("()".to_string(), nil_hint);
            } else {
                for e in elems {
                    warn_expr_sink(e, nil_hint);
                }
            }
        }
        _ => {
            if let Some(desc) = describe_useless(expr) {
                emit(desc, nil_hint);
            }
        }
    }
}

fn emit(desc: String, nil_hint: bool) {
    let suffix = if nil_hint {
        " (use Nil instead to suppress this warning)"
    } else {
        ""
    };
    super::add_parse_warning(format!(
        "Useless use of {} in sink context{} (line 1)",
        desc, suffix
    ));
}

/// Returns the description (the "X" in "Useless use of X in sink context") if
/// `expr` is a pure, side-effect-free value/expression, otherwise `None`.
fn describe_useless(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Grouped(inner) => describe_useless(inner),
        Expr::PositionalPair(inner) => describe_useless(inner),
        Expr::Literal(v) => describe_literal(v),
        Expr::Var(n) => Some(format!("${}", n)),
        Expr::ArrayVar(n) => Some(format!("@{}", n)),
        Expr::HashVar(n) => Some(format!("%{}", n)),
        Expr::ArrayLiteral(elems) if elems.is_empty() => Some("()".to_string()),
        Expr::BareWord(s) if is_type_name(s) => Some(format!("constant value {}", s)),
        Expr::Binary { left, op, right } => {
            let sym = pure_op_symbol(op)?;
            // Only flag when both operands are themselves useless (no side
            // effects), e.g. `1 + 2` but not `$x + foo()`.
            describe_useless(left)?;
            describe_useless(right)?;
            let rendered = render_source(expr)?;
            Some(format!("\"{}\" in expression \"{}\"", sym, rendered))
        }
        _ => None,
    }
}

fn describe_literal(v: &Value) -> Option<String> {
    match v {
        Value::Int(n) => Some(format!("constant integer {}", n)),
        Value::BigInt(n) => Some(format!("constant integer {}", n)),
        Value::Num(_) => Some(format!(
            "constant floating-point number {}",
            v.to_string_value()
        )),
        Value::Rat(..) | Value::FatRat(..) | Value::BigRat(..) => {
            Some(format!("constant rational {}", v.to_string_value()))
        }
        Value::Str(s) => Some(format!("constant string \"{}\"", s)),
        Value::Complex(..) => Some(format!("constant value {}", v.to_string_value())),
        Value::Package(_) => Some(format!("constant value {}", v.to_string_value())),
        Value::Mixin(inner, _) => {
            // `<123.456>` etc. parse to a Mixin carrying a Str representation;
            // describe by the inner value's category but the stringified whole.
            describe_literal(inner).map(|_| format!("constant value {}", v.to_string_value()))
        }
        _ => None,
    }
}

/// Approximate the source text of a useless expression for the "in expression"
/// part of an operator warning.
fn render_source(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Grouped(inner) | Expr::PositionalPair(inner) => render_source(inner),
        Expr::Literal(Value::Str(s)) => Some((**s).clone()),
        Expr::Literal(v) => Some(v.to_string_value()),
        Expr::Var(n) => Some(format!("${}", n)),
        Expr::ArrayVar(n) => Some(format!("@{}", n)),
        Expr::HashVar(n) => Some(format!("%{}", n)),
        Expr::BareWord(s) => Some(s.clone()),
        Expr::Binary { left, op, right } => {
            let sym = pure_op_symbol(op)?;
            let l = render_source(left)?;
            let r = render_source(right)?;
            if *op == TokenKind::FatArrow {
                Some(format!("{} {} {}", l, sym, r))
            } else {
                Some(format!("{}{}{}", l, sym, r))
            }
        }
        _ => None,
    }
}

/// Pure infix operators that produce a value with no side effect. Operators not
/// listed here are treated as potentially effectful and never flagged.
fn pure_op_symbol(op: &TokenKind) -> Option<&'static str> {
    Some(match op {
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Star => "*",
        TokenKind::StarStar => "**",
        TokenKind::Slash => "/",
        TokenKind::Percent => "%",
        TokenKind::Tilde => "~",
        TokenKind::EqEq => "==",
        TokenKind::BangEq => "!=",
        TokenKind::Lt => "<",
        TokenKind::Lte => "<=",
        TokenKind::Gt => ">",
        TokenKind::Gte => ">=",
        TokenKind::LtEqGt => "<=>",
        TokenKind::FatArrow => "=>",
        _ => return None,
    })
}

/// Core type-object names that appear as bare words and are useless in sink
/// context (e.g. `Mu`, `Any`, `Cool`).
fn is_type_name(s: &str) -> bool {
    matches!(
        s,
        "Mu" | "Any"
            | "Cool"
            | "Junction"
            | "Int"
            | "Num"
            | "Rat"
            | "Complex"
            | "Str"
            | "Bool"
            | "Nil"
            | "Real"
            | "Numeric"
    )
}
