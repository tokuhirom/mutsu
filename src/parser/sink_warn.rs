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

use crate::ast::{CallArg, Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Entry point: emit sink-context warnings for the mainline statement list.
pub(super) fn add_sink_warnings(stmts: &[Stmt]) {
    walk_stmts(stmts, false);
    // A `gather { ... }` block evaluates its body in sink context regardless of
    // where the `gather` appears (mainline, an initializer, inside a sub, an
    // argument, ...). Scan the whole tree for gather blocks and warn their
    // bodies, independently of the enclosing statement's own context.
    scan_gathers_stmts(stmts);
}

/// Walk the entire program looking for `gather` blocks. For each one, emit sink
/// warnings for its body. Only `Gather` nodes trigger a warning; every other
/// node is traversed purely to reach nested gathers, so an unhandled variant can
/// at worst miss a warning (a false negative), never produce a spurious one.
fn scan_gathers_stmts(stmts: &[Stmt]) {
    for stmt in stmts {
        scan_gathers_stmt(stmt);
    }
}

fn scan_gathers_stmt(stmt: &Stmt) {
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::Die(e)
        | Stmt::Fail(e)
        | Stmt::Take(e, _)
        | Stmt::Goto(e) => scan_gathers_expr(e),
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => scan_gathers_expr(expr),
        Stmt::Call { args, .. } => {
            for arg in args {
                match arg {
                    CallArg::Positional(e) | CallArg::Invocant(e) | CallArg::Slip(e) => {
                        scan_gathers_expr(e)
                    }
                    CallArg::Named { value: Some(e), .. } => scan_gathers_expr(e),
                    CallArg::Named { value: None, .. } => {}
                }
            }
        }
        Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                scan_gathers_expr(e);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            scan_gathers_expr(cond);
            scan_gathers_stmts(then_branch);
            scan_gathers_stmts(else_branch);
        }
        Stmt::While { cond, body, .. } => {
            scan_gathers_expr(cond);
            scan_gathers_stmts(body);
        }
        Stmt::For { iterable, body, .. } => {
            scan_gathers_expr(iterable);
            scan_gathers_stmts(body);
        }
        Stmt::Given { topic, body } => {
            scan_gathers_expr(topic);
            scan_gathers_stmts(body);
        }
        Stmt::When { cond, body } => {
            scan_gathers_expr(cond);
            scan_gathers_stmts(body);
        }
        Stmt::Whenever { supply, body, .. } => {
            scan_gathers_expr(supply);
            scan_gathers_stmts(body);
        }
        Stmt::Loop { body, .. }
        | Stmt::React { body }
        | Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::Phaser { body, .. }
        | Stmt::SubDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::ClassDecl { body, .. }
        | Stmt::RoleDecl { body, .. }
        | Stmt::Package { body, .. } => scan_gathers_stmts(body),
        Stmt::Label { stmt, .. } => scan_gathers_stmt(stmt),
        _ => {}
    }
}

fn scan_gathers_expr(expr: &Expr) {
    match expr {
        Expr::Gather(body) => {
            // The gather body is in sink context. Warn its useless statements,
            // then keep scanning for gathers nested inside it.
            walk_stmts(body, false);
            scan_gathers_stmts(body);
        }
        Expr::Grouped(e)
        | Expr::PositionalPair(e)
        | Expr::ZenSlice(e)
        | Expr::Itemize(e)
        | Expr::Eager(e)
        | Expr::Unary { expr: e, .. }
        | Expr::PostfixOp { expr: e, .. }
        | Expr::AssignExpr { expr: e, .. }
        | Expr::Reduction { expr: e, .. }
        | Expr::IndirectTypeLookup(e)
        | Expr::SymbolicDeref { expr: e, .. } => scan_gathers_expr(e),
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            scan_gathers_expr(left);
            scan_gathers_expr(right);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            scan_gathers_expr(cond);
            scan_gathers_expr(then_expr);
            scan_gathers_expr(else_expr);
        }
        Expr::Index { target, index, .. } => {
            scan_gathers_expr(target);
            scan_gathers_expr(index);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            scan_gathers_expr(target);
            scan_gathers_expr(index);
            scan_gathers_expr(value);
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            scan_gathers_expr(target);
            for a in args {
                scan_gathers_expr(a);
            }
        }
        Expr::CallOn { target, args } => {
            scan_gathers_expr(target);
            for a in args {
                scan_gathers_expr(a);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                scan_gathers_expr(a);
            }
        }
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::CaptureLiteral(es)
        | Expr::StringInterpolation(es) => {
            for e in es {
                scan_gathers_expr(e);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    scan_gathers_expr(e);
                }
            }
        }
        Expr::InfixFunc { left, right, .. } => {
            scan_gathers_expr(left);
            for e in right {
                scan_gathers_expr(e);
            }
        }
        Expr::Block(body)
        | Expr::AnonSub { body, .. }
        | Expr::AnonSubParams { body, .. }
        | Expr::Lambda { body, .. }
        | Expr::DoBlock { body, .. }
        | Expr::PhaserExpr { body, .. }
        | Expr::Once { body } => scan_gathers_stmts(body),
        Expr::Try { body, catch } => {
            scan_gathers_stmts(body);
            if let Some(c) = catch {
                scan_gathers_stmts(c);
            }
        }
        Expr::DoStmt(stmt) => scan_gathers_stmt(stmt),
        _ => {}
    }
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
        // A source-preserving numeric literal: describe by the inner value's
        // category but render the number using the original source text so the
        // warning keeps the radix / scientific / Unicode form the user wrote.
        Expr::LiteralSrc(v, src) => describe_literal_with_src(v, src),
        // A `Var` node whose name already carries a sigil (`@a`, `%h`) is a
        // synthetic artifact of `:=` bind desugaring — the trailing result
        // expression a container bind leaves in its SyntheticBlock — not a
        // user-written bare scalar. A real bare `@a` parses to `ArrayVar("a")`,
        // so this never suppresses a genuine sink warning; it only stops the
        // spurious "Useless use of $@a in sink context" on `my @a := @b`.
        // A bare anonymous `$` (the unnamed state scalar) parses to a synthetic
        // `__ANON_STATE_<id>__` name. Raku reports it as "unnamed $ variable",
        // not by its internal name.
        Expr::Var(n) if n.starts_with("__ANON_STATE_") => Some("unnamed $ variable".to_string()),
        Expr::Var(n) if n.starts_with(['$', '@', '%', '&']) => None,
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
        Expr::Unary { op, expr: inner } => {
            let sym = pure_prefix_symbol(op)?;
            // Only flag when the operand itself is useless (no side effects),
            // e.g. `-5` / `?5` / `-$x` but not `-foo()`.
            describe_useless(inner)?;
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

/// Like [`describe_literal`], but substitutes the literal's original source text
/// for the canonical number rendering, keeping the user's radix / scientific /
/// Unicode format in the warning.
fn describe_literal_with_src(v: &Value, src: &str) -> Option<String> {
    match v {
        Value::Int(_) | Value::BigInt(_) => Some(format!("constant integer {}", src)),
        Value::Num(_) => Some(format!("constant floating-point number {}", src)),
        // A source-preserving colonpair (`:foo(42)`): echo the original syntax
        // quoted, matching Raku's `Useless use of ":foo(42)" in sink context`.
        Value::Pair(..) | Value::ValuePair(..) => Some(format!("\"{}\"", src)),
        _ => describe_literal(v),
    }
}

/// Approximate the source text of a useless expression for the "in expression"
/// part of an operator warning.
fn render_source(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Grouped(inner) | Expr::PositionalPair(inner) => render_source(inner),
        Expr::Literal(Value::Str(s)) => Some((**s).clone()),
        Expr::Literal(v) => Some(v.to_string_value()),
        Expr::LiteralSrc(_, src) => Some(src.to_string()),
        Expr::Var(n) => Some(format!("${}", n)),
        Expr::ArrayVar(n) => Some(format!("@{}", n)),
        Expr::HashVar(n) => Some(format!("%{}", n)),
        Expr::BareWord(s) => Some(s.clone()),
        Expr::Unary { op, expr: inner } => {
            let sym = pure_prefix_symbol(op)?;
            Some(format!("{}{}", sym, render_source(inner)?))
        }
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

/// Pure prefix operators that produce a value with no side effect. Operators
/// not listed here are treated as potentially effectful and never flagged.
fn pure_prefix_symbol(op: &TokenKind) -> Option<&'static str> {
    Some(match op {
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Tilde => "~",
        TokenKind::Bang => "!",
        TokenKind::Question => "?",
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
