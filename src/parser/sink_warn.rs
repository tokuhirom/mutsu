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

use crate::ast::parsed::{ModifierKind, ParsedStmt};
use crate::ast::{DoBlockOrigin, Expr, SrcSpan, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt as walk_ast_stmt};
use super::sink_describe::describe_useless;

/// A source `do` block evaluates every statement but its last in sink context,
/// regardless of whether the block's result is used. Scan all expression
/// positions, including initializers and routine bodies. The outer sink walk
/// handles the last statement when the block's result is also discarded.
/// `unit_passes` runs it statement by statement, keeping its line across them.
pub(super) struct SourceDoScan {
    line: std::cell::Cell<i64>,
}

impl SourceDoScan {
    // Cost: O(1).
    pub(super) fn new() -> Self {
        SourceDoScan {
            line: std::cell::Cell::new(1),
        }
    }
}

impl<'ast> Visit<'ast> for SourceDoScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::SetLine(line, _) = stmt {
            self.line.set(*line);
        }
        walk_ast_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::DoBlock {
            body,
            origin: DoBlockOrigin::SourceBlock,
            ..
        } = expr
        {
            walk_stmts(sunk_prefix(body), false, &self.line);
        }
        walk_expr(self, expr);
    }
}

/// Walk the entire program looking for `gather` blocks. For each one, emit sink
/// warnings for its body. The search is the typed AST visitor (ADR-0137), so
/// a `gather` anywhere outside a signature — an argument, an initializer, an
/// attribute default, a regex code block, a routine body — is found.
// Cost: O(n), n = size of the AST (plus the sink walk of each gather body).
pub(super) struct GatherScan;

impl<'ast> Visit<'ast> for GatherScan {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Gather(body) = expr {
            // The gather body is in sink context. This is a separate
            // top-level scan (not fed a `line` from an enclosing walk), so it
            // starts its own tracker -- corrected immediately by the body's
            // own leading `SetLine` marker.
            let line = std::cell::Cell::new(1i64);
            walk_stmts(body, false, &line);
        }
        // Keep scanning for gathers nested inside, a gather body included.
        walk_expr(self, expr);
    }

    // Rakudo does not sink-check a gather inside a signature (a parameter
    // default or `where` clause): measured, `sub f($x = gather { 42; take 1
    // }) { }` warns nothing.
    fn visit_param(&mut self, _param: &'ast crate::ast::ParamDef) {}
}

/// Walk a sink-context statement list. `nil_hint` is true when these statements
/// are the body of a loop/topicalizer modifier, where Raku suggests using `Nil`
/// to suppress the warning. `line` tracks the source line a sink warning
/// found here should be attributed to: it starts at whatever the caller last
/// saw and is updated on every `Stmt::SetLine` marker encountered, mirroring
/// how the compiler/runtime derive "current line" from the same markers
/// (this pass runs post-parse over the whole `Stmt` tree, so there is no raw
/// source-text position left to compute it from directly).
fn walk_stmts(stmts: &[Stmt], nil_hint: bool, line: &std::cell::Cell<i64>) {
    for stmt in stmts {
        match stmt {
            // `my $d = 6, 3`: the declaration's trailing items are one list
            // headed by the declared scalar (the statement's value). That head
            // is not a user-written term, so only the trailing items can be
            // useless.
            Stmt::Expr(Expr::ArrayLiteral(elems)) if is_decl_value_head(stmts, elems) => {
                for e in &elems[1..] {
                    warn_expr_sink(e, nil_hint, line.get());
                }
            }
            _ => walk_stmt(stmt, nil_hint, line),
        }
    }
}

thread_local! {
    /// The span of the statement the last `SetLine` marker placed, until the
    /// statement after it is walked: a sunk expression that IS that statement
    /// is named by its source text, as rakudo names it.
    static STATEMENT_SPAN: std::cell::Cell<SrcSpan> = const { std::cell::Cell::new(SrcSpan::NONE) };
}

pub(super) fn walk_stmt(stmt: &Stmt, nil_hint: bool, line: &std::cell::Cell<i64>) {
    if let Stmt::SetLine(n, span) = stmt {
        line.set(*n);
        STATEMENT_SPAN.with(|s| s.set(*span));
        return;
    }
    let span = STATEMENT_SPAN.with(|s| s.replace(SrcSpan::NONE));
    match stmt {
        Stmt::SetLine(..) => {}
        Stmt::Expr(e) => warn_statement_expr(e, nil_hint, line.get(), span),
        // Bare blocks stay in sink context.
        Stmt::Block(body) => walk_stmts(body, false, line),
        // Loop / topicalizer bodies are sunk. The "(use Nil instead...)" hint is
        // only emitted for the statement-modifier form (`1 while 0`), not the
        // block form (`while 0 { 1 }`); the modifier desugaring leaves the body
        // without a leading SetLine marker.
        Stmt::For { body, .. }
        | Stmt::Given { body, .. }
        | Stmt::While { body, .. }
        | Stmt::Loop { body, .. } => {
            let body = loop_sink_body(body);
            walk_stmts(body, is_modifier_body(body), line)
        }
        // Conditionals are sunk but do not suggest `Nil`.
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            walk_stmts(then_branch, false, line);
            walk_stmts(else_branch, false, line);
        }
        // The same, for a conditional a `.AST` parse keeps in its RakuAST
        // shape (ADR-10723 Stage 2).
        Stmt::Ast(node) => match node.as_ref() {
            ParsedStmt::When { block, .. } | ParsedStmt::Default(block) => {
                walk_stmts(sunk_prefix(&block.body), false, line)
            }
            // A modified statement is sunk, except under `when`, whose
            // statement is the value it succeeds with. Only a loop or
            // topicalizer modifier alone (`1 while 0`) suggests `Nil`.
            ParsedStmt::Modified(m) => {
                if !m
                    .condition
                    .as_ref()
                    .is_some_and(|c| c.kind == ModifierKind::When)
                {
                    walk_stmt(&m.stmt, m.condition.is_none() && m.loop_.is_some(), line);
                }
            }
            // A declaration with a tail (`my $x = 1, 123`, `my $x = 1 and 2`) is
            // sunk like the plain `SyntheticBlock([head, tail])` it lowers to.
            ParsedStmt::DeclTail { head, tail } => {
                walk_stmt(head, false, line);
                match tail {
                    Expr::ArrayLiteral(elems)
                        if is_decl_value_head(std::slice::from_ref(head.as_ref()), elems) =>
                    {
                        for e in &elems[1..] {
                            warn_expr_sink(e, nil_hint, line.get());
                        }
                    }
                    _ => warn_expr_sink(tail, nil_hint, line.get()),
                }
            }
            // A declaration with sunk list items: each statement is sunk.
            ParsedStmt::CommaDecls(stmts) => walk_stmts(stmts, false, line),
            // The same for a `.=` declaration with a list (`my Int $c .= new(), 5`).
            ParsedStmt::CallAssignList { head, items } => {
                walk_stmt(head, false, line);
                for e in items {
                    warn_expr_sink(e, nil_hint, line.get());
                }
            }
            other if other.inner_stmt().is_some() => {
                if let Some(stmt) = other.inner_stmt() {
                    walk_stmt(stmt, nil_hint, line);
                }
            }
            other => {
                let is_loop = matches!(other, ParsedStmt::WhileLoop(_) | ParsedStmt::Loop(_));
                for block in other.blocks() {
                    let body = if is_loop {
                        loop_sink_body(&block.body)
                    } else {
                        &block.body[..]
                    };
                    walk_stmts(body, false, line);
                }
            }
        },
        // A `when`/`default` block's final statement is the block's *value* —
        // `when` succeeds out of the enclosing topicalizer with it — so it is not
        // sunk. Rakudo warns for `if True { 1 }` but not for `when 'a' { 1 }`,
        // and mutsu warned for both; the spurious warning fired even on a branch
        // that never ran (`when 'darwin' { skip-rest, "…" }` in a Linux-only
        // test file). Everything before the last statement is still sunk.
        Stmt::When { body, .. } | Stmt::Default(body) => walk_stmts(sunk_prefix(body), false, line),
        // Everything else (declarations, calls, say/print, assignments, and the
        // bodies of subs/classes/etc.) is not analysed for sink warnings.
        _ => {}
    }
}

/// Whether `elems` is the value list the parser appends after a scalar
/// declaration with trailing comma items (`my $d = 6, 3`): its head is the
/// variable a `VarDecl` in the same desugaring block declares.
// Cost: O(m), m = statements in `body`.
fn is_decl_value_head(body: &[Stmt], elems: &[Expr]) -> bool {
    decl_value_head(elems).is_some_and(|head| body.iter().any(|s| declared_scalar(s) == Some(head)))
}

/// The variable heading a declaration's value list (see [`is_decl_value_head`]).
// Cost: O(1).
pub(super) fn decl_value_head(elems: &[Expr]) -> Option<&str> {
    match elems.first() {
        Some(Expr::Var(head)) => Some(head.as_str()),
        _ => None,
    }
}

/// The variable a `VarDecl` statement declares.
// Cost: O(1).
pub(super) fn declared_scalar(stmt: &Stmt) -> Option<&str> {
    match stmt {
        Stmt::VarDecl { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

/// The statements of `body` that are genuinely in sink context when the block's
/// final statement is its value: everything up to (and not including) the last
/// real statement. Trailing bookkeeping markers (`SetLine`) are not statements
/// and are kept out of the reckoning.
fn sunk_prefix(body: &[Stmt]) -> &[Stmt] {
    match crate::ast::last_value_stmt_index(body, crate::ast::TailSkip::Markers) {
        Some(last) => &body[..last],
        None => body,
    }
}

/// The part of a loop body that is sunk. A `KEEP` / `UNDO` phaser reads the
/// iteration's value, so with one the body's last statement is that value, not
/// sunk (#16616).
fn loop_sink_body(body: &[Stmt]) -> &[Stmt] {
    let reads_value = body.iter().any(|s| {
        matches!(
            s,
            Stmt::Phaser {
                kind: crate::ast::PhaserKind::Keep | crate::ast::PhaserKind::Undo,
                ..
            }
        )
    });
    if reads_value { sunk_prefix(body) } else { body }
}

/// A loop/topicalizer body produced by a statement modifier (`1 while 0`) is a
/// single statement with no leading `SetLine` marker, unlike a brace block.
fn is_modifier_body(body: &[Stmt]) -> bool {
    !matches!(body.first(), Some(Stmt::SetLine(..)))
}

/// Warn for an expression evaluated in sink context. Comma lists distribute the
/// sink to each element.
pub(super) fn warn_expr_sink(expr: &Expr, nil_hint: bool, line: i64) {
    match expr {
        // A parenthesized expression is never reported, whatever it holds
        // (`(5);`, `(1, 2);`, `(1 + 2);` are silent on rakudo); only an
        // empty `()` is still a useless value. A bare comma list without
        // the parentheses distributes the sink below (`(1, 2), 3;` reports
        // the 3 alone).
        Expr::Grouped(inner) => {
            if matches!(inner.as_ref(), Expr::ArrayLiteral(elems) if elems.is_empty()) {
                emit("()".to_string(), nil_hint, line);
            }
        }
        // A term whose spelling the parse kept (`<42i>`): the wrapper only
        // records how it was written, never what it is.
        Expr::Spelled(spelled) => warn_expr_sink(&spelled.expr, nil_hint, line),
        Expr::DoBlock {
            body,
            origin: DoBlockOrigin::SourceBlock,
            ..
        } => {
            if let Some(last) =
                crate::ast::last_value_stmt_index(body, crate::ast::TailSkip::Markers)
            {
                let current_line = std::cell::Cell::new(line);
                for stmt in &body[..last] {
                    if let Stmt::SetLine(n, _) = stmt {
                        current_line.set(*n);
                    }
                }
                walk_stmt(&body[last], nil_hint, &current_line);
            }
        }
        // A bare comma list `1, 2` distributes sink to each element. An empty
        // `()` is itself a useless value.
        Expr::ArrayLiteral(elems) => {
            if elems.is_empty() {
                emit("()".to_string(), nil_hint, line);
            } else {
                for e in elems {
                    warn_expr_sink(e, nil_hint, line);
                }
            }
        }
        _ => {
            if let Some(desc) = describe_useless(expr) {
                emit(desc, nil_hint, line);
            }
        }
    }
}

/// [`warn_expr_sink`] for an expression that is a whole statement, `span`
/// being the statement's: an operator expression is named by its source text
/// (`Useless use of 1  +   2`), as rakudo names it, and the worry points at it.
// Cost: O(n), n = the size of the expression (plus the statement's text).
fn warn_statement_expr(expr: &Expr, nil_hint: bool, line: i64, span: SrcSpan) {
    // A list, a parenthesized expression and a `do` block distribute or
    // suppress the sink; only a single term or operator is reported here.
    if matches!(
        expr,
        Expr::ArrayLiteral(_) | Expr::Grouped(_) | Expr::Spelled(_) | Expr::DoBlock { .. }
    ) {
        warn_expr_sink(expr, nil_hint, line);
        return;
    }
    let Some(desc) = describe_useless(expr) else {
        return;
    };
    let desc = if matches!(expr, Expr::Binary { .. } | Expr::Unary { .. }) {
        super::primary::span_text::span_text(span).unwrap_or(desc)
    } else {
        desc
    };
    emit_at(desc, nil_hint, line, Some(span.from));
}

fn emit(desc: String, nil_hint: bool, line: i64) {
    emit_at(desc, nil_hint, line, None);
}

/// Report "Useless use of `desc` in sink context" as one of rakudo's
/// "Potential difficulties", pointing at unit offset `at` when it is known.
// Cost: O(l), l = the length of the pointed-at line.
fn emit_at(desc: String, nil_hint: bool, line: i64, at: Option<u32>) {
    let suffix = if nil_hint {
        " (use Nil instead to suppress this warning)"
    } else {
        ""
    };
    // A multi-line expression keeps its line breaks, indented like the rest
    // of the worry.
    let desc = desc.replace('\n', "\n    ");
    super::add_parse_worry(
        format!("Useless use of {} in sink context{}", desc, suffix),
        line,
        at.and_then(super::primary::span_text::here_pointer),
    );
}
