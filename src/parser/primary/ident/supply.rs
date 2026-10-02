use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut, walk_stmts_mut};
use crate::parser::stmt::simple::is_user_declared_sub;
use crate::regex_tree::RegexNode;
use crate::symbol::Symbol;

static SUPPLY_EMITTER_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

pub(crate) fn supply_method_call(body: Vec<Stmt>) -> Expr {
    // ADR-0048 Phase 2: `supply {}` does not take a signature in raku. Unlike
    // `start`/`sink` (which wrap their body via `make_anon_sub`, consuming a
    // stray placeholder as that closure's own parameter), `supply {}` always
    // builds its own `Expr::Lambda` below with a fixed (non-placeholder)
    // parameter — the emitter — so a `$^c` written in the source body is
    // still literally present here. Detect it directly and hand back a
    // `DoBlock`, whose compiler already rejects a stray placeholder with
    // `X::Placeholder::Block`, instead of building the on-demand Lambda
    // (which would otherwise silently let `$^c` read `Any` at runtime, since
    // nothing ever binds it).
    //
    // `%_` (a method's implicit `*%_` for leftover named args) is exempted
    // from this parse-time check even outside a method: a legitimate
    // `supply { ...; %_ ... }` inside a method body (e.g. DBIish-style
    // `method connect-or-skip { supply { ...|%_... } }`) must still build the
    // real on-demand Lambda, not degrade to an eagerly-run `DoBlock` that
    // breaks `supply`'s async semantics. Whether `%_` is genuinely valid here
    // depends on `self.lexically_in_method`, which does not exist yet at
    // parse time -- `compile_do_block_expr`'s own `%_`-in-method check is the
    // real enforcement for the (rare) non-method case where this exemption is
    // too permissive, since a stray `Expr::DoBlock` produced by *this* check
    // for a genuine `$^`/`@_` placeholder still reaches that check normally.
    // `@_` is NOT exempted here -- only a METHOD gets an implicit `*%_`,
    // never `*@_`.
    if crate::ast::collect_unattached_placeholders(&body)
        .into_iter()
        .any(|ph| ph != "%_")
    {
        return Expr::desugar_block(body);
    }
    // Each `supply { ... }` block gets a UNIQUE emitter variable name. The
    // emitter is bound as the on-demand lambda's parameter and `emit` is
    // rewritten to `$emitter.emit(...)`. A shared name would be clobbered when
    // supply blocks nest at runtime: a chained transform
    // `supply { whenever (supply { ... }) { emit ... } }` runs the inner block
    // inside the outer's frame, and with one shared name the outer's `emit`
    // would resolve to the inner emitter (an infinite emit loop). A per-parse
    // unique name keeps each block's `emit` bound to its own emitter.
    let id = SUPPLY_EMITTER_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let emitter_name = format!("{}{id}", crate::parser::SUPPLY_EMITTER_PREFIX);
    let lowered_body = rewrite_supply_body(body, &emitter_name);
    Expr::MethodCall {
        target: Box::new(Expr::BareWord("Supply".to_string())),
        name: Symbol::intern("on-demand"),
        args: vec![Expr::Lambda {
            param: emitter_name,
            body: lowered_body,
            is_whatever_code: false,
            param_sigilless: false,
        }],
        modifier: None,
        quoted: false,
    }
}

/// Rewrites a `supply { ... }` body for the on-demand lambda whose parameter
/// is `emitter_name`: `emit`/`done` become calls on that emitter, and a CLOSE
/// phaser becomes its registration, hoisted to the head of its block.
// Cost: O(n), n = size of the body's subtree.
fn rewrite_supply_body(mut stmts: Vec<Stmt>, emitter_name: &str) -> Vec<Stmt> {
    SupplyBody {
        emitter: emitter_name,
    }
    .visit_stmts_mut(&mut stmts);
    stmts
}

/// The supply-body rewrite, on the mutable AST visitor (ADR-10499). It covers
/// everything that runs in the supply block's own frame: statements,
/// conditions, operands, inline and `do` blocks, phaser bodies and `whenever`
/// bodies. It stops where code runs elsewhere:
///
/// - a closure the body merely *builds* (`AnonSub`, `Lambda`,
///   `AnonSubParams`, a `gather`) runs wherever it is later called, which is
///   what the dynamic emitter stack is for;
/// - a nested routine or type declaration. TODO: compile to bytecode /
///   capture. `emit`/`done` inside a nested `my sub` defined within the supply
///   body (`supply { my sub relay($s) { whenever $s { emit … } }; relay(…) }`,
///   e.g. IO::Notification::Recursive) should forward to this supply's
///   emitter, but rewriting the sub body to `$emitter.emit(...)` surfaces a
///   closure-capture gap (the nested sub does not capture the on-demand
///   Lambda's emitter parameter), so it is left unrewritten for now — such
///   code parses (the whenever-scope check accepts it) but its nested-sub
///   `emit` is a runtime no-op;
/// - a `try` body (see the hook);
/// - a regex tree, a copy of the regex a match runs (#10550).
struct SupplyBody<'a> {
    emitter: &'a str,
}

impl SupplyBody<'_> {
    /// `$emitter.NAME(ARGS)`.
    fn emitter_call(&self, name: &str, args: Vec<Expr>) -> Expr {
        Expr::MethodCall {
            target: Box::new(Expr::Var(self.emitter.to_string())),
            name: Symbol::intern(name),
            args,
            modifier: None,
            quoted: false,
        }
    }
}

fn is_builtin_emit(name: &Symbol) -> bool {
    name.resolve().as_str() == "emit" && !is_user_declared_sub("emit")
}

impl VisitMut for SupplyBody<'_> {
    // Phasers are set up at block entry, not when control textually reaches
    // them. Hoist the CLOSE phaser registrations of each block to its front so
    // a CLOSE that appears after a (potentially non-terminating) loop is still
    // registered before the loop runs — e.g.
    //   supply { until my $done { emit(...) } CLOSE { $done = True } }
    // relies on the CLOSE phaser being able to break the loop.
    //
    // A registration is hoisted no further than the block's own `my`
    // declarations its body reads, though: the phaser closes over those
    // lexicals, and a closure built before `my $id = ...` runs would capture
    // the not-yet-initialised variable (#10832).
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        walk_stmts_mut(self, body);
        *body = hoist_close_registrations(std::mem::take(body));
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            // `.emit` (topic method call) inside a supply block is
            // `$emitter.emit($_)` — emit the current topic value.
            Stmt::Expr(Expr::MethodCall {
                target, name, args, ..
            }) if matches!(target.as_ref(), Expr::Var(n) if n == "_")
                && name.resolve().as_str() == "emit"
                && args.is_empty()
                && !is_user_declared_sub("emit") =>
            {
                *stmt = Stmt::Expr(self.emitter_call("emit", vec![Expr::Var("_".to_string())]));
            }
            // Statement-form `emit ARGS;` becomes `$emitter.emit(ARGS)`.
            Stmt::Call { name, args } if is_builtin_emit(name) => {
                let mut positional: Vec<Expr> = std::mem::take(args)
                    .into_iter()
                    .filter_map(|arg| match arg {
                        crate::ast::CallArg::Positional(expr) => Some(expr),
                        _ => None,
                    })
                    .collect();
                for e in &mut positional {
                    walk_expr_mut(self, e);
                }
                *stmt = Stmt::Expr(self.emitter_call("emit", positional));
            }
            Stmt::ReactDone => {
                *stmt = Stmt::SyntheticBlock(vec![
                    Stmt::Expr(self.emitter_call("done", Vec::new())),
                    // Not `Stmt::Return`: a routine-return signal raised from a
                    // closure created inside a *method* gets stamped with that
                    // method's callable id and escapes past the (long-returned)
                    // method frame to the tap as an uncaught `CX::Return` —
                    // see todo/tickets/supply-done-in-method-supply-block-escapes-as-cx-return.md.
                    // `SupplyBodyDone` is always caught at the raising
                    // closure's own frame boundary regardless of nesting.
                    Stmt::SupplyBodyDone,
                ]);
            }
            // A CLOSE phaser in a `supply { ... }` block registers its body as
            // a close callback on the emitter, to run when the tap is closed
            // or the supply terminates. Rewrite it to a registration call so it
            // survives as a value (a bare phaser compiles to a no-op).
            Stmt::Phaser {
                kind: crate::ast::PhaserKind::Close,
                body,
                ..
            } => {
                let mut body = std::mem::take(body);
                self.visit_stmts_mut(&mut body);
                let register = vec![make_anon_sub(body)];
                *stmt = Stmt::Expr(self.emitter_call("__mutsu_register_close_phaser", register));
            }
            // See the type doc: a nested routine or type keeps its `emit`s.
            Stmt::SubDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::Package { .. }
            | Stmt::AugmentClass { .. } => {}
            _ => walk_stmt_mut(self, stmt),
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        match expr {
            // `emit` within an expression — the ternary
            // `$x ~~ T ?? emit($x) !! die "…"` that Cro's middleware role uses.
            // Leaving it bare fell back to the dynamic emitter stack, which in
            // a pipeline is a neighbouring stage's emitter.
            Expr::Call { name, .. } if is_builtin_emit(name) => {
                walk_expr_mut(self, expr);
                if let Expr::Call { args, .. } = expr {
                    let args = std::mem::take(args);
                    *expr = self.emitter_call("emit", args);
                }
            }
            // See the type doc: these run where they are later called.
            Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Gather(_) => {}
            // A rewritten `done` ends with `SupplyBodyDone`, which the `try`'s
            // own frame would catch, so the supply would keep running; a bare
            // `done` in a `try` reaches the drive loop through the dynamic
            // path instead.
            Expr::Try { .. } => {}
            _ => walk_expr_mut(self, expr),
        }
    }

    fn visit_regex_node_mut(&mut self, _node: &mut RegexNode) {}
}

/// Moves each CLOSE registration up to the earliest point of `stmts` that
/// follows every top-level declaration of a variable its body reads (the head
/// of the block when it reads none), keeping the registrations in source
/// order.
// Cost: O(n * d), n = size of the registrations' subtrees plus the statement
// count, d = names declared at the top level of `stmts`.
fn hoist_close_registrations(stmts: Vec<Stmt>) -> Vec<Stmt> {
    let mut rest: Vec<Stmt> = Vec::with_capacity(stmts.len());
    // (number of `rest` statements the registration goes after, registration)
    let mut closes: Vec<(usize, Stmt)> = Vec::new();
    for stmt in stmts {
        if !is_close_registration(&stmt) {
            rest.push(stmt);
            continue;
        }
        let mut reads = VarReads::default();
        crate::ast_visit::Visit::visit_stmt(&mut reads, &stmt);
        let after_decl = rest
            .iter()
            .rposition(|s| declared_names(s).iter().any(|n| reads.names.contains(*n)))
            .map_or(0, |i| i + 1);
        let floor = closes.last().map_or(0, |(pos, _)| *pos);
        closes.push((after_decl.max(floor), stmt));
    }
    let mut out = Vec::with_capacity(rest.len() + closes.len());
    let mut closes = closes.into_iter().peekable();
    for (i, stmt) in rest.into_iter().enumerate() {
        while let Some((_, close)) = closes.next_if(|(pos, _)| *pos == i) {
            out.push(close);
        }
        out.push(stmt);
    }
    out.extend(closes.map(|(_, close)| close));
    out
}

/// The variables a statement declares at its own level, spelled the way
/// [`VarReads`] records a read (a scalar without its sigil).
fn declared_names(stmt: &Stmt) -> Vec<&str> {
    match stmt {
        Stmt::VarDecl { name, .. } => vec![name.as_str()],
        Stmt::SyntheticBlock(inner) => inner
            .iter()
            .filter_map(|s| match s {
                Stmt::VarDecl { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Every variable a subtree reads, in `VarDecl` spelling: `$x` as `x`, `@a`
/// as `@a`, `%h` as `%h`.
#[derive(Default)]
struct VarReads {
    names: std::collections::HashSet<String>,
}

impl<'ast> crate::ast_visit::Visit<'ast> for VarReads {
    fn visit_name(&mut self, name: &str, kind: crate::ast_visit::NameKind) {
        use crate::ast_visit::NameKind;
        let spelled = match kind {
            NameKind::Var => name.to_string(),
            NameKind::ArrayVar => format!("@{name}"),
            NameKind::HashVar => format!("%{name}"),
            _ => return,
        };
        self.names.insert(spelled);
    }
}

/// True if `stmt` is the registration call a CLOSE phaser is lowered to.
fn is_close_registration(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Expr(Expr::MethodCall { name, .. })
            if name.resolve().as_str() == "__mutsu_register_close_phaser"
    )
}
