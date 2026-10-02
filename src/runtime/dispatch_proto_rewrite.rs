use super::*;
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut};
use crate::compiler::scope_scan::is_code_object;

fn dispatch_call() -> Expr {
    Expr::Call {
        name: Symbol::intern("__PROTO_DISPATCH__"),
        args: Vec::new(),
    }
}

/// `{*}`: a block whose only statement (besides line markers) is a bare `*`.
// Cost: O(n), n = statements of `body`.
pub(crate) fn is_only_star_block(body: &[Stmt]) -> bool {
    let mut stmts = body.iter().filter(|s| !s.is_marker());
    matches!(
        (stmts.next(), stmts.next()),
        (Some(Stmt::Expr(Expr::Whatever)), None)
    )
}

/// Rewrites every `{*}` of a proto body, in any position: rakudo dispatches
/// from a `{*}` in a call argument, a `say`, a `given`/`when` or an
/// interpolation too. A nested routine or type declaration has its own `{*}`.
///
/// A `{*}` in a closure *inside a method call's arguments* (`.map({ {*} })`) is
/// not a dispatch point: rakudo looks the dispatcher up through the closure's
/// callers, and the method the closure is handed to -- a builtin like `map`, a
/// user method, a `multi` -- is the nearest routine that has one, so the `{*}`
/// evaluates to `Nil` instead of reaching the proto. (A closure the proto body
/// calls itself, `my &c = { {*} }; c()`, or hands to a plain `sub`, has no such
/// routine in between and dispatches.) A `{*}` that is itself an argument
/// (`.map({*})`) is evaluated at the call, as any argument is, and dispatches.
// TODO: rakudo decides this from the callers at run time, so a closure kept in
// a variable and a routine the body calls behave differently than this static
// rule says. See #10746.
#[derive(Default)]
struct ProtoDispatch {
    /// How many method-call argument lists enclose the node being walked.
    method_args: u32,
    /// How many closures enclose it that sit in such an argument list.
    callbacks: u32,
}

impl ProtoDispatch {
    /// What a `{*}` becomes here: the dispatch, or `Nil` inside a callback.
    fn star(&self) -> Expr {
        if self.callbacks > 0 {
            Expr::Literal(Value::NIL)
        } else {
            dispatch_call()
        }
    }
}

impl VisitMut for ProtoDispatch {
    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(Expr::Whatever) => *stmt = Stmt::Expr(self.star()),
            // A `{*}` in a nested routine or type body is that routine's.
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
            Expr::AnonSub { body, .. } if is_only_star_block(body) => *expr = self.star(),
            Expr::MethodCall { args, .. }
            | Expr::HyperMethodCall { args, .. }
            | Expr::DynamicMethodCall { args, .. }
            | Expr::HyperMethodCallDynamic { args, .. } => {
                // The invocant and the rest of the call are ordinary
                // expressions; the arguments are walked as callback context.
                let mut args = std::mem::take(args);
                walk_expr_mut(self, expr);
                self.method_args += 1;
                for arg in &mut args {
                    self.visit_expr_mut(arg);
                }
                self.method_args -= 1;
                if let Expr::MethodCall { args: slot, .. }
                | Expr::HyperMethodCall { args: slot, .. }
                | Expr::DynamicMethodCall { args: slot, .. }
                | Expr::HyperMethodCallDynamic { args: slot, .. } = expr
                {
                    *slot = args;
                }
            }
            _ if self.method_args > 0 && is_code_object(expr) => {
                self.callbacks += 1;
                walk_expr_mut(self, expr);
                self.callbacks -= 1;
            }
            _ => walk_expr_mut(self, expr),
        }
    }
}

impl Interpreter {
    /// A proto body statement with every `{*}` dispatch point replaced by the
    /// `__PROTO_DISPATCH__` call: a clone rewritten in place by
    /// [`ProtoDispatch`] (ADR-10499).
    // Cost: O(n), n = size of `stmt`'s subtree.
    pub(super) fn rewrite_proto_dispatch_stmt(stmt: &Stmt) -> Stmt {
        let mut out = stmt.clone();
        ProtoDispatch::default().visit_stmt_mut(&mut out);
        out
    }

    /// Restore the caller's env after a proto body ran, carrying over the new
    /// value of every caller-visible name the body (or the multi it dispatched
    /// to) rebound.
    ///
    /// `Env::keys` exposes only the env's own overlay tier, not the names it
    /// reaches through its parent chain. So the carry-over walks BOTH overlays:
    /// the saved one (names the caller's tier owns) and the current one, whose
    /// overlay also holds every name the body wrote -- including one that the
    /// caller only sees through a parent tier. Walking only the saved overlay
    /// dropped such a write: `{ f(@a) }` with an explicit `proto f` whose multi
    /// did `@array does R` left `@a` un-mixed, because `@a` belongs to the
    /// enclosing block's parent tier (#9336).
    // Cost: O(k), k = keys in the saved and current overlays.
    pub(super) fn restore_env_preserving_existing(&mut self, saved_env: &Env, params: &[String]) {
        let current = self.env.clone();
        let mut restored = saved_env.clone();
        let skip = |key: &Symbol| {
            params.iter().any(|p| *key == p.as_str()) || *key == "_" || *key == "@_" || *key == "%_"
        };
        for key in saved_env.keys() {
            if skip(key) {
                continue;
            }
            if let Some(v) = current.get_sym(*key) {
                restored.insert_sym(*key, v.clone());
            }
        }
        for (key, v) in current.iter() {
            if skip(key) {
                continue;
            }
            // A name the body declared itself is its own lexical, not the
            // caller's: only a name the caller can already see is carried.
            if saved_env.contains_key_sym(*key) {
                restored.insert_sym(*key, v.clone());
            }
        }
        self.env = restored;
    }
}
