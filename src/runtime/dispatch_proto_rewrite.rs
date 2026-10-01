use super::*;
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut};

fn dispatch_call() -> Expr {
    Expr::Call {
        name: Symbol::intern("__PROTO_DISPATCH__"),
        args: Vec::new(),
    }
}

/// `{*}`: a block whose only statement (besides line markers) is a bare `*`.
fn is_only_star_block(body: &[Stmt]) -> bool {
    let mut stmts = body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    matches!(
        (stmts.next(), stmts.next()),
        (Some(Stmt::Expr(Expr::Whatever)), None)
    )
}

/// Rewrites every `{*}` of a proto body, in any position: rakudo dispatches
/// from a `{*}` in a call argument, a `say`, a `given`/`when` or an
/// interpolation too. A nested routine or type declaration has its own `{*}`.
struct ProtoDispatch;

impl VisitMut for ProtoDispatch {
    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(Expr::Whatever) => *stmt = Stmt::Expr(dispatch_call()),
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
            Expr::AnonSub { body, .. } if is_only_star_block(body) => *expr = dispatch_call(),
            // TODO: rewrite method-call arguments too, as rakudo does
            // (`.map({ {*} })`). A callback a builtin method invokes runs
            // outside the proto's dispatch context here, so the rewritten
            // `{*}` would die "used outside proto"; it is left a block, as
            // before the port. See #10555.
            Expr::MethodCall { args, .. }
            | Expr::HyperMethodCall { args, .. }
            | Expr::DynamicMethodCall { args, .. }
            | Expr::HyperMethodCallDynamic { args, .. } => {
                let args = std::mem::take(args);
                walk_expr_mut(self, expr);
                if let Expr::MethodCall { args: slot, .. }
                | Expr::HyperMethodCall { args: slot, .. }
                | Expr::DynamicMethodCall { args: slot, .. }
                | Expr::HyperMethodCallDynamic { args: slot, .. } = expr
                {
                    *slot = args;
                }
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
        ProtoDispatch.visit_stmt_mut(&mut out);
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
