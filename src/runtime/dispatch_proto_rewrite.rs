use super::*;

/// A routine body `{*}` (or `{ * }`): only a bare `*` besides line markers.
/// The onlystar *term* `{*}` written inside a body is parsed straight into a
/// `__PROTO_DISPATCH__()` call instead (#10746).
// Cost: O(n), n = statements of `body`.
pub(crate) fn is_only_star_block(body: &[Stmt]) -> bool {
    let mut stmts = body.iter().filter(|s| !s.is_marker());
    matches!(
        (stmts.next(), stmts.next()),
        (Some(Stmt::Expr(Expr::Whatever)), None)
    )
}

impl Interpreter {
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
