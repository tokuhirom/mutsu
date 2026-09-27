//! The `"..."` qq thunks (`crate::regex_qq_atoms`) of a `token`/`rule`/`regex`
//! declaration, made live while a grammar runs the rule.
//!
//! A declaration carries its thunks on its body's regex value, under the
//! `MetaNs::RegexQq` keys (`exec_register_token_decl_op`,
//! `Interpreter::token_body_with_qq_thunks`). Matching that value directly
//! (`$s ~~ &tok`) installs them with the rest of its scope. A grammar does not
//! match the value, though: it resolves the rule by name, re-parses its
//! pattern text and matches the result. So the thunks run where the rule's
//! other per-invocation bindings are made — the resolve-and-match window of
//! [`Interpreter::install_subrule_dynamic_params_named`] — and their results
//! sit in `env` under the same keys, where the interpolation pre-pass splices
//! them in as literals while the rule's pattern is parsed.

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, Ordering};

use super::super::*;
use super::regex_dynparams::SavedDynParams;
use crate::runtime::meta_ns::MetaNs;

/// Set the first time a rule whose body carries a qq thunk is registered, so
/// every other program skips the per-subrule lookup behind one relaxed load.
static ANY_TOKEN_QQ_THUNK: AtomicBool = AtomicBool::new(false);

/// The (key, thunk) pairs a rule's candidates carry.
type QqThunks = Arc<Vec<(String, Value)>>;

thread_local! {
    /// (pkg, rule name) → its candidates' qq thunks, under the
    /// `TOKEN_DEFS_GEN` the entry was built for (the invalidation discipline
    /// of the other per-rule caches in this module).
    static TOKEN_QQ_THUNKS: RefCell<rustc_hash::FxHashMap<(Symbol, String), (u64, QqThunks)>> =
        RefCell::new(rustc_hash::FxHashMap::default());
}

/// The qq thunks on a rule body's regex value.
fn body_qq_thunks(body: &[Stmt]) -> impl Iterator<Item = (String, Value)> {
    let scope = body.iter().rev().find_map(|stmt| match stmt {
        Stmt::Expr(Expr::Literal(v)) | Stmt::Return(Expr::Literal(v)) => v.regex_closure_scope(),
        _ => None,
    });
    let prefix = MetaNs::RegexQq.prefix();
    scope.into_iter().flat_map(move |scope| {
        scope
            .iter()
            .filter(|(k, _)| k.starts_with(prefix))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<Vec<_>>()
    })
}

/// Note a freshly registered rule's body, arming [`ANY_TOKEN_QQ_THUNK`] when
/// it carries a qq thunk.
// Cost: O(s), s = the body regex's captured scope size.
pub(crate) fn note_token_def_qq_thunks(body: &[Stmt]) {
    if !ANY_TOKEN_QQ_THUNK.load(Ordering::Relaxed) && body_qq_thunks(body).next().is_some() {
        ANY_TOKEN_QQ_THUNK.store(true, Ordering::Relaxed);
    }
}

impl Interpreter {
    fn subrule_qq_thunks(&self, name: &str, pkg: Symbol) -> QqThunks {
        let tok_gen =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        let key = (pkg, name.to_string());
        if let Some(hit) = TOKEN_QQ_THUNKS.with(|c| {
            c.borrow()
                .get(&key)
                .filter(|(cached_gen, _)| *cached_gen == tok_gen)
                .map(|(_, v)| Arc::clone(v))
        }) {
            return hit;
        }
        let thunks: Vec<(String, Value)> = self
            .resolve_token_defs_in_pkg(name, pkg)
            .iter()
            .flat_map(|def| body_qq_thunks(&def.body))
            .collect();
        let arc = Arc::new(thunks);
        TOKEN_QQ_THUNKS.with(|c| {
            c.borrow_mut().insert(key, (tok_gen, Arc::clone(&arc)));
        });
        arc
    }

    /// Whether rule `name`'s candidates carry a qq thunk. Such a body's
    /// parse depends on the thunks' results, which exist only inside the
    /// rule's resolve-and-match window, so a declarative (LTM) measurement
    /// outside it ends at the call — as Rakudo's NFA ends at the atom, which
    /// it compiles to code.
    // Cost: O(1) amortized (cached per (pkg, name)); one relaxed load when no
    // rule has any.
    pub(in crate::runtime::regex) fn subrule_has_qq_thunks(&self, name: &str, pkg: Symbol) -> bool {
        ANY_TOKEN_QQ_THUNK.load(Ordering::Relaxed) && !self.subrule_qq_thunks(name, pkg).is_empty()
    }

    /// Run the qq thunks of rule `name`'s candidates and bind each result
    /// under its key for the rule's resolve-and-match window, appending the
    /// shadowed bindings to `prior` (restored by
    /// [`Self::restore_subrule_dynamic_params`]). A thunk that throws is left
    /// out, and the pre-pass falls back to its own reading of the atom.
    // Cost: O(t) plus the thunks' own runs, t = the rule's thunks (a cached
    // per-(pkg, name) lookup; one relaxed load when no rule has any).
    pub(crate) fn install_subrule_qq_thunks(
        &mut self,
        name: &str,
        pkg: Symbol,
        prior: Option<SavedDynParams>,
    ) -> Option<SavedDynParams> {
        if !ANY_TOKEN_QQ_THUNK.load(Ordering::Relaxed) {
            return prior;
        }
        let thunks = self.subrule_qq_thunks(name, pkg);
        if thunks.is_empty() {
            return prior;
        }
        let mut saved = prior.unwrap_or_default();
        for (key, thunk) in thunks.iter() {
            let Ok(result) = self.call_sub_value(thunk.clone(), Vec::new(), false) else {
                continue;
            };
            saved.push((key.clone(), self.env.get(key).cloned()));
            self.env
                .insert(key.clone(), Value::str(result.to_string_value()));
        }
        (!saved.is_empty()).then_some(saved)
    }
}
