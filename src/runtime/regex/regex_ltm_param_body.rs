//! The body an LTM measurement walks for a call with arguments (ADR-12917).
//!
//! Measuring `<block($indent, 0)>` ignores the arguments (ADR-0127 §2.2), so
//! it used to resolve the callee through the argument-less candidate path. For
//! a rule whose parameters are bound at run time (`regex_rule_params`) that
//! path hands out the body *without* its parameter declaration, so `$indent`
//! was spliced in from whatever the env held at measurement time, and the body
//! was re-interpolated on every measurement: ~2,100 times per `load-yaml` of
//! the benchmark document.
//!
//! The measurement now walks the body every call of the rule runs — the one
//! with the declaration block, where `$indent` is a match-time `VarInterp`
//! and so ends the declarative prefix, as rakudo's interpolation does. That
//! body is memoized per (package, rule) under the token generation, but only
//! when the interpolation pre-pass leaves its text unchanged: then nothing in
//! it was read from the env, and the parse is a function of the text alone.

use std::cell::RefCell;
use std::sync::Arc;

use super::super::*;

/// `(pkg, rule name) -> (token generation, the body and its package)`.
type LtmParamBodyCache =
    rustc_hash::FxHashMap<(Symbol, Symbol), (u64, Option<(Arc<RegexPattern>, Symbol)>)>;

thread_local! {
    static LTM_PARAM_BODY: RefCell<LtmParamBodyCache> =
        RefCell::new(rustc_hash::FxHashMap::default());
}

impl Interpreter {
    /// The body an LTM measurement of a call with arguments to `name` (from
    /// `pkg`) walks: the rule's value-independent body when its parameters
    /// are bound at run time, else `None` and the caller keeps the
    /// argument-less candidate path.
    // Cost: O(1) expected on a memo hit; a miss resolves and interpolates the
    // body, O(b), b = its length, plus its parse (cached by its text).
    pub(super) fn ltm_param_rule_body(
        &mut self,
        name: &str,
        name_sym: Symbol,
        pkg: Symbol,
    ) -> Option<(Arc<RegexPattern>, Symbol)> {
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        let key = (pkg, name_sym);
        if let Some(hit) = LTM_PARAM_BODY.with(|c| {
            c.borrow()
                .get(&key)
                .filter(|(cached, _)| *cached == generation)
                .map(|(_, body)| body.clone())
        }) {
            return hit;
        }
        let (marked, sub_pkg) = self.runtime_param_rule_text(name, pkg)??;
        let value_independent = self
            .interpolate_regex_scalars(&marked)
            .is_ok_and(|interpolated| interpolated == marked);
        let body = self
            .parse_candidate_in_pkg(&marked, sub_pkg)
            .map(|parsed| (parsed, sub_pkg));
        if value_independent {
            LTM_PARAM_BODY.with(|c| {
                c.borrow_mut().insert(key, (generation, body.clone()));
            });
        }
        body
    }
}
