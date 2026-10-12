//! A scope enclosing a running frame, as `nqp::ctxouter` and a computed
//! `nqp::getlexouter` see it (ADR-12529 phase 3; built by
//! `Env::enclosing_scopes`).

use super::Env;
use crate::symbol::Symbol;
use crate::value::Value;
use std::sync::Arc;

/// One scope enclosing a running frame, as `nqp::ctxouter` and a computed
/// `nqp::getlexouter` see it.
#[derive(Clone)]
pub(crate) enum OuterScope {
    /// The program scope (or another env): its tiers, callers excluded.
    Env(Env),
    /// A closure's capture: the scope that created it.
    Capture(Arc<crate::env_capture_view::CaptureView>),
}

impl OuterScope {
    /// The scope as a flat env.
    // Cost: O(n), n = entries visible in the scope.
    pub(crate) fn to_env(&self) -> Env {
        match self {
            OuterScope::Env(env) => env.flattened(),
            OuterScope::Capture(view) => {
                let mut env = Env::default();
                for (k, v) in view.iter() {
                    env.insert_sym(*k, v.clone());
                }
                env
            }
        }
    }

    /// A lexical by its env key, `None` when the scope has no such name.
    // Cost: O(d), d = chain tiers (an env) or capture layers.
    pub(crate) fn get(&self, key: &str) -> Option<Value> {
        match self {
            OuterScope::Env(env) => env.get(key).cloned(),
            OuterScope::Capture(view) => view.get(&Symbol::intern(key)).cloned(),
        }
    }
}
