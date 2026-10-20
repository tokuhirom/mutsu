//! The `Parameter` object a custom parameter trait mutated, kept per declared
//! parameter.
//!
//! Rakudo builds one `Parameter` per declared parameter, at compile time, and
//! a `trait_mod:<is>(Parameter ...)` candidate mutates that very object
//! (`$param does HasMCPType($mcp-type)`). Every later `.signature.params`
//! hands out the same object, carrying the role and its attribute values.
//!
//! mutsu materializes a fresh `Parameter` from the declaration on every
//! access, so the object the trait ran on used to die with the call. This slot
//! keeps it. It lives on the declaration (`ParamDef`) and travels into every
//! `SigParam` converted from it. Clones share the slot, so the copy a routine
//! registers and the signature built from it see what the trait left.

use super::Value;
use std::sync::{Arc, Mutex};

/// A shared, initially empty home for one declared parameter's
/// trait-mutated `Parameter` object.
#[derive(Clone, Default)]
pub(crate) struct ParamTraitMeta(Arc<Mutex<Option<Value>>>);

impl ParamTraitMeta {
    /// The object the parameter's custom traits left, if any ran.
    // Cost: O(1), one lock.
    pub(crate) fn get(&self) -> Option<Value> {
        self.0.lock().unwrap_or_else(|e| e.into_inner()).clone()
    }

    /// Record the object the parameter's custom traits ran on. A
    /// re-registration (a closure built again) replaces the earlier one.
    // Cost: O(1), one lock.
    pub(crate) fn set(&self, object: Value) {
        *self.0.lock().unwrap_or_else(|e| e.into_inner()) = Some(object);
    }
}

impl std::fmt::Debug for ParamTraitMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ParamTraitMeta")
    }
}

/// Not part of the declaration's identity: two declarations that differ only
/// in what a trait did at run time are the same declaration.
impl std::hash::Hash for ParamTraitMeta {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}
