//! `.raku` of a multi dispatcher.
//!
//! A routine with `multi` candidates is, as a *value*, its dispatcher: the
//! `proto` the source declared, or the one Rakudo generates when it declared
//! none. Rakudo spells it as that proto, never as a candidate or a bare body:
//!
//! ```raku
//! proto sub hs (|) { * }; multi sub hs(@a) { 1 }
//! say &hs.raku;    # proto sub hs (|) {*}
//! ```
//!
//! A declared proto shows its own signature; a protoless multi shows the
//! generated proto's, `(;; Mu |)`.

use super::*;
use crate::symbol::Symbol;
use crate::value::signature::{make_signature_value, param_defs_to_sig_info};

impl Interpreter {
    /// The `.raku` of the dispatcher that `package`/`name` names, or `None`
    /// when it names no multi: neither a proto nor any candidate is registered
    /// under it, so the caller falls through to the plain-routine rendering.
    ///
    /// `name` may already be package-qualified (a `&Pkg::name` handle), in
    /// which case it spells the whole registry key.
    // Cost: O(f), f = registered functions (the candidate scan of
    // `routine_candidate_subs`, run only when no proto is declared).
    pub(super) fn dispatcher_raku(&mut self, package: &str, name: &str) -> Option<String> {
        let key = if crate::qualified::is_qualified(Symbol::intern(name)) {
            name.to_string()
        } else {
            crate::qualified::qualified(Symbol::intern(package), Symbol::intern(name))
                .as_str()
                .to_string()
        };
        let proto = self
            .resolve_proto_function(&key)
            .or_else(|| self.resolve_proto_function(name));
        if proto.is_none() && self.routine_candidate_subs(package, name).is_empty() {
            return None;
        }
        let signature = match proto {
            Some(def) => {
                let defs = if def.param_defs.is_empty() {
                    def.params
                        .iter()
                        .map(|p| super::methods_format::positional_param(p))
                        .collect()
                } else {
                    def.param_defs.clone()
                };
                let sig = make_signature_value(
                    param_defs_to_sig_info(&defs, def.return_type.clone()),
                    Some(&*self),
                );
                match sig.view() {
                    ValueView::Instance { attributes, .. } => attributes
                        .as_map()
                        .get("gist")
                        .map(Value::to_string_value)
                        .unwrap_or_else(|| "()".to_string()),
                    _ => "()".to_string(),
                }
            }
            None => "(;; Mu |)".to_string(),
        };
        let bare = crate::qualified::unqualified_part(Symbol::intern(name));
        Some(format!("proto sub {} {} {{*}}", bare.as_str(), signature))
    }
}
