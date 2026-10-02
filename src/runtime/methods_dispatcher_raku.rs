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
    /// The signature of the dispatcher that `package`/`name` names, or `None`
    /// when it names no multi: neither a proto nor any candidate is registered
    /// under it. A declared proto answers its own signature; a protoless multi
    /// answers the generated proto's, `(;; Mu |)`. Rakudo's `.signature`,
    /// `.arity` and `.count` on a dispatcher are this, never its candidates'.
    ///
    /// `name` may already be package-qualified (a `&Pkg::name` handle), in
    /// which case it spells the whole registry key.
    // Cost: O(f + p), f = registered functions (the candidate scan of
    // `routine_candidate_subs`, run only when no proto is declared),
    // p = proto parameters.
    pub(super) fn dispatcher_signature(&mut self, package: &str, name: &str) -> Option<Value> {
        let proto = self.dispatcher_proto(package, name);
        // A plain (non-multi) sub is listed among the candidates too; only a
        // `multi` one makes the name a dispatcher.
        if proto.is_none()
            && !self
                .routine_candidate_subs(package, name)
                .iter()
                .any(|c| match c.view() {
                    ValueView::Sub(data) => data.env.contains_key("__mutsu_is_multi_candidate"),
                    _ => false,
                })
        {
            return None;
        }
        Some(self.proto_signature_value(proto.as_ref()))
    }

    /// The name a materialized multi *sub* dispatcher (`&name`, carrying its
    /// candidates) dispatches on; `None` for any other `Sub`, a multi method's
    /// dispatcher included.
    // Cost: O(1).
    fn multi_sub_dispatcher_name(data: &crate::value::SubData) -> Option<String> {
        let ValueView::Str(disp_name) = data.env.get("__mutsu_multi_dispatch_name")?.view() else {
            return None;
        };
        if matches!(
            data.env.get("__mutsu_callable_type").map(Value::view),
            Some(ValueView::Str(kind)) if matches!(kind.as_str(), "Method" | "Submethod")
        ) {
            return None;
        }
        Some(disp_name.to_string())
    }

    /// The signature of a materialized dispatcher `Sub` (`&name` of a `multi
    /// sub`, carrying its candidates), or `None` when `data` is not one. A
    /// multi *method*'s dispatcher keeps its candidate-based answers.
    // Cost: O(f + p), as `dispatcher_signature`.
    pub(super) fn sub_dispatcher_signature(
        &mut self,
        data: &crate::value::SubData,
    ) -> Option<Value> {
        let disp_name = Self::multi_sub_dispatcher_name(data)?;
        if let Some(sig) = self.dispatcher_signature(&data.package.resolve(), &disp_name) {
            return Some(sig);
        }
        // The name's own scope has popped (an imported multi used after its
        // import scope ended): only the captured candidates are left, so the
        // dispatcher is answered as the generated proto.
        // TODO: capture the declared proto alongside the candidates so a
        // declared proto's own signature survives here too.
        data.env
            .contains_key("__mutsu_multi_dispatch_candidates")
            .then(|| self.proto_signature_value(None))
    }

    /// The signature value of `proto`, or of the proto Rakudo generates for a
    /// protoless multi (`(;; Mu |)`) when there is none.
    // Cost: O(p), p = proto parameters.
    pub(super) fn proto_signature_value(&self, proto: Option<&FunctionDef>) -> Value {
        let (defs, return_type) = match proto {
            Some(def) if def.param_defs.is_empty() => (
                def.params
                    .iter()
                    .map(|p| super::methods_format::positional_param(p))
                    .collect(),
                def.return_type.clone(),
            ),
            Some(def) => (def.param_defs.clone(), def.return_type.clone()),
            None => {
                let mut capture = super::methods_format::positional_param("_capture");
                capture.slurpy = true;
                capture.required = false;
                capture.multi_invocant = false;
                capture.type_constraint = Some("Mu".to_string());
                (vec![capture], None)
            }
        };
        make_signature_value(param_defs_to_sig_info(&defs, return_type), Some(self))
    }

    /// The proto declared for `package`/`name` (qualified key first, then the
    /// bare name), if any.
    // Cost: O(1) amortized (registry lookups).
    fn dispatcher_proto(&mut self, package: &str, name: &str) -> Option<FunctionDef> {
        let key = if crate::qualified::is_qualified(Symbol::intern(name)) {
            name.to_string()
        } else {
            crate::qualified::qualified(Symbol::intern(package), Symbol::intern(name))
                .as_str()
                .to_string()
        };
        self.resolve_proto_function(&key)
            .or_else(|| self.resolve_proto_function(name))
    }

    /// The `.raku` of `value` when it is a multi sub's dispatcher -- a
    /// `Routine` handle naming a multi, or a `Sub` carrying its candidates --
    /// and `None` for anything else.
    // Cost: O(f + p), as `dispatcher_signature`.
    pub(crate) fn routine_dispatcher_raku(&mut self, value: &Value) -> Option<String> {
        match value.view() {
            ValueView::Routine { package, name, .. } => {
                self.dispatcher_raku(&package.resolve(), &name.resolve())
            }
            ValueView::Sub(data) => {
                let disp_name = Self::multi_sub_dispatcher_name(&data)?;
                self.dispatcher_raku(&data.package.resolve(), &disp_name)
            }
            _ => None,
        }
    }

    /// The `.raku` of the dispatcher that `package`/`name` names, or `None`
    /// when it names no multi, so the caller falls through to the
    /// plain-routine rendering.
    // Cost: O(f + p), as `dispatcher_signature`.
    pub(super) fn dispatcher_raku(&mut self, package: &str, name: &str) -> Option<String> {
        let sig = self.dispatcher_signature(package, name)?;
        let gist = match sig.view() {
            ValueView::Instance { attributes, .. } => attributes
                .as_map()
                .get("gist")
                .map(Value::to_string_value)
                .unwrap_or_else(|| "()".to_string()),
            _ => "()".to_string(),
        };
        let bare = crate::qualified::unqualified_part(Symbol::intern(name));
        Some(format!("proto sub {} {} {{*}}", bare.as_str(), gist))
    }
}
