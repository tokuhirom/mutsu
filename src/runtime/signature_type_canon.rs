//! A signature's type names, resolved in the scope that declares the routine.
//!
//! Rakudo stores a parameter's type and a routine's `--> T` as *type objects*,
//! looked up where the routine is declared. mutsu stores the spelling and
//! resolves it later, wherever the type is asked for. That is the same thing
//! unless the spelling is a `constant` alias of a type, which is exactly how
//! upstream `NativeCall` exports its C types:
//!
//! ```raku
//! my constant size_t is export(:types, :DEFAULT) = NativeCall::Types::size_t;
//! ```
//!
//! `sub strlen(Str --> size_t) is native {*}` then has to report
//! `NativeCall::Types::size_t` (REPR `P6int`) from `&strlen.returns`, read
//! inside the `NativeCall` module, where the importer's `size_t` alias is not
//! in scope. Kept as the bare spelling, mutsu answered a different, unrelated
//! type object (`size_t`, REPR `P6opaque`), and upstream's
//! `check_routine_sanity` rejected it (#11555).
//!
//! So registration rewrites each spelling that names a type alias to the
//! aliased type's own name, once, while the declaring scope is live.

use super::*;

impl Interpreter {
    /// The type a signature spelling denotes in the current (declaring) scope,
    /// when the spelling is an alias of another type: `Some("M::T::size_t:D")`
    /// for `size_t:D` under `constant size_t = M::T::size_t`. `None` when the
    /// spelling already names its own type, or names no type.
    // Cost: O(a), a = alias-chain links followed (at most 16), each one or two
    // env probes.
    pub(crate) fn declared_type_alias_target(&self, spelling: &str) -> Option<String> {
        let (base, smiley) = crate::runtime::types::strip_type_smiley(spelling);
        // An alias is declared under a plain identifier, and a spelling nobody
        // interned cannot be bound to anything.
        let sym = crate::symbol::Symbol::lookup(base)?;
        if crate::qualified::is_qualified(sym) {
            return None;
        }
        let target = self.resolve_type_alias_chain(base)?;
        // A lexical type's mangled storage name (ADR-0047) is resolved where
        // the type is asked for, by its source spelling; keep that.
        if target.contains('\u{0}')
            || !(self.has_type(&target)
                || self.native_decl(&target).is_some()
                || crate::runtime::utils::is_known_type_constraint(&target))
        {
            return None;
        }
        Some(match smiley {
            Some(smiley) => format!("{target}{smiley}"),
            None => target,
        })
    }

    /// `defs` with every alias spelling in a type position (the parameter's
    /// own type, a sub-signature's, a `&callback (...)` signature's and its
    /// `--> T`) replaced by the aliased type's name. `None` when nothing
    /// changes, which is the overwhelming case and allocates nothing.
    // Cost: O(p * a), p = parameters including nested signatures, a = as
    // `declared_type_alias_target`.
    pub(crate) fn canonical_signature_param_types(
        &self,
        defs: &[ParamDef],
    ) -> Option<Vec<ParamDef>> {
        let mut out: Option<Vec<ParamDef>> = None;
        for (i, pd) in defs.iter().enumerate() {
            let type_constraint = pd
                .type_constraint
                .as_deref()
                .and_then(|tc| self.declared_type_alias_target(tc));
            let sub_signature = pd
                .sub_signature
                .as_deref()
                .and_then(|sig| self.canonical_signature_param_types(sig));
            let code_signature = pd.code_signature.as_ref().and_then(|(sig, ret)| {
                let params = self.canonical_signature_param_types(sig);
                let ret_target = ret
                    .as_deref()
                    .and_then(|r| self.declared_type_alias_target(r));
                (params.is_some() || ret_target.is_some()).then(|| {
                    (
                        params.unwrap_or_else(|| sig.clone()),
                        ret_target.or_else(|| ret.clone()),
                    )
                })
            });
            if type_constraint.is_none() && sub_signature.is_none() && code_signature.is_none() {
                if let Some(out) = out.as_mut() {
                    out.push(pd.clone());
                }
                continue;
            }
            let out = out.get_or_insert_with(|| defs[..i].to_vec());
            let mut pd = pd.clone();
            if let Some(tc) = type_constraint {
                pd.type_constraint = Some(tc);
            }
            if let Some(sig) = sub_signature {
                pd.sub_signature = Some(sig);
            }
            if let Some(cs) = code_signature {
                pd.code_signature = Some(cs);
            }
            out.push(pd);
        }
        out
    }
}
