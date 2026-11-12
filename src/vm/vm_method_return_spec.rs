//! A method's effective `--> T` return spec: the declared spec with a role
//! pun's pseudo-types filled in, a type capture resolved and the name resolved
//! against the owner's package chain.
//!
//! Only the capture step reads the call's env. Every other step reads the
//! type registry and the package type aliases, so its result is remembered per
//! `(owner, spec)` and re-derived when either changes (#17331: Monad's type
//! hooks are `method maybe-type(--> Mu)`, and resolving that `Mu` cost ~3.3K
//! instructions on each call).

use super::*;

/// One `(owner, spec)` entry of
/// [`ResolutionCaches::method_return_spec_memo`]: the resolved spec and the
/// state it was resolved under. The alias table is held, not just compared by
/// address, so a later write to it copies the table and moves the address.
///
/// [`ResolutionCaches::method_return_spec_memo`]:
///     crate::runtime::ResolutionCaches::method_return_spec_memo
pub(crate) struct MethodReturnSpecMemo {
    type_stamp: u64,
    aliases: std::sync::Arc<crate::runtime::PackageKeyed<String>>,
    resolved: String,
}

impl Interpreter {
    /// The spec a method's return value is checked against (see the module
    /// docs). Must run while the callee env is still live: a `--> T` names a
    /// capture this call bound (#7984).
    // Cost: O(1) expected on a memo hit plus the capture probe; a miss is the
    // full resolution, O(d) in the owner's package depth d.
    pub(super) fn method_effective_return_spec(&mut self, owner_class: &str, spec: &str) -> String {
        let punned = pun_pseudo_type(spec, owner_class);
        // The env-dependent step decides whether the memo applies at all: a
        // spec, or the base of a smileyed spec, that a capture of this call
        // may name takes the full resolution every time.
        let base = [":D", ":U", ":_"]
            .iter()
            .find_map(|smiley| punned.strip_suffix(smiley))
            .unwrap_or(&punned);
        let capture_free = !self.has_type_capture_binding(&punned)
            && !self.has_type_capture_binding(base)
            && self.try_resolved_type_capture_name(&punned).is_none();
        if !capture_free {
            // A definite return (`--> Empty`, `--> Nil`, `--> 42`) names a
            // value, never a type: resolving it first would turn the CORE
            // term `Empty` into any loaded `Foo::Empty` class (#9530).
            if self.is_definite_return_spec(spec) {
                return spec.to_string();
            }
            let captured = self.resolved_type_capture_name(&punned);
            return self.resolve_method_type_name(owner_class, &captured);
        }
        let key = (Symbol::intern(owner_class), Symbol::intern(spec));
        let type_stamp = self.registry().type_stamp();
        if let Some(memo) = self.caches.method_return_spec_memo.get(&key)
            && memo.type_stamp == type_stamp
            && std::sync::Arc::ptr_eq(&memo.aliases, &self.types.package_type_aliases)
        {
            return memo.resolved.clone();
        }
        let resolved = if self.is_definite_return_spec(spec) {
            spec.to_string()
        } else {
            self.resolve_method_type_name(owner_class, &punned)
        };
        let memo = MethodReturnSpecMemo {
            type_stamp,
            aliases: std::sync::Arc::clone(&self.types.package_type_aliases),
            resolved: resolved.clone(),
        };
        self.caches.method_return_spec_memo.insert(key, memo);
        resolved
    }
}

/// A role method run on its own role (a pun) keeps `::?CLASS` / `::?ROLE` in
/// its return type, since only a consuming class resolves them at composition;
/// the pun is the role itself.
// Cost: O(len), len = the return spec's length.
fn pun_pseudo_type<'a>(spec: &'a str, owner: &str) -> std::borrow::Cow<'a, str> {
    if spec.contains("::?") {
        std::borrow::Cow::Owned(spec.replace("::?CLASS", owner).replace("::?ROLE", owner))
    } else {
        std::borrow::Cow::Borrowed(spec)
    }
}
