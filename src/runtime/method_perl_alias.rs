//! `Mu.perl` is `self.raku` (ADR-11276 §9.152).
//!
//! Rakudo declares `perl` once, on `Mu`, as `method perl(|c) { self.raku(|c) }`.
//! mutsu used to treat the two names as aliases at every place that answered
//! either one: about a hundred `"raku" | "perl"` tests across the cascades,
//! the VM's call paths and the rows, which drifted (a class with only a user
//! `raku` rendered its attribute dump under `.perl`). The call entries now
//! resolve the name once, so nothing below them sees `perl` unless a
//! user-declared `perl` is in reach.

use crate::runtime::Interpreter;
use crate::symbol::{Symbol, wk};
use crate::value::{Value, ValueView};

impl Interpreter {
    /// The method a call to `method` on `target` dispatches as: `raku` for a
    /// `perl` that no user-declared `perl` answers, `method` otherwise.
    // Cost: O(1) for every name but `perl` (one symbol compare); for `perl`,
    // O(w) to scan the wrap chains, w = wrapped methods, plus O(1) amortized
    // for the user-method probe (memoized) and, when some role declares
    // `perl`, O(r + d), r = roles, d = MRO depth; a mixin adds O(m), m =
    // mixed-in roles.
    pub(crate) fn perl_as_raku(&mut self, target: &Value, method: Symbol) -> Symbol {
        if method != wk::perl() || self.user_perl_in_reach(target) {
            method
        } else {
            wk::raku()
        }
    }

    /// Whether a user-declared `perl` (a class's, a composed or mixed-in
    /// role's, an `augment`'s or a `.wrap` of a `perl`) answers a call to
    /// `perl` on `target`.
    fn user_perl_in_reach(&mut self, target: &Value) -> bool {
        let perl = wk::perl();
        // A `.wrap` of a `perl` method (`Mu.perl`'s included) runs before it.
        if self
            .registry()
            .method_wrap_chains
            .iter()
            .any(|((_, name, _), chain)| name == "perl" && !chain.is_empty())
        {
            return true;
        }
        let target = target.descalarize();
        if target.is_mixin_value() {
            if self.mixin_role_has_method(&target, perl.as_str()) {
                return true;
            }
            if let ValueView::Mixin(inner, _) = target.view() {
                return self.user_perl_in_reach(&inner);
            }
            return false;
        }
        let owner = match target.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Package(name) => name,
            _ => Symbol::intern(crate::runtime::utils::value_type_name(&target)),
        };
        let owner = owner.resolve();
        if self.has_user_method_sym(&owner, perl) {
            return true;
        }
        // A role's methods stay in the role registry, which the class method
        // table does not list (`grammar_has_user_method_sym` reads it the same
        // way); a punned role's type object is the role itself.
        let registry = self.registry();
        if !registry.roles.values().any(|role| declares_public_perl(role)) {
            return false;
        }
        self.mro_readonly(&owner).iter().any(|ancestor| {
            self.registry()
                .roles
                .get(ancestor.as_str())
                .is_some_and(|role| declares_public_perl(role))
        }) || self
            .registry()
            .roles
            .get(owner.as_str())
            .is_some_and(|role| declares_public_perl(role))
    }
}

fn declares_public_perl(role: &crate::runtime::RoleDef) -> bool {
    role.methods
        .get("perl")
        .is_some_and(|defs| defs.iter().any(|def| !def.is_private))
}
