//! The lexical scope a role body gets in each class that composes it.
//!
//! Rakudo runs a role body once per composing class, and the role's methods
//! close over that run's lexicals, `sub`s included. So a role method calling
//! `f()` reaches the role body's `sub f` even when the composing class declares
//! a `sub f` of its own; the class's methods reach the class's (#14177).
//!
//! mutsu resolves a bare routine call through packages
//! (`bare_name_packages`), and a composed role method runs in the composing
//! class's package `K`. The role body's `sub`s are declared once more per
//! composer (`register_role_body_subs_per_composer`); one whose name the class
//! also declares goes into the composition scope `K::&role:R` instead, and the
//! role's methods composed into `K` run in that scope (`method_body_package`).
//! It is a routine-scoped package name (the `::&` marker), so a lookup from it
//! tries the scope, then `K`, then `K`'s enclosing packages, as a lookup from a
//! nested sub's scope does; the per-package call caches tell it apart from `K`.

use crate::symbol::Symbol;

/// The composition scope of `role`'s body in `class`: `class::&role:role`,
/// built once per pair.
// Cost: O(1) amortized: memoized per (class, role).
pub(crate) fn role_composition_scope(class: Symbol, role: Symbol) -> Symbol {
    thread_local! {
        static SCOPES: std::cell::RefCell<rustc_hash::FxHashMap<(Symbol, Symbol), Symbol>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(scope) = SCOPES.with(|c| c.borrow().get(&(class, role)).copied()) {
        return scope;
    }
    let marker = Symbol::intern(&format!("&role:{}", role.as_str()));
    let scope = crate::qualified::qualified(class, marker);
    SCOPES.with(|c| {
        c.borrow_mut().insert((class, role), scope);
    });
    scope
}
