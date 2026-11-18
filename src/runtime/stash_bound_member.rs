//! Qualified names through packages that stash bindings placed.
//!
//! mutsu keys packages by their names, while a stash binding can put a package
//! under any other one: `M.WHO.BIND-KEY('Nested', $stub)` makes the package
//! named `Nested` the member `M::Nested`, and `Nested.WHO.BIND-KEY('In', $g)`
//! then binds `Nested::In`. rakudo resolves `M::Nested::In` by walking
//! `M.WHO<Nested>.WHO<In>`; this follows the same chain by name.

use super::*;

impl Interpreter {
    /// `M::Nested::In` where a stash binding put the package `Nested` under
    /// `M` (`M.WHO.BIND-KEY('Nested', $stub)`, Grammar::BNF's `modern-install`):
    /// the rest of the name is looked up in the bound package's own stash, as
    /// rakudo walks `M.WHO<Nested>.WHO<In>`. The longest bound prefix is
    /// followed first, and each step restarts from the rewritten name.
    // Cost: O(s^2) symbol-store probes, s = the name's segments.
    pub(crate) fn stash_bound_member(&self, name: &str) -> Option<Value> {
        let mut name = Symbol::intern(name);
        // Each rewrite replaces a bound prefix by the package it names; the
        // segment count bounds the steps a well-formed chain takes.
        for _ in 1..crate::qualified::segments(name).len() {
            let ancestors: Vec<Symbol> =
                crate::qualified::package_ancestors(crate::qualified::package_parent(name)?).collect();
            let (bound, real) = ancestors.iter().find_map(|prefix| {
                let bound = self.get_our_var(prefix.as_str())?.deref_container();
                match bound.view() {
                    ValueView::Package(real) if real != *prefix => Some((*prefix, real)),
                    _ => None,
                }
            })?;
            let rest = Symbol::intern(&name.as_str()[bound.as_str().len() + 2..]);
            name = crate::qualified::qualified(real, rest);
            if let Some(found) = self.get_our_var(name.as_str()) {
                return Some(found.clone());
            }
            if self.has_type(name.as_str()) {
                return Some(Value::package(name));
            }
        }
        None
    }
}
