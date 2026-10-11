//! The declared types of typed class-body `my` statics (#10524).
//!
//! A class body's `my` lexicals outlive the body in the package's static store
//! (`package_lexicals`), where its methods read and write them. A scalar's
//! declared type, though, was registered in the name-keyed env lane
//! (`__mutsu_type::<name>`) and stayed there after the body: the next class
//! body declaring a same-named typed static overwrote it, and the first
//! class's methods then checked their stores against the second class's type
//! (`class A { my A $i; method m { $i = A.new } }; class B { my B $i }` died
//! assigning an `A` to `$i`). The BEGIN prologue registers every class ahead
//! of the run-time code, which made that the common case.
//!
//! So leaving a class body records each static's type per class, and puts
//! the env lane back the way the enclosing scope had it, as it already did
//! for the binding itself. A by-name store from the class's code checks the
//! record.

use super::*;

impl Interpreter {
    /// Leaving the body of `class`: record the declared type of its scalar
    /// static `bare` (what the body's `__mutsu_type::` entry says, if any)
    /// and restore that entry to what `saved_env`, the enclosing scope, had.
    // Cost: O(1), two env probes and one table insert.
    pub(crate) fn leave_class_body_static_type(
        &mut self,
        class: &str,
        bare: &str,
        saved_env: &crate::env::Env,
    ) {
        // A container's element type travels with the container (ADR-0042).
        if bare.starts_with(['@', '%', '&']) {
            return;
        }
        let sym = Symbol::intern(bare);
        if let Some(constraint) = self.var_type_constraint_sym(sym) {
            crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_types)
                .entry(class.to_string())
                .or_default()
                .insert(bare.to_string(), constraint);
        }
        let key = Self::type_meta_key_for_sym(sym);
        match saved_env.get_sym(key) {
            Some(previous) => {
                let previous = previous.clone();
                self.env_mut().insert_sym(key, previous);
            }
            None => {
                self.env_mut().remove_sym(key);
            }
        }
    }

    /// The type a by-name store to `name` checks when `name` is a class-body
    /// static of the running code's class: `Some(constraint)`, where
    /// `constraint` is `None` for an untyped static. `None` when `name` is
    /// not such a static.
    // Cost: O(|name|), `package_scope_lexical_key` plus two table probes.
    pub(crate) fn class_body_static_store_type(&self, name: &str) -> Option<Option<String>> {
        if self.lexicals.class_body_static_names.is_empty() {
            return None;
        }
        let (class, key) = self.package_scope_lexical_key(name)?;
        if !self
            .lexicals
            .class_body_static_names
            .get(class)
            .is_some_and(|names| names.contains(key.as_ref()))
        {
            return None;
        }
        Some(
            self.lexicals
                .class_body_static_types
                .get(class)
                .and_then(|types| types.get(key.as_ref()))
                .cloned(),
        )
    }
}
