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
        let sym = Symbol::intern(bare);
        // An `@`/`%` static's element and key types are read from its env
        // lanes too (a `%` store keys by its declared key type, not by the
        // hash it holds), and a `unit class` body runs in its loader's env:
        // the next module's same-named `my %seen{Hash}` overwrote the lanes,
        // and this class's methods checked their stores against it (#12903).
        // Record the lanes per class, for `inject_class_body_statics` to put
        // back on method entry, and restore them like the scalar's.
        if bare.starts_with(['@', '%']) {
            let mut lanes = vec![Self::type_meta_key_for_sym(sym)];
            if bare.starts_with('%') {
                lanes.push(Self::hash_key_meta_key_for_sym(sym));
            }
            for key in lanes {
                if let Some(ValueView::Str(tc)) = self.env().get_sym(key).map(Value::view) {
                    let tc = tc.as_str().to_owned();
                    crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_types)
                        .entry(class.to_string())
                        .or_default()
                        .insert(key.resolve(), tc);
                }
                self.restore_static_meta_lane(key, saved_env);
            }
            return;
        }
        if bare.starts_with('&') {
            return;
        }
        if let Some(constraint) = self.var_type_constraint_sym(sym) {
            crate::runtime::cow_table_mut(&mut self.lexicals.class_body_static_types)
                .entry(class.to_string())
                .or_default()
                .insert(bare.to_string(), constraint);
        }
        self.restore_static_meta_lane(Self::type_meta_key_for_sym(sym), saved_env);
    }

    /// Put the env metadata lane `key` back to what `saved_env`, the scope
    /// enclosing a class body, had for it.
    // Cost: O(1), one env probe and one insert or remove.
    fn restore_static_meta_lane(&mut self, key: Symbol, saved_env: &crate::env::Env) {
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
