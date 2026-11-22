//! A `sub` declared in a role body has its traits applied once (#18097).
//!
//! In rakudo a role-body `sub g is probe { }` is a compile-time declaration
//! of the role body: `trait_mod:<is>` runs once, and every composition,
//! parameterization or pun gets a clone of the already-traited routine. mutsu
//! re-runs the role body at the role's declaration and at each composition,
//! which re-registered the sub and ran its custom traits again every time
//! (NativeCall's `is native` sanity check warned once per composition).
//!
//! The first registration that runs the traits records under (role, routine)
//! the routine they left: its registry def (a `.wrap` or a rebound `$!do`
//! applies to the routine that def builds) and the mixin a trait bound on
//! `&g`, if any. A later registration of the same declaration skips the trait
//! handlers, keeps the routine's wrap chain, puts the traited def back and
//! carries the mixin over, as rakudo's clone of the traited routine keeps
//! them.

use crate::value::{Value, ValueView};

use super::Interpreter;

impl Interpreter {
    /// The (role, routine) key a role-body `sub` named `name`, with body
    /// identity `identity`, records its traited routine under; `None` outside
    /// a role body.
    // Cost: O(n), n = the length of the role and routine names.
    pub(crate) fn role_body_sub_trait_key(
        &self,
        name: &str,
        identity: u64,
    ) -> Option<(String, String)> {
        // A role body runs with the role as its current package.
        let role = self.current_package();
        let routine = format!("{name}\0{identity}");
        if self.registry().roles.contains_key(&role) {
            return Some((role, routine));
        }
        // A role body's sub re-declared in a composing class's package
        // (`register_role_body_subs_per_composer`) answers to the role's
        // record (#18721).
        self.registry()
            .role_body_traited_subs
            .keys()
            .find(|(_, r)| *r == routine)
            .cloned()
    }

    /// Whether role `role`'s traited body `sub` `name` can be re-declared
    /// over a composing class's cells: its traits ran (a record exists) and
    /// did not wrap the routine or rebind its body (a `.wrap`, NativeCall's
    /// `$!do`), which belong to the role's one routine (#18721).
    // Cost: O(t), t = recorded traited role-body subs.
    pub(crate) fn role_body_sub_reclosable(&self, role: &str, name: &str) -> bool {
        let root = |r: &str| r.split(['[', '\0']).next().unwrap_or(r).to_string();
        let role = root(role);
        let prefix = format!("{name}\0");
        let package = self
            .registry()
            .role_body_traited_subs
            .iter()
            .find(|((r, routine), _)| root(r) == role && routine.starts_with(&prefix))
            .and_then(|(_, record)| record.def.as_ref().map(|def| def.package));
        package.is_some_and(|package| {
            !self.routine_is_wrapped(&super::code_do_attr::wrap_ledger_name(package, name))
                && !self.routine_is_wrapped(name)
        })
    }

    /// Whether a role-body `sub` named `name` (body identity `identity`)
    /// already had its traits applied by an earlier registration, so this one
    /// keeps what they built (a bound `$!do` in its wrap chain included).
    // Cost: O(n) for the key, plus one map probe.
    pub(crate) fn role_body_sub_traits_recorded(&self, name: &str, identity: u64) -> bool {
        self.role_body_sub_trait_key(name, identity)
            .is_some_and(|key| self.registry().role_body_traited_subs.contains_key(&key))
    }

    /// Give the routine just registered under `name` what its traits left at
    /// the first registration. `false` when this is the first registration
    /// (the caller runs the traits).
    // Cost: O(1) amortized (map lookups and inserts), plus a clone of the
    // recorded mixin's overrides.
    pub(crate) fn replay_role_body_sub_traits(
        &mut self,
        key: &(String, String),
        name: &str,
    ) -> bool {
        let Some(first) = self.registry().role_body_traited_subs.get(key).cloned() else {
            return false;
        };
        let fq = crate::symbol::Symbol::intern(self.current_package_qualified(name));
        // Re-declared over a composing class's cells, the routine keeps its
        // own def (it closes over the class's cells); only a mixin is
        // carried over.
        let composer = self.current_package() != key.0;
        let first = if composer {
            super::registry::RoleBodyTraitedSub {
                def: None,
                callable_id: None,
                ..first
            }
        } else {
            first
        };
        // The traited def is the routine: a trait that wrapped it or rebound
        // its `$!do` (NativeCall's `setup-nativecall`) did so to the routine
        // this def builds, so it replaces the one just registered.
        // The routine keeps the registration id its wrap chain was built
        // under, or the chain reads as a redefinition's and is dropped.
        if let Some(id) = first.callable_id {
            let key = crate::meta_ns::MetaNs::CallableId
                .key_pair_for_strs(&self.current_package(), name);
            self.env.insert_sym_noting(key, Value::int(id));
        }
        if let Some(def) = first.def {
            // The registration may also have bound `&name` itself; that code
            // object is the traited routine too.
            let code_var_key = format!("&{name}");
            if self
                .env
                .get(&code_var_key)
                .is_some_and(|v| matches!(v.view(), ValueView::Sub(_)))
            {
                let routine = self.sub_value_from_function_def((*def).clone());
                self.env.insert(code_var_key, routine);
            }
            self.routines_mut().functions_mut().insert(fq, def);
            self.invalidate_fn_resolution_for_keys([fq]);
        }
        // A role a trait mixed in is carried over onto the routine, cloned
        // over this composition's scope as rakudo clones the traited routine.
        if let Some(mixin) = first.mixin
            && let ValueView::Mixin(traited, mixins) = mixin.view()
        {
            let code_var_key = format!("&{name}");
            let fresh = self.env.get(&code_var_key).cloned();
            let routine = match (traited.view(), fresh.as_ref().map(Value::view)) {
                (ValueView::Sub(traited), Some(ValueView::Sub(fresh))) => {
                    let mut clone = (**traited).clone();
                    clone.env = fresh.env.clone();
                    Value::sub_value(crate::gc::Gc::new(clone))
                }
                _ => traited.as_ref().clone(),
            };
            self.env
                .insert(code_var_key, Value::mixin_with_state(routine, (**mixins).clone()));
        }
        true
    }

    /// Record what a role-body `sub`'s traits left: its registry def and the
    /// mixin a trait bound under `&name`, if any.
    // Cost: O(1) amortized (one env lookup, one registry lookup and one map
    // insert).
    pub(crate) fn record_role_body_sub_traits(&mut self, key: (String, String), name: &str) {
        let mixin = self
            .env
            .get(&format!("&{name}"))
            .filter(|v| matches!(v.view(), ValueView::Mixin(..)))
            .cloned();
        let def = self
            .routines()
            .functions
            .get(&crate::symbol::Symbol::intern(
                self.current_package_qualified(name),
            ))
            .cloned();
        let callable_id = self.registration_callable_id(
            crate::meta_ns::MetaNs::CallableId.key_pair_for_strs(&self.current_package(), name),
        );
        self.registry_mut()
            .role_body_traited_subs
            .entry(key)
            .or_insert(super::registry::RoleBodyTraitedSub {
                def,
                mixin,
                callable_id,
            });
    }
}
