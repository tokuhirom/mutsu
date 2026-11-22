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
//! the mixin they left on `&g`, if any (a role a trait mixed in). A later
//! registration of the same declaration skips the trait handlers and gives
//! the freshly registered routine -- whose closure belongs to this
//! composition -- the recorded mixin, as rakudo's clone keeps it.

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
        if !self.registry().roles.contains_key(&role) {
            return None;
        }
        Some((role, format!("{name}\0{identity}")))
    }

    /// Whether a role-body `sub` named `name` (body identity `identity`)
    /// already had its traits applied by an earlier registration, so this one
    /// keeps what they built (a bound `$!do` in its wrap chain included).
    // Cost: O(n) for the key, plus one map probe.
    pub(crate) fn role_body_sub_traits_recorded(&self, name: &str, identity: u64) -> bool {
        self.role_body_sub_trait_key(name, identity)
            .is_some_and(|key| self.registry().role_body_traited_subs.contains_key(&key))
    }

    /// Re-apply the traits a role-body `sub` received at its first
    /// registration to the routine just registered under `&name`. `false`
    /// when this is the first registration (the caller runs the traits).
    // Cost: O(1) amortized (one map lookup and one env insert), plus a clone
    // of the recorded mixin's overrides.
    pub(crate) fn replay_role_body_sub_traits(
        &mut self,
        key: &(String, String),
        name: &str,
    ) -> bool {
        let Some(first) = self.registry().role_body_traited_subs.get(key).cloned() else {
            return false;
        };
        let code_var_key = format!("&{name}");
        // The routine just registered, built the way the trait handlers
        // receive it (`&name` may not be bound in env yet).
        let fresh = self.env.get(&code_var_key).cloned().or_else(|| {
            let def = self
                .routines()
                .functions
                .get(&crate::symbol::Symbol::intern(
                    self.current_package_qualified(name),
                ))
                .cloned()?;
            Some(self.sub_value_from_function_def((*def).clone()))
        });
        if let ValueView::Mixin(traited, mixins) = first.view()
            && let Some(fresh) = fresh
        {
            let fresh = match fresh.view() {
                ValueView::Mixin(inner, _) => inner.as_ref().clone(),
                _ => fresh,
            };
            // A trait may have replaced the routine's body too (NativeCall's
            // `setup-nativecall`): the traited routine is cloned, closing over
            // this composition's scope, as rakudo clones it per composition.
            let routine = match (traited.view(), fresh.view()) {
                (ValueView::Sub(traited), ValueView::Sub(fresh)) => {
                    let mut clone = (**traited).clone();
                    clone.env = fresh.env.clone();
                    Value::sub_value(crate::gc::Gc::new(clone))
                }
                _ => fresh,
            };
            self.env
                .insert(code_var_key, Value::mixin_with_state(routine, (**mixins).clone()));
        }
        true
    }

    /// Record that a role-body `sub`'s traits ran, with the mixin they left
    /// under `&name` if any.
    // Cost: O(1) amortized (one env lookup and one map insert).
    pub(crate) fn record_role_body_sub_traits(&mut self, key: (String, String), name: &str) {
        // A trait that mixed a role into the routine left the mixin under
        // `&name`; one that did not leaves nothing to carry over, but the
        // traits still ran.
        let value = self
            .env
            .get(&format!("&{name}"))
            .filter(|v| matches!(v.view(), ValueView::Mixin(..)))
            .cloned()
            .unwrap_or(Value::NIL);
        self.registry_mut()
            .role_body_traited_subs
            .entry(key)
            .or_insert(value);
    }
}
