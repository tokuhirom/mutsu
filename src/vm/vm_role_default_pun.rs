//! A method call on a parametric role's type object whose type parameters all
//! have defaults (`role P[::T = Any] { method m {...} }; P.m`) runs on the pun
//! of the default parameterization, as `.new` does (#11652).
//!
//! Deciding that took a role probe, a walk of the role and its parents for
//! the method, a materialization of the default parameterization and a lookup
//! of its pun class by its rendered name -- on every call, ~20k instructions
//! over the call itself. Monad calls `.some`/`.right`/`.left` on such role
//! type objects in every combinator (#17331).
//!
//! The decision is recorded per `(role, method)` together with the registry
//! write generation; registering, composing, punning or withdrawing any type
//! moves the generation, and an entry from an older one is not served. Only a
//! redirect to a pun class (a `Package`) is recorded.
//!
//! The two method-call entries -- the plain `CallMethod` one and the
//! container-aware `CallMethodMut` one -- both ask this one helper, so a role
//! type object puns to its default parameterization through either. The plain
//! entry used to compose and withdraw a pun named after the role itself on
//! every call instead (#17408).

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// The recorded redirect of a call of `method` on the role type object
    /// `role`, when it was recorded under the current registry write
    /// generation.
    // Cost: O(1).
    pub(crate) fn role_default_pun_memo_hit(&self, role: Symbol, method: Symbol) -> Option<Symbol> {
        let &(pun, generation) = self.caches.role_default_pun_memo.get(&(role, method))?;
        (generation == self.registry_write_generation()).then_some(pun)
    }

    /// The pun class a call of `method` on the role type object `target`
    /// (named `role`) re-dispatches to, when the role is a parametric role
    /// whose type parameters all have defaults and it provides `method`.
    /// `None` for any other receiver or method, and for a role not punned
    /// to a default parameterization.
    // Cost: O(1) on a memo hit; otherwise O(m), m = the role and its parents'
    // method tables, plus the first pun's composition.
    pub(crate) fn role_default_pun_redirect(
        &mut self,
        target: &Value,
        role: Symbol,
        method: Symbol,
    ) -> Result<Option<Value>, RuntimeError> {
        if let Some(pun) = self.role_default_pun_memo_hit(role, method) {
            return Ok(Some(Value::package(pun)));
        }
        let name = role.as_str();
        let method_name = method.as_str();
        if self.registry().classes.contains_key(name)
            || !self.is_role(name)
            || crate::runtime::role_type_object_keeps_mu_method(method_name)
            || !self.role_or_parent_has_method(&self.role_group_name(name), method_name)
        {
            return Ok(None);
        }
        let Some(punned) = self.default_parametric_role_pun(target)? else {
            return Ok(None);
        };
        if let ValueView::Package(pun) = punned.view() {
            // Read after the pun was made: composing it moved the generation.
            let generation = self.registry_write_generation();
            self.caches
                .role_default_pun_memo
                .insert((role, method), (pun, generation));
        }
        Ok(Some(punned))
    }
}
