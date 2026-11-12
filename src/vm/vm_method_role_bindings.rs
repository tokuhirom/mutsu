//! A role method's composition bindings: the role's type parameters (`T` of
//! `role R[::T]`) and the other per-composition names the class composed with
//! the role carries, put into the method's env before its body runs.
//!
//! Both method binders (`call_compiled_method_in_unit` and its fast twin) run
//! the same steps from here, so a role method no longer has to give up the
//! fast binder just because its body can see role context (#17331).

use super::*;

impl Interpreter {
    /// Put a method's own composition bindings (a role's type parameters and the
    /// role-body lexicals persisted with the composition) into the method's env.
    /// `::?CLASS` is the frame record's, not an env key.
    // Cost: O(b), b = the method's bindings.
    pub(super) fn apply_method_role_bindings(&mut self, method_def: &crate::runtime::MethodDef) {
        if let Some(bindings) = method_def.role_param_bindings.as_deref() {
            for (name, value) in bindings {
                if name != "?CLASS" {
                    // Interned in place: `insert` would clone the name into a
                    // `String` only to intern it.
                    let sym = Symbol::intern(name);
                    self.env_mut().insert_sym_noting(sym, value.clone());
                }
            }
        }
    }

    /// Whether a call of `method_def` (declared by `owner_class`, dispatched on
    /// `receiver_class_name`) has role composition bindings to install.
    // Cost: O(1) expected: two hash probes.
    pub(super) fn method_has_role_bindings(
        &self,
        method_def: &crate::runtime::MethodDef,
        owner_class: &str,
        receiver_class_name: &str,
    ) -> bool {
        if method_def.role_param_bindings.is_some() {
            return true;
        }
        let registry = self.registry();
        registry.class_role_param_bindings.contains_key(owner_class)
            || registry
                .class_role_param_bindings
                .contains_key(receiver_class_name)
    }

    /// Install the role parameter bindings a method call sees, before its
    /// parameters are bound (a `T`-typed parameter resolves `T` through them).
    ///
    /// The per-class map (`class_role_param_bindings`) carries more than
    /// type-param bindings (`T => Int`) — composing a role with a nested
    /// generic class (`role R[::T] { my package G { class A is Array[T] {} } }`)
    /// also stores a per-composition rename entry keyed by the class's
    /// UNPARAMETRIZED name (`"G::A" => Package("R::G::A[Int]")`, see
    /// `registration_class_compose_body.rs`'s `rename_generic_composed_class`
    /// call), which a bareword read of `G::A` inside the method body resolves
    /// through this same env injection. So the class-level map is always
    /// applied first; the candidate's own `role_param_bindings` (present when
    /// the SAME role is composed twice with different type args, `does R[Int]
    /// does R[Str]` — the class-level map is last-write-wins there) is then
    /// overlaid on top to correct just the type-param keys for this specific
    /// candidate.
    ///
    /// Returns the class-level names it installed: they are this call's own
    /// bindings, which the exit merge must not carry back into the caller.
    // Cost: O(b), b = the class's and the method's bindings.
    pub(super) fn install_method_role_bindings(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_name: &str,
        method_def: &crate::runtime::MethodDef,
    ) -> std::sync::Arc<[String]> {
        if matches!(method_name, "BUILD" | "TWEAK") {
            let mut installed = Vec::new();
            // Construction-phase role methods are run with the consuming class
            // as the receiver even when their owner is the role declaration.
            // Its concrete role bindings must win over the bare role pun's
            // default bindings so captures such as `ValueType()` coerce
            // constructor arguments against the consuming class's type.
            if let Some(role_bindings) = self.class_role_param_bindings(receiver_class_name) {
                for (name, value) in role_bindings {
                    installed.push(name.clone());
                    self.env_mut().insert(name, value);
                }
            }
            if let Some(role_bindings) = self.class_role_param_bindings(owner_class) {
                for (name, value) in role_bindings {
                    if !self.env().contains_key(&name) {
                        installed.push(name.clone());
                        self.env_mut().insert(name, value);
                    }
                }
            }
            self.apply_method_role_bindings(method_def);
            return installed.into();
        }
        let memo = self.method_role_bindings(owner_class, receiver_class_name);
        for (sym, value) in memo.pairs.iter() {
            self.env_mut().insert_sym_noting(*sym, value.clone());
        }
        self.apply_method_role_bindings(method_def);
        memo.names
    }

    /// The class-level bindings an ordinary (not `BUILD`/`TWEAK`) role method
    /// call installs: the owner's `class_role_param_bindings` entry, or the
    /// receiver's when the owner has none. A pure function of that table, so
    /// it is read once per `(owner, receiver)` and table state instead of
    /// cloning the per-class map (and its `String` keys) on every call
    /// (#17331).
    // Cost: O(1) expected on a hit (one hash probe and a table-state compare);
    // O(b) on a miss, b = the class's bindings.
    fn method_role_bindings(
        &mut self,
        owner_class: &str,
        receiver: &str,
    ) -> MethodRoleBindingsMemo {
        let key = (Symbol::intern(owner_class), Symbol::intern(receiver));
        let state = self.registry().class_role_param_bindings.state_id();
        if let Some(memo) = self.caches.method_role_bindings_memo.get(&key)
            && memo.state == state
        {
            return memo.clone();
        }
        let bindings = self
            .class_role_param_bindings(owner_class)
            .or_else(|| self.class_role_param_bindings(receiver))
            .unwrap_or_default();
        let mut pairs = Vec::with_capacity(bindings.len());
        let mut names = Vec::with_capacity(bindings.len());
        for (name, value) in bindings {
            pairs.push((Symbol::intern(&name), value));
            names.push(name);
        }
        let memo = MethodRoleBindingsMemo {
            state,
            pairs: pairs.into(),
            names: names.into(),
        };
        self.caches
            .method_role_bindings_memo
            .insert(key, memo.clone());
        memo
    }
}

/// One `(owner, receiver)` entry of [`ResolutionCaches::method_role_bindings_memo`]:
/// the bindings in the order they are installed, their names for the exit
/// merge, and the table state they were read under.
///
/// [`ResolutionCaches::method_role_bindings_memo`]:
///     crate::runtime::resolution_caches::ResolutionCaches::method_role_bindings_memo
#[derive(Clone)]
pub(crate) struct MethodRoleBindingsMemo {
    state: (usize, u64),
    pairs: std::sync::Arc<[(Symbol, Value)]>,
    names: std::sync::Arc<[String]>,
}
