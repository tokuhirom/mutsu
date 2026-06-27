use super::*;

impl Interpreter {
    pub(crate) fn env(&self) -> &Env {
        &self.env
    }

    #[allow(dead_code)] // env-loan (CP-1 1e): VM callers migrated to the seam; kept for carriers.
    pub(crate) fn env_insert(&mut self, key: String, value: Value) {
        self.env.insert(key, value);
    }

    /// Clone the env for capture across a call/block/thread boundary. For a flat
    /// env this is the O(1) `Arc::clone`; for a *scoped* env (a converted call
    /// frame's transient overlay-over-parent) it flattens parent+overlay into a
    /// flat env so the captured copy exposes the full lexical view to consumers
    /// that iterate it overlay-only (nested call merges, `clone_for_thread`). See
    /// docs/vm-dual-store.md (Slice 6).
    #[allow(dead_code)] // env-loan (CP-1 1e): VM callers migrated to the seam; kept for carriers.
    pub(crate) fn clone_env(&self) -> Env {
        self.env.flattened()
    }

    /// Replace the entire env.
    #[allow(dead_code)]
    pub(crate) fn set_env(&mut self, env: Env) {
        self.env = env;
    }

    /// Take the env out, replacing it with an empty Env.
    #[allow(dead_code)]
    pub(crate) fn take_env(&mut self) -> Env {
        std::mem::take(&mut self.env)
    }

    pub(crate) fn normalize_var_meta_name(name: &str) -> &str {
        name.trim_start_matches(['$', '@', '%', '&'])
    }

    fn var_meta_value_key(name: &str) -> String {
        format!("__mutsu_var_meta::{}", name)
    }

    pub(crate) fn set_var_dynamic(&mut self, name: &str, dynamic: bool) {
        let key = Self::normalize_var_meta_name(name).to_string();
        self.var_dynamic_flags.insert(key, dynamic);
    }

    pub(crate) fn set_var_meta_value(&mut self, name: &str, value: Value) {
        self.env.insert(Self::var_meta_value_key(name), value);
    }

    pub(crate) fn var_meta_value(&self, name: &str) -> Option<Value> {
        self.env.get(&Self::var_meta_value_key(name)).cloned()
    }

    pub(crate) fn set_var_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        let key = name.to_string();
        let meta_key = format!("__mutsu_type::{}", key);
        if let Some(constraint) = constraint {
            let info = Self::parse_container_constraint(name, &constraint);
            if info.value_type == "atomicint" || constraint.contains("atomicint") {
                self.atomic_var_seen = true;
            }
            self.var_type_constraints
                .insert(key.clone(), info.value_type.clone());
            self.env
                .insert(meta_key, Value::str(info.value_type.clone()));
            let hash_key_meta_key = format!("__mutsu_hash_key_type::{}", key);
            if let Some(key_type) = info.key_type.clone() {
                self.var_hash_key_constraints
                    .insert(key.clone(), key_type.clone());
                self.env.insert(hash_key_meta_key, Value::str(key_type));
            } else {
                self.var_hash_key_constraints.remove(&key);
                self.env.remove(&hash_key_meta_key);
            }
            // Only register container type metadata for container-sigil variables
            // (`@a`, `%h`). For scalar parameters (e.g. `Mu $a`) the bound value
            // may share an `Arc` with a caller's container, and tagging that Arc
            // would corrupt the caller's container type metadata via Arc pointer
            // keying (and Arc pointer reuse after drop).
            if name.starts_with('@') || name.starts_with('%') {
                self.register_var_container_type_metadata(&key, &info);
            }
        } else {
            self.var_type_constraints.remove(&key);
            self.var_hash_key_constraints.remove(&key);
            self.env.remove(&meta_key);
            self.env.remove(&format!("__mutsu_hash_key_type::{}", key));
        }
    }

    /// Register the type constraint of a *bound routine parameter*. For scalar
    /// parameters the constraint is written ONLY to the `env`-keyed
    /// `__mutsu_type::name` metadata (which is scoped — dropped when the callee's
    /// env is restored) and NOT to the global, name-keyed `var_type_constraints`
    /// map. This is what stops a typed parameter (`Str:D $x`) from leaking its
    /// constraint onto a same-named lexical in the *caller* (`my $x = f(...)`,
    /// where `f`'s parameter is also `$x`): the global map would otherwise retain
    /// the entry after the callee returns, and the env-first/`var_type_constraints`-
    /// fallback read would surface it. `my`-declared and `subset` constraints
    /// still go through `set_var_type_constraint` (both stores), so the global-map
    /// fallback remains available where the env entry isn't visible (e.g. an
    /// `EVAL`'d re-assignment to a `subset`-typed lexical). Container parameters
    /// (`@a`/`%h`) keep the full behaviour — their element/key-type metadata is
    /// consulted via the global map / container metadata for element checks.
    pub(crate) fn bind_param_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        if name.starts_with('@') || name.starts_with('%') {
            self.set_var_type_constraint(name, constraint);
            return;
        }
        let meta_key = format!("__mutsu_type::{}", name);
        match constraint {
            Some(c) => {
                let info = Self::parse_container_constraint(name, &c);
                if info.value_type == "atomicint" || c.contains("atomicint") {
                    self.atomic_var_seen = true;
                }
                self.env.insert(meta_key, Value::str(info.value_type));
            }
            None => {
                // An untyped scalar parameter shadows any same-named lexical: it
                // has NO constraint in the callee's scope. Clear both the env
                // metadata AND a possibly-stale `var_type_constraints` entry left
                // by an earlier `my Type $x` declaration whose block has exited
                // (the global map is not block-scoped). Without this, reading the
                // (Nil-defaulted) parameter would surface the stale constraint via
                // the global-map fallback and return the type object instead of
                // Nil — roast S02-types/nil.t f4. An enclosing typed lexical that
                // is still in scope keeps its own env metadata, so its enforcement
                // (read env-first) survives the callee's return.
                self.env.remove(&meta_key);
                self.var_type_constraints.remove(name);
            }
        }
    }

    pub(crate) fn var_type_constraint(&self, name: &str) -> Option<String> {
        let key = name;
        let meta_key = format!("__mutsu_type::{}", key);
        if let Some(Value::Str(tc)) = self.env.get(&meta_key) {
            return Some(tc.to_string());
        }
        if let Some(tc) = self.var_type_constraints.get(key) {
            return Some(tc.clone());
        }
        None
    }

    /// Fast type constraint lookup — only checks the `var_type_constraints` HashMap,
    /// skipping the `format!("__mutsu_type::...")` + env lookup. Used by the SetLocal
    /// fast path for simple scalar variables where the env-based constraint is never set.
    pub(crate) fn var_type_constraint_fast(&self, name: &str) -> Option<&String> {
        self.var_type_constraints.get(name)
    }

    /// Whether any `atomicint`/atomic-storage variable has ever been registered
    /// (monotonic). When false, the hot variable-read path can skip the entire
    /// atomic-variable check (which otherwise costs `format!`s and constraint
    /// lookups on every `GetGlobal`/`GetLocal`).
    #[inline(always)]
    pub(crate) fn atomic_var_seen(&self) -> bool {
        self.atomic_var_seen
    }

    /// Mark that an atomic variable / atomic storage has been registered.
    pub(crate) fn mark_atomic_var_seen(&mut self) {
        self.atomic_var_seen = true;
    }

    /// Set the default value for a variable declared with `is default(...)`.
    pub(crate) fn set_var_default(&mut self, name: &str, value: Value) {
        self.var_defaults.insert(name.to_string(), value);
    }

    /// Get the default value for a variable, if one was set with `is default(...)`.
    pub(crate) fn var_default(&self, name: &str) -> Option<&Value> {
        self.var_defaults.get(name)
    }

    /// Remove a variable's cached `is default(...)` value. Called on
    /// variable redeclaration so a new `my @a` does not inherit the
    /// default from an earlier same-named variable.
    pub(crate) fn clear_var_default(&mut self, name: &str) {
        self.var_defaults.remove(name);
    }

    /// Get the evaluated `is default(...)` value for a class attribute.
    pub(crate) fn class_attribute_default(
        &self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<Value> {
        self.registry()
            .class_attribute_defaults
            .get(&(class_name.to_string(), attr_name.to_string()))
            .cloned()
    }

    /// Get the `is DEPRECATED` message for a class attribute accessor.
    pub(crate) fn class_attribute_deprecated(
        &self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<String> {
        self.registry()
            .class_attribute_deprecated
            .get(&(class_name.to_string(), attr_name.to_string()))
            .cloned()
    }

    /// Attach an `is default(...)` element default to a container, returning
    /// the (possibly rebuilt) value. For both arrays and hashes the default is
    /// embedded in the backing `ArrayData`/`HashData` so it travels with the
    /// container through copy-on-write; callers MUST store the returned value
    /// back into the slot it came from.
    pub(crate) fn tag_container_default(&mut self, value: Value, default: Value) -> Value {
        match value {
            Value::Array(mut arc, kind) => {
                if arc.default.as_deref() != Some(&default) {
                    Arc::make_mut(&mut arc).default = Some(Box::new(default));
                }
                Value::Array(arc, kind)
            }
            Value::Hash(mut map) => {
                if map.default.as_deref() != Some(&default) {
                    Arc::make_mut(&mut map).default = Some(Box::new(default));
                }
                Value::Hash(map)
            }
            other => other,
        }
    }

    /// Get the element default for a container (Array/Hash).
    pub(crate) fn container_default<'v>(&'v self, value: &'v Value) -> Option<&'v Value> {
        match value {
            Value::Array(items, ..) => items.default.as_deref(),
            Value::Hash(map) => map.default.as_deref(),
            _ => None,
        }
    }

    pub(crate) fn var_hash_key_constraint(&self, name: &str) -> Option<String> {
        let key = name;
        let meta_key = format!("__mutsu_hash_key_type::{}", key);
        if let Some(Value::Str(tc)) = self.env.get(&meta_key) {
            return Some(tc.to_string());
        }
        self.var_hash_key_constraints.get(key).cloned()
    }

    /// Fast check for hash key constraint — only checks the HashMap,
    /// skipping the `format!("__mutsu_hash_key_type::...")` + env lookup.
    pub(crate) fn var_hash_key_constraint_fast(&self, name: &str) -> bool {
        self.var_hash_key_constraints.contains_key(name)
    }
}
