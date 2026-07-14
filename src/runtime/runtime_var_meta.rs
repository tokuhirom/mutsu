use super::*;

/// Process-global, monotonic: set the first time any atomic variable / atomic
/// storage is registered on ANY interpreter. See
/// [`Interpreter::atomic_var_seen`] for why this cannot be per-interpreter.
static ATOMIC_VAR_SEEN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

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
        // `is_var_dynamic` reads this map as `.get(bare).copied().unwrap_or(false)`,
        // so an absent entry already means "not dynamic". The common case — a plain
        // non-dynamic `my $x` — therefore needs no owned-String key allocation +
        // insert; only remove a stale `true` left by a same-named dynamic shadow
        // (and only when the map is non-empty, avoiding the sigil-strip alloc).
        let key = Self::normalize_var_meta_name(name);
        if dynamic {
            self.var_dynamic_flags.insert(key.to_string(), true);
        } else if !self.var_dynamic_flags.is_empty() {
            self.var_dynamic_flags.remove(key);
        }
    }

    pub(crate) fn set_var_meta_value(&mut self, name: &str, value: Value) {
        self.env.insert(Self::var_meta_value_key(name), value);
    }

    pub(crate) fn var_meta_value(&self, name: &str) -> Option<Value> {
        self.env.get(&Self::var_meta_value_key(name)).cloned()
    }

    pub(crate) fn set_var_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        if let Some(constraint) = constraint {
            let key = name.to_string();
            let meta_key = format!("__mutsu_type::{}", key);
            let info = Self::parse_container_constraint(name, &constraint);
            if info.value_type == "atomicint" || constraint.contains("atomicint") {
                self.mark_atomic_var_seen();
            }
            self.var_type_constraints
                .insert(key.clone(), info.value_type.clone());
            self.env
                .insert(meta_key, Value::str(info.value_type.clone()));
            self.env_type_constraint_seen = true;
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
            // Fast path for the overwhelmingly common case: a plain `my $x`
            // declaration clearing a constraint that was never set. Every such
            // declaration reaches here (via `SetVarDynamic`), so avoid the two
            // `format!` key allocations + the `Symbol::intern`ing `env.remove`s
            // (the env is Symbol-keyed) unless there is actually something to
            // clear. `env_type_constraint_seen` latches true only once an
            // env-scoped `__mutsu_type::*` entry has ever been inserted, so when
            // it is false no such env entry can exist to remove. The two map
            // removes borrow `name` (`&str`) and never allocate.
            let had_constraint = self.var_type_constraints.remove(name).is_some();
            let had_hash_key = self.var_hash_key_constraints.remove(name).is_some();
            if had_constraint || had_hash_key || self.env_type_constraint_seen {
                self.env.remove(&format!("__mutsu_type::{}", name));
                self.env.remove(&format!("__mutsu_hash_key_type::{}", name));
            }
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
                    self.mark_atomic_var_seen();
                }
                self.env.insert(meta_key, Value::str(info.value_type));
                self.env_type_constraint_seen = true;
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
        // Fast path: if no env-scoped constraint has ever been registered, the
        // name-keyed global map is authoritative — skip the `format!` + env lookup.
        // env-first ordering only matters once a param has shadowed a lexical, which
        // requires an env constraint (flag=true). See `env_type_constraint_seen`.
        if !self.env_type_constraint_seen {
            return self.var_type_constraints.get(key).cloned();
        }
        let meta_key = format!("__mutsu_type::{}", key);
        if let Some(ValueView::Str(tc)) = self.env.get(&meta_key).map(Value::view) {
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
    /// *on this interpreter* (monotonic). When false, the hot variable-read path
    /// skips the entire atomic-variable check (which otherwise costs `format!`s
    /// and constraint lookups on every `GetGlobal`/`GetLocal`). Deliberately a
    /// plain field, not the atomic below: this is read on the hottest op in the
    /// VM, and an opaque atomic load there is not free.
    #[inline(always)]
    pub(crate) fn atomic_var_seen(&self) -> bool {
        self.atomic_var_seen
    }

    /// Whether any atomic variable has ever been registered *anywhere in the
    /// process* (monotonic). Needed — and only used — by the reset path
    /// (`reset_atomic_var_key`), because `cas $x` inside a `start` block runs on
    /// the WORKER's interpreter: with a per-interpreter flag the parent's copy
    /// stays false, so a later `my $x` redeclaration in the parent skipped the
    /// reset that detaches the new variable from the worker's shared atomic cell
    /// (`t/cross-thread-shared-var-writeback-coherence.t` 4/6 — a later block's
    /// `$seen` inherited an earlier block's contents). An over-set is
    /// conservative: it only makes the (correct) reset run.
    #[inline(always)]
    pub(crate) fn atomic_var_seen_anywhere() -> bool {
        ATOMIC_VAR_SEEN.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Mark that an atomic variable / atomic storage has been registered, both on
    /// this interpreter (for the read gates) and process-wide (for the reset gate).
    pub(crate) fn mark_atomic_var_seen(&mut self) {
        self.atomic_var_seen = true;
        ATOMIC_VAR_SEEN.store(true, std::sync::atomic::Ordering::Relaxed);
    }

    /// Whether any sigilless-parameter alias (`__mutsu_sigilless_alias::name` env
    /// key) has ever been registered (monotonic). When false, the write-back path
    /// can skip the entire `propagate_sigilless_alias_chain` walk (which otherwise
    /// costs a `format!` + env lookup on every inc-dec / compound-assign).
    #[inline(always)]
    pub(crate) fn sigilless_alias_seen(&self) -> bool {
        self.sigilless_alias_seen
    }

    /// Mark that a sigilless-parameter alias env key has been registered. Called at
    /// every `__mutsu_sigilless_alias::*` insert site (see `sigilless_alias_key`).
    pub(crate) fn mark_sigilless_alias_seen(&mut self) {
        self.sigilless_alias_seen = true;
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
    pub(crate) fn tag_container_default(&mut self, mut value: Value, default: Value) -> Value {
        if matches!(value.view(), ValueView::Array(..)) {
            let (mut arc, kind) = value.into_array().unwrap();
            if arc.default.as_deref() != Some(&default) {
                crate::gc::Gc::make_mut(&mut arc).default = Some(Box::new(default));
            }
            return Value::array_with_kind(arc, kind);
        }
        value.with_hash_mut(|map| {
            if map.default.as_deref() != Some(&default) {
                crate::gc::Gc::make_mut(map).default = Some(Box::new(default));
            }
        });
        value
    }

    /// Embed each `@`/`%` attribute's `is default(...)` element default (from
    /// `class_attribute_defaults`) into the freshly-constructed instance's
    /// containers, so a missing-element read returns the declared default and
    /// the value survives copy-on-write. Scalar attributes are skipped (their
    /// default is carried via `var_defaults` and the unassigned-scalar read).
    pub(crate) fn apply_container_attribute_defaults(
        &mut self,
        class_name: &str,
        attributes: &mut crate::value::AttrMap,
    ) {
        let names: Vec<crate::symbol::Symbol> = attributes.keys().copied().collect();
        for attr_name in names {
            if !matches!(
                attributes.get(attr_name).map(Value::view),
                Some(ValueView::Array(..)) | Some(ValueView::Hash(_))
            ) {
                continue;
            }
            // Prefer the already-evaluated default (non-generic classes); fall back
            // to a deferred expression carried from a parametric role, evaluated now
            // (the caller has bound the role's type params in `self.env`).
            let def = self
                .class_attribute_default(class_name, attr_name.as_str())
                .or_else(|| {
                    let expr = self
                        .registry()
                        .class_attribute_default_exprs
                        .get(&(class_name.to_string(), attr_name.resolve()))
                        .cloned()?;
                    self.eval_block_value(&[crate::ast::Stmt::Expr(expr)]).ok()
                });
            if let Some(def) = def
                && let Some(val) = attributes.remove(attr_name)
            {
                let tagged = self.tag_container_default(val, def);
                attributes.insert(attr_name, tagged);
            }
        }
    }

    /// Get the element default for a container (Array/Hash).
    pub(crate) fn container_default(&self, value: &Value) -> Option<Value> {
        match value.view() {
            ValueView::Array(items, ..) => items.default.as_deref().cloned(),
            ValueView::Hash(map) => map.default.as_deref().cloned(),
            _ => None,
        }
    }

    pub(crate) fn var_hash_key_constraint(&self, name: &str) -> Option<String> {
        let key = name;
        let meta_key = format!("__mutsu_hash_key_type::{}", key);
        if let Some(ValueView::Str(tc)) = self.env.get(&meta_key).map(Value::view) {
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
