use super::*;

/// Process-global, monotonic: set the first time any atomic variable / atomic
/// storage is registered on ANY interpreter. See
/// [`Interpreter::atomic_var_seen`] for why this cannot be per-interpreter.
static ATOMIC_VAR_SEEN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Process-global, monotonic: set the first time any variable type constraint
/// is registered on ANY interpreter (see
/// [`Interpreter::env_type_constraint_seen`]). It gates a `format!` + `env.get`
/// that would otherwise run on every variable write-back, and the overwhelming
/// majority of programs declare no typed lexical at all.
///
/// Process-global rather than per-interpreter for the same reason
/// [`Interpreter::atomic_var_seen_anywhere`] is: an interpreter that *adopts*
/// another one's env (the `throws-like` nested EVAL, the regex-scratch
/// interpreters, `clone_for_thread`) inherits its `__mutsu_type::*` keys but is
/// constructed fresh, so a per-interpreter flag starts `false` and silently
/// disables enforcement for constraints that are demonstrably right there in
/// the env it was handed (`roast/S02-types/type.t` 5-11). Enumerating every
/// such adoption site is exactly the completeness-dependent design CLAUDE.md
/// calls the higher-risk route, and getting it wrong turns a loud refusal into
/// a silent wrong answer. An over-set is conservative: it only makes the
/// (correct) env lookup run.
static ENV_TYPE_CONSTRAINT_SEEN: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

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

    /// The env key for `name`'s type-constraint metadata, `__mutsu_type::<name>`,
    /// as a pre-interned `Symbol`.
    ///
    /// Every typed-lexical probe (`var_type_constraint`, the bind/set writers)
    /// used to `format!` the key and intern the fresh `String`; once any
    /// program declares one typed lexical, that ran on every `SetLocal`,
    /// `SetGlobal` and parameter bind in it. The `name -> key` mapping never
    /// changes (symbols are append-only), so it is memoized per thread, keyed
    /// by the name's own symbol.
    pub(crate) fn type_meta_key_sym(name: &str) -> Symbol {
        thread_local! {
            static META_KEYS: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Symbol>> =
                std::cell::RefCell::new(rustc_hash::FxHashMap::default());
        }
        let name_sym = Symbol::intern(name);
        if let Some(sym) = META_KEYS.with(|c| c.borrow().get(&name_sym).copied()) {
            return sym;
        }
        let sym = Symbol::intern(&format!("{}{}", crate::symbol::TYPE_META_PREFIX, name));
        META_KEYS.with(|c| {
            c.borrow_mut().insert(name_sym, sym);
        });
        sym
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
        self.set_var_type_constraint_impl(name, constraint, true);
    }

    /// [`Self::set_var_type_constraint`] for DECLARATION position (`my Int
    /// @a`): registers the name-keyed constraint but does NOT tag whatever
    /// same-named value currently sits in `env` with container type metadata.
    /// At declaration time the initializer has not run yet, so the env value —
    /// when one exists at all — belongs to an OUTER scope or a previous loop
    /// iteration (env frames are inherited): a module method's `my CSV::Field
    /// @f` tagged the CALLER script's `@f` Arc, making its `.raku` render
    /// `Array[CSV::Field].new(...)` (Text::CSV 46_eol_si). The declared
    /// variable's own value is tagged by the assignment/default paths, which
    /// consult the name-keyed constraint registered here.
    pub(crate) fn set_var_type_constraint_decl(&mut self, name: &str, constraint: Option<String>) {
        self.set_var_type_constraint_impl(name, constraint, false);
    }

    /// [`Self::set_var_type_constraint_decl`] for a scalar `my`/`state`
    /// declared LEXICALLY INSIDE a routine (`OpCode::SetVarTypeScoped`):
    /// registers the constraint ONLY in the env-scoped `__mutsu_type::`
    /// metadata, exactly like a typed parameter
    /// ([`Self::bind_param_type_constraint`]), and never in the global
    /// name-keyed metadata. The env entry is dropped with the routine frame
    /// and travels with a captured closure env, so the constraint cannot leak
    /// onto a same-named variable in another frame — the Text::CSV
    /// `t/66_formula.t` shape, where a module method's `my Str $e = ...`
    /// poisoned the caller script's untyped `$e` (see
    /// `news/2026-09/type-constraint-global-side-table-retired.md`).
    ///
    /// Since ADR-0042 slice 3 this differs from
    /// [`Self::set_var_type_constraint_decl`] only in NOT tagging a same-named
    /// env value with container metadata; both write the same single
    /// env-scoped lane.
    pub(crate) fn set_var_type_constraint_routine_scoped(&mut self, name: &str, constraint: &str) {
        let info = Self::parse_container_constraint(name, constraint);
        if info.value_type == "atomicint" || constraint.contains("atomicint") {
            self.mark_atomic_var_seen();
        }
        self.env.insert(
            format!("__mutsu_type::{}", name),
            Value::str(info.value_type),
        );
        // ADR-0042 slice 1: an object-hash's key type (`my %h{Int}`) must be
        // scoped the same way its value type is. `var_hash_key_constraint`
        // checks this env-scoped key first, so registering it here is what
        // lets the container-tagging step right after `SetVarTypeScoped`
        // (`exec_set_var_type`) embed the key type onto the freshly-declared
        // hash. Without it a key-only object hash declared inside a routine
        // (now scoped since step 3 stopped excluding `%` from the scoped
        // opcode) silently lost key-type enforcement.
        let hash_key_meta_key = format!("__mutsu_hash_key_type::{}", name);
        if let Some(key_type) = info.key_type {
            self.env.insert(hash_key_meta_key, Value::str(key_type));
        } else {
            self.env.remove(&hash_key_meta_key);
        }
        Self::mark_env_type_constraint_seen();
    }

    fn set_var_type_constraint_impl(
        &mut self,
        name: &str,
        constraint: Option<String>,
        tag_env_value: bool,
    ) {
        if let Some(constraint) = constraint {
            let key = name.to_string();
            let meta_key = Self::type_meta_key_sym(name);
            let info = Self::parse_container_constraint(name, &constraint);
            if info.value_type == "atomicint" || constraint.contains("atomicint") {
                self.mark_atomic_var_seen();
            }
            self.env
                .insert_sym(meta_key, Value::str(info.value_type.clone()));
            Self::mark_env_type_constraint_seen();
            let hash_key_meta_key = format!("__mutsu_hash_key_type::{}", key);
            if let Some(key_type) = info.key_type.clone() {
                self.env.insert(hash_key_meta_key, Value::str(key_type));
            } else {
                self.env.remove(&hash_key_meta_key);
            }
            // Only register container type metadata for container-sigil variables
            // (`@a`, `%h`). For scalar parameters (e.g. `Mu $a`) the bound value
            // may share an `Arc` with a caller's container, and tagging that Arc
            // would corrupt the caller's container type metadata via Arc pointer
            // keying (and Arc pointer reuse after drop).
            if tag_env_value && (name.starts_with('@') || name.starts_with('%')) {
                self.register_var_container_type_metadata(&key, &info);
            }
        } else {
            // Fast path for the overwhelmingly common case: a plain `my $x`
            // declaration clearing a constraint that was never set. Every such
            // declaration reaches here (via `SetVarDynamic`), so avoid the two
            // `format!` key allocations + the `Symbol::intern`ing `env.remove`s
            // (the env is Symbol-keyed) unless there is actually something to
            // clear. `env_type_constraint_seen` latches true only once a
            // `__mutsu_type::*` entry has ever been inserted, so when it is
            // false no such env entry can exist to remove.
            if !Self::env_type_constraint_seen() {
                return;
            }
            self.env.remove(&format!("__mutsu_type::{}", name));
            self.env.remove(&format!("__mutsu_hash_key_type::{}", name));
        }
    }

    /// Re-attach the key type of an object hash that is being bound to a `%`
    /// parameter whose own declaration says nothing about keys.
    ///
    /// Object-hash-ness lives in two places: `HashData::key_type` on the value,
    /// and the env-scoped `__mutsu_hash_key_type::` metadata every subscript
    /// path consults by name. Binding `my %o{Mu}` to a plain
    /// `sub f(%h)` parameter registered `%h` with the implicit value type `Any`
    /// and NO key type — which both hid the object-hash keying from `%h`'s
    /// subscripts (they stringified the key object, warning
    /// "Use of uninitialized value of type S in string context") and, because the
    /// registration re-tags the value, stripped `key_type` off the *caller's*
    /// hash. The entries stay physically `.WHICH`-keyed either way, so dropping
    /// the flag does not un-key them; it just makes them unreadable.
    ///
    /// A parameter imposes no key type of its own, so fold the argument's key
    /// type into the constraint (`Any` -> `Any{Mu}`) and let the normal
    /// registration path carry it. An explicitly key-typed parameter
    /// (`%h{Str}`) already carries one and is left alone.
    fn keep_object_hash_key_type(&self, name: &str, constraint: Option<String>) -> Option<String> {
        if !name.starts_with('%') {
            return constraint;
        }
        let Some(value_type) = constraint.as_deref() else {
            return constraint;
        };
        if value_type.contains('{') {
            return constraint;
        }
        let key_type = self.env.get(name).and_then(|v| match v.view() {
            ValueView::Hash(map) => map.key_type.clone().filter(|kt| !kt.is_empty()),
            _ => None,
        });
        match key_type {
            Some(kt) => Some(format!("{value_type}{{{kt}}}")),
            None => constraint,
        }
    }

    /// Register the type constraint of a *bound routine parameter*. For scalar
    /// parameters the constraint is written to the `env`-keyed
    /// `__mutsu_type::name` metadata, which is scoped — dropped when the
    /// callee's env is restored. That is what stops a typed parameter
    /// (`Str:D $x`) from leaking its constraint onto a same-named lexical in
    /// the *caller* (`my $x = f(...)`, where `f`'s parameter is also `$x`).
    /// Since ADR-0042 slice 3 retired the process-global side table this is
    /// simply how EVERY name-keyed constraint is registered; what still
    /// distinguishes a parameter is the `None` arm below, which must actively
    /// clear an inherited entry because an untyped parameter shadows a
    /// same-named outer lexical. Container parameters (`@a`/`%h`) go through
    /// the full `set_var_type_constraint`, which also tags the bound value so
    /// element checks can read the constraint off the container.
    pub(crate) fn bind_param_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        if name.starts_with('@') || name.starts_with('%') {
            let constraint = self.keep_object_hash_key_type(name, constraint);
            self.set_var_type_constraint(name, constraint);
            return;
        }
        let meta_key = Self::type_meta_key_sym(name);
        match constraint {
            Some(c) => {
                let info = Self::parse_container_constraint(name, &c);
                if info.value_type == "atomicint" || c.contains("atomicint") {
                    self.mark_atomic_var_seen();
                }
                self.env.insert_sym(meta_key, Value::str(info.value_type));
                Self::mark_env_type_constraint_seen();
            }
            None => {
                // An untyped scalar parameter shadows any same-named lexical: it
                // has NO constraint in the callee's scope, so drop the
                // (inherited) env metadata for the callee frame. The caller's
                // own entry lives in the caller's env and is restored with it,
                // so the enclosing lexical keeps its enforcement after the
                // callee returns.
                self.env.remove_sym(meta_key);
            }
        }
    }

    /// The name-keyed type constraint currently in effect for `name`, or `None`.
    ///
    /// ADR-0042 slice 3: there is exactly ONE name-keyed lane left, the
    /// env-scoped `__mutsu_type::<name>` entry. It is dropped with the frame /
    /// block that declared it, so a typed declaration can no longer be observed
    /// from a scope it does not enclose. Everything else about a constraint —
    /// enforcement on assignment, on element stores, and through a
    /// differently-named bound alias — is read off the container that carries
    /// it (`ContainerCell`'s scalar `of`, `ArrayData`/`HashData`'s
    /// `value_type`/`key_type`), which is why deleting the global side table
    /// this method used to fall back to changed no observable behaviour.
    pub(crate) fn var_type_constraint(&self, name: &str) -> Option<String> {
        // Most programs declare no typed lexical at all; when the monotonic
        // flag is clear no `__mutsu_type::*` entry can exist, so skip the
        // `format!` + env probe entirely.
        if !Self::env_type_constraint_seen() {
            return None;
        }
        let meta_key = Self::type_meta_key_sym(name);
        match self.env.get_sym(meta_key).map(Value::view) {
            Some(ValueView::Str(tc)) => Some(tc.to_string()),
            _ => None,
        }
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

    /// Whether any variable type constraint has ever been registered in this
    /// process (monotonic). See [`ENV_TYPE_CONSTRAINT_SEEN`] for why this is
    /// process-global rather than a per-interpreter field.
    #[inline(always)]
    pub(crate) fn env_type_constraint_seen() -> bool {
        ENV_TYPE_CONSTRAINT_SEEN.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Latch [`Self::env_type_constraint_seen`]. Called at every
    /// `__mutsu_type::*` / `__mutsu_hash_key_type::*` env-insert site.
    #[inline(always)]
    pub(crate) fn mark_env_type_constraint_seen() {
        ENV_TYPE_CONSTRAINT_SEEN.store(true, std::sync::atomic::Ordering::Relaxed);
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
        if self.var_defaults.is_empty() {
            return None;
        }
        self.var_defaults.get(name)
    }

    /// Remove a variable's cached `is default(...)` value. Called on
    /// variable redeclaration so a new `my @a` does not inherit the
    /// default from an earlier same-named variable.
    pub(crate) fn clear_var_default(&mut self, name: &str) {
        // Runs on every `my` declaration; `is default(...)` is rare, so the
        // common program's map is empty and the (SipHash-keyed) probe is waste.
        if self.var_defaults.is_empty() {
            return;
        }
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

    /// [`Self::class_attribute_default`], falling back to evaluating a
    /// role-composed attribute's deferred `is default(...)` expression.
    ///
    /// A *directly declared* class attribute's `is default(...)` is
    /// evaluated once at registration and cached in `class_attribute_defaults`
    /// (a `Value` table). A *role-composed* attribute's `is default(...)` may
    /// reference the role's type parameters (`is default(T)`), so it cannot
    /// be evaluated until composition — it is copied onto the consuming class
    /// as a raw expression in `class_attribute_default_exprs` instead (see
    /// `registration_class_compose.rs`). Reading only the `Value` table (as
    /// `class_attribute_default` does) silently treats a role-composed
    /// attribute as if it declared no default at all — this is the fallback
    /// that `apply_container_attribute_defaults` already applies for
    /// `@`/`%` element defaults; scalar restore-on-Nil callers need the same.
    pub(crate) fn class_attribute_default_with_role_fallback(
        &mut self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<Value> {
        self.class_attribute_default(class_name, attr_name)
            .or_else(|| {
                let arg = self
                    .registry()
                    .class_attribute_default_exprs
                    .get(&(class_name.to_string(), attr_name.to_string()))
                    .cloned()?;
                self.eval_decl_trait_arg(&arg).ok()
            })
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
        // Fast gate: a class with no `is default(...)` element default makes every
        // per-attribute registry probe below return `None`, so the whole scan (the
        // keys `Vec` plus a `(String, String)` registry-key allocation per container
        // attribute) is pure waste. The per-class flag lives on the cached
        // `NativeCtorPlan`; `dispatch_bless` fetches (and caches) the plan before
        // calling here, so the cache hit covers every construction including the
        // first. A cache miss (unregistered / not-yet-planned class) falls through
        // to the full scan, which is safe.
        let skip = self
            .native_ctor_plan_cache
            .get(&crate::symbol::Symbol::intern(class_name))
            .is_some_and(|p| !p.has_container_defaults);
        if skip {
            return;
        }
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
                    let arg = self
                        .registry()
                        .class_attribute_default_exprs
                        .get(&(class_name.to_string(), attr_name.resolve()))
                        .cloned()?;
                    self.eval_decl_trait_arg(&arg).ok()
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

    /// The object-hash key type in effect for `name` (`my %h{Int}`), or `None`.
    ///
    /// The twin of [`Self::var_type_constraint`], and retired the same way by
    /// ADR-0042 slice 3: the env-scoped `__mutsu_hash_key_type::<name>` entry
    /// is the only name-keyed lane, with `HashData::key_type` on the value
    /// carrying it everywhere a name is not available. The attribute fallback
    /// stays — an attribute's declared type lives in the class registry and is
    /// not a lexical at all.
    pub(crate) fn var_hash_key_constraint(&self, name: &str) -> Option<String> {
        let meta_key = format!("__mutsu_hash_key_type::{}", name);
        if let Some(ValueView::Str(tc)) = self.env.get(&meta_key).map(Value::view) {
            return Some(tc.to_string());
        }
        self.attr_hash_key_constraint(name)
    }

    /// The key type of an object-hash *attribute* (`has Callable %!Conv{Mu:U}`)
    /// referenced as `%!Conv` / `%.Conv` inside a method. The lexical
    /// `__mutsu_hash_key_type::` lane cannot carry this — an attribute is not
    /// a lexical and its declared type lives in the class registry — so
    /// resolve it against the current `self`'s class, exactly as
    /// `scalar_attr_type_constraint` does for typed scalar attributes. The
    /// declared type is stored as `ValueType{KeyType}` (see
    /// `parser::stmt::decl::has_decl`), so the key part is split back out here.
    fn attr_hash_key_constraint(&self, name: &str) -> Option<String> {
        if !name.starts_with('%') {
            return None;
        }
        let (bare, _) = crate::value::attr_twigil_base(name)?;
        let tc = self.self_attr_type_constraint(bare)?;
        let (_, key_type) = crate::runtime::types::split_object_hash_constraint(&tc);
        key_type.map(str::to_string)
    }
}

impl Interpreter {
    /// The methods a `.VAR` container descriptor answers ITSELF (ADR-0064).
    ///
    /// Everything NOT listed here is a question about the value the container
    /// holds, and Raku answers it from that value: `.VAR` hands back the real
    /// `Scalar`/`Array`/`Hash` container, and a container is transparent for
    /// ordinary method dispatch. Only the container's own properties -- its
    /// name, its dynamism, its declared default and element type, and its
    /// identity/type reflection -- stop at the descriptor.
    ///
    /// `defined` is deliberately owned: a container object is always concrete,
    /// so `my @a; @a[0].VAR.defined` is `True` in Raku even though the element
    /// is `Any`.
    /// The value a `.VAR` reflection descriptor's container currently holds,
    /// or `None` when `target` is not such a descriptor.
    ///
    /// An ELEMENT descriptor (`@a[0].VAR`, built by `builtin_index_var_meta`)
    /// carries the element it was built from: nothing else can find it again,
    /// because `__mutsu_var_target` names the *container* the element lives in,
    /// not the element.
    ///
    /// A VARIABLE descriptor (`$x.VAR`) carries either the variable's shared
    /// `ContainerRef` cell -- dereferenced here, so reads through the
    /// descriptor stay live -- or, when the variable is not boxed, the value
    /// the VM handed `.VAR`, refreshed on every `.VAR` call (see
    /// `var_meta_contained_snapshot`).
    pub(crate) fn var_meta_contained_value(&self, target: &Value) -> Option<Value> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        let map = attributes.as_map();
        let name = match map.get("__mutsu_var_target").map(Value::view) {
            Some(ValueView::Str(name)) => name.to_string(),
            _ => return None,
        };
        if let Some(v) = map.get("__mutsu_var_value") {
            return Some(v.with_deref(|inner| inner.clone()));
        }
        self.env
            .get(&name)
            .map(|v| v.with_deref(|inner| inner.clone()))
    }

    /// What a variable's `.VAR` descriptor should record as the value its
    /// container holds (ADR-0064).
    ///
    /// A variable that is currently boxed in a shared `ContainerRef` cell
    /// records the CELL: every read through the descriptor then dereferences
    /// it, so the descriptor tracks later assignments exactly as Raku's real
    /// container does. An unboxed variable has nothing shareable to point at,
    /// so it records `target` -- the value the VM handed this `.VAR` call,
    /// which is authoritative even when the env half of the dual store has not
    /// been synced from `locals` yet (a plain `my $x` inside a mainline block
    /// is frequently absent from `env` entirely).
    pub(crate) fn var_meta_contained_snapshot(&self, name: &str, target: &Value) -> Value {
        match self.env.get(name) {
            Some(v) if matches!(v.view(), ValueView::ContainerRef(_)) => v.clone(),
            _ => target.clone(),
        }
    }

    /// ADR-0064: dispatch a method on a `.VAR` container descriptor to the
    /// value the container holds.
    ///
    /// The stored/looked-up value is already in its ITEMIZED form -- that is
    /// how a real `Array`/`Hash` element and a `$`-variable's value are both
    /// represented (ADR-0040) -- which is what makes the two Raku spellings
    /// fall straight out:
    ///
    /// - `.gist` shows the container, so it renders the itemized value's
    ///   `.raku` (`@a[1].VAR.gist` is `$[3, 4]`, not `[3 4]`);
    /// - `.raku` shows the contained value, so it decontainerizes first
    ///   (`@a[1].VAR.raku` is `[3, 4]`).
    ///
    /// Both are `Scalar`-only: an `@`/`%` descriptor IS the container, so
    /// `@a.VAR.gist`/`.raku` are just the Array's own (`[1 [3, 4]]` /
    /// `[1, [3, 4]]`).
    pub(crate) fn try_var_meta_delegate(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if var_meta_owns_method(method) {
            return None;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        // A native array's element descriptor (`IntPosRef` & co.) is the same
        // kind of thing as a `Scalar`: a per-element container whose content
        // the descriptor stands in for.
        let is_scalar_container =
            class_name == "Scalar" || class_name.resolve().ends_with("PosRef");
        let value = self.var_meta_contained_value(target)?;
        // A `Scalar` container is NOT `Positional`, so subscripting it follows
        // the one-item rule every non-positional value obeys (`42[0]` is `42`,
        // `42[1]` is an X::OutOfRange over `0..0`) -- with the container itself
        // as that one item. Delegating instead would subscript the CONTENT
        // (`@a[1].VAR[0]` would be `3`), which is a level too deep: raku
        // answers `$[3, 4]`.
        if is_scalar_container
            && method == "AT-POS"
            && let [index] = args
            && let Some(i) = index.as_int()
        {
            return Some(if i == 0 {
                Ok(value)
            } else {
                Ok(RuntimeError::out_of_range_failure(
                    "Index",
                    Value::int(i),
                    "0..0",
                ))
            });
        }
        if is_scalar_container && args.is_empty() {
            match method {
                "gist" => return Some(self.call_method_with_values(value, "raku", vec![])),
                "raku" | "perl" => {
                    return Some(self.call_method_with_values(
                        value.deitemize_element(),
                        "raku",
                        vec![],
                    ));
                }
                _ => {}
            }
        }
        Some(self.call_method_with_values(value, method, args.to_vec()))
    }
}

/// The methods a `.VAR` container descriptor answers ITSELF (ADR-0064).
///
/// Everything NOT listed here is a question about the value the container
/// holds, and Raku answers it from that value: `.VAR` hands back the real
/// `Scalar`/`Array`/`Hash` container, and a container is transparent for
/// ordinary method dispatch. Only the container's own properties -- its name,
/// its dynamism, its declared default and element type, and its identity/type
/// reflection -- stop at the descriptor.
///
/// `defined` is deliberately owned: a container object is always concrete, so
/// `my @a; @a[0].VAR.defined` is `True` in Raku even though the element is
/// `Any`.
pub(crate) fn var_meta_owns_method(method: &str) -> bool {
    // Metamethods (`.^name`, `.^mro`, ...) and private calls describe the
    // descriptor's own type; they never reach the contained value.
    method.starts_with('^')
        || method.starts_with('!')
        || matches!(
            method,
            "VAR"
                | "var"
                | "name"
                | "dynamic"
                | "default"
                | "of"
                | "WHICH"
                | "WHAT"
                | "HOW"
                | "WHO"
                | "WHY"
                | "WHERE"
                | "REPR"
                | "DEFINITE"
                | "defined"
                | "isa"
                | "does"
                | "self"
        )
}

/// Is `target` a `.VAR` container descriptor that must NOT answer `method`
/// natively? (ADR-0064.)
///
/// The native method tables see an attribute-only `Instance` and would answer
/// `.elems`/`.gist`/`.raku`/... out of an empty attribute map. Only the
/// interpreter can resolve the value the container holds -- for a variable
/// descriptor that means reading the variable's live env entry -- so the
/// native fast paths defer, and `Interpreter::try_var_meta_delegate` takes it.
pub(crate) fn var_meta_descriptor_defers(target: &Value, method: &str) -> bool {
    !var_meta_owns_method(method)
        && matches!(
            target.view(),
            ValueView::Instance { attributes, .. }
                if matches!(
                    attributes.as_map().get("__mutsu_var_target").map(Value::view),
                    Some(ValueView::Str(_))
                )
        )
}
