use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView, next_instance_id};

use super::{CustomTypeData, Interpreter};

impl Interpreter {
    /// Dispatch methods on Metamodel::Primitives.
    pub(super) fn metamodel_primitives_dispatch(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "create_type" => self.mp_create_type(args),
            "configure_type_checking" => self.mp_configure_type_checking(args),
            "compose_type" => self.mp_compose_type(args),
            "rebless" => self.mp_rebless(args),
            "is_type" => self.mp_is_type(args),
            "install_method_cache" => self.mp_install_method_cache(args),
            "set_parameterizer" => self.mp_set_parameterizer(args),
            "parameterize_type" => self.mp_parameterize_type(args),
            _ => Err(RuntimeError::new(format!(
                "X::Method::NotFound: No such method '{}' for Metamodel::Primitives",
                method
            ))),
        }
    }

    /// Metamodel::Primitives.create_type($how, $repr?, :$mixin)
    fn mp_create_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let how = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("create_type requires a HOW argument"))?;

        let mut repr = "P6opaque".to_string();

        // Named options such as `:mixin` are accepted but not modelled.
        for arg in args.iter().skip(1) {
            if let ValueView::Str(s) = arg.view() {
                repr = s.to_string();
            }
        }

        let id = next_instance_id();
        self.types.custom_type_data.insert(
            id,
            CustomTypeData::new(),
        );

        Ok(Value::custom_type(
            Box::new(how),
            repr,
            Symbol::intern(""),
            id,
        ))
    }

    /// Metamodel::Primitives.configure_type_checking($type, @cache, :$authoritative, :$call_accepts)
    fn mp_configure_type_checking(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let type_val = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("configure_type_checking requires a type argument"))?;

        let id = match type_val.view() {
            ValueView::CustomType(c) => c.id,
            _ => {
                return Err(RuntimeError::new(
                    "configure_type_checking: first argument must be a custom type",
                ));
            }
        };

        // Second arg is the type check cache (array of types)
        let cache = if let Some(cache_val) = args.get(1) {
            match cache_val.view() {
                ValueView::Array(items, _) => Some(items.to_vec()),
                _ => None,
            }
        } else {
            None
        };

        let mut authoritative = false;
        let mut call_accepts = false;

        for arg in args.iter().skip(2) {
            if let ValueView::Pair(k, v) = arg.view() {
                match k.as_str() {
                    "authoritative" => authoritative = v.truthy(),
                    "call_accepts" => call_accepts = v.truthy(),
                    _ => {}
                }
            }
        }

        if let Some(data) = self.types.custom_type_data.get_mut(&id) {
            data.type_check_cache = cache;
            data.authoritative = authoritative;
            data.call_accepts = call_accepts;
        }

        Ok(type_val)
    }

    /// Metamodel::Primitives.compose_type($type, $configuration)
    fn mp_compose_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let type_val = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("compose_type requires a type argument"))?;

        let id = match type_val.view() {
            ValueView::CustomType(c) => c.id,
            _ => {
                return Err(RuntimeError::new(
                    "compose_type: first argument must be a custom type",
                ));
            }
        };

        if let Some(data) = self.types.custom_type_data.get_mut(&id) {
            data.composed = true;
        }

        Ok(type_val)
    }

    fn custom_type_id(type_val: &Value, op: &str) -> Result<u64, RuntimeError> {
        match type_val.view() {
            ValueView::CustomType(c) => Ok(c.id),
            _ => Err(RuntimeError::new(format!(
                "{op}: first argument must be a custom type"
            ))),
        }
    }

    /// Metamodel::Primitives.install_method_cache($type, %cache, :$authoritative = True)
    // Cost: O(c), c = entries of the cache.
    fn mp_install_method_cache(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let type_val = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("install_method_cache requires a type argument"))?;
        let id = Self::custom_type_id(&type_val, "install_method_cache")?;
        let mut entries = Vec::new();
        if let Some(cache) = args.get(1)
            && let ValueView::Hash(items) = cache.view()
        {
            for (k, v) in items.iter() {
                entries.push((k.to_string(), v.clone()));
            }
        }
        let mut authoritative = true;
        for arg in args.iter().skip(2) {
            if let ValueView::Pair(k, v) = arg.view()
                && k.as_str() == "authoritative"
            {
                authoritative = v.truthy();
            }
        }
        if let Some(data) = self.types.custom_type_data.get_mut(&id) {
            data.method_cache = Some(entries);
            data.method_cache_authoritative = authoritative;
        }
        Ok(Value::NIL)
    }

    /// Metamodel::Primitives.set_parameterizer($obj, &parameterizer)
    // Cost: O(1).
    fn mp_set_parameterizer(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("set_parameterizer requires two arguments"));
        }
        // A package or a custom type may carry the cache; key it by identity.
        let id = match args[0].view() {
            ValueView::CustomType(c) => c.id,
            _ => package_cache_id(&args[0]),
        };
        let entry = self.types.custom_type_data.entry(id).or_insert_with(|| {
            CustomTypeData::new()
        });
        entry.parameterizer = Some(args[1].clone());
        entry.parameterizations.clear();
        Ok(Value::NIL)
    }

    /// Metamodel::Primitives.parameterize_type($obj, @args): the cached
    /// parameterization for these arguments, produced on first use by the
    /// installed parameterizer called as `(root, args)`.
    // Cost: O(p * a) per lookup, p = cached parameterizations, a = argument count.
    fn mp_parameterize_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("parameterize_type requires two arguments"));
        }
        let root = args[0].clone();
        let id = match root.view() {
            ValueView::CustomType(c) => c.id,
            _ => package_cache_id(&root),
        };
        let list = args[1].clone();
        // A List's own identity is per-object; the arguments' element keys
        // are what make two calls with the same arguments share a result.
        let key = match list.view() {
            ValueView::Array(items, _) => items
                .iter()
                .map(crate::value::which_key::value_which_key)
                .collect::<Vec<_>>()
                .join("\u{1f}"),
            _ => crate::value::which_key::value_which_key(&list),
        };
        let Some(data) = self.types.custom_type_data.get(&id) else {
            return Err(RuntimeError::new(
                "parameterize_type: no parameterizer was set for this type",
            ));
        };
        if let Some((_, hit)) = data.parameterizations.iter().find(|(k, _)| *k == key) {
            return Ok(hit.clone());
        }
        let Some(parameterizer) = data.parameterizer.clone() else {
            return Err(RuntimeError::new(
                "parameterize_type: no parameterizer was set for this type",
            ));
        };
        let result = self.call_sub_value(parameterizer, vec![root.clone(), list.clone()])?;
        if let Some(data) = self.types.custom_type_data.get_mut(&id) {
            data.parameterizations.push((key, result.clone()));
        }
        // Remember where a type-object result came from, for
        // `nqp::typeparameterized` and friends.
        if let Some(result_id) = type_cache_id(&result) {
            self.types
                .custom_type_data
                .entry(result_id)
                .or_insert_with(CustomTypeData::new)
                .parameterized_from = Some((root, list));
        }
        Ok(result)
    }

    /// `nqp::setparameterizer($type, $code)`.
    // Cost: O(1).
    pub(crate) fn nqp_set_parameterizer(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.mp_set_parameterizer(args)
    }

    /// `nqp::parameterizetype($type, $params)`.
    // Cost: O(p * a) per lookup, as `Metamodel::Primitives.parameterize_type`.
    pub(crate) fn nqp_parameterize_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.mp_parameterize_type(args)
    }

    /// The `(root, params)` a parameterization was made from, if it is one.
    // Cost: O(1).
    pub(crate) fn type_parameterization(&self, ty: &Value) -> Option<(Value, Value)> {
        let id = type_cache_id(ty)?;
        self.types.custom_type_data.get(&id)?.parameterized_from.clone()
    }

    /// The method-cache entry for `name` on a custom type object: `Some(Ok(v))`
    /// when installed, `Some(Err)` when the cache is authoritative and lacks it,
    /// `None` when no cache applies.
    // Cost: O(c), c = entries of the cache.
    pub(crate) fn custom_type_cached_method(
        &mut self,
        type_val: &Value,
        name: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::CustomType(c) = type_val.view() else {
            return None;
        };
        let data = self.types.custom_type_data.get(&c.id)?;
        let cache = data.method_cache.as_ref()?;
        if let Some((_, code)) = cache.iter().find(|(k, _)| k == name) {
            return Some(Ok(code.clone()));
        }
        // Only ordinary lowercase names are subject to the cache: the
        // upper-case introspection methods (`WHAT`, `HOW`, `WHICH`, ...) and
        // `^meta` calls are answered by the object system itself.
        let ordinary = name.chars().next().is_some_and(|ch| ch.is_ascii_lowercase());
        if !(data.method_cache_authoritative && ordinary) {
            return None;
        }
        let class = match self.custom_type_how_name(type_val) {
            Ok(Some(named)) => named,
            _ => c.name.resolve(),
        };
        Some(Err(RuntimeError::new(format!(
            "X::Method::NotFound: Method '{name}' not found for invocant of class '{class}'"
        ))))
    }

    /// Metamodel::Primitives.rebless($obj, $type)
    fn mp_rebless(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("rebless requires two arguments"));
        }

        let obj = &args[0];
        let target_type = &args[1];

        match (obj.view(), target_type.view()) {
            (ValueView::CustomTypeInstance(d), ValueView::CustomType(c)) => {
                // Store the new HOW in the rebless map so .HOW returns the new type
                self.types.rebless_map.insert(d.id, (*c.how).clone());
                Ok(obj.clone())
            }
            _ => Ok(args[0].clone()),
        }
    }

    /// Metamodel::Primitives.is_type($obj, $type)
    fn mp_is_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("is_type requires two arguments"));
        }

        let obj = &args[0];
        let type_check = &args[1];

        // Use the general smartmatch type checking
        let result = self.smart_match(obj, type_check);
        Ok(Value::truth(result))
    }

    /// Call a user HOW method (`type_check` / `accepts_type` / `find_method`)
    /// during type checking, recording any captured-outer caller scalar it
    /// mutates so the owning caller *local slot* is refreshed at the smartmatch
    /// call site.
    ///
    /// env_dirty substrate (docs/captured-outer-cell-sharing.md §10): a custom
    /// HOW method (e.g. `method type_check(Mu $, Mu \c) { ++$counter; … }`) runs
    /// via the slow `run_resolved_instance_method` path, which merges its
    /// captured-outer scalar writes back into env but does NOT set `env_dirty` or
    /// record a precise writeback. The caller slot is then refreshed only by the
    /// blanket pull — a no-op under double-OFF. Worse, the counter is a
    /// read-modify-write (`++$counter`): with the slot stale, each successive
    /// smartmatch statement re-flushes the stale slot into env, so the increment
    /// is lost across statements. Snapshot the writeback-safe env scalars around
    /// the dispatch and record the names it changes into the retain-on-miss
    /// caller-var writeback, drained at the smartmatch op
    /// (`apply_pending_caller_var_writeback`). Ungated (like the proto-multi slice
    /// S16): the recording is scoped to custom-HOW type-check dispatch only, it is
    /// a precise subset of the blanket reconcile (never changes a correct result),
    /// and it also fixes the latent default-build case where successive bare
    /// sink-context smartmatches (`$obj ~~ T; $obj ~~ U;`) lost the counter.
    pub(super) fn call_how_method_recording_writeback(
        &mut self,
        how: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let pre_env: std::collections::HashMap<Symbol, Value> = self
            .env
            .iter()
            .filter(|(_, v)| Self::is_writeback_safe_scalar(v))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        let result = self.call_method_with_values(how, method, args);
        let changed: Vec<String> = self
            .env
            .iter()
            .filter(|(k, v)| {
                let kn = k.resolve();
                kn != "_"
                    && kn != "$_"
                    && Self::is_writeback_safe_scalar(v)
                    && pre_env.get(*k).map(|p| p != *v).unwrap_or(true)
            })
            .map(|(k, _)| k.resolve())
            .collect();
        for n in changed {
            self.record_caller_var_writeback(&n);
        }
        result
    }

    /// Check if a value matches a CustomType (used by smartmatch).
    /// Implements the Raku type checking protocol:
    /// - With cache: check cache directly, then call_accepts if set
    /// - Without cache (before compose): return false (default ACCEPTS behavior)
    pub(super) fn custom_type_check(&mut self, lhs: &Value, rhs_id: u64, rhs_how: &Value) -> bool {
        let data = self.types.custom_type_data.get(&rhs_id).cloned();
        if let Some(ref data) = data
            && let Some(ref cache) = data.type_check_cache
        {
            // If call_accepts is set, delegate to HOW.accepts_type
            if data.call_accepts {
                // Clear stale pending call arg sources that may have been left
                // behind by a previous VM method call dispatch.  Some dispatch
                // paths (e.g. Metamodel::Primitives methods) do not consume
                // the sources via bind_function_args_values, leaving stale
                // data that corrupts sigilless parameter binding on subsequent
                // method calls.
                self.pending_call_arg_sources = None;
                if let Ok(result) = self.call_how_method_recording_writeback(
                    rhs_how.clone(),
                    "accepts_type",
                    vec![Value::NIL, lhs.clone()],
                ) {
                    return result.truthy();
                }
            }

            // Cache-only check: identity-based matching
            for cached_type in cache {
                if lhs == cached_type {
                    return true;
                }
                if let ValueView::Package(type_name) = cached_type.view()
                    && lhs.isa_check(&type_name.resolve())
                {
                    return true;
                }
            }

            if data.authoritative {
                return false;
            }
        }

        // No cache (before compose): call find_method for ACCEPTS on the HOW
        // (this is what Raku's MOP does), but the default ACCEPTS returns False.
        let _ = self.call_how_method_recording_writeback(
            rhs_how.clone(),
            "find_method",
            vec![Value::NIL, Value::str_from("ACCEPTS")],
        );
        false
    }

    /// Install a custom metaclass on a just-declared grammar: instantiate the
    /// EXPORTHOW-provided HOW type and record the instance as the grammar's
    /// `.HOW`. When the HOW class (or an ancestor) defines a user
    /// `find_method`, the grammar is also entered in `grammar_custom_how` so
    /// the regex engine routes subrule dispatch through it
    /// (Metamodel::GrammarHOW protocol).
    pub(crate) fn install_custom_grammar_how(
        &mut self,
        grammar_name: &str,
        how_type: Value,
    ) -> Result<(), RuntimeError> {
        let how_class = match how_type.view() {
            ValueView::Package(sym) => sym.resolve(),
            _ => return Ok(()),
        };
        // The HOW type must be a user-declared class (a stale/foreign stash
        // entry is ignored rather than failing the grammar declaration).
        if !self.registry().classes.contains_key(&how_class) {
            return Ok(());
        }
        let instance = self.call_method_with_values(how_type, "new", Vec::new())?;
        // `class_mro` computes and caches the MRO on demand — `ClassDef::mro`
        // itself is lazily filled and still empty right after a module load.
        let has_user_find_method = self.class_mro(&how_class).iter().any(|c| {
            self.registry()
                .user_method_overloads(c.as_str(), "find_method")
                .is_some()
        });
        let mut reg = self.registry_mut();
        reg.class_how_values
            .insert(grammar_name.to_string(), instance.clone());
        if has_user_find_method {
            reg.grammar_custom_how
                .insert(grammar_name.to_string(), instance);
        }
        Ok(())
    }

    /// A `class` declared while an EXPORTHOW `class` metaclass mapping is
    /// installed (`EXPORTHOW.WHO.<class> = SomeHOW`, from a `use`d module) gets
    /// an instance of that HOW as its meta-object, so `TheClass.HOW.<method>`
    /// dispatches to the custom HOW's user methods (e.g. the AOP example's
    /// `add_aspect`). Returns `true` when the HOW class defines a user `compose`
    /// method, signalling the caller to run it after the class's custom `is`
    /// traits have been applied (see `advent2011-day14`).
    pub(crate) fn install_custom_class_how(
        &mut self,
        class_name: &str,
        how_type: Value,
    ) -> Result<bool, RuntimeError> {
        let how_class = match how_type.view() {
            ValueView::Package(sym) => sym.resolve(),
            _ => return Ok(false),
        };
        // The HOW type must be a user-declared class (a stale/foreign stash
        // entry is ignored rather than failing the class declaration).
        if !self.registry().classes.contains_key(&how_class) {
            return Ok(false);
        }
        // `class_mro` computes and caches the MRO on demand — `ClassDef::mro`
        // itself is lazily filled and still empty right after a module load.
        let has_user_compose = self.class_mro(&how_class).iter().any(|c| {
            self.registry()
                .user_method_overloads(c.as_str(), "compose")
                .is_some()
        });
        let instance = self.call_method_with_values(how_type, "new", Vec::new())?;
        self.registry_mut()
            .class_how_values
            .insert(class_name.to_string(), instance);
        Ok(has_user_compose)
    }

    /// Whether `class_name`'s HOW is a `does`-mixin carrying a user `compose`
    /// hook (a role mixed in by a class trait: `$class.HOW does SomeRole`).
    pub(crate) fn class_how_compose_hook(&self, class_name: &str) -> bool {
        self.registry()
            .class_how_values
            .get(class_name)
            .is_some_and(|how| self.mixin_has_compose_hook(how))
    }

    /// Whether `role_name` declares an attribute with a custom trait: the
    /// trait handler may mix a meta-role (with a `specialize`) into the role's HOW.
    // Cost: O(p), p = recorded attribute-trait entries.
    pub(crate) fn role_has_attribute_trait_hooks(&self, role_name: &str) -> bool {
        let registry = self.registry();
        registry
            .role_attribute_pending_traits
            .keys()
            .any(|(role, _)| role == role_name)
            || registry
                .class_attribute_trait_objects
                .keys()
                .any(|(role, _)| role == role_name)
    }

    /// Compose a synthesized mixin type (`C+{R}`) whose HOW a role's `specialize`
    /// hook mixed a meta-role into, as the class declaration op does for a
    /// written class: the HOW's `compose`, else the native compose, then the
    /// `create_BUILDPLAN` / `publish_method_cache` overrides. A no-op without
    /// such a HOW.
    // Cost: O(1) without a HOW mixin; else the hooks' own cost.
    pub(crate) fn compose_mixin_class_how(&mut self, class_name: &str) -> Result<(), RuntimeError> {
        let Some(how) = self.registry().class_how_values.get(class_name).cloned() else {
            return Ok(());
        };
        if !matches!(how.view(), ValueView::Mixin(..)) {
            return Ok(());
        }
        if self.class_how_compose_hook(class_name) {
            let type_obj = Value::package(Symbol::intern(class_name));
            self.call_method_with_values(how, "compose", vec![type_obj])?;
        } else if !self.registry().composed_classes.contains(class_name) {
            self.mop_compose(vec![Value::package(Symbol::intern(class_name))])?;
        }
        self.run_how_compose_overrides(class_name)
    }

    /// Run the user `specialize` override of every composed role's HOW for
    /// `class_name` (queued by `queue_role_how_specialize`): rakudo calls
    /// `$role.HOW.specialize($role, $class)` while composing, so a hook mixed
    /// into a role's HOW can add methods to the composing class.
    // Cost: O(r), r = roles the class composed whose HOW overrides `specialize`.
    pub(crate) fn drive_role_how_specialize(
        &mut self,
        class_name: &str,
    ) -> Result<(), RuntimeError> {
        let (mine, rest): (Vec<_>, Vec<_>) = std::mem::take(
            &mut self.registry_mut().pending_role_specialize,
        )
        .into_iter()
        .partition(|(class, _, _)| class == class_name);
        self.registry_mut().pending_role_specialize = rest;
        for (_, key, role_name) in mine {
            // The individual role's HOW, or the group's, whichever carries a
            // user `specialize` (see `queue_role_how_specialize`).
            // A lexical role (`my role R`) is stored under a mangled name
            // (`R\0N`); a trait's `$*PACKAGE.HOW does ...` is keyed by the
            // name the program sees.
            let visible = role_name.split('\u{0}').next().unwrap_or(&role_name);
            let how = [key.as_str(), role_name.as_str(), visible]
                .into_iter()
                .filter_map(|k| self.registry().class_how_values.get(k).cloned())
                .find(|how| self.mixin_has_hook(how, "specialize"));
            if let Some(how) = how {
                let role_obj = Value::package(Symbol::intern(&role_name));
                let class_obj = Value::package(Symbol::intern(class_name));
                self.call_method_with_values(how, "specialize", vec![role_obj, class_obj])?;
            }
        }
        Ok(())
    }

    /// Queue `class_name` for a post-trait `compose` call when its custom `is`
    /// traits mixed a role with a `compose` hook into its HOW
    /// (`had_hook_before` is [`Interpreter::class_how_compose_hook`] from
    /// before the traits ran). Rakudo applies class traits before composing
    /// the class, so the mixed-in `compose` override runs as part of
    /// composition -- here via the same `pending_class_compose` drain the
    /// EXPORTHOW metaclass hook uses.
    pub(crate) fn queue_trait_how_compose(&mut self, class_name: &str, had_hook_before: bool) {
        if had_hook_before || !self.class_how_compose_hook(class_name) {
            return;
        }
        let mut reg = self.registry_mut();
        // An attribute trait that mixed the hook in already ran it once.
        if reg.attr_trait_composed.remove(class_name) {
            return;
        }
        if !reg.pending_class_compose.iter().any(|c| c == class_name) {
            reg.pending_class_compose.push(class_name.to_string());
        }
    }

    /// Whether the (user) class of the installed HOW instance for `class_name`
    /// defines `method_name` anywhere in its MRO.
    fn declare_how_has_user_method(&mut self, how_val: &Value, method_name: &str) -> bool {
        let ValueView::Instance {
            class_name: how_class,
            ..
        } = how_val.view()
        else {
            return false;
        };
        self.class_mro(&how_class.resolve()).iter().any(|c| {
            self.registry()
                .user_method_overloads(c.as_str(), method_name)
                .is_some()
        })
    }

    /// Drive the user HOW protocol for a DECLARE'd class right after its
    /// native registration (Rakudo's World does the same calls during
    /// compilation): the HOW's `new_type` override runs with `callsame`
    /// resolving to the already-registered type object, then `add_method`
    /// runs for every method declared in the body — the override typically
    /// `.wrap`s the method (landing in `method_wrap_chains` via the Method
    /// object's owner markers) and re-adds it through the fully-qualified
    /// native `self.Metamodel::ClassHOW::add_method`, which is a no-op for an
    /// already-registered method. The user `compose` hook is NOT called here —
    /// the caller queues it via `pending_class_compose` so it runs after the
    /// class's custom `is` traits, like the EXPORTHOW `class` mapping.
    pub(crate) fn declare_drive_how_protocol(
        &mut self,
        class_name: &str,
        is_rw: bool,
    ) -> Result<(), RuntimeError> {
        let Some(how_val) = self.registry().class_how_values.get(class_name).cloned() else {
            return Ok(());
        };
        let type_obj = Value::package(Symbol::intern(class_name));
        // `is rw` on the declaration reaches the HOW's `set_rw` (ValueClass's
        // HOW dies there: "value-class Bla can't be rw").
        if is_rw && self.declare_how_has_user_method(&how_val, "set_rw") {
            self.call_method_with_values(how_val.clone(), "set_rw", vec![type_obj.clone()])?;
        }
        if self.declare_how_has_user_method(&how_val, "new_type") {
            self.types.pending_declare_new_type = Some(type_obj.clone());
            let result = self.call_method_with_values(
                how_val.clone(),
                "new_type",
                vec![Value::pair(
                    "name".to_string(),
                    Value::str(class_name.to_string()),
                )],
            );
            self.types.pending_declare_new_type = None;
            result?;
        }
        if self.declare_how_has_user_method(&how_val, "add_method") {
            // The declared (public, non-multi) methods, sorted for a
            // deterministic protocol order. Each is passed as a Method object
            // carrying its owner markers so a `.wrap` inside the user
            // `add_method` becomes a class-keyed wrap chain.
            // ADR-0019 F4c-1: enumerate via the canonical reverse index
            // instead of `class_def.methods.keys()` (the enumeration site
            // F4b deferred here; zero-mismatch shadow-checked across the
            // full local `t/` suite before this cutover).
            let registry = self.registry();
            let mut method_names: Vec<String> = registry
                .owner_method_names(class_name)
                .into_iter()
                .map(|name| name.resolve())
                .filter(|name| {
                    registry
                        .user_method_overloads(class_name, name)
                        .is_some_and(|overloads| {
                            overloads
                                .iter()
                                .find(|def| !def.is_private)
                                .is_some_and(|first| !first.is_submethod && !first.is_multi)
                        })
                })
                .collect();
            drop(registry);
            method_names.sort();
            for method_name in method_names {
                // A private method shares the name's candidate list with a
                // public one (`method stop` next to `method !stop`, as
                // Timer::Stopwatch declares); only the public candidates are
                // the method `add_method` receives. Handing the private one
                // over too let the HOW's re-add replace the name's whole
                // candidate list, and `self!stop` stopped resolving.
                let Some(overloads) = self
                    .registry()
                    .user_method_overloads(class_name, &method_name)
                    .map(|defs| {
                        defs.into_iter()
                            .filter(|def| !def.is_private)
                            .collect::<Vec<_>>()
                    })
                else {
                    continue;
                };
                let Some(first) = overloads.first() else {
                    continue;
                };
                let method_obj = self.make_method_object_with_owner(
                    &method_name,
                    first,
                    false,
                    first.return_type.clone(),
                    Some(&overloads),
                    Some(class_name),
                );
                self.call_method_with_values(
                    how_val.clone(),
                    "add_method",
                    vec![
                        type_obj.clone(),
                        Value::str(method_name.clone()),
                        method_obj,
                    ],
                )?;
            }
        }
        Ok(())
    }
}

/// The identity key of a type object (custom type or package), `None` for
/// any other value.
fn type_cache_id(ty: &Value) -> Option<u64> {
    match ty.view() {
        ValueView::CustomType(c) => Some(c.id),
        ValueView::Package(_) => Some(package_cache_id(ty)),
        _ => None,
    }
}

/// Identity key for a non-CustomType parameterization root (a package).
fn package_cache_id(root: &Value) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    crate::value::which_key::value_which_key(root).hash(&mut h);
    h.finish() | (1 << 63)
}
