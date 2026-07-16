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
        let mut is_mixin = false;

        for arg in args.iter().skip(1) {
            match arg.view() {
                ValueView::Str(s) => repr = s.to_string(),
                ValueView::Pair(k, v) if k == "mixin" => {
                    is_mixin = v.truthy();
                }
                _ => {}
            }
        }

        let id = next_instance_id();
        self.custom_type_data.insert(
            id,
            CustomTypeData {
                type_check_cache: None,
                authoritative: false,
                call_accepts: false,
                composed: false,
                is_mixin,
            },
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

        if let Some(data) = self.custom_type_data.get_mut(&id) {
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

        if let Some(data) = self.custom_type_data.get_mut(&id) {
            data.composed = true;
        }

        Ok(type_val)
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
                self.rebless_map.insert(d.id, (*c.how).clone());
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
    /// via the slow `run_instance_method_resolved` path, which merges its
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
        let data = self.custom_type_data.get(&rhs_id).cloned();
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
        let has_user_find_method = self
            .registry()
            .classes
            .get(&how_class)
            .map(|cd| cd.mro.clone())
            .unwrap_or_default()
            .iter()
            .any(|c| {
                self.registry()
                    .classes
                    .get(c.as_str())
                    .is_some_and(|cd| cd.methods.contains_key("find_method"))
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
        let mro = self
            .registry()
            .classes
            .get(&how_class)
            .map(|cd| cd.mro.clone())
            .unwrap_or_default();
        let has_user_compose = mro.iter().any(|c| {
            self.registry()
                .classes
                .get(c.as_str())
                .is_some_and(|cd| cd.methods.contains_key("compose"))
        });
        let instance = self.call_method_with_values(how_type, "new", Vec::new())?;
        self.registry_mut()
            .class_how_values
            .insert(class_name.to_string(), instance);
        Ok(has_user_compose)
    }
}
