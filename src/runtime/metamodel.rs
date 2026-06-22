use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, next_instance_id};

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
            match arg {
                Value::Str(s) => repr = s.to_string(),
                Value::Pair(k, v) if k == "mixin" => {
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

        let id = match &type_val {
            Value::CustomType(c) => c.id,
            _ => {
                return Err(RuntimeError::new(
                    "configure_type_checking: first argument must be a custom type",
                ));
            }
        };

        // Second arg is the type check cache (array of types)
        let cache = if let Some(cache_val) = args.get(1) {
            match cache_val {
                Value::Array(items, _) => Some(items.to_vec()),
                _ => None,
            }
        } else {
            None
        };

        let mut authoritative = false;
        let mut call_accepts = false;

        for arg in args.iter().skip(2) {
            if let Value::Pair(k, v) = arg {
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

        let id = match &type_val {
            Value::CustomType(c) => c.id,
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

        match (obj, target_type) {
            (Value::CustomTypeInstance(d), Value::CustomType(c)) => {
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
        Ok(Value::Bool(result))
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
                    vec![Value::Nil, lhs.clone()],
                ) {
                    return result.truthy();
                }
            }

            // Cache-only check: identity-based matching
            for cached_type in cache {
                if lhs == cached_type {
                    return true;
                }
                if let Value::Package(type_name) = cached_type
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
            vec![Value::Nil, Value::str_from("ACCEPTS")],
        );
        false
    }
}
