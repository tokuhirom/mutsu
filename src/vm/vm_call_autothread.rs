use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl VM {
    /// Check if any function call arguments are Junctions that need auto-threading.
    /// Returns Some(result) if auto-threading was performed, None if no auto-threading needed.
    pub(super) fn maybe_autothread_func_call(
        &mut self,
        code: &CompiledCode,
        name: &str,
        args: &[Value],
        arg_sources: &Option<Vec<Option<String>>>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Option<Value>, RuntimeError> {
        // Skip auto-threading for internal functions and junction constructors
        if name.starts_with("__mutsu_")
            || matches!(
                name,
                "any"
                    | "all"
                    | "one"
                    | "none"
                    | "so"
                    | "not"
                    | "defined"
                    | "return"
                    | "return-rw"
                    | "die"
                    | "fail"
                    | "exit"
                    | "leave"
                    | "succeed"
                    | "infix:<,>"
                    | "infix:<=>>"
                    | "say"
                    | "note"
                    | "dd"
                    | "warn"
                    // Test functions accept Mu parameters
                    | "ok"
                    | "nok"
                    | "is"
                    | "isnt"
                    | "is-deeply"
                    | "is-approx"
                    | "is_approx"
                    | "isa-ok"
                    | "does-ok"
                    | "can-ok"
                    | "like"
                    | "unlike"
                    | "cmp-ok"
                    | "dies-ok"
                    | "lives-ok"
                    | "eval-dies-ok"
                    | "eval-lives-ok"
                    | "throws-like"
                    | "subtest"
                    | "skip"
                    | "todo"
                    | "pass"
                    | "flunk"
                    | "bail-out"
                    | "done-testing"
                    | "diag"
                    | "plan"
                    | "is-deeply-junction"
                    // Collection/container functions
                    | "push"
                    | "pop"
                    | "shift"
                    | "unshift"
                    | "append"
                    | "prepend"
                    | "elems"
                    | "join"
                    | "grep"
                    | "map"
                    | "sort"
                    | "reverse"
                    | "flat"
                    | "eager"
                    | "lazy"
                    | "sink"
            )
        {
            return Ok(None);
        }

        // Find arg indices that contain Junctions (positional or named)
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, v)| {
                // Check for junction in named arg (Pair value)
                if let Value::Pair(_, val) = v {
                    return Self::unwrap_junction_value(val).is_some();
                }
                Self::unwrap_junction_value(v).is_some()
            })
            .map(|(i, _)| i)
            .collect();

        if junction_indices.is_empty() {
            return Ok(None);
        }

        // Get param_defs if available to check which params accept Junction
        let param_defs = self.get_func_param_defs_for_autothread(name, args);

        // Without param_defs, we can't safely auto-thread
        if param_defs.is_none() {
            return Ok(None);
        }
        let pds = param_defs.unwrap();

        // Filter to only junction args whose parameter does NOT accept Junction
        let autothread_indices: Vec<usize> = junction_indices
            .into_iter()
            .filter(|&idx| {
                // Check if the arg is a named arg (Pair) — find matching named param
                if let Value::Pair(key, _) = &args[idx] {
                    if let Some(pd) = pds.iter().find(|pd| pd.named && pd.name == *key) {
                        if let Some(tc) = &pd.type_constraint {
                            return !matches!(tc.as_str(), "Mu" | "Junction");
                        }
                        return true; // No type constraint = default Any
                    }
                    return true; // No matching named param found, auto-thread
                }

                // Positional arg — find corresponding positional param
                let positional_pds: Vec<&crate::ast::ParamDef> =
                    pds.iter().filter(|pd| !pd.named).collect();
                if let Some(pd) = positional_pds.get(idx) {
                    // Don't auto-thread if param accepts Mu or Junction
                    if let Some(tc) = &pd.type_constraint {
                        return !matches!(tc.as_str(), "Mu" | "Junction");
                    }
                    // No type constraint means default Any — needs auto-threading
                    return true;
                }
                // If param is slurpy or we're past the defined params, don't auto-thread
                false
            })
            .collect();

        if autothread_indices.is_empty() {
            return Ok(None);
        }

        // Pick the junction to thread over based on priority:
        // all/none junctions first (leftmost), then any/one (leftmost)
        let thread_idx = self.pick_autothread_junction_index(args, &autothread_indices);

        let (kind, values, is_pair) = match Self::extract_junction_from_arg(&args[thread_idx]) {
            Some((k, v, p)) => (k, v, p),
            std::option::Option::None => return Ok(None),
        };

        // Thread over the chosen junction: call the function for each eigenstate
        let mut results = Vec::with_capacity(values.len());
        for eigenstate in values.iter() {
            let mut threaded_args = args.to_vec();
            if let Some(ref pair_key) = is_pair {
                // Replace the Pair value while keeping the key
                threaded_args[thread_idx] =
                    Value::Pair(pair_key.clone(), Box::new(eigenstate.clone()));
            } else {
                threaded_args[thread_idx] = eigenstate.clone();
            }

            // Recursively check for more junctions that need auto-threading
            if let Some(recursive_result) = self.maybe_autothread_func_call(
                code,
                name,
                &threaded_args,
                arg_sources,
                compiled_fns,
            )? {
                results.push(recursive_result);
            } else {
                // No more junctions to thread — actually call the function
                let call_me_override = self.get_call_me_override(name);
                let result = self.dispatch_func_call_inner(
                    code,
                    name,
                    threaded_args,
                    arg_sources.clone(),
                    call_me_override,
                    compiled_fns,
                )?;
                results.push(result);
            }
        }

        Ok(Some(Value::junction(kind, results)))
    }

    /// Unwrap a Junction value (possibly through Scalar wrapper).
    pub(super) fn unwrap_junction_value(
        val: &Value,
    ) -> Option<(crate::value::JunctionKind, Arc<Vec<Value>>)> {
        match val {
            Value::Junction { kind, values } => Some((kind.clone(), values.clone())),
            Value::Scalar(inner) => {
                if let Value::Junction { kind, values } = inner.as_ref() {
                    Some((kind.clone(), values.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract junction from an argument, handling both positional and named (Pair) args.
    /// Returns (kind, values, pair_key) where pair_key is Some(key) for Pair args.
    pub(super) fn extract_junction_from_arg(
        val: &Value,
    ) -> Option<(crate::value::JunctionKind, Arc<Vec<Value>>, Option<String>)> {
        if let Value::Pair(key, inner) = val {
            Self::unwrap_junction_value(inner).map(|(k, v)| (k, v, Some(key.clone())))
        } else {
            Self::unwrap_junction_value(val).map(|(k, v)| (k, v, None))
        }
    }

    /// Pick the junction argument to thread over, based on Raku's priority:
    /// all/none junctions are threaded first (leftmost), then any/one (leftmost).
    pub(super) fn pick_autothread_junction_index(
        &self,
        args: &[Value],
        indices: &[usize],
    ) -> usize {
        use crate::value::JunctionKind::{All, None as JNone};
        // First, look for leftmost all/none junction
        for &idx in indices {
            if let Some((kind, _, _)) = Self::extract_junction_from_arg(&args[idx])
                && matches!(kind, All | JNone)
            {
                return idx;
            }
        }
        // Then, leftmost any/one junction
        indices[0]
    }

    /// Get CALL-ME override for a function name (extracted from exec_call_func_op).
    pub(super) fn get_call_me_override(&self, name: &str) -> Option<Value> {
        self.interpreter
            .env()
            .get(&format!("&{}", name))
            .cloned()
            .and_then(|callable| {
                if let Value::Mixin(_, ref mixins) = callable {
                    let has_call_me = mixins.keys().any(|key| {
                        key.strip_prefix("__mutsu_role__")
                            .is_some_and(|rn| self.interpreter.role_has_method(rn, "CALL-ME"))
                    });
                    if has_call_me {
                        return Some(callable);
                    }
                }
                None
            })
    }

    /// Check if any method call arguments are Junctions that need auto-threading.
    /// Returns Some(result) if auto-threading was performed, None otherwise.
    pub(super) fn maybe_autothread_method_args(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        // Don't auto-thread args for methods that natively handle junctions
        // or that accept Mu/Junction arguments (matchers, comparators, etc.)
        if matches!(
            method,
            "Bool"
                | "so"
                | "WHAT"
                | "WHICH"
                | "^name"
                | "gist"
                | "Str"
                | "defined"
                | "THREAD"
                | "raku"
                | "perl"
                | "return"
                // Methods that accept matchers/code blocks where junctions are used as values
                | "grep"
                | "first"
                | "map"
                | "sort"
                | "min"
                | "max"
                | "minmax"
                | "classify"
                | "categorize"
                | "reduce"
                | "produce"
                | "supply"
                | "unique"
                | "repeated"
                | "squish"
                | "race"
                | "hyper"
                // Collection methods that accept any value
                | "push"
                | "unshift"
                | "append"
                | "prepend"
                | "ACCEPTS"
                | "cmp"
                | "STORE"
                | "AT-POS"
                | "AT-KEY"
                | "ASSIGN-POS"
                | "ASSIGN-KEY"
                | "BIND-POS"
                | "BIND-KEY"
                | "EXISTS-POS"
                | "EXISTS-KEY"
                | "DELETE-POS"
                | "DELETE-KEY"
                // Smartmatch and type checking
                | "isa"
                | "does"
                | "can"
                | "FALLBACK"
                // IO and output
                | "say"
                | "print"
                | "put"
                | "note"
                | "printf"
                | "sprintf"
        ) {
            return Ok(None);
        }

        // Find junction arg indices (including junctions inside named Pair args)
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, v)| {
                if let Value::Pair(_, val) = v {
                    return Self::unwrap_junction_value(val).is_some();
                }
                Self::unwrap_junction_value(v).is_some()
            })
            .map(|(i, _)| i)
            .collect();

        if junction_indices.is_empty() {
            return Ok(None);
        }

        // Pick the junction to thread over based on priority
        let thread_idx = self.pick_autothread_junction_index(args, &junction_indices);

        let (kind, values, is_pair) = match Self::extract_junction_from_arg(&args[thread_idx]) {
            Some((k, v, p)) => (k, v, p),
            std::option::Option::None => return Ok(None),
        };

        // Get instance identity for refreshing target after each call
        let target_identity = match target {
            Value::Instance { class_name, id, .. } => Some((class_name.resolve(), *id)),
            _ => None,
        };

        // Thread over the chosen junction
        let mut results = Vec::with_capacity(values.len());
        let mut current_target = target.clone();
        for eigenstate in values.iter() {
            let mut threaded_args = args.to_vec();
            if let Some(ref pair_key) = is_pair {
                threaded_args[thread_idx] =
                    Value::Pair(pair_key.clone(), Box::new(eigenstate.clone()));
            } else {
                threaded_args[thread_idx] = eigenstate.clone();
            }

            // Recursively check for more junctions
            if let Some(recursive_result) =
                self.maybe_autothread_method_args(&current_target, method, &threaded_args)?
            {
                results.push(recursive_result);
            } else {
                // No more junctions — actually call the method
                let r = if let Some(nr) =
                    self.try_native_method(&current_target, Symbol::intern(method), &threaded_args)
                {
                    nr?
                } else {
                    self.try_compiled_method_or_interpret(
                        current_target.clone(),
                        method,
                        threaded_args,
                    )?
                };
                results.push(r);
            }

            // Mark env as dirty so subsequent reads see updated values
            self.env_dirty = true;

            // Refresh the target from the environment to pick up attribute mutations
            if let Some((ref cn, id)) = target_identity
                && let Some(refreshed) = self.find_instance_in_env(cn, id)
            {
                current_target = refreshed;
            }
        }

        Ok(Some(Value::junction(kind, results)))
    }

    /// Find an instance in the environment by class name and id.
    /// Used to refresh a target after mutation during auto-threading.
    fn find_instance_in_env(&self, class_name: &str, id: u64) -> Option<Value> {
        for v in self.interpreter.env().values() {
            if let Value::Instance {
                class_name: cn,
                id: vid,
                ..
            } = v
                && cn.resolve() == class_name
                && *vid == id
            {
                return Some(v.clone());
            }
        }
        None
    }

    /// Recursively unwrap a Junction to get a non-junction eigenstate.
    fn unwrap_junction_deep(val: &Value) -> Value {
        match val {
            Value::Junction { values, .. } => {
                if let Some(first) = values.first() {
                    Self::unwrap_junction_deep(first)
                } else {
                    Value::Nil
                }
            }
            Value::Scalar(inner) if matches!(inner.as_ref(), Value::Junction { .. }) => {
                Self::unwrap_junction_deep(inner)
            }
            Value::Pair(key, val) => {
                let inner = Self::unwrap_junction_deep(val);
                if std::ptr::eq(val.as_ref(), &inner) {
                    return val.as_ref().clone();
                }
                Value::Pair(key.clone(), Box::new(inner))
            }
            other => other.clone(),
        }
    }

    /// Try to get param_defs for a function to determine which params accept Junction.
    fn get_func_param_defs_for_autothread(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Vec<crate::ast::ParamDef>> {
        // Replace junction args with non-junction placeholder values for type resolution
        let resolved_args: Vec<Value> = args.iter().map(Self::unwrap_junction_deep).collect();
        self.interpreter
            .resolve_function_with_types(name, &resolved_args)
            .map(|def| def.param_defs)
    }
}
