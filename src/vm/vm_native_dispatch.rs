use super::*;

impl Interpreter {
    pub(super) fn try_native_method(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        fn collection_contains_instance(value: &Value) -> bool {
            // Recursive: does any element (transitively) carry an Instance whose
            // `.gist` may need interpreter dispatch?
            fn contains_instance(value: &Value) -> bool {
                match value {
                    Value::Instance { .. } => true,
                    v if v.as_list_items().is_some() => {
                        v.as_list_items().unwrap().iter().any(contains_instance)
                    }
                    Value::Hash(map) => map.values().any(contains_instance),
                    _ => false,
                }
            }
            // Only a *collection* receiver triggers the gist bypass. A bare
            // instance (e.g. a `Buf`, whose gist `native_method_0arg` renders
            // purely via `dispatch_core_repr`) is dispatched normally — the
            // builtins layer itself defers a collection whose elements may have a
            // user `method gist`, so bypassing a bare instance here only forced a
            // pure native gist (Buf/Blob/Uni) onto the interpreter for nothing.
            match value {
                v if v.as_list_items().is_some() => {
                    v.as_list_items().unwrap().iter().any(contains_instance)
                }
                Value::Hash(map) => map.values().any(contains_instance),
                _ => false,
            }
        }
        let method_name = method_sym.resolve();
        // A chained `.map`/`.grep` on a lazy map/grep pipeline must append
        // another lazy stage (interpreter `dispatch_map_method`/`dispatch_grep`),
        // not run the native impl over the pipeline's (empty) cache. Defer.
        if let Value::LazyList(ll) = target
            && ll.lazy_pipe.is_some()
            && matches!(method_name.as_str(), "map" | "grep")
        {
            return None;
        }
        // A laziness-preserving coercion on a lazy map/grep pipeline returns the
        // pipeline unchanged (it stays pullable). Intercept before the native
        // impl, which would otherwise wrap/empty the pipeline's (empty) cache.
        if let Value::LazyList(ll) = target
            && ll.lazy_pipe.is_some()
            && Self::lazy_pipe_preserving_coercion(method_name.as_str())
        {
            return Some(Ok(target.clone()));
        }
        // Eager list operations cannot run on a lazy/infinite source: throw
        // X::Cannot::Lazy instead of hanging while the native impl materializes
        // it (matches raku). Shared with the interpreter dispatch path.
        if let Some(err) =
            crate::runtime::Interpreter::lazy_guard_error(method_name.as_str(), target)
        {
            return Some(Err(err));
        }
        // Early exit for Proxy containers
        if matches!(target, Value::Proxy { .. })
            && !matches!(
                method_name.as_str(),
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            )
        {
            return None;
        }
        // Squish/tail always bypass native
        if method_sym == "squish" || method_sym == "tail" {
            return None;
        }
        // Mixin role method bypass
        if self.mixin_role_has_method(target, &method_name) {
            return None;
        }
        // Instance-specific bypasses (avoid for non-Instance targets)
        if matches!(target, Value::Instance { .. }) {
            if let Value::Instance { class_name, .. } = target {
                let cn = class_name.resolve();
                // Supply methods
                if cn == "Supply"
                    && matches!(
                        method_name.as_str(),
                        "max"
                            | "min"
                            | "lines"
                            | "elems"
                            | "head"
                            | "flat"
                            | "sort"
                            | "comb"
                            | "words"
                            | "batch"
                            | "throttle"
                            | "rotor"
                            | "rotate"
                            | "produce"
                            | "snip"
                            | "minmax"
                            | "start"
                            | "wait"
                            | "zip"
                            | "zip-latest"
                    )
                {
                    return None;
                }
                // Supplier.Supply
                if cn == "Supplier" && method_sym == "Supply" && args.is_empty() {
                    return None;
                }
                // Numeric bridge — Real/Numeric instances route through the
                // interpreter's Numeric bridge for arithmetic and coercion.
                // Exception: pure-render methods (gist/Str/raku/...) don't need
                // the bridge, so let `native_method_0arg` render the known
                // builtin numeric instances (Instant/Duration) directly. A
                // user numeric class that overrides one of these keeps the
                // bypass so its method runs; unknown numeric instances return
                // `None` from native and still fall through to the interpreter.
                let is_pure_render = matches!(
                    method_name.as_str(),
                    "gist" | "Str" | "Stringy" | "raku" | "perl"
                );
                let render_overridden = is_pure_render && self.has_user_method(&cn, &method_name);
                if (!is_pure_render || render_overridden)
                    && (self.type_matches_value("Real", target)
                        || self.type_matches_value("Numeric", target)
                        || self.has_user_method(&cn, "Bridge"))
                {
                    return None;
                }
                // Native method on instance class
                if self.is_native_method(&cn, &method_name) {
                    return None;
                }
            }
        } else if matches!(target, Value::Package(name) if name == "Supply")
            && matches!(
                method_name.as_str(),
                "max"
                    | "min"
                    | "lines"
                    | "elems"
                    | "head"
                    | "flat"
                    | "sort"
                    | "comb"
                    | "words"
                    | "batch"
                    | "throttle"
                    | "rotor"
                    | "rotate"
                    | "produce"
                    | "snip"
                    | "minmax"
                    | "start"
                    | "wait"
                    | "zip"
                    | "zip-latest"
            )
        {
            return None;
        }
        // Collection gist bypass
        if method_sym == "gist" && args.is_empty() && collection_contains_instance(target) {
            return None;
        }
        // Pick/roll on Package/Str
        if (method_sym == "pick" || method_sym == "roll")
            && args.len() <= 1
            && matches!(target, Value::Package(_) | Value::Str(_))
        {
            return None;
        }
        // Hash-specific bypasses
        if matches!(target, Value::Hash(_)) && args.is_empty() {
            let mn = method_name.as_str();
            if (mn == "raku" || mn == "perl" || mn == "keyof")
                && self.container_type_metadata(target).is_some()
            {
                return None;
            }
            if mn == "keyof" {
                return None;
            }
        }
        // Typed array .raku/.perl bypass
        if matches!(target, Value::Array(..))
            && args.is_empty()
            && matches!(method_name.as_str(), "raku" | "perl")
            && self
                .container_type_metadata(target)
                .is_some_and(|info| info.value_type != "Any" && info.value_type != "Mu")
        {
            return None;
        }
        let mut result = if args.len() == 2 {
            crate::builtins::native_method_2arg(target, method_sym, &args[0], &args[1])
        } else if args.len() == 1 {
            crate::builtins::native_method_1arg(target, method_sym, &args[0])
        } else if args.is_empty() {
            crate::builtins::native_method_0arg(target, method_sym)
        } else {
            return None;
        };

        // For gather-based LazyList, .List and .values preserve laziness
        // by returning the LazyList itself (which can be forced on demand).
        if result.is_none()
            && let Value::LazyList(ll) = target
            && ll.coroutine.is_some()
            && matches!(method_name.as_str(), "List" | "values")
        {
            return Some(Ok(target.clone()));
        }

        // Handle .Slip/.List/.Seq on scan-based LazyList by forcing elements via Interpreter.
        if result.is_none()
            && let Value::LazyList(ll) = target
            && ll.scan_spec.is_some()
            && matches!(method_name.as_str(), "Slip" | "List" | "Seq" | "Array")
        {
            match self.force_lazy_list_vm(ll) {
                Ok(items) => {
                    let val = match method_name.as_str() {
                        "Slip" => Value::Slip(std::sync::Arc::new(items)),
                        "List" => Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new(items)),
                            crate::value::ArrayKind::List,
                        ),
                        "Seq" => Value::Seq(std::sync::Arc::new(items)),
                        "Array" => Value::real_array(items),
                        _ => unreachable!(),
                    };
                    return Some(Ok(val));
                }
                Err(e) => return Some(Err(e)),
            }
        }

        if method_name == "decode" {
            return result.map(|res| {
                res.map(|value| match value {
                    Value::Str(decoded) => Value::str(self.translate_newlines_for_decode(&decoded)),
                    other => other,
                })
            });
        }

        // For Hash values with declared_type "Map", override gist/raku/perl
        // to use Map.new((...)) format instead of {...} format.
        if let Value::Hash(map) = target {
            let is_map = matches!(method_name.as_str(), "gist" | "raku" | "perl")
                && self
                    .container_type_metadata(target)
                    .and_then(|info| info.declared_type)
                    .is_some_and(|dt| dt == "Map");
            if is_map {
                if map.is_empty() {
                    return Some(Ok(Value::str("Map.new".to_string())));
                }
                let mut sorted_keys: Vec<&String> = map.keys().collect();
                sorted_keys.sort();
                if method_name == "gist" {
                    let total = sorted_keys.len();
                    let truncated = total > 100;
                    let display_keys = if truncated {
                        &sorted_keys[..100]
                    } else {
                        &sorted_keys[..]
                    };
                    let mut parts: Vec<String> = display_keys
                        .iter()
                        .map(|k| format!("{} => {}", k, crate::runtime::gist_value(&map[*k])))
                        .collect();
                    if truncated {
                        parts.push("...".to_string());
                    }
                    return Some(Ok(Value::str(format!("Map.new(({}))", parts.join(", ")))));
                } else {
                    let parts: Vec<String> = sorted_keys
                        .iter()
                        .map(|k| {
                            let v = &map[*k];
                            if let Value::Bool(true) = v {
                                format!(":{}", k)
                            } else if let Value::Bool(false) = v {
                                format!(":!{}", k)
                            } else {
                                let repr = if matches!(v, Value::Nil) {
                                    "Any".to_string()
                                } else {
                                    crate::builtins::methods_0arg::raku_value(v)
                                };
                                format!(":{}({})", k, repr)
                            }
                        })
                        .collect();
                    return Some(Ok(Value::str(format!("Map.new(({}))", parts.join(", ")))));
                }
            }
        }

        // When cloning a container (Hash/Array/Set/Bag/Mix) that has type
        // metadata, copy the metadata to the cloned value so the clone
        // preserves the type constraint (e.g. %h.clone ~~ Hash[Int,Int]).
        if method_name == "clone"
            && matches!(&result, Some(Ok(_)))
            && let Some(info) = self.container_type_metadata(target)
            && let Some(Ok(v)) = result.take()
        {
            // Hashes embed metadata in `HashData`; re-tag the cloned value
            // (no-op Arc for array/instance side-table containers).
            result = Some(Ok(self.tag_container_metadata(v, info)));
        }

        result
    }

    pub(super) fn try_native_function(
        &mut self,
        name_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.iter().any(|arg| matches!(arg, Value::Instance { .. })) {
            // Junction constructors wrap their arguments verbatim regardless of
            // type, so an Instance argument is safe to handle natively; every
            // other native function bails out on Instance args (they may need
            // method dispatch) and falls through to the interpreter.
            let name = name_sym.resolve();
            // `unpack(Blob, Str)` takes a Blob (Instance) argument and is
            // handled natively; everything else bails out on Instance args.
            if !matches!(name.as_str(), "any" | "all" | "one" | "none" | "unpack") {
                return None;
            }
        }
        // For functions that need to iterate their arguments (e.g. flat),
        // force any LazyList arguments first so the pure builtin can process them.
        if args.iter().any(|a| matches!(a, Value::LazyList(_))) {
            let name = name_sym.resolve();
            if matches!(name.as_str(), "flat" | "eager") {
                // For `flat`, if the single argument is lazy, preserve laziness
                // by returning the lazy list directly (flat of a lazy list is still lazy).
                if name.as_str() == "flat"
                    && args.len() == 1
                    && matches!(&args[0], Value::LazyList(_))
                {
                    return Some(Ok(args[0].clone()));
                }
                let mut forced_args = Vec::with_capacity(args.len());
                for arg in args {
                    if let Value::LazyList(ll) = arg {
                        match self.force_lazy_list_vm(ll) {
                            Ok(items) => {
                                forced_args.push(Value::Seq(std::sync::Arc::new(items)));
                            }
                            Err(e) => return Some(Err(e)),
                        }
                    } else {
                        forced_args.push(arg.clone());
                    }
                }
                return crate::builtins::native_function(name_sym, &forced_args);
            }
        }
        crate::builtins::native_function(name_sym, args)
    }
}
