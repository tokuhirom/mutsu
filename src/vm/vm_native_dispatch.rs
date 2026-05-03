use super::*;

impl VM {
    pub(super) fn try_native_method(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        fn collection_contains_instance(value: &Value) -> bool {
            match value {
                Value::Instance { .. } => true,
                v if v.as_list_items().is_some() => v
                    .as_list_items()
                    .unwrap()
                    .iter()
                    .any(collection_contains_instance),
                Value::Hash(map) => map.values().any(collection_contains_instance),
                _ => false,
            }
        }
        let bypass_supply_extrema_fastpath = (method_sym == "max"
            || method_sym == "min"
            || method_sym == "lines"
            || method_sym == "elems"
            || method_sym == "head"
            || method_sym == "flat"
            || method_sym == "sort"
            || method_sym == "comb"
            || method_sym == "words"
            || method_sym == "batch"
            || method_sym == "rotor"
            || method_sym == "rotate"
            || method_sym == "produce"
            || method_sym == "snip"
            || method_sym == "minmax"
            || method_sym == "start"
            || method_sym == "wait"
            || method_sym == "zip"
            || method_sym == "zip-latest")
            && (matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supply"
            ) || matches!(target, Value::Package(name) if name == "Supply"));
        let bypass_supplier_supply_fastpath = method_sym == "Supply"
            && args.is_empty()
            && matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supplier"
            );
        let bypass_gist_fastpath =
            method_sym == "gist" && args.is_empty() && collection_contains_instance(target);
        let bypass_pickroll_type_fastpath = (method_sym == "pick" || method_sym == "roll")
            && args.len() <= 1
            && matches!(target, Value::Package(_) | Value::Str(_));
        let bypass_squish_fastpath = method_sym == "squish";
        let bypass_tail_fastpath = method_sym == "tail";
        let bypass_numeric_bridge_instance_fastpath = matches!(target, Value::Instance { .. })
            && (self.interpreter.type_matches_value("Real", target)
                || self.interpreter.type_matches_value("Numeric", target)
                || matches!(target, Value::Instance { class_name, .. }
                    if self.interpreter.has_user_method(&class_name.resolve(), "Bridge")));
        let method_name = method_sym.resolve();
        let bypass_runtime_native_instance_fastpath = matches!(target, Value::Instance { class_name, .. }
                if self
                    .interpreter
                    .is_native_method(&class_name.resolve(), &method_name));
        // Constrained Hash: bypass builtins for .raku/.perl/.keyof so runtime handles them
        let bypass_constrained_hash = matches!(target, Value::Hash(_))
            && args.is_empty()
            && matches!(method_sym.resolve().as_ref(), "raku" | "perl" | "keyof")
            && self.interpreter.container_type_metadata(target).is_some();
        // Native typed array (e.g. array[int]): bypass builtins for .raku/.perl
        // so the runtime can produce the correct type prefix (e.g. "array[int].new(...)")
        let bypass_typed_array_raku = matches!(target, Value::Array(..))
            && args.is_empty()
            && matches!(method_sym.resolve().as_ref(), "raku" | "perl")
            && self
                .interpreter
                .container_type_metadata(target)
                .is_some_and(|info| info.value_type != "Any" && info.value_type != "Mu");
        // Unconstrained Hash .keyof must also bypass builtins (returns Str(Any))
        let bypass_hash_keyof = matches!(target, Value::Hash(_))
            && args.is_empty()
            && method_sym == "keyof"
            && !bypass_constrained_hash;
        // Proxy containers must auto-FETCH before dispatching methods (except meta-methods)
        // Mixin with a role-defined method: bypass native so role-method
        // dispatch (including `handles` delegation forwarders) takes precedence
        // over the built-in Cool fallbacks like `.uc`/`.lc`/`.gist`.
        let bypass_mixin_role_method = self.interpreter.mixin_role_has_method(target, &method_name);
        let bypass_proxy = matches!(target, Value::Proxy { .. })
            && !matches!(
                method_sym.resolve().as_ref(),
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            );
        if bypass_supply_extrema_fastpath
            || bypass_supplier_supply_fastpath
            || bypass_gist_fastpath
            || bypass_pickroll_type_fastpath
            || bypass_squish_fastpath
            || bypass_tail_fastpath
            || bypass_numeric_bridge_instance_fastpath
            || bypass_runtime_native_instance_fastpath
            || bypass_proxy
            || bypass_constrained_hash
            || bypass_hash_keyof
            || bypass_typed_array_raku
            || bypass_mixin_role_method
        {
            return None;
        }
        let result = if args.len() == 2 {
            crate::builtins::native_method_2arg(target, method_sym, &args[0], &args[1])
        } else if args.len() == 1 {
            crate::builtins::native_method_1arg(target, method_sym, &args[0])
        } else if args.is_empty() {
            crate::builtins::native_method_0arg(target, method_sym)
        } else {
            return None;
        };

        if method_name == "decode" {
            return result.map(|res| {
                res.map(|value| match value {
                    Value::Str(decoded) => {
                        Value::str(self.interpreter.translate_newlines_for_decode(&decoded))
                    }
                    other => other,
                })
            });
        }

        // For Hash values with declared_type "Map", override gist/raku/perl
        // to use Map.new((...)) format instead of {...} format.
        if let Value::Hash(map) = target {
            let is_map = matches!(method_name.as_str(), "gist" | "raku" | "perl")
                && self
                    .interpreter
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
            && let Some(Ok(ref cloned)) = result
            && let Some(info) = self.interpreter.container_type_metadata(target)
        {
            self.interpreter
                .register_container_type_metadata(cloned, info);
        }

        result
    }

    pub(super) fn try_native_function(
        &mut self,
        name_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.iter().any(|arg| matches!(arg, Value::Instance { .. })) {
            return None;
        }
        // For functions that need to iterate their arguments (e.g. flat),
        // force any LazyList arguments first so the pure builtin can process them.
        if args.iter().any(|a| matches!(a, Value::LazyList(_))) {
            let name = name_sym.resolve();
            if matches!(name.as_str(), "flat" | "eager") {
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
