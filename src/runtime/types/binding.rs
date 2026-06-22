use super::*;

/// Does this legacy-path placeholder/signature param list contain a *plain
/// positional* param — a real signature name like `p` (from a pointy block
/// `-> $p {...}`), as opposed to a `$:name` named placeholder, a `^`-twigil
/// implicit placeholder (`$^a`/`@^x`/`%^h`/`&^cb`), or the `_`/`@_`/`%_`
/// topic/args slots?
///
/// Pointy lambdas compile to `params: ["p"]` with an empty `param_defs`, so they
/// reach the legacy binding path. When a positional pair value (`ValuePair(Str,
/// _)`, produced by `pair_as_positional` for `.first`/etc. over Hash/Pair
/// elements) is passed, it must bind to such a plain param positionally rather
/// than be siphoned off as a named argument.
fn legacy_has_plain_positional_param(params: &[String]) -> bool {
    params.iter().any(|p| {
        !p.starts_with(':')
            && !p.starts_with("@:")
            && !p.starts_with("%:")
            && !p.starts_with('^')
            && !p.starts_with("@^")
            && !p.starts_with("%^")
            && !p.starts_with("&^")
            && p != "_"
            && p != "@_"
            && p != "%_"
    })
}

impl Interpreter {
    /// Whether a named parameter is a plain readonly scalar `$` param eligible
    /// for container sharing (Slice 2d named follow-up). A scalar named param is
    /// stored sigil-less in `ParamDef::name` (`:$n` -> "n"), unlike `:@l`/`:%h`
    /// which keep their sigil; exclude attribute twigils (`:$!x` -> "!x"), `$_`,
    /// `is copy`/`is rw`/`is raw`, slurpy, and sub-signature params.
    fn named_scalar_container_share_eligible(&self, pd: &crate::ast::ParamDef) -> bool {
        !pd.traits
            .iter()
            .any(|t| t == "copy" || t == "rw" || t == "raw")
            && !pd.slurpy
            && !pd.double_slurpy
            && pd.sub_signature.is_none()
            && pd.name != "_"
            && !pd.name.starts_with(['@', '%', '&', '!', '.'])
            && pd
                .name
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
    }

    /// Evaluate a parameter's default expression under Raku's parameter
    /// scoping: the parameter being bound is in scope (as its undefined type
    /// object) *within its own default*, shadowing any outer symbol of the same
    /// name. This is why `sub f(&foo = &foo)` defaults to the (undefined)
    /// parameter — not the outer routine `foo` — and `$x = $x` is undefined.
    /// Earlier parameters remain visible because they were bound in prior loop
    /// iterations. The topic (`$_`/`_`) is saved/restored so the eval does not
    /// leak it into the caller's scope.
    fn eval_param_default(
        &mut self,
        pd: &ParamDef,
        default_expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        let saved_topic = self.env.get("_").cloned();
        let saved_dollar_topic = self.env.get("$_").cloned();
        // Shadow the parameter with its undefined type object (or Nil) so a
        // self-reference in the default resolves to the parameter rather than
        // an outer symbol of the same name.
        let saved_self = if !pd.name.is_empty() {
            let prev = self.env.get(&pd.name).cloned();
            let shadow = pd
                .type_constraint
                .as_deref()
                .filter(|c| !c.starts_with("::"))
                .and_then(|c| self.resolve_type_object(c))
                .unwrap_or(Value::Nil);
            self.env.insert(pd.name.clone(), shadow);
            Some(prev)
        } else {
            None
        };
        let value = self.eval_block_value(&[Stmt::Expr(default_expr.clone())]);
        // Restore the parameter's prior env binding; the caller performs the
        // real bind right after. On the error path this prevents the shadow
        // from leaking.
        if let Some(prev) = saved_self {
            match prev {
                Some(v) => {
                    self.env.insert(pd.name.clone(), v);
                }
                None => {
                    self.env.remove(&pd.name);
                }
            }
        }
        if let Some(t) = saved_topic {
            self.env.insert("_".to_string(), t);
        } else {
            self.env.remove("_");
        }
        if let Some(t) = saved_dollar_topic {
            self.env.insert("$_".to_string(), t);
        } else {
            self.env.remove("$_");
        }
        value
    }

    fn parameter_binding_error(message: String) -> RuntimeError {
        let mut err = RuntimeError::new(message);
        let mut ex_attrs = std::collections::HashMap::new();
        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
        let exception =
            Value::make_instance(Symbol::intern("X::TypeCheck::Binding::Parameter"), ex_attrs);
        err.exception = Some(Box::new(exception));
        err
    }

    fn normalize_coercion_binding_error(err: RuntimeError) -> RuntimeError {
        if matches!(
            err.exception.as_deref(),
            Some(Value::Instance { class_name, .. }) if class_name.resolve() == "X::Coerce::Impossible"
        ) {
            err
        } else {
            Self::parameter_binding_error(err.message)
        }
    }

    pub(crate) fn bind_function_args_values(
        &mut self,
        param_defs: &[ParamDef],
        params: &[String],
        args: &[Value],
    ) -> Result<Vec<(String, String)>, RuntimeError> {
        let filtered_args: Vec<Value> = args
            .iter()
            .filter(|arg| !is_internal_named_arg(&unwrap_varref_value((*arg).clone())))
            .cloned()
            .collect();
        let plain_args: Vec<Value> = filtered_args
            .iter()
            .cloned()
            .map(unwrap_varref_value)
            .collect();
        // Always set @_ for legacy Perl-style argument access.
        // Skip the insert when args are empty and @_ is already empty,
        // to avoid triggering Arc::make_mut deep clone on the CoW env.
        let skip_at_underscore = plain_args.is_empty()
            && self
                .env
                .get("@_")
                .is_some_and(|v| matches!(v, Value::Array(elems, _) if elems.is_empty()));
        if !skip_at_underscore {
            self.env
                .insert("@_".to_string(), Value::array(plain_args.clone()));
        }
        // Single-argument rule: when exactly one positional arg is a Seq/List
        // and the function expects multiple positional params, flatten the Seq/List
        // into individual positional args.
        let filtered_args: Vec<Value> = {
            let positional_count = filtered_args
                .iter()
                .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                .count();
            let required_positional_count = param_defs
                .iter()
                .filter(|pd| {
                    !pd.named
                        && !pd.slurpy
                        && !pd.double_slurpy
                        && !pd.onearg
                        && !pd.optional_marker
                        && !pd.name.is_empty()
                        && !pd.name.starts_with(':')
                        && !pd.is_invocant
                        && pd.default.is_none()
                })
                .count();
            if positional_count == 1 && required_positional_count >= 2 {
                // Check if the single positional arg is a Seq or non-itemized Array/List
                let single_pos = filtered_args
                    .iter()
                    .find(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)));
                let named_args: Vec<Value> = filtered_args
                    .iter()
                    .filter(|a| matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                    .cloned()
                    .collect();
                if let Some(single) = single_pos {
                    let unwrapped = unwrap_varref_value(single.clone());
                    match unwrapped {
                        Value::Seq(items) => {
                            let mut expanded = items.to_vec();
                            expanded.extend(named_args);
                            expanded
                        }
                        Value::Array(items, kind) if !kind.is_itemized() => {
                            let mut expanded = items.to_vec();
                            expanded.extend(named_args);
                            expanded
                        }
                        Value::Slip(items) => {
                            let mut expanded = items.to_vec();
                            expanded.extend(named_args);
                            expanded
                        }
                        _ => filtered_args,
                    }
                } else {
                    filtered_args
                }
            } else {
                filtered_args
            }
        };
        let args = filtered_args.as_slice();
        let arg_sources = self.take_pending_call_arg_sources();
        let mut rw_bindings = Vec::new();
        let mut raw_nonlvalue_params: Vec<String> = Vec::new();
        let mut raw_slurpy_sources = std::collections::HashSet::new();
        if let Some(invocant_value) = self
            .env
            .get("self")
            .cloned()
            .or_else(|| self.env.get("?CLASS").cloned())
        {
            for pd in param_defs {
                if !(pd.is_invocant || pd.traits.iter().any(|t| t == "invocant")) {
                    continue;
                }
                if let Some(captured_name) = pd
                    .type_constraint
                    .as_deref()
                    .and_then(|constraint| constraint.strip_prefix("::"))
                {
                    self.bind_type_capture(captured_name, &invocant_value);
                }
            }
        }
        if param_defs.is_empty() {
            if params.is_empty() {
                // No param_defs and no placeholder params -- nothing to bind.
                // Argument rejection (for named subs with empty signature) is handled
                // by callers that set `empty_sig` on FunctionDef / CompiledFunction.
                return Ok(rw_bindings);
            }
            // Legacy path: bind positional placeholders ($^a, $^b) by position,
            // and named placeholders ($:name) by matching Pair arg keys.
            //
            // A `ValuePair(Str, _)` is normally a colonpair passed positionally
            // that should feed a `$:name` named placeholder. But a pointy block
            // `-> $p {...}` reaches this path with a plain positional param (and
            // no named placeholders), and `.first`/`.sort`/etc. pass Hash/Pair
            // elements as `ValuePair(Str, _)` via `pair_as_positional`: those
            // must bind to `$p` positionally. So when the param list has a plain
            // positional name, treat `ValuePair(Str, _)` as positional (and not
            // as a named arg) — otherwise the pair is dropped and `$p` is unbound.
            let promote_valuepair_positional = legacy_has_plain_positional_param(params);
            let positional_args: Vec<Value> = plain_args
                .iter()
                .filter(|a| match a {
                    Value::Pair(..) => false,
                    Value::ValuePair(key, _) if matches!(key.as_ref(), Value::Str(..)) => {
                        promote_valuepair_positional
                    }
                    _ => true,
                })
                .cloned()
                .collect();
            let named_args: Vec<(String, Value)> = plain_args
                .iter()
                .filter_map(|a| {
                    if let Value::Pair(key, val) = a {
                        Some((key.clone(), *val.clone()))
                    } else if let Value::ValuePair(key, val) = a {
                        if promote_valuepair_positional {
                            return None;
                        }
                        if let Value::Str(name) = key.as_ref() {
                            Some((name.to_string(), *val.clone()))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect();
            let mut consumed_named = std::collections::HashSet::new();
            let mut positional_idx = 0usize;
            for param in params.iter() {
                // Named placeholder: $:f, @:f, %:f -- match by Pair key
                let named_key = param
                    .strip_prefix(':')
                    .or_else(|| param.strip_prefix("@:"))
                    .or_else(|| param.strip_prefix("%:"));
                if let Some(key) = named_key {
                    // Use rfind so the rightmost named argument wins
                    if let Some((_, val)) = named_args.iter().rfind(|(k, _)| k == key) {
                        self.bind_param_value(param, val.clone());
                        // Also bind the bare :key for GetArrayVar/GetHashVar fallback
                        self.env.insert(format!(":{}", key), val.clone());
                        consumed_named.insert(key.to_string());
                    }
                } else if positional_idx < positional_args.len() {
                    let value = positional_args[positional_idx].clone();
                    if param.starts_with("&^") && !self.type_matches_value("Callable", &value) {
                        return Err(RuntimeError::typecheck_binding_parameter(
                            param,
                            "Callable",
                            crate::runtime::value_type_name(&value),
                            None,
                        ));
                    }
                    if param.starts_with("@^") && !self.type_matches_value("Positional", &value) {
                        return Err(RuntimeError::typecheck_binding_parameter(
                            param,
                            "Positional",
                            crate::runtime::value_type_name(&value),
                            None,
                        ));
                    }
                    if param.starts_with("%^") && !self.type_matches_value("Associative", &value) {
                        return Err(RuntimeError::typecheck_binding_parameter(
                            param,
                            "Associative",
                            crate::runtime::value_type_name(&value),
                            None,
                        ));
                    }
                    self.bind_param_value(param, value);
                    positional_idx += 1;
                } else if param.starts_with('^')
                    || param.starts_with("@^")
                    || param.starts_with("%^")
                    || param.starts_with("&^")
                {
                    return Err(RuntimeError::new(format!(
                        "Missing required implicit placeholder parameter ${}",
                        param
                    )));
                }
            }
            self.env.insert(
                "@_".to_string(),
                Value::array(positional_args[positional_idx..].to_vec()),
            );
            // Insert %_ if explicitly listed in params, if named placeholders
            // ($:Name) are present, or if the sub uses any `^`-twigil placeholder
            // ($^a / @^arr / %^h / &^cb): such a sub also exposes the implicit
            // %_ slurpy, so leftover named args must be captured there (mirrors
            // the unconditional @_ capture above).
            // WhateverCode closures use this legacy path with params like ["_"]
            // (no `^`), and must NOT overwrite the caller's %_ hash.
            if params
                .iter()
                .any(|p| p == "%_" || p.starts_with(':') || p.contains('^'))
            {
                let mut leftover_named = std::collections::HashMap::new();
                for (key, val) in named_args {
                    if !consumed_named.contains(&key) {
                        leftover_named.insert(key, val);
                    }
                }
                self.env
                    .insert("%_".to_string(), Value::hash(leftover_named));
            }
            return Ok(rw_bindings);
        }
        // Pre-compute the set of explicit named parameter keys so that
        // slurpy hash (*%rest) can exclude args already bound to named params.
        let explicit_named_keys: std::collections::HashSet<String> = param_defs
            .iter()
            .filter(|pd| (pd.named || pd.name.starts_with(':')) && !pd.slurpy)
            .map(|pd| {
                let name = if pd.name.starts_with(':') {
                    &pd.name[1..]
                } else if let Some(rest) = pd
                    .name
                    .strip_prefix("@:")
                    .or_else(|| pd.name.strip_prefix("%:"))
                {
                    rest
                } else if pd.named {
                    let after_sigil = pd
                        .name
                        .strip_prefix('@')
                        .or_else(|| pd.name.strip_prefix('%'))
                        .unwrap_or(&pd.name);
                    after_sigil
                        .strip_prefix('!')
                        .or_else(|| after_sigil.strip_prefix('.'))
                        .unwrap_or(after_sigil)
                } else {
                    &pd.name
                };
                name.to_string()
            })
            .collect();
        let mut positional_idx = 0usize;
        for pd in param_defs {
            if pd.onearg {
                // +@ (single-argument rule slurpy): if exactly one remaining positional
                // arg is Iterable (Array, List, etc.), use its elements directly.
                // Otherwise, collect remaining args like *@ (flattening slurpy).
                let remaining_positional: Vec<_> = args[positional_idx..]
                    .iter()
                    .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                    .cloned()
                    .collect();
                let items = if remaining_positional.len() == 1 {
                    let single = unwrap_varref_value(remaining_positional[0].clone());
                    match single {
                        Value::Array(arr, kind) if !kind.is_itemized() => arr.to_vec(),
                        Value::Slip(arr) => arr.to_vec(),
                        Value::Seq(arr) => arr.to_vec(),
                        single @ (Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                        | Value::GenericRange { .. }) => {
                            // A single iterable argument is used as the argument
                            // list: a finite range flattens to its elements.
                            let mut items = Vec::new();
                            flatten_into_slurpy(&[single], &mut items);
                            items
                        }
                        other => vec![other],
                    }
                } else {
                    // Multiple top-level args: the single-argument rule does NOT
                    // flatten them — each top-level argument becomes one element
                    // (e.g. `f(1..3, 4)` is a 2-element list, the range is not
                    // expanded). Only Slips flatten, since slips always flatten.
                    let mut items = Vec::new();
                    for val in remaining_positional {
                        let val = unwrap_varref_value(val);
                        match val {
                            Value::Pair(..) => {} // skip named args
                            Value::Slip(arr) => {
                                flatten_into_slurpy(&arr, &mut items);
                            }
                            other => {
                                items.push(other);
                            }
                        }
                    }
                    items
                };
                positional_idx = args.len(); // consume all remaining args
                let slurpy_value = Value::real_array(items);
                if pd.sigilless {
                    // Sigilless single-argument rule slurpy (`+foo`): bind a
                    // read-only List under the bare name, with no `@` sigil.
                    if !pd.name.is_empty() {
                        self.env
                            .insert(sigilless_readonly_key(&pd.name), Value::Bool(true));
                        self.env.remove(&sigilless_alias_key(&pd.name));
                        self.bind_param_value(&pd.name, slurpy_value.clone());
                    }
                } else if !pd.name.is_empty() {
                    let key = if pd.name.starts_with('@') {
                        pd.name.clone()
                    } else {
                        format!("@{}", pd.name)
                    };
                    self.bind_param_value(&key, slurpy_value.clone());
                    self.set_var_type_constraint(&key, pd.type_constraint.clone());
                }
                if let Some(sub_params) = &pd.sub_signature {
                    bind_sub_signature_from_value(self, sub_params, &slurpy_value)?;
                }
            } else if pd.slurpy {
                let is_hash_slurpy = pd.name.starts_with('%');
                if pd.sigilless {
                    // |c -- capture parameter: preserve positional and named parts.
                    // Collect remaining positional args from positional_idx onwards,
                    // and also collect any Pair (named) args from the ENTIRE arg list
                    // that were not consumed by explicit named parameters.
                    let mut positional = Vec::new();
                    let mut named = std::collections::HashMap::new();
                    // First, collect remaining args from positional_idx
                    for arg in args[positional_idx..].iter().cloned() {
                        let arg = unwrap_varref_value(arg);
                        if let Value::Pair(key, val) = arg {
                            if !explicit_named_keys.contains(&key) {
                                named.insert(key, *val);
                            }
                        } else {
                            positional.push(arg);
                        }
                    }
                    // Also collect Pair args that were skipped before positional_idx
                    // (e.g., named args that appeared between positional args)
                    for arg in args[..positional_idx].iter().cloned() {
                        let arg = unwrap_varref_value(arg);
                        if let Value::Pair(key, val) = arg
                            && !explicit_named_keys.contains(&key)
                        {
                            named.insert(key, *val);
                        }
                    }
                    let capture_value = Value::capture(positional, named);
                    if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, capture_value.clone());
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                    if let Some(sub_params) = &pd.sub_signature {
                        bind_sub_signature_from_value(self, sub_params, &capture_value)?;
                    }
                } else if is_hash_slurpy {
                    // *%hash -- collect Pair arguments into a hash,
                    // excluding args already bound to explicit named parameters.
                    let mut hash_items = std::collections::HashMap::new();
                    for arg in args.iter() {
                        let arg = unwrap_varref_value(arg.clone());
                        if let Value::Pair(k, v) = arg
                            && !explicit_named_keys.contains(&k)
                        {
                            hash_items.insert(k.clone(), *v.clone());
                        }
                    }
                    if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, Value::hash(hash_items));
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                } else if pd.double_slurpy {
                    // **@ (non-flattening slurpy): keep each argument as-is, skip Pairs
                    let mut items = Vec::new();
                    while positional_idx < args.len() {
                        let val = unwrap_varref_value(args[positional_idx].clone());
                        if !matches!(&val, Value::Pair(..)) {
                            items.push(val);
                        }
                        positional_idx += 1;
                    }
                    if !pd.name.is_empty() {
                        let key = if pd.name.starts_with('@') {
                            pd.name.clone()
                        } else {
                            format!("@{}", pd.name)
                        };
                        self.bind_param_value(&key, Value::real_array(items));
                        self.set_var_type_constraint(&key, pd.type_constraint.clone());
                    }
                } else if !pd.name.starts_with('@') {
                    // *$x -- slurpy scalar: captures what would otherwise be the
                    // next element of the variadic array (S06 "List parameters").
                    // It consumes a single positional argument; named (Pair) args
                    // are skipped so they fall through to a *%_ slurpy. With no
                    // positional left it binds an undefined value (slurpy is
                    // always optional).
                    let mut value = Value::Nil;
                    while positional_idx < args.len() {
                        let raw_arg = args[positional_idx].clone();
                        positional_idx += 1;
                        let arg = unwrap_varref_value(raw_arg);
                        if matches!(&arg, Value::Pair(..)) {
                            // Named arg -- leave for *%_ slurpy; keep scanning.
                            continue;
                        }
                        value = arg;
                        break;
                    }
                    if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, value.clone());
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                } else {
                    let mut items = Vec::new();
                    let is_raw_slurpy = pd.traits.iter().any(|t| t == "raw");
                    while positional_idx < args.len() {
                        let raw_arg = args[positional_idx].clone();
                        if is_raw_slurpy
                            && let Some((source_name, source_value, source_index)) =
                                indexed_varref_from_value(&raw_arg)
                        {
                            if raw_slurpy_sources.insert(source_name.clone()) {
                                rw_bindings.push((source_name.clone(), source_name.clone()));
                            }
                            self.env.insert(source_name.clone(), source_value.clone());
                            if !pd.double_slurpy
                                && let Value::Array(arr, kind) = &source_value
                                && !kind.is_itemized()
                            {
                                for (idx, item) in arr.iter().cloned().enumerate() {
                                    items.push(make_varref_value(
                                        source_name.clone(),
                                        item,
                                        Some(idx),
                                    ));
                                }
                            } else {
                                items.push(make_varref_value(
                                    source_name,
                                    source_value,
                                    source_index,
                                ));
                            }
                            positional_idx += 1;
                            continue;
                        }
                        // *@ (flattening slurpy): recursively flatten list args
                        // but preserve itemized Arrays ($[...] / .item) as single values.
                        // Skip Pair values -- they are named args for *%_ or will be rejected
                        match unwrap_varref_value(raw_arg) {
                            Value::Pair(..) => {
                                // Named arg -- leave for *%_ slurpy or post-loop check
                            }
                            Value::Array(arr, kind) => {
                                if kind.is_itemized() {
                                    items.push(Value::Array(arr.clone(), kind));
                                } else {
                                    flatten_into_slurpy(&arr, &mut items);
                                }
                            }
                            val @ (Value::Range(..)
                            | Value::RangeExcl(..)
                            | Value::RangeExclStart(..)
                            | Value::RangeExclBoth(..)
                            | Value::GenericRange { .. }
                            | Value::Seq(..)
                            | Value::Slip(..)) => {
                                flatten_into_slurpy(&[val], &mut items);
                            }
                            other => {
                                items.push(other);
                            }
                        }
                        positional_idx += 1;
                    }
                    let slurpy_value = Value::real_array(items);
                    if !pd.name.is_empty() {
                        let key = if pd.name.starts_with('@') {
                            pd.name.clone()
                        } else {
                            format!("@{}", pd.name)
                        };
                        self.bind_param_value(&key, slurpy_value.clone());
                        self.set_var_type_constraint(&key, pd.type_constraint.clone());
                    }
                    // Check where constraint for slurpy params
                    if let Some(where_expr) = &pd.where_constraint {
                        let saved_topic = self.env.get("_").cloned();
                        self.env.insert("_".to_string(), slurpy_value.clone());
                        let ok = match where_expr.as_ref() {
                            Expr::AnonSub { body, .. } => self
                                .eval_block_value(body)
                                .map(|v| v.truthy())
                                .unwrap_or(false),
                            expr => self
                                .eval_block_value(&[Stmt::Expr(expr.clone())])
                                .map(|v| self.smart_match(&slurpy_value, &v))
                                .unwrap_or(false),
                        };
                        if let Some(previous) = saved_topic {
                            self.env.insert("_".to_string(), previous);
                        } else {
                            self.env.remove("_");
                        }
                        if !ok {
                            let mut err = RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: where constraint failed for parameter '{}'",
                                pd.name
                            ));
                            let mut ex_attrs = std::collections::HashMap::new();
                            ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                            let exception = Value::make_instance(
                                Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                ex_attrs,
                            );
                            err.exception = Some(Box::new(exception));
                            return Err(err);
                        }
                    }
                    // Unpack sub-signature from the slurpy array (e.g., *[$a, $b, $c])
                    if let Some(sub_params) = &pd.sub_signature {
                        bind_sub_signature_from_value(self, sub_params, &slurpy_value)?;
                    }
                }
            } else if pd.named || pd.name.starts_with(':') {
                // Look for a matching named argument (Pair) in args
                let match_key = if pd.name.starts_with(':') {
                    &pd.name[1..]
                } else if let Some(rest) = pd
                    .name
                    .strip_prefix("@:")
                    .or_else(|| pd.name.strip_prefix("%:"))
                {
                    rest
                } else if pd.named {
                    // Named params like :@l or :%h have name "@l" or "%h";
                    // strip the sigil to match the Pair key "l" or "h".
                    // Also strip twigil prefixes: :$!x has name "!x", :$.x has name ".x",
                    // :@!types has name "@!types" -- match against Pair key "types".
                    // First strip sigil (@, %), then strip twigil (!, .).
                    let after_sigil = pd
                        .name
                        .strip_prefix('@')
                        .or_else(|| pd.name.strip_prefix('%'))
                        .unwrap_or(&pd.name);
                    after_sigil
                        .strip_prefix('!')
                        .or_else(|| after_sigil.strip_prefix('.'))
                        .unwrap_or(after_sigil)
                } else {
                    &pd.name
                };
                let mut found = false;
                // Iterate in reverse so that the rightmost named argument wins
                // when the same key is provided multiple times.
                for (arg_idx, raw_arg) in args.iter().enumerate().rev() {
                    let arg = unwrap_varref_value(raw_arg.clone());
                    if let Value::Pair(key, val) = arg
                        && key == match_key
                    {
                        if let Some((sig_params, sig_ret)) = &pd.code_signature
                            && !code_signature_matches_value(self, sig_params, sig_ret, &val)
                        {
                            let mut err = RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Callable with matching signature, got {}",
                                pd.name,
                                crate::runtime::value_type_name(&val)
                            ));
                            let mut ex_attrs = std::collections::HashMap::new();
                            ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                            let exception = Value::make_instance(
                                Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                ex_attrs,
                            );
                            err.exception = Some(Box::new(exception));
                            return Err(err);
                        }
                        // Slice 2d (named follow-up): an `@`/`%` *variable* passed by
                        // name to a plain readonly scalar `$` named param binds the
                        // same mutable container in Raku (`sub f(:$n){ $n.push }`
                        // mutates the caller's `@a`), exactly like the positional
                        // case. The source variable name is encoded "key=source" in
                        // `arg_sources` (`positional_arg_source_name`). Promote the
                        // bound value to a shared `ContainerRef` cell and register the
                        // exit-time rw writeback. Only `=` rebinding stays forbidden
                        // (named scalar params are readonly). A `$`-scalar source
                        // (`:$n` shorthand over `my $n = @a`) is excluded — it shares
                        // by reference already, like the positional scalar source.
                        let mut bound_value = *val.clone();
                        if self.named_scalar_container_share_eligible(pd)
                            && matches!(bound_value, Value::Array(..) | Value::Hash(..))
                            && let Some(source_name) = arg_sources
                                .as_ref()
                                .and_then(|names| names.get(arg_idx))
                                .and_then(|n| n.as_ref())
                                .and_then(|encoded| {
                                    encoded.split_once('=').map(|(_, s)| s.to_string())
                                })
                                .filter(|s| s.starts_with('@') || s.starts_with('%'))
                        {
                            bound_value = Value::ContainerRef(std::sync::Arc::new(
                                std::sync::Mutex::new(bound_value),
                            ));
                            rw_bindings.push((pd.name.clone(), source_name));
                        }
                        self.bind_param_value(&pd.name, bound_value);
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                        if let Some(sub_params) = &pd.sub_signature {
                            bind_named_rename_sub_signature(self, sub_params, &val)?;
                        }
                        found = true;
                        break;
                    }
                }
                // Alias matching: for :a(:$b), also accept b => val
                if !found && let Some(sub_params) = &pd.sub_signature {
                    for sub_pd in sub_params {
                        if found {
                            break;
                        }
                        if !sub_pd.named {
                            continue;
                        }
                        let inner_key = sub_pd.name.strip_prefix(':').unwrap_or(&sub_pd.name);
                        for arg in args.iter().rev() {
                            let arg = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, inner_val) = arg
                                && key == inner_key
                            {
                                self.bind_param_value(&pd.name, *inner_val.clone());
                                self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                                bind_named_rename_sub_signature(self, sub_params, &inner_val)?;
                                found = true;
                                break;
                            }
                        }
                    }
                }
                // Check where constraint for named params after binding
                if found && let Some(where_expr) = &pd.where_constraint {
                    let bound_val = self.env.get(&pd.name).cloned().unwrap_or(Value::Nil);
                    let saved_topic = self.env.get("_").cloned();
                    self.env.insert("_".to_string(), bound_val.clone());
                    // env_dirty substrate (docs/captured-outer-cell-sharing.md §10):
                    // a `where { $t ~= 'a' }` clause can mutate a captured-outer
                    // caller lexical by name. The write reaches env, but the owning
                    // caller slot is refreshed only by the call site's blanket pull
                    // — a no-op once env_dirty is removed. Snapshot the env scalars
                    // before the clause runs and record the names it changes into the
                    // retain-on-miss caller-var writeback, drained at the call site.
                    let pre_env: Option<std::collections::HashMap<crate::symbol::Symbol, Value>> =
                        Some(
                            self.env
                                .iter()
                                .filter(|(_, v)| Self::is_writeback_safe_scalar(v))
                                .map(|(k, v)| (*k, v.clone()))
                                .collect(),
                        );
                    let ok = match where_expr.as_ref() {
                        Expr::AnonSub { body, .. } => self
                            .eval_block_value(body)
                            .map(|v| v.truthy())
                            .unwrap_or(false),
                        Expr::MethodCall { target, .. } if matches!(target.as_ref(), Expr::Var(name) if name == "_") => {
                            self.eval_block_value(&[Stmt::Expr(where_expr.as_ref().clone())])
                                .map(|v| v.truthy())
                                .unwrap_or(false)
                        }
                        expr => self
                            .eval_block_value(&[Stmt::Expr(expr.clone())])
                            .map(|v| self.smart_match(&bound_val, &v))
                            .unwrap_or(false),
                    };
                    if let Some(pre_env) = pre_env {
                        let changed: Vec<String> = self
                            .env
                            .iter()
                            .filter(|(k, v)| {
                                k.resolve() != "_"
                                    && Self::is_writeback_safe_scalar(v)
                                    && pre_env.get(*k).map(|p| p != *v).unwrap_or(true)
                            })
                            .map(|(k, _)| k.resolve())
                            .collect();
                        for name in changed {
                            self.record_caller_var_writeback(&name);
                        }
                    }
                    if let Some(previous) = saved_topic {
                        self.env.insert("_".to_string(), previous);
                    } else {
                        self.env.remove("_");
                    }
                    if !ok {
                        return Err(RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: where constraint failed for parameter '{}'",
                            pd.name
                        )));
                    }
                }
                if !found && let Some(default_expr) = &pd.default {
                    let value = self.eval_param_default(pd, default_expr)?;
                    let value = self.checked_default_param_value(pd, value)?;
                    let value = if pd
                        .type_constraint
                        .as_deref()
                        .is_some_and(|constraint| constraint.starts_with("::"))
                    {
                        self.normalize_type_capture_value(value)
                    } else {
                        value
                    };
                    if let Some((sig_params, sig_ret)) = &pd.code_signature
                        && !code_signature_matches_value(self, sig_params, sig_ret, &value)
                    {
                        let mut err = RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Callable with matching signature, got {}",
                            pd.name,
                            crate::runtime::value_type_name(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    if let Some(captured_name) = pd
                        .type_constraint
                        .as_deref()
                        .and_then(|constraint| constraint.strip_prefix("::"))
                    {
                        self.bind_type_capture(captured_name, &value);
                        if !pd.name.is_empty() {
                            self.bind_param_value(&pd.name, value.clone());
                            self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                        }
                    } else if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, value.clone());
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                    // For renamed named params like :foo($y) = $x, also bind the
                    // sub-signature variable ($y) to the default value.
                    if let Some(sub_params) = &pd.sub_signature {
                        bind_named_rename_sub_signature(self, sub_params, &Box::new(value))?;
                    }
                } else if !found && pd.required {
                    // A missing required named parameter is a runtime X::AdHoc in
                    // Raku (not the compile-time arity X::TypeCheck::Argument that
                    // a missing positional yields). Carry the typed exception so it
                    // surfaces as X::AdHoc instead of the bare "Exception" default.
                    return Err(RuntimeError::typed_msg(
                        "X::AdHoc",
                        format!("Required named parameter '{}' not passed", pd.name),
                    ));
                } else if !found && !pd.name.is_empty() {
                    // Only bind a default if the env doesn't already have a value
                    // (e.g. BUILD/TWEAK attribute bindings pre-populate the env).
                    if !self.env.contains_key(&pd.name) {
                        let value = Self::missing_optional_param_value(pd);
                        self.bind_param_value(&pd.name, value);
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                }
            } else if pd.name == "__subsig__"
                && let Some(sub_params) = &pd.sub_signature
            {
                // An anonymous capture `|(...)` consumes ALL remaining arguments
                // (positional and named) and binds them via its subsignature.
                // This must happen even when no arguments remain, so that an
                // empty capture `|()` binds successfully against a 0-arity call.
                let capture = sub_signature_target_from_remaining_args(&args[positional_idx..]);
                bind_sub_signature_from_value(self, sub_params, &capture)?;
                positional_idx = args.len();
                continue;
            } else {
                // Positional param -- skip over Value::Pair entries (named args)
                while positional_idx < args.len()
                    && matches!(
                        unwrap_varref_value(args[positional_idx].clone()),
                        Value::Pair(..)
                    )
                {
                    positional_idx += 1;
                }
                if positional_idx < args.len() {
                    let is_rw = pd.traits.iter().any(|t| t == "rw");
                    let is_raw = pd.traits.iter().any(|t| t == "raw");
                    if is_rw || is_raw {
                        let source_name = arg_sources
                            .as_ref()
                            .and_then(|names| names.get(positional_idx))
                            .and_then(|name| name.as_ref())
                            .cloned()
                            .or_else(|| {
                                varref_from_value(&args[positional_idx]).map(|(name, _)| name)
                            });
                        if let Some(source_name) = source_name {
                            rw_bindings.push((pd.name.clone(), source_name.clone()));
                            // Set up a sigilless alias so that subsequent `:=`
                            // bindings (e.g. `$a := $arg`) can transitively
                            // resolve through the `is rw` parameter to the
                            // caller's variable.
                            let alias_key = format!("__mutsu_sigilless_alias::{}", pd.name);
                            self.env.insert(alias_key, Value::str(source_name));
                        } else if is_rw {
                            return Err(RuntimeError::new(format!(
                                "X::Parameter::RW: '{}' expects a writable variable argument",
                                pd.name
                            )));
                        } else {
                            // is raw with non-lvalue: param stays readonly
                            // (not added to rw_bindings, will be marked readonly below)
                            raw_nonlvalue_params.push(pd.name.clone());
                        }
                    }
                    let raw_arg = args[positional_idx].clone();
                    let source_name = varref_from_value(&raw_arg)
                        .map(|(source_name, _)| source_name)
                        .or_else(|| {
                            arg_sources
                                .as_ref()
                                .and_then(|names| names.get(positional_idx))
                                .and_then(|name| name.as_ref())
                                .cloned()
                        });
                    // Plain positional `@`/`%` params bind the caller's container
                    // by alias (Raku readonly-container semantics): element
                    // assignment, `.push`, `splice`, and whole-container `=`
                    // assignment all propagate to the caller, while `is copy`
                    // copies and `:=` rebinding is forbidden. mutsu reaches this
                    // by routing the param through the same writeback machinery
                    // `is rw`/`is raw` use: at return `apply_rw_bindings_to_env`
                    // writes the param's final container value back to the
                    // caller's variable. Scalar `$` params remain readonly.
                    let is_copy = pd.traits.iter().any(|t| t == "copy");
                    let alias_plain_container = !is_rw
                        && !is_raw
                        && !is_copy
                        && !pd.slurpy
                        && !pd.double_slurpy
                        && !pd.onearg
                        && !pd.is_invocant
                        && pd.sub_signature.is_none()
                        && (pd.name.starts_with('@') || pd.name.starts_with('%'))
                        // Exclude attribute-binding twigil params (@!x, %.y, ...).
                        && !pd.name[1..].starts_with(['!', '.']);
                    if alias_plain_container && let Some(source_name) = &source_name {
                        rw_bindings.push((pd.name.clone(), source_name.clone()));
                    }
                    // Slice 2d: a readonly scalar `$` param receiving an array/hash
                    // *variable* argument binds the same (mutable) container in
                    // Raku — `$n.push` / `$n[0]=` / `my @a := @$n` all mutate one
                    // container shared with the caller's `@z` (only `$n = …`
                    // rebinding is forbidden). Promote the bound value to a shared
                    // `ContainerRef` cell (below, after type coercion) and register
                    // the writeback so the final value reaches the caller. This
                    // replaces the statement-order-fragile env_dirty copy-back that
                    // previously made `$n.push` propagate only by luck.
                    let scalar_container_share = !is_rw
                        && !is_raw
                        && !is_copy
                        && !pd.slurpy
                        && !pd.double_slurpy
                        && !pd.onearg
                        && !pd.is_invocant
                        && !pd.sigilless
                        && pd.sub_signature.is_none()
                        // Plain `$` scalar param: stored sigil-less (`$n` -> "n"),
                        // unlike `@`/`%`/`&` which keep their sigil. Exclude the
                        // twigil/dynamic forms (`!x`/`.x`/`*x`) and `$_`.
                        && pd.name != "_"
                        && pd
                            .name
                            .as_bytes()
                            .first()
                            .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
                        && source_name.is_some()
                        && matches!(
                            unwrap_varref_value(raw_arg.clone()),
                            Value::Array(..) | Value::Hash(..)
                        );
                    let source_type_constraint = source_name.as_deref().and_then(|source_name| {
                        self.var_type_constraint(source_name).or_else(|| {
                            self.var_type_constraint(
                                source_name.trim_start_matches(['$', '@', '%', '&']),
                            )
                        })
                    });
                    let bound_type_constraint = source_type_constraint
                        .clone()
                        .or_else(|| pd.type_constraint.clone());
                    let mut value = unwrap_varref_value(raw_arg.clone());
                    if pd.sigilless {
                        let alias_key = sigilless_alias_key(&pd.name);
                        let readonly_key = sigilless_readonly_key(&pd.name);
                        if let Some((source_name, inner)) = varref_from_value(&raw_arg) {
                            let resolved_source =
                                self.resolve_sigilless_alias_source_name(&source_name);
                            value = inner;
                            self.env.insert(alias_key, Value::str(resolved_source));
                            self.env.insert(readonly_key, Value::Bool(false));
                        } else if let Some(source_name) = arg_sources
                            .as_ref()
                            .and_then(|names| names.get(positional_idx))
                            .and_then(|name| name.as_ref())
                            .cloned()
                        {
                            // Compile-time pseudo-variables (?CLASS, ?ROLE, ?PACKAGE, etc.)
                            // should NOT be re-resolved from the callee's env because the
                            // env may have already overwritten them (e.g., ?CLASS is set to
                            // the method's owner class). Use the argument value as-is.
                            let is_compile_time_pseudo = source_name.starts_with('?');
                            let resolved_source =
                                self.resolve_sigilless_alias_source_name(&source_name);
                            if is_compile_time_pseudo {
                                self.env.remove(&alias_key);
                                self.env.insert(readonly_key, Value::Bool(true));
                            } else if let Some(source_val) = self.env.get(&resolved_source).cloned()
                            {
                                value = source_val;
                                self.env.insert(alias_key, Value::str(resolved_source));
                                self.env.insert(readonly_key, Value::Bool(false));
                            } else {
                                self.env.remove(&alias_key);
                                self.env.insert(readonly_key, Value::Bool(true));
                            }
                        } else {
                            self.env.remove(&alias_key);
                            self.env.insert(readonly_key, Value::Bool(true));
                        }
                    }
                    if let Some(constraint) = &pd.type_constraint
                        && (pd.name != "__type_only__" || self.is_resolvable_type(constraint))
                    {
                        let resolved_constraint = self.resolved_type_capture_name(constraint);
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        // For &-sigil parameters, the type constraint specifies the
                        // callable's return type, not the type of the value itself.
                        // e.g. `Callable &x` means Callable[Callable], `Int &x` means Callable[Int].
                        if pd.name.starts_with('&') {
                            // First, the value must be Callable
                            if !self.type_matches_value("Callable", &value) {
                                let mut err = RuntimeError::new(format!(
                                    "{}: Type check failed in binding to parameter '{}'; expected Callable[{}] but got {} ({})",
                                    type_error_kind,
                                    pd.name,
                                    resolved_constraint,
                                    crate::runtime::value_type_name(&value),
                                    crate::runtime::utils::gist_value(&value)
                                ));
                                let mut ex_attrs = std::collections::HashMap::new();
                                ex_attrs
                                    .insert("message".to_string(), Value::str(err.message.clone()));
                                let exception = Value::make_instance(
                                    Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                    ex_attrs,
                                );
                                err.exception = Some(Box::new(exception));
                                return Err(err);
                            }
                            // Then check the callable's return type matches the constraint
                            let return_type = self.callable_return_type(&value);
                            let return_ok = return_type
                                .as_deref()
                                .is_some_and(|rt| rt == resolved_constraint);
                            if !return_ok {
                                let mut err = RuntimeError::new(format!(
                                    "{}: Type check failed in binding to parameter '{}'; expected Callable[{}] but got {} ({})",
                                    type_error_kind,
                                    pd.name,
                                    resolved_constraint,
                                    crate::runtime::value_type_name(&value),
                                    crate::runtime::utils::gist_value(&value)
                                ));
                                let mut ex_attrs = std::collections::HashMap::new();
                                ex_attrs
                                    .insert("message".to_string(), Value::str(err.message.clone()));
                                let exception = Value::make_instance(
                                    Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                    ex_attrs,
                                );
                                err.exception = Some(Box::new(exception));
                                return Err(err);
                            }
                        } else if let Some(captured_name) = resolved_constraint.strip_prefix("::") {
                            self.bind_type_capture(captured_name, &value);
                        } else if let Some((target, source)) =
                            parse_coercion_type(&resolved_constraint)
                        {
                            // Coercion type: check source type if specified, then coerce.
                            // A `T(S)` parameter accepts a value that is already a `T`
                            // (no coercion needed) as well as an `S` (coerced via `.T`),
                            // so only reject a value that matches neither.
                            if let Some(src) = source
                                && !self.type_matches_value(src, &value)
                                && !self.type_matches_value(target, &value)
                            {
                                let mut err = RuntimeError::new(format!(
                                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                                    pd.name,
                                    resolved_constraint,
                                    crate::runtime::value_type_name(&value)
                                ));
                                let mut ex_attrs = std::collections::HashMap::new();
                                ex_attrs
                                    .insert("message".to_string(), Value::str(err.message.clone()));
                                let exception = Value::make_instance(
                                    Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                    ex_attrs,
                                );
                                err.exception = Some(Box::new(exception));
                                return Err(err);
                            }
                            let original = value.clone();
                            value = self
                                .try_coerce_value_for_constraint(&resolved_constraint, value)
                                .map_err(Self::normalize_coercion_binding_error)?;
                            // A Failure from coercion is passed through as-is
                            // (it will throw when sunk or used). Only check
                            // type match for non-Failure results.
                            if !matches!(&value, Value::Instance { class_name, .. } if class_name.resolve() == "Failure")
                                && !self.type_matches_value(target, &value)
                            {
                                return Err(coerce_impossible_error(
                                    &resolved_constraint,
                                    &original,
                                ));
                            }
                        } else if pd.name.starts_with('@') || pd.name.starts_with('%') {
                            let expected = self
                                .typed_container_param_expected(&pd.name, &resolved_constraint)
                                .unwrap_or_else(|| resolved_constraint.clone());
                            if !self.typed_container_param_matches(
                                &pd.name,
                                &resolved_constraint,
                                &value,
                                source_name.as_deref(),
                                source_type_constraint.as_deref(),
                            ) {
                                let mut err = RuntimeError::new(format!(
                                    "{}: Type check failed in binding to parameter '{}'; expected {} but got {} ({})",
                                    type_error_kind,
                                    pd.name,
                                    expected,
                                    crate::runtime::value_type_name(&value),
                                    crate::runtime::utils::gist_value(&value)
                                ));
                                let mut ex_attrs = std::collections::HashMap::new();
                                ex_attrs
                                    .insert("message".to_string(), Value::str(err.message.clone()));
                                let exception = Value::make_instance(
                                    Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                    ex_attrs,
                                );
                                err.exception = Some(Box::new(exception));
                                return Err(err);
                            }
                        } else if resolved_constraint == "Num"
                            && matches!(
                                value,
                                Value::Int(_)
                                    | Value::Num(_)
                                    | Value::Rat(_, _)
                                    | Value::FatRat(_, _)
                                    | Value::BigRat(_, _)
                            )
                        {
                            // Binding accepts numeric widening into Num parameters.
                        } else if !self.type_matches_value(&resolved_constraint, &value) {
                            // :D/:U smiley mismatch → X::Parameter::InvalidConcreteness
                            let (base_type, smiley) =
                                crate::runtime::types::strip_type_smiley(&resolved_constraint);
                            if smiley.is_some_and(|s| s == ":D" || s == ":U")
                                && self.type_matches_value(base_type, &value)
                            {
                                let should_be_concrete = smiley == Some(":D");
                                let got_type = if let Value::Package(pkg) = &value {
                                    pkg.resolve().to_string()
                                } else {
                                    crate::runtime::value_type_name(&value).to_string()
                                };
                                let routine = self
                                    .samewith_context_stack
                                    .last()
                                    .map(|(name, _)| name.as_str())
                                    .unwrap_or("<anon>");
                                return Err(RuntimeError::parameter_invalid_concreteness(
                                    base_type,
                                    &got_type,
                                    routine,
                                    &pd.name,
                                    should_be_concrete,
                                    pd.is_invocant,
                                ));
                            }
                            let display_name = if pd.name == "__type_only__" {
                                format!("parameter '{}'", resolved_constraint)
                            } else {
                                pd.name.clone()
                            };
                            let got = crate::runtime::value_type_name(&value);
                            return Err(RuntimeError::typecheck_binding_parameter(
                                &display_name,
                                &resolved_constraint,
                                got,
                                Some(format!(
                                    "{}: Type check failed for {}: expected {}, got {}",
                                    type_error_kind, display_name, resolved_constraint, got
                                )),
                            ));
                        } else {
                            value = self
                                .try_coerce_value_for_constraint(&resolved_constraint, value)
                                .map_err(Self::normalize_coercion_binding_error)?;
                        }
                        if (resolved_constraint.starts_with("Associative[")
                            || resolved_constraint.starts_with("Hash["))
                            && let Value::Array(items, ..) = &value
                        {
                            let mut map = std::collections::HashMap::new();
                            for item in items.iter() {
                                if let Value::Pair(k, v) = item {
                                    map.insert(k.clone(), *v.clone());
                                }
                            }
                            value = Value::hash(map);
                        }
                    }
                    // Wrap native integer values for sub parameter binding (overflow wrapping)
                    if let Some(constraint) = &pd.type_constraint {
                        value = wrap_native_int_for_binding(constraint, value)?;
                    }
                    // Implicit Any constraint: untyped $ parameters default to Any,
                    // which rejects Junction (a direct subtype of Mu, not Any).
                    // Skip when a where constraint is present: the where clause
                    // handles the type checking, and junctions should be passed
                    // through to the where clause for proper checking.
                    if pd.type_constraint.is_none()
                        && pd.where_constraint.is_none()
                        && !pd.name.starts_with('@')
                        && !pd.name.starts_with('%')
                        && !pd.name.starts_with('&')
                        && !pd.slurpy
                        && !self.type_matches_value("Any", &value)
                    {
                        let mut err = RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Any but got {} ({})",
                            if pd.name.is_empty() {
                                "<anon>"
                            } else {
                                &pd.name
                            },
                            crate::runtime::value_type_name(&value),
                            crate::runtime::utils::gist_value(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    // Implicit Positional constraint: untyped @-sigiled parameters
                    // require the argument to be Positional (Array, List, etc.).
                    if pd.type_constraint.is_none()
                        && pd.name.starts_with('@')
                        && !pd.slurpy
                        && !self.type_matches_value("Positional", &value)
                    {
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        let mut err = RuntimeError::new(format!(
                            "{}: Type check failed in binding to parameter '{}'; expected Positional but got {} ({})",
                            type_error_kind,
                            pd.name,
                            crate::runtime::value_type_name(&value),
                            crate::runtime::utils::gist_value(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    // Implicit Associative constraint: untyped %-sigiled parameters
                    // require the argument to be Associative (Hash, Map, etc.).
                    if pd.type_constraint.is_none()
                        && pd.name.starts_with('%')
                        && !pd.slurpy
                        && !self.type_matches_value("Associative", &value)
                    {
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        let mut err = RuntimeError::new(format!(
                            "{}: Type check failed in binding to parameter '{}'; expected Associative but got {} ({})",
                            type_error_kind,
                            pd.name,
                            crate::runtime::value_type_name(&value),
                            crate::runtime::utils::gist_value(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    // Implicit Callable constraint: untyped &-sigiled parameters
                    // require the argument to be Callable (Sub, Block, etc.).
                    if pd.type_constraint.is_none()
                        && pd.name.starts_with('&')
                        && !pd.slurpy
                        && !self.type_matches_value("Callable", &value)
                    {
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        let mut err = RuntimeError::new(format!(
                            "{}: Type check failed in binding to parameter '{}'; expected Callable but got {} ({})",
                            type_error_kind,
                            pd.name,
                            crate::runtime::value_type_name(&value),
                            crate::runtime::utils::gist_value(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    if let Some((sig_params, sig_ret)) = &pd.code_signature
                        && !code_signature_matches_value(self, sig_params, sig_ret, &value)
                    {
                        let mut err = RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Callable with matching signature, got {}",
                            pd.name,
                            crate::runtime::value_type_name(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    if let Some(where_expr) = &pd.where_constraint {
                        let saved_param = if pd.name.is_empty() {
                            None
                        } else {
                            self.env.get(&pd.name).cloned()
                        };
                        if !pd.name.is_empty() {
                            self.bind_param_value(&pd.name, value.clone());
                        }
                        let saved_topic = self.env.get("_").cloned();
                        self.env.insert("_".to_string(), value.clone());
                        let ok = match where_expr.as_ref() {
                            Expr::AnonSub { body, .. } => self
                                .eval_block_value(body)
                                .map(|v| v.truthy())
                                .unwrap_or(false),
                            // Method calls on $_ (topic) in where constraints:
                            // evaluate and check truthiness of the result, not smart-match.
                            // `where .method: args` is equivalent to `where { .method: args }`.
                            Expr::MethodCall { target, .. } if matches!(target.as_ref(), Expr::Var(name) if name == "_") => {
                                self.eval_block_value(&[Stmt::Expr(where_expr.as_ref().clone())])
                                    .map(|v| v.truthy())
                                    .unwrap_or(false)
                            }
                            expr => self
                                .eval_block_value(&[Stmt::Expr(expr.clone())])
                                .map(|v| self.smart_match(&value, &v))
                                .unwrap_or(false),
                        };
                        if let Some(previous) = saved_topic {
                            self.env.insert("_".to_string(), previous);
                        } else {
                            self.env.remove("_");
                        }
                        if !pd.name.is_empty() {
                            if let Some(previous) = saved_param {
                                self.env.insert(pd.name.clone(), previous);
                            } else {
                                self.env.remove(&pd.name);
                            }
                        }
                        if !ok {
                            let mut err = RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: where constraint failed for parameter '{}'",
                                pd.name
                            ));
                            let mut ex_attrs = std::collections::HashMap::new();
                            ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                            let exception = Value::make_instance(
                                Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                ex_attrs,
                            );
                            err.exception = Some(Box::new(exception));
                            return Err(err);
                        }
                    }
                    // Resolve type capture prefixes (e.g., `::T` → `Int`) so
                    // that the stored variable type constraint uses the
                    // concrete captured type name, not the raw `::T` token.
                    let bound_type_constraint = bound_type_constraint.map(|tc| {
                        if let Some(captured_name) = tc.strip_prefix("::") {
                            self.resolved_type_capture_name(captured_name)
                        } else {
                            self.resolved_type_capture_name(&tc)
                        }
                    });
                    if !pd.name.is_empty()
                        && pd.name != "__type_only__"
                        && !pd.name.starts_with("__type_capture__")
                    {
                        if pd.name.starts_with('%')
                            && let Value::Array(items, ..) = &value
                        {
                            let mut map = std::collections::HashMap::new();
                            for item in items.iter() {
                                if let Value::Pair(k, v) = item {
                                    map.insert(k.clone(), *v.clone());
                                }
                            }
                            self.bind_param_value(&pd.name, Value::hash(map));
                            self.set_var_type_constraint(&pd.name, bound_type_constraint.clone());
                            if let Some(sub_params) = &pd.sub_signature {
                                let target = self.env.get(&pd.name).cloned().unwrap_or(Value::Nil);
                                bind_sub_signature_from_value(self, sub_params, &target)?;
                            }
                            positional_idx += 1;
                            continue;
                        }
                        // Shape constraint check for array parameters
                        if let Some(shape_exprs) = &pd.shape_constraints {
                            self.check_shape_constraint(&pd.name, &value, shape_exprs, args)?;
                        }
                        // Slice 2d: wrap a scalar-param array/hash in a shared cell
                        // so in-sub mutations (incl. `my @a := @$n`) propagate to
                        // the caller via the rw-writeback at return. Only an `@`/`%`
                        // source variable needs this: the fast paths copy the array
                        // out of the `@`-variable and detach it. A `$`-scalar source
                        // (`my $aref = [0]; f($aref)`) already shares the array by
                        // reference, so promoting it would wrongly turn `$aref` into
                        // a cell and break `$aref[0]++` (S06 named-parameters 68-69).
                        if scalar_container_share
                            && matches!(value, Value::Array(..) | Value::Hash(..))
                            && let Some(source_name) = &source_name
                            && (source_name.starts_with('@') || source_name.starts_with('%'))
                        {
                            value = Value::ContainerRef(std::sync::Arc::new(
                                std::sync::Mutex::new(value),
                            ));
                            rw_bindings.push((pd.name.clone(), source_name.clone()));
                        }
                        self.bind_param_value(&pd.name, value);
                        self.set_var_type_constraint(&pd.name, bound_type_constraint.clone());
                    }
                    if let Some(sub_params) = &pd.sub_signature {
                        let target = self
                            .env
                            .get(&pd.name)
                            .cloned()
                            .unwrap_or_else(|| args[positional_idx].clone());
                        bind_sub_signature_from_value(self, sub_params, &target)?;
                    }
                    positional_idx += 1;
                } else if let Some(default_expr) = &pd.default {
                    let value = self.eval_param_default(pd, default_expr)?;
                    let value = self.checked_default_param_value(pd, value)?;
                    if let Some(captured_name) = pd
                        .type_constraint
                        .as_deref()
                        .and_then(|constraint| constraint.strip_prefix("::"))
                    {
                        self.bind_type_capture(captured_name, &value);
                    } else if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, value);
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                    if let Some(sub_params) = &pd.sub_signature {
                        let target = self.env.get(&pd.name).cloned().unwrap_or(Value::Nil);
                        bind_sub_signature_from_value(self, sub_params, &target)?;
                    }
                } else if !pd.optional_marker && !pd.name.is_empty() {
                    // Positional parameter with no default and no `?` marker is required.
                    // Count required and total positional params for the error message.
                    let total_positional = param_defs
                        .iter()
                        .filter(|p| {
                            !p.named
                                && !p.slurpy
                                && !p.double_slurpy
                                && !p.onearg
                                && !p.name.starts_with(':')
                        })
                        .count();
                    let positional_arg_count = args
                        .iter()
                        .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                        .count();
                    return Err(RuntimeError::new(format!(
                        "Too few positionals passed; expected {} argument{} but got {}",
                        total_positional,
                        if total_positional == 1 { "" } else { "s" },
                        positional_arg_count
                    )));
                } else if !pd.name.is_empty() {
                    // Optional parameters use typed empties/type objects when omitted.
                    self.bind_param_value(&pd.name, Self::missing_optional_param_value(pd));
                    self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                }
            }
        }
        // Check for unexpected named arguments when no hash/capture slurpy is present
        let has_hash_slurpy = param_defs
            .iter()
            .any(|pd| pd.slurpy && (pd.name.starts_with('%') || pd.sigilless));
        // Only skip for positional sub-signatures (unpacking dispatch), not named renaming
        let has_positional_sub_sig = param_defs
            .iter()
            .any(|pd| !pd.named && pd.sub_signature.is_some());
        if !has_hash_slurpy && !has_positional_sub_sig {
            for arg in plain_args.iter() {
                let arg = unwrap_varref_value(arg.clone());
                if let Value::Pair(key, _) = arg {
                    // Empty-string named args (e.g. from `'' => val` in a hash) are
                    // silently ignored -- they cannot bind to any named parameter.
                    if key.is_empty() {
                        continue;
                    }
                    // Check if this named arg was consumed by a named param or colon placeholder
                    let consumed = param_defs.iter().any(|pd| {
                        if (pd.named && pd.name == key)
                            || pd.name == format!(":{}", key)
                            || pd.name == format!("@:{}", key)
                            || pd.name == format!("%:{}", key)
                            // Named params with sigils: :@l has name "@l", match key "l"
                            || (pd.named && (pd.name == format!("@{}", key) || pd.name == format!("%{}", key)))
                        {
                            return true;
                        }
                        // Also check inner named aliases from sub-signatures
                        if let Some(sub_params) = &pd.sub_signature {
                            for sp in sub_params {
                                if sp.named && sp.name == key {
                                    return true;
                                }
                            }
                        }
                        false
                    });
                    if !consumed {
                        return Err(RuntimeError::new(format!(
                            "Unexpected named argument '{}' passed",
                            key
                        )));
                    }
                }
                // Note: ValuePair is a positional pair (e.g. from parenthesized (:a(3))),
                // so it's NOT treated as a named argument.
            }
        }
        // Check for extra positional arguments when no array/capture slurpy is present
        let has_array_slurpy = param_defs
            .iter()
            .any(|pd| pd.onearg || (pd.slurpy && (!pd.name.starts_with('%') || pd.sigilless)));
        if !has_array_slurpy && !has_positional_sub_sig {
            let positional_param_count = param_defs
                .iter()
                .filter(|pd| !pd.named && !pd.slurpy && !pd.onearg)
                .count();
            let positional_arg_count = plain_args
                .iter()
                .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                .count();
            if positional_arg_count > positional_param_count {
                return Err(RuntimeError::new(format!(
                    "Too many positionals passed; expected {} arguments but got {}",
                    positional_param_count, positional_arg_count
                )));
            }
        }
        // Mark parameters as readonly unless they have `is rw`, `is copy`, or `is raw` traits.
        // Sigilless params (\x) are always writable (they are raw aliases).
        // For `is raw` with non-lvalue args, also mark readonly.
        for pd in param_defs {
            if pd.name.is_empty() || pd.name == "__type_only__" || pd.name == "__subsig__" {
                continue;
            }
            if pd.sigilless {
                continue; // Sigilless params are raw aliases, always writable
            }
            if pd.name.starts_with('!') || pd.name.starts_with('.') {
                continue; // Attribute-binding params ($!attr, $.attr) are always writable
            }
            let has_mutable_trait = pd
                .traits
                .iter()
                .any(|t| t == "rw" || t == "copy" || t == "raw");
            // Plain `@`/`%` params are NOT readonly the way `$` params are: the
            // bound container is writable (element/whole-container assignment and
            // `.push` are allowed and propagate to the caller); only `:=`
            // rebinding is forbidden. So keep `$` scalar params readonly but let
            // array/hash params through. (`@!attr`/`%.attr` twigil params are
            // attribute binds, already skipped above.)
            let is_container_param = pd.name.starts_with('@') || pd.name.starts_with('%');
            if !is_container_param
                && (!has_mutable_trait || raw_nonlvalue_params.contains(&pd.name))
            {
                self.readonly_vars.insert(pd.name.clone());
            }
        }
        Ok(rw_bindings)
    }

    /// Check shape constraint for array parameters in signatures.
    fn check_shape_constraint(
        &mut self,
        param_name: &str,
        value: &Value,
        shape_exprs: &[Expr],
        all_args: &[Value],
    ) -> Result<(), RuntimeError> {
        let _ = all_args; // reserved for future use
        let actual_shape = crate::runtime::utils::shaped_array_shape(value);

        // Evaluate expected dimensions from shape expressions
        let mut expected_dims: Vec<Option<usize>> = Vec::new();
        for expr in shape_exprs {
            match expr {
                Expr::Whatever | Expr::HyperWhatever => {
                    // * means any size for this dimension
                    expected_dims.push(None);
                }
                _ => {
                    let dim_val = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                    match &dim_val {
                        Value::Whatever | Value::HyperWhatever => {
                            expected_dims.push(None);
                        }
                        Value::Int(n) => {
                            expected_dims.push(Some(*n as usize));
                        }
                        Value::BigInt(n) => {
                            use num_traits::ToPrimitive;
                            expected_dims.push(Some(n.to_usize().unwrap_or(0)));
                        }
                        Value::Num(n) if n.is_infinite() || n.is_nan() => {
                            // Inf/NaN means wildcard (e.g. * coerced to Inf)
                            expected_dims.push(None);
                        }
                        Value::Num(n) => {
                            expected_dims.push(Some(*n as usize));
                        }
                        _ => {
                            let coerced = crate::runtime::utils::coerce_to_numeric(dim_val);
                            match &coerced {
                                Value::Int(n) => expected_dims.push(Some(*n as usize)),
                                Value::Num(n) if n.is_infinite() || n.is_nan() => {
                                    expected_dims.push(None);
                                }
                                _ => expected_dims.push(None),
                            }
                        }
                    }
                }
            }
        }

        // Get actual shape
        let actual = match &actual_shape {
            Some(shape) => shape.clone(),
            None => {
                // Unshaped array - reject unless shape constraint is just [*]
                // (single wildcard dimension accepts unshaped arrays)
                if expected_dims.len() == 1 && expected_dims[0].is_none() {
                    return Ok(());
                }
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected shaped array with {} dimension(s)",
                    param_name,
                    expected_dims.len()
                )));
            }
        };

        // Check number of dimensions matches
        if actual.len() != expected_dims.len() {
            return Err(RuntimeError::new(format!(
                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {} dimension(s), got {}",
                param_name,
                expected_dims.len(),
                actual.len()
            )));
        }

        // Check each dimension
        for (i, (expected, &actual_dim)) in expected_dims.iter().zip(actual.iter()).enumerate() {
            if let Some(expected_dim) = expected
                && *expected_dim != actual_dim
            {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; dimension {} expected {}, got {}",
                    param_name, i, expected_dim, actual_dim
                )));
            }
        }

        Ok(())
    }
}
