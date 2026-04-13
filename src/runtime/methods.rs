use super::methods_signature::{
    make_method_not_found_error, make_multi_no_match_error, make_x_immutable_error,
};
use super::*;
use crate::symbol::Symbol;
use crate::value::signature::extract_sig_info;

impl Interpreter {
    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Scalar containers are transparent for method dispatch (except .item itself)
        if let Value::Scalar(inner) = target {
            return self.call_method_with_values(*inner, method, args);
        }
        // .return method: triggers a return from the enclosing sub with the invocant
        if method == "return" && args.is_empty() {
            let mut err = RuntimeError::new("return");
            err.return_value = Some(target);
            return Err(err);
        }
        // .WALK(name, :roles) — walk class+role chain calling the named
        // submethod once per "own" definition. Returns a no-arg Sub that,
        // when invoked, yields the list of results.
        if method == "WALK"
            && let Some(value) = self.try_walk_method(&target, &args)?
        {
            return Ok(value);
        }
        // Unhandled Failure explosion
        if let Value::Instance { class_name, .. } = &target
            && class_name.resolve() == "Failure"
            && !target.is_failure_handled()
            && !matches!(
                method,
                "exception"
                    | "handled"
                    | "self"
                    | "defined"
                    | "Bool"
                    | "so"
                    | "not"
                    | "gist"
                    | "Str"
                    | "raku"
                    | "perl"
                    | "WHICH"
                    | "backtrace"
                    | "is-handling"
                    | "WHAT"
                    | "^name"
                    | "isa"
                    | "does"
                    | "ACCEPTS"
                    | "Failure"
                    | "sink"
            )
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&target)
        {
            return Err(err);
        }
        // Deprecation.report
        if method == "report"
            && matches!(&target, Value::Package(name) if name.resolve() == "Deprecation")
        {
            return Ok(match super::deprecation::take_report() {
                Some(report) => Value::str(report),
                None => Value::Nil,
            });
        }

        // Junction auto-threading
        if Self::should_autothread_method(method)
            && let Value::Junction { kind, values } = &target
        {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                results.push(self.call_method_with_values(value.clone(), method, args.clone())?);
            }
            return Ok(Value::junction(kind.clone(), results));
        }
        if Self::should_autothread_method(method)
            && let Some((idx, kind, values)) =
                args.iter().enumerate().find_map(|(idx, arg)| match arg {
                    Value::Junction { kind, values } => Some((idx, kind.clone(), values.clone())),
                    _ => None,
                })
        {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                let mut threaded_args = args.clone();
                threaded_args[idx] = value.clone();
                results.push(self.call_method_with_values(
                    target.clone(),
                    method,
                    threaded_args,
                )?);
            }
            return Ok(Value::junction(kind, results));
        }
        // Junction .raku/.perl/.gist/.Str rendering
        if args.is_empty()
            && matches!(method, "raku" | "perl" | "gist" | "Str")
            && let Value::Junction { kind, values } = &target
        {
            let kind_name = match kind {
                JunctionKind::Any => "any",
                JunctionKind::All => "all",
                JunctionKind::One => "one",
                JunctionKind::None => "none",
            };
            let render_method = match method {
                "gist" => "gist",
                "Str" => "Str",
                _ => "raku",
            };
            let mut parts = Vec::with_capacity(values.len());
            for value in values.iter() {
                if method == "gist" && matches!(value, Value::Nil) {
                    parts.push("Nil".to_string());
                    continue;
                }
                let rendered =
                    self.call_method_with_values(value.clone(), render_method, vec![])?;
                parts.push(rendered.to_string_value());
            }
            return Ok(Value::str(format!("{}({})", kind_name, parts.join(", "))));
        }
        // Enum type collection methods
        if let Value::Package(pkg_name) = &target
            && args.is_empty()
            && matches!(
                method,
                "pairs" | "keys" | "values" | "kv" | "antipairs" | "invert"
            )
        {
            if let Some(variants) = self.enum_types.get(&pkg_name.resolve()) {
                let variants = variants.clone();
                return self.dispatch_enum_type_collection(method, &variants);
            }
            return Ok(Value::array(Vec::new()));
        }

        // Buf/Blob write-num32 / write-num64 — non-mut entry point.
        // Handles both type object (e.g. `buf8.write-num32(0, 42e0)`) and
        // instance form (e.g. `(my $b := buf8.new).write-num32(0, 42e0)`).
        // For instances we mutate the underlying instance id so that any
        // bindings observing the same instance see the change.
        if super::buf_write_num::write_num_size(method).is_some() {
            let (is_inst, cn_opt, base_bytes, class_sym_opt, id_opt, attrs_opt) = match &target {
                Value::Package(name) => {
                    let cn = name.resolve();
                    if crate::runtime::utils::is_buf_or_blob_class(&cn) {
                        (false, Some(cn), Vec::new(), None, None, None)
                    } else {
                        (false, None, Vec::new(), None, None, None)
                    }
                }
                Value::Instance {
                    class_name,
                    attributes,
                    id,
                } => {
                    let cn = class_name.resolve();
                    if crate::runtime::utils::is_buf_or_blob_class(&cn) {
                        let mut v: Vec<u8> = Vec::new();
                        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                            v.reserve(items.len());
                            for it in items.iter() {
                                v.push(match it {
                                    Value::Int(i) => (*i).clamp(0, 255) as u8,
                                    Value::Num(f) => (*f as i64).clamp(0, 255) as u8,
                                    _ => 0,
                                });
                            }
                        }
                        (
                            true,
                            Some(cn),
                            v,
                            Some(*class_name),
                            Some(*id),
                            Some(attributes.as_ref().clone()),
                        )
                    } else {
                        (false, None, Vec::new(), None, None, None)
                    }
                }
                _ => (false, None, Vec::new(), None, None, None),
            };
            if let Some(cn) = cn_opt {
                if is_inst && (cn == "Blob" || cn.starts_with("Blob[") || cn.starts_with("blob")) {
                    return Err(RuntimeError::new(format!(
                        "Cannot modify immutable {} with {}",
                        cn, method
                    )));
                }
                if args.len() < 2 || args.len() > 3 {
                    return Err(RuntimeError::new(format!(
                        "{} expects 2 or 3 arguments, got {}",
                        method,
                        args.len()
                    )));
                }
                let offset_i64 = match &args[0] {
                    Value::Int(i) => *i,
                    Value::Num(f) => *f as i64,
                    _ => 0,
                };
                let endian_val = if args.len() == 3 {
                    super::buf_write_num::decode_endian(&args[2])
                } else {
                    0
                };
                let mut bytes = base_bytes;
                super::buf_write_num::apply_write_num(
                    &mut bytes, method, offset_i64, &args[1], endian_val,
                )?;
                if is_inst {
                    let class_sym = class_sym_opt.unwrap();
                    let id = id_opt.unwrap();
                    let mut updated_attrs = attrs_opt.unwrap();
                    updated_attrs.insert(
                        "bytes".to_string(),
                        Value::array(bytes.into_iter().map(|b| Value::Int(b as i64)).collect()),
                    );
                    self.overwrite_instance_bindings_by_identity(&cn, id, updated_attrs.clone());
                    return Ok(Value::make_instance_with_id(class_sym, updated_attrs, id));
                }
                let normalized = crate::runtime::utils::normalize_buf_type_name(&cn);
                return Ok(super::buf_write_num::make_buf_value(&normalized, bytes));
            }
        }

        // IterationBuffer dispatch
        if matches!(&target, Value::Instance { class_name, .. } if class_name == "IterationBuffer")
            && matches!(
                method,
                "elems"
                    | "AT-POS"
                    | "BIND-POS"
                    | "push"
                    | "unshift"
                    | "List"
                    | "Slip"
                    | "Seq"
                    | "append"
                    | "prepend"
                    | "clear"
            )
        {
            return self.dispatch_instance_and_fallback(target, method, args);
        }
        // Iterator predictive methods
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Iterator"
        {
            match method {
                "count-only" if args.is_empty() => {
                    if let Some(value) = self.iterator_count_only_from_attrs(attributes.as_ref())? {
                        return Ok(value);
                    }
                }
                "bool-only" if args.is_empty() => {
                    if let Some(value) = self.iterator_bool_only_from_attrs(attributes.as_ref())? {
                        return Ok(value);
                    }
                }
                "can"
                    if args.len() == 1
                        && Self::iterator_supports_predictive_methods(attributes.as_ref()) =>
                {
                    let method_name = args[0].to_string_value();
                    if matches!(method_name.as_str(), "count-only" | "bool-only") {
                        return Ok(Value::array(vec![Value::str(method_name)]));
                    }
                }
                _ => {}
            }
        }
        // DateTime/Date formatter rendering
        if args.is_empty()
            && matches!(method, "Str" | "gist")
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|name| name == "DateTime" || name == "Date")
            && let Some(formatter) = attributes.get("formatter")
        {
            let saved_env = self.env().clone();
            let saved_readonly = self.save_readonly_vars();
            let rendered = self.eval_call_on_value(formatter.clone(), vec![target.clone()])?;
            *self.env_mut() = saved_env;
            self.restore_readonly_vars(saved_readonly);
            return Ok(Value::str(rendered.to_string_value()));
        }
        // Immutable List/Range: push/pop/shift/unshift/append/prepend/splice must throw
        if matches!(
            method,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        ) {
            let is_immutable = match &target {
                Value::Array(_, kind) => !kind.is_real_array(),
                Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. } => true,
                _ => false,
            };
            if is_immutable {
                let typename = match &target {
                    Value::Array(..) => "List",
                    _ => "Range",
                };
                return Err(make_x_immutable_error(method, typename));
            }
        }
        // Non-container definite values: mutating methods throw errors.
        // In Raku, push/unshift/append/prepend/splice are defined on Any:U so calling
        // on a definite non-array value throws X::Multi::NoMatch; pop/shift have no
        // such proto and throw X::Method::NotFound.
        if matches!(
            method,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        ) && matches!(
            &target,
            Value::Int(_)
                | Value::Num(_)
                | Value::Str(_)
                | Value::Bool(_)
                | Value::Rat(..)
                | Value::Complex(..)
        ) {
            if matches!(method, "pop" | "shift") {
                let type_name = crate::runtime::utils::value_type_name(&target);
                return Err(make_method_not_found_error(method, type_name, false));
            }
            return Err(make_multi_no_match_error(method));
        }
        // Mutating array methods on Value::Array (non-container path)
        if matches!(
            method,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend"
        ) && matches!(&target, Value::Array(_, kind) if kind.is_real_array())
        {
            // Check element type constraints from container metadata (e.g., typed attribute arrays)
            if matches!(method, "push" | "append" | "unshift" | "prepend") {
                self.check_array_value_element_types(&target, &args)?;
            }
            return self.array_mutate_copy(target, method, args);
        }
        // IO::Special.new("<STDOUT>")
        if let Value::Package(name) = &target
            && name.resolve() == "IO::Special"
            && method == "new"
        {
            return self.native_io_special(&HashMap::new(), "new", args);
        }
        // IO::Spec::* class methods
        if let Value::Package(name) = &target {
            let cn = name.resolve();
            if cn.starts_with("IO::Spec") {
                let is_win32 = cn == "IO::Spec::Win32";
                match method {
                    "canonpath" => {
                        // Separate positional and named args (e.g. `:parent`).
                        let mut positional: Vec<&Value> = Vec::new();
                        let mut parent = false;
                        for a in &args {
                            if let Value::Pair(k, v) = a {
                                if k == "parent" {
                                    parent = v.truthy();
                                }
                            } else {
                                positional.push(a);
                            }
                        }
                        let first = positional.first().copied();
                        // Undefined invocant (Any / Nil / type object) -> ''
                        let is_undef =
                            matches!(first, None | Some(Value::Nil) | Some(Value::Package(_)));
                        if is_undef {
                            return Ok(Value::str_from(""));
                        }
                        let path = first.map(|v| v.to_string_value()).unwrap_or_default();
                        let cleaned = if is_win32 {
                            Self::cleanup_io_path_lexical_win32(&path)
                        } else {
                            Self::canonpath_unix(&path, parent)
                        };
                        return Ok(Value::str(cleaned));
                    }
                    "is-absolute" => {
                        let path = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let abs = if is_win32 {
                            // Drive-letter, UNC, or leading slash/backslash.
                            let bytes = path.as_bytes();
                            (bytes.len() >= 2 && bytes[1] == b':' && bytes[0].is_ascii_alphabetic())
                                || path.starts_with('/')
                                || path.starts_with('\\')
                        } else {
                            // Unix: starts with '/'. Use chars().next() so that a
                            // leading '/' followed by combining marks still counts.
                            path.starts_with('/')
                        };
                        return Ok(Value::Bool(abs));
                    }
                    "dir-sep" => {
                        return Ok(Value::str_from(if is_win32 { "\\" } else { "/" }));
                    }
                    "devnull" => {
                        return Ok(Value::str_from(if is_win32 { "NUL" } else { "/dev/null" }));
                    }
                    _ => {}
                }
            }
        }
        // Buf/Blob.allocate
        if method == "allocate"
            && let Value::Package(name) = &target
        {
            let cn = name.resolve();
            if cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("buf")
                || cn.starts_with("blob")
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
            {
                return self.buf_allocate(*name, &args);
            }
        }
        // Coerce Instance args for log/exp/atan2
        let mut args = args;
        if matches!(method, "log" | "exp" | "atan2") {
            for arg in &mut args {
                if !matches!(arg, Value::Instance { .. }) {
                    continue;
                }
                let original = arg.clone();
                if let Ok(coerced) = self
                    .call_method_with_values(original.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(original.clone(), "Bridge", vec![]))
                {
                    *arg = coerced;
                }
            }
        }
        // .arity / .count on callable values
        if matches!(method, "arity" | "count")
            && args.is_empty()
            && let Some(sig_info) = extract_sig_info(&target)
        {
            return Ok(if method == "arity" {
                Value::Int(Self::signature_required_positional_count(&sig_info))
            } else {
                Self::signature_count_value(&sig_info)
            });
        }

        // .throw on user-defined Exception subclasses
        if method == "throw"
            && args.is_empty()
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
        {
            let cn = class_name.resolve();
            let is_exception = cn == "Exception"
                || cn.starts_with("X::")
                || cn.starts_with("CX::")
                || self
                    .class_mro(&cn)
                    .iter()
                    .any(|p| p == "Exception" || p.starts_with("X::") || p.starts_with("CX::"));
            if is_exception {
                let msg = attributes
                    .get("message")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| target.to_string_value());
                let mut err = RuntimeError::new(&msg);
                err.exception = Some(Box::new(target.clone()));
                return Err(err);
            }
        }

        // .Str identity for Str-inheriting instances
        if method == "Str"
            && args.is_empty()
            && let Value::Instance { class_name, .. } = &target
        {
            let receiver = class_name.resolve();
            if !self.class_mro(&receiver).iter().any(|cn| cn == "Str") {
                // Non-Str classes keep normal method lookup
            } else if let Some((owner, _)) = self.resolve_method_with_owner(&receiver, "Str", &[]) {
                if owner == "Str" {
                    return Ok(target.clone());
                }
            } else {
                return Ok(target.clone());
            }
        }

        // Private method call on non-Instance
        if let Some(result) = self.dispatch_private_method_on_non_instance(&target, method) {
            return result;
        }

        // Qualified method: Class::method on Instance
        if let Some(result) = self.dispatch_qualified_instance_method(&target, method, args.clone())
        {
            return result;
        }

        // Qualified method on non-Instance values
        if let Some(result) =
            self.dispatch_qualified_non_instance_method(&target, method, args.clone())
        {
            return result;
        }

        // Proxy subclass method dispatch
        if let Some(result) = self.dispatch_proxy_subclass_method(&target, method, &args) {
            return result;
        }

        // Auto-FETCH Proxy values
        if let Some(result) = self.dispatch_proxy_auto_fetch(&target, method, args.clone()) {
            return result;
        }

        // Dispatch temporal n-arg methods
        if let Some(result) =
            super::methods_temporal::dispatch_temporal_method(&target, method, &args)
        {
            let val = result?;
            if let Value::Instance { ref attributes, .. } = val
                && attributes.contains_key("formatter")
                && !attributes.contains_key("__formatter_rendered")
            {
                let formatter = attributes.get("formatter").unwrap().clone();
                return self.render_date_formatter(val, formatter);
            }
            return Ok(val);
        }

        // Format instance dispatch
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Format"
        {
            let fmt = attributes
                .get("format")
                .map(Value::to_string_value)
                .unwrap_or_default();
            match method {
                "CALL-ME" => {
                    return Ok(Value::str(super::sprintf::format_sprintf_args(&fmt, &args)));
                }
                "Str" | "gist" => return Ok(Value::str(fmt)),
                _ => {}
            }
        }
        // Match.make
        if let Value::Instance {
            class_name,
            attributes,
            id,
        } = &target
            && class_name == "Match"
            && method == "make"
        {
            let value = args.first().cloned().unwrap_or(Value::Nil);
            let mut attrs = crate::value::InstanceAttrs::clone(attributes);
            attrs.insert("ast".to_string(), value.clone());
            let updated = Value::Instance {
                class_name: *class_name,
                attributes: std::sync::Arc::new(crate::value::InstanceAttrs::new(
                    *class_name,
                    attrs,
                    *id,
                    false,
                )),
                id: *id,
            };
            self.env.insert("/".to_string(), updated);
            self.env.insert("made".to_string(), value.clone());
            self.action_made = Some(value.clone());
            return Ok(value);
        }

        // Routine::WrapHandle .restore()
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name.resolve() == "Routine::WrapHandle"
            && method == "restore"
        {
            let sub_id = attributes.get("sub-id").and_then(|v| {
                if let Value::Int(i) = v {
                    Some(*i as u64)
                } else {
                    None
                }
            });
            let handle_id = attributes.get("handle-id").and_then(|v| {
                if let Value::Int(i) = v {
                    Some(*i as u64)
                } else {
                    None
                }
            });
            if let (Some(sub_id), Some(handle_id)) = (sub_id, handle_id) {
                if let Some(chain) = self.wrap_chains.get_mut(&sub_id) {
                    chain.retain(|(hid, _)| *hid != handle_id);
                    if chain.is_empty() {
                        self.cleanup_wrap_name_entries(sub_id);
                    }
                }
                return Ok(Value::Bool(true));
            }
            return Err(RuntimeError::new(
                "Invalid WrapHandle: missing sub-id or handle-id",
            ));
        }
        // .gist with Instance-containing collections
        if method == "gist" && args.is_empty() {
            fn collection_contains_instance(value: &Value) -> bool {
                match value {
                    Value::Instance { .. } => true,
                    _ if value.as_list_items().is_some() => value
                        .as_list_items()
                        .unwrap()
                        .iter()
                        .any(collection_contains_instance),
                    Value::Hash(map) => map.values().any(collection_contains_instance),
                    _ => false,
                }
            }
            fn gist_item(interp: &mut Interpreter, value: &Value) -> String {
                match value {
                    Value::Nil => "Nil".to_string(),
                    _ if value.as_list_items().is_some() => {
                        let inner = value
                            .as_list_items()
                            .unwrap()
                            .iter()
                            .map(|item| gist_item(interp, item))
                            .collect::<Vec<_>>()
                            .join(" ");
                        format!("({inner})")
                    }
                    other => match interp.call_method_with_values(other.clone(), "gist", vec![]) {
                        Ok(Value::Str(s)) => s.to_string(),
                        Ok(v) => v.to_string_value(),
                        Err(_) => other.to_string_value(),
                    },
                }
            }
            if let Some(items) = target.as_list_items()
                && items.iter().any(collection_contains_instance)
            {
                let inner = items
                    .iter()
                    .map(|item| gist_item(self, item))
                    .collect::<Vec<_>>()
                    .join(" ");
                return Ok(Value::str(format!("({inner})")));
            }
        }
        // Supply type-object method error
        if matches!(
            method,
            "max"
                | "min"
                | "lines"
                | "delayed"
                | "reduce"
                | "classify"
                | "start"
                | "squish"
                | "produce"
                | "map"
                | "flat"
                | "batch"
                | "rotor"
                | "rotate"
                | "comb"
                | "words"
                | "snip"
                | "minmax"
                | "wait"
        ) && matches!(&target, Value::Package(name) if name == "Supply")
        {
            return Err(RuntimeError::new(format!(
                "Cannot call .{} on a Supply type object",
                method
            )));
        }
        // Supply.merge/zip as class methods
        if method == "merge" && matches!(&target, Value::Package(name) if name == "Supply") {
            return self.dispatch_supply_merge(&args);
        }
        if method == "zip" && matches!(&target, Value::Package(name) if name == "Supply") {
            return self.dispatch_supply_zip_class(&args);
        }
        // Supply.delayed with non-positive delay
        if method == "delayed"
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
            && args.first().is_some_and(|delay| delay.to_f64() <= 0.0)
        {
            return Ok(target);
        }
        // Array-specific methods: EXISTS-POS, ASSIGN-POS, BIND-POS, DELETE-POS, clone
        if let Value::Array(items, arr_kind) = &target {
            match (method, args.as_slice()) {
                ("EXISTS-POS", [idx]) => {
                    let index = match idx {
                        Value::Int(i) if *i >= 0 => Some(*i as usize),
                        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                        _ => None,
                    };
                    return Ok(Value::Bool(index.is_some_and(|i| i < items.len())));
                }
                ("ASSIGN-POS", [idx, value]) => {
                    let index = match idx {
                        Value::Int(i) if *i >= 0 => Some(*i as usize),
                        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                        _ => None,
                    };
                    let Some(index) = index else {
                        return Ok(Value::Nil);
                    };

                    if !matches!(value, Value::Nil)
                        && let Some((var_name, constraint)) =
                            self.env.iter().find_map(|(name, bound)| {
                                if let Value::Array(existing, ..) = bound
                                    && std::sync::Arc::ptr_eq(existing, items)
                                    && let Some(constraint) = self.var_type_constraint(name)
                                {
                                    return Some((name.clone(), constraint));
                                }
                                None
                            })
                        && !self.type_matches_value(&constraint, value)
                    {
                        return Err(RuntimeError::new(
                            crate::runtime::utils::type_check_element_error(
                                &var_name,
                                &constraint,
                                value,
                            ),
                        ));
                    }

                    let mut updated = items.to_vec();
                    if index >= updated.len() {
                        updated.resize(index + 1, Value::Package(Symbol::intern("Any")));
                    }
                    updated[index] = value.clone();
                    self.overwrite_array_bindings_by_identity(
                        items,
                        Value::Array(std::sync::Arc::new(updated), *arr_kind),
                    );
                    return Ok(value.clone());
                }
                ("BIND-POS", [_, _]) => {
                    return Err(RuntimeError::new("Cannot bind to a natively typed array"));
                }
                ("DELETE-POS", [_]) => {
                    return Err(RuntimeError::new(
                        "Cannot delete from a natively typed array",
                    ));
                }
                ("clone", _) => {
                    let cloned = items.to_vec();
                    return Ok(Value::Array(Arc::new(cloned), *arr_kind));
                }
                _ => {}
            }
        }

        // Mixin dispatch
        if let Value::Mixin(..) = &target
            && let Some(result) = self.dispatch_mixin_method_call(&target, method, args.clone())
        {
            return result;
            // If mixin dispatch didn't handle it, check for mixin __mutsu_attr__ and
            // delegate to inner value at the end (handled later in this function)
        }

        // Role type-object method punning
        if method != "new" {
            if let Value::Package(role_name) = &target
                && let Some(role) = self.roles.get(&role_name.resolve())
            {
                let is_public_attr_accessor = args.is_empty()
                    && role
                        .attributes
                        .iter()
                        .any(|(attr_name, is_public, ..)| *is_public && attr_name == method);
                if role.methods.contains_key(method) || is_public_attr_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            } else if let Value::ParametricRole {
                base_name,
                type_args,
            } = &target
                && let Some((role, _)) =
                    self.resolve_parametric_role_runtime(&base_name.resolve(), type_args)
            {
                let is_public_attr_accessor = args.is_empty()
                    && role
                        .attributes
                        .iter()
                        .any(|(attr_name, is_public, ..)| *is_public && attr_name == method);
                if role.methods.contains_key(method) || is_public_attr_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            }
        }

        // Enum type-object dispatch for pick/roll
        if let Some(result) = self.dispatch_enum_pick_roll(&target, method, &args) {
            return result;
        }

        // String pick/roll (character-wise)
        if let Some(result) = self.dispatch_string_pick_roll(&target, method, &args) {
            return result;
        }

        // Native fast path bypass and dispatch
        let skip_pseudo = self
            .skip_pseudo_method_native
            .as_ref()
            .is_some_and(|m| m == method);
        if skip_pseudo {
            self.skip_pseudo_method_native = None;
        }
        let is_pseudo_method = matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        );
        let bypass_native_fastpath = self.should_bypass_native_fastpath(
            &target,
            method,
            &args,
            skip_pseudo,
            is_pseudo_method,
        );
        let native_result = if bypass_native_fastpath {
            None
        } else {
            let method_sym = crate::symbol::Symbol::intern(method);
            match args.as_slice() {
                [] => crate::builtins::native_method_0arg(&target, method_sym),
                [a] => crate::builtins::native_method_1arg(&target, method_sym, a),
                [a, b] => crate::builtins::native_method_2arg(&target, method_sym, a, b),
                _ => None,
            }
        };
        if method == "tail"
            && !bypass_native_fastpath
            && !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
        {
            return self.dispatch_tail(target, &args);
        }
        // .raku/.perl on constrained Hash
        if matches!(method, "raku" | "perl")
            && args.is_empty()
            && matches!(&target, Value::Hash(_))
            && let Some(info) = self.container_type_metadata(&target)
            && let Value::Hash(map) = &target
        {
            return self.dispatch_constrained_hash_raku(map, &info);
        }
        // .raku/.perl on native typed shaped array (e.g. array[int])
        if matches!(method, "raku" | "perl")
            && args.is_empty()
            && matches!(&target, Value::Array(_, crate::value::ArrayKind::Shaped))
            && let Some(info) = self.container_type_metadata(&target)
            && info.value_type != "Any"
            && info.value_type != "Mu"
        {
            let raku_str = crate::builtins::methods_0arg::raku_repr::raku_value(&target);
            let type_prefix = if let Some(ref dt) = info.declared_type {
                dt.clone()
            } else {
                format!("array[{}]", info.value_type)
            };
            // Replace the "Array.new(" prefix with the typed prefix
            let result = if let Some(rest) = raku_str.strip_prefix("Array.new(") {
                format!("{}.new({}", type_prefix, rest)
            } else {
                raku_str
            };
            return Ok(Value::str(result));
        }
        // .raku/.perl on constrained regular Array (e.g. Array[Int])
        if matches!(method, "raku" | "perl")
            && args.is_empty()
            && matches!(&target, Value::Array(_, kind) if *kind != crate::value::ArrayKind::Shaped)
            && let Some(info) = self.container_type_metadata(&target)
            && info.value_type != "Any"
            && info.value_type != "Mu"
            && let Value::Array(items, _) = &target
        {
            let inner = items
                .iter()
                .map(crate::builtins::methods_0arg::raku_repr::raku_value)
                .collect::<Vec<_>>()
                .join(", ");
            let type_name = info
                .declared_type
                .unwrap_or_else(|| format!("Array[{}]", info.value_type));
            return Ok(Value::str(format!("{type_name}.new({inner})")));
        }

        // ACCEPTS for allomorphic types with Instance arguments
        // This handles custom classes with .Numeric/.Str methods that the
        // pure builtin cannot call.
        if method == "ACCEPTS"
            && matches!(&target, Value::Mixin(_, m) if m.contains_key("Str"))
            && args.len() == 1
            && matches!(&args[0], Value::Instance { .. })
            && let Value::Mixin(target_inner, _) = &target
        {
            // Call .Numeric on the instance to get its numeric value
            let numeric_val = self.call_method_with_values(args[0].clone(), "Numeric", vec![])?;
            // Compare numerically using == semantics
            let tf = match target_inner.as_ref() {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Complex(r, _) => *r,
                _ => 0.0,
            };
            let af = match &numeric_val {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Complex(r, _) => *r,
                _ => 0.0,
            };
            let result = (tf - af).abs() < 1e-15;
            return Ok(Value::Bool(result));
        }

        if let Some(result) = native_result {
            if method == "decode" {
                return result.map(|value| match value {
                    Value::Str(decoded) => Value::str(self.translate_newlines_for_decode(&decoded)),
                    other => other,
                });
            }
            return result;
        }

        // .pick/.roll/.grab/.grabpairs/.pickpairs with Callable arg on
        // Bag/BagHash/Set/SetHash/Mix/MixHash/Array/List/Range:
        // invoke the callable to get the actual count, then re-dispatch.
        // NOTE: Supply.grab takes a Callable that transforms values, so exclude Supplies.
        let is_supply_target = matches!(
            &target,
            Value::Instance { class_name, .. } if class_name == "Supply"
        ) || matches!(&target, Value::Package(name) if name == "Supply");
        if matches!(method, "pick" | "roll" | "grab" | "grabpairs" | "pickpairs")
            && args.len() == 1
            && args[0].as_sub().is_some()
            && !is_supply_target
        {
            let callable = args[0].clone();
            // For pick/grab, pass total; for pickpairs/grabpairs, pass elems
            let input = match method {
                "pickpairs" | "grabpairs" => {
                    // .elems
                    let method_sym = crate::symbol::Symbol::intern("elems");
                    crate::builtins::native_method_0arg(&target, method_sym)
                        .unwrap_or(Ok(Value::Int(0)))?
                }
                _ => {
                    // .total
                    let method_sym = crate::symbol::Symbol::intern("total");
                    crate::builtins::native_method_0arg(&target, method_sym)
                        .unwrap_or(Ok(Value::Int(0)))?
                }
            };
            let count = self.call_sub_value(callable, vec![input], false)?;
            // Convert to Int
            let count_int = match &count {
                Value::Int(n) => Value::Int(*n),
                Value::Num(f) => Value::Int(*f as i64),
                Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                other => {
                    let method_sym = crate::symbol::Symbol::intern("Int");
                    crate::builtins::native_method_0arg(other, method_sym)
                        .unwrap_or(Ok(count.clone()))?
                }
            };
            return self.call_method_with_values(target, method, vec![count_int]);
        }

        // Comprehensive split handler
        if method == "split"
            && !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
        {
            return self.handle_split_method(target, args);
        }

        // .of on Array/Hash
        if method == "of" && args.is_empty() && matches!(&target, Value::Array(..) | Value::Hash(_))
        {
            if let Some(info) = self.container_type_metadata(&target) {
                return Ok(Value::Package(Symbol::intern(&info.value_type)));
            }
            return Ok(Value::Package(Symbol::intern("Mu")));
        }

        // .keyof on Hash
        if method == "keyof" && args.is_empty() && matches!(&target, Value::Hash(_)) {
            if let Some(info) = self.container_type_metadata(&target)
                && let Some(ref key_type) = info.key_type
            {
                return Ok(Value::Package(Symbol::intern(key_type)));
            }
            return Ok(Value::Package(Symbol::intern("Str(Any)")));
        }

        // Complex->Num conversion
        if method == "Num"
            && args.is_empty()
            && let Value::Complex(r, im) = &target
        {
            return self.dispatch_complex_to_num(*r, *im, &target);
        }

        // Zero-denominator Rat/FatRat .Str
        if matches!(method, "Str" | "gist")
            && args.is_empty()
            && matches!(&target, Value::Rat(_, 0) | Value::FatRat(_, 0))
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str_from("Attempt to divide by zero when coercing Rational to Str"),
            );
            let ex = Value::make_instance(Symbol::intern("X::Numeric::DivideByZero"), attrs);
            let mut err =
                RuntimeError::new("Attempt to divide by zero when coercing Rational to Str");
            err.exception = Some(Box::new(ex));
            return Err(err);
        }

        // Force LazyList and re-dispatch as Seq
        if let Value::LazyList(ll) = &target
            && Self::should_force_lazy_list(method)
        {
            let saved_env = self.env.clone();
            let items = self.force_lazy_list_bridge(ll)?;
            if !matches!(method, "elems" | "hyper" | "race") {
                self.env = saved_env;
            }
            let seq = Value::Seq(std::sync::Arc::new(items));
            return self.call_method_with_values(seq, method, args);
        }

        // Callable introspection
        if matches!(
            &target,
            Value::Routine { .. } | Value::Sub(_) | Value::WeakSub(_)
        ) && let Some(result) = self.dispatch_callable_method(&target, method, &args)
        {
            return result;
        }

        // .VAR on VarRef
        if method == "VAR"
            && args.is_empty()
            && let Some((source_name, inner)) = Self::varref_parts(&target)
        {
            return self.call_method_mut_with_values(&source_name, inner, "VAR", vec![]);
        }

        // .var on meta value
        if method == "var"
            && args.is_empty()
            && let Some(source_name) = Self::var_target_from_meta_value(&target)
        {
            let source_value = self.env.get(&source_name).cloned().unwrap_or(Value::Nil);
            let mut named = std::collections::HashMap::new();
            named.insert("__mutsu_varref_name".to_string(), Value::str(source_name));
            named.insert("__mutsu_varref_value".to_string(), source_value);
            return Ok(Value::Capture {
                positional: Vec::new(),
                named,
            });
        }

        // .join on LazyList
        if method == "join"
            && let Value::LazyList(list) = &target
        {
            let items = self.force_lazy_list_bridge(list)?;
            return self.call_method_with_values(Value::real_array(items), method, args);
        }

        // ^meta_method dispatch (except ^name)
        if let Some(meta_method) = method.strip_prefix('^')
            && meta_method != "name"
        {
            let how = self.call_method_with_values(target.clone(), "HOW", vec![])?;
            let mut how_args = Vec::with_capacity(args.len() + 1);
            how_args.push(target.clone());
            how_args.extend(args.clone());
            return self.call_method_with_values(how, meta_method, how_args);
        }

        // ClassHOW/SubsetHOW/EnumHOW method dispatch
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && Self::is_metamodel_how(class_name)
            && Self::is_classhow_method(method)
        {
            let mut how_args = args.to_vec();
            if !matches!(
                how_args.first(),
                Some(Value::Package(_))
                    | Some(Value::Instance { .. })
                    | Some(Value::ParametricRole { .. })
                    | Some(Value::Mixin(_, _))
            ) && let Some(Value::Str(type_name)) = attributes.get("name")
            {
                how_args.insert(0, Value::Package(Symbol::intern(type_name)));
            }
            return self.dispatch_classhow_method(method, how_args);
        }

        // Archetypes.composable
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Perl6::Metamodel::Archetypes"
            && method == "composable"
            && args.is_empty()
        {
            return Ok(attributes
                .get("composable")
                .cloned()
                .unwrap_or(Value::Bool(false)));
        }

        // CREATE method
        if method == "CREATE"
            && args.is_empty()
            && let Some(result) = self.dispatch_create(&target)
        {
            return result;
        }

        // Type-object coercion
        if method == "coerce"
            && args.len() == 1
            && let Value::Package(type_name) = &target
        {
            return self.try_coerce_value_for_constraint(&type_name.resolve(), args[0].clone());
        }

        // Custom type method dispatch
        if !matches!(
            method,
            "HOW"
                | "WHAT"
                | "WHO"
                | "WHY"
                | "WHICH"
                | "WHERE"
                | "DEFINITE"
                | "VAR"
                | "REPR"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "say"
                | "print"
                | "put"
                | "note"
                | "new"
        ) && let Value::CustomType { ref how, .. } | Value::CustomTypeInstance { ref how, .. } =
            target
        {
            let how_clone = *how.clone();
            let found = self.call_method_with_values(
                how_clone,
                "find_method",
                vec![target.clone(), Value::str(method.to_string())],
            );
            if let Ok(callable) = found
                && !matches!(callable, Value::Nil)
            {
                return self.eval_call_on_value(callable, vec![target.clone()]);
            }
        }

        if method == "leave" {
            return self.builtin_leave_method(target, &args);
        }

        // Enum.new error check
        if method == "new"
            && let Some(result) = self.dispatch_enum_new_check(&target, &args)
        {
            return result;
        }

        // Trig methods on Instance values
        if let Some(result) =
            self.dispatch_trig_instance_method(target.clone(), method, args.clone())
        {
            return result;
        }

        // Primary method dispatch by name (group 1: string, IO, coercion, misc)
        if let Some(result) = self.dispatch_method_by_name_1(target.clone(), method, args.clone()) {
            return result;
        }

        // Primary method dispatch by name (group 2: collection/iteration)
        if let Some(result) = self.dispatch_method_by_name_2(target.clone(), method, args.clone()) {
            return result;
        }

        // Primary method dispatch by name (group 3: Supply, network, temporal, misc)
        if let Some(result) = self.dispatch_method_by_name_3(target.clone(), method, args.clone()) {
            return result;
        }

        // Constructor dispatch (new, bless, Mu::new, new-from-pairs)
        if matches!(
            method,
            "new" | "bless" | "Mu::new" | "handled" | "new-from-pairs"
        ) && let Some(result) = self.dispatch_new_and_constructors(&target, method, args.clone())
        {
            return result;
        }

        // Enum dispatch
        if let Some(result) = self.dispatch_enum_method(&target, method, &args) {
            return result;
        }

        // SharedPromise dispatch
        if let Value::Promise(ref shared) = target {
            return self.dispatch_promise_method(shared, method, args, &target);
        }

        // SharedChannel dispatch
        if let Value::Channel(ref ch) = target {
            return self.dispatch_channel_method(ch, method, args);
        }

        // Promise::Vow forwards keep/break
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name.resolve() == "Promise::Vow"
        {
            return self.dispatch_promise_vow_method(attributes, method, args);
        }

        // Mixin fallback: check __mutsu_attr__ and delegate to inner
        if let Value::Mixin(inner, mixins) = &target {
            if args.is_empty() {
                let attr_key = format!("__mutsu_attr__{}", method);
                if let Some(value) = mixins.get(&attr_key) {
                    return Ok(value.clone());
                }
            }
            return self.call_method_with_values(inner.as_ref().clone(), method, args);
        }

        // Instance dispatch, package dispatch, and fallback paths
        self.dispatch_instance_and_fallback(target, method, args)
    }
}
