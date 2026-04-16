use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch "new", "bless", and related constructor methods.
    /// Returns Some(result) if the method was handled, None to fall through.
    pub(super) fn dispatch_new_and_constructors(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "new" if matches!(target, Value::Package(name) if matches!(name.resolve().as_str(), "ObjAt" | "ValueObjAt")) =>
            {
                let class_name = if let Value::Package(n) = target {
                    n.resolve()
                } else {
                    unreachable!()
                };
                // Find the first non-Pair positional argument (Pairs are named args)
                let positional = args
                    .iter()
                    .find(|a| !matches!(a, Value::Pair(_, _) | Value::ValuePair(_, _)));
                match positional {
                    Some(val) => {
                        let which_str = val.to_string_value();
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("WHICH".to_string(), Value::str(which_str));
                        Some(Ok(Value::make_instance(Symbol::intern(&class_name), attrs)))
                    }
                    None => Some(Err(RuntimeError::new(
                        "Too few positionals passed; expected 2 arguments but got 1".to_string(),
                    ))),
                }
            }
            "new" if matches!(target, Value::Package(name) if matches!(name.resolve().as_str(), "IntStr" | "NumStr" | "RatStr" | "ComplexStr")) =>
            {
                let type_name = if let Value::Package(n) = target {
                    n.resolve()
                } else {
                    unreachable!()
                };
                if args.len() < 2 {
                    return Some(Err(RuntimeError::new(format!(
                        "{}.new requires two arguments (numeric, string)",
                        type_name
                    ))));
                }
                let numeric = args[0].clone();
                let string = args[1].to_string_value();
                let mut mixins = std::collections::HashMap::new();
                mixins.insert("Str".to_string(), Value::str(string));
                Some(Ok(Value::mixin(numeric, mixins)))
            }
            "new" if matches!(target, Value::Package(name) if name == "Failure") => {
                let default_exception = || {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str("Failed".to_string()));
                    Value::make_instance(Symbol::intern("Exception"), attrs)
                };
                let raw_exception = args
                    .first()
                    .cloned()
                    .filter(|v| !matches!(v, Value::Nil))
                    .or_else(|| {
                        self.env
                            .get("!")
                            .cloned()
                            .filter(|v| !matches!(v, Value::Nil))
                    })
                    .unwrap_or_else(default_exception);
                // Wrap non-Exception values in X::AdHoc (Raku behavior)
                let exception = if let Value::Instance { class_name, .. } = &raw_exception {
                    let cn = class_name.resolve();
                    if cn == "Exception"
                        || cn.starts_with("X::")
                        || cn.starts_with("CX::")
                        || self.mro_readonly(&cn).iter().any(|p| {
                            p == "Exception" || p.starts_with("X::") || p.starts_with("CX::")
                        })
                    {
                        raw_exception
                    } else {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("payload".to_string(), raw_exception.clone());
                        attrs.insert(
                            "message".to_string(),
                            Value::str(raw_exception.to_string_value()),
                        );
                        Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
                    }
                } else {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("payload".to_string(), raw_exception.clone());
                    attrs.insert(
                        "message".to_string(),
                        Value::str(raw_exception.to_string_value()),
                    );
                    Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
                };
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("exception".to_string(), exception);
                attrs.insert("handled".to_string(), Value::Bool(false));
                Some(Ok(Value::make_instance(Symbol::intern("Failure"), attrs)))
            }
            "handled" if matches!(target, Value::Instance { class_name, .. } if class_name == "Failure") =>
            {
                if args.is_empty() {
                    // Read the handled state
                    Some(Ok(Value::Bool(target.is_failure_handled())))
                } else if args.len() == 1 {
                    // Set the handled state: $failure.handled = value
                    let val = args[0].truthy();
                    if let Value::Instance { id, .. } = target {
                        crate::value::set_failure_handled(*id, val);
                    }
                    Some(Ok(Value::Bool(val)))
                } else {
                    None
                }
            }
            "new-from-pairs" => Some(self.dispatch_new_from_pairs(target, args)),
            "new" => Some(self.dispatch_new(target.clone(), args)),
            // self.Mu::new(...)  –  qualified call to Mu's default constructor.
            "Mu::new" => Some(self.call_method_with_values(target.clone(), "bless", args)),
            "bless" => Some(self.dispatch_bless(target, args)),
            _ => None,
        }
    }

    /// Handle new-from-pairs: creates a Set/Bag/Mix from Pair arguments.
    /// Pair keys become element keys, Pair values become weights/counts.
    /// Duplicate keys have weights summed. Zero-weight entries are removed.
    fn dispatch_new_from_pairs(
        &mut self,
        target: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let type_name = match target {
            Value::Package(name) => name.resolve(),
            _ => {
                return Err(RuntimeError::new(
                    "new-from-pairs can only be called on a type object",
                ));
            }
        };
        // Check for lazy iterables
        for arg in &args {
            if Self::is_lazy_for_set_ops(arg) {
                let what = type_name.split('[').next().unwrap_or(&type_name);
                let mut err = RuntimeError::new(format!("Cannot .{} a lazy list", what));
                err.exception = Some(Box::new(Value::make_instance(
                    Symbol::intern("X::Cannot::Lazy"),
                    [("what".to_string(), Value::str(what.to_string()))]
                        .into_iter()
                        .collect(),
                )));
                return Err(err);
            }
        }
        // Flatten a single list argument
        let items: Vec<Value> = if args.len() == 1 {
            Self::value_to_list(&args[0])
        } else {
            args
        };
        match type_name.as_str() {
            "Mix" | "MixHash" => {
                let mut weights: HashMap<String, f64> = HashMap::new();
                for item in &items {
                    let (key, weight) = match item {
                        Value::Pair(k, v) => (k.clone(), Self::mix_pair_weight(v)?),
                        Value::ValuePair(k, v) => (k.to_string_value(), Self::mix_pair_weight(v)?),
                        other => (other.to_string_value(), 1.0),
                    };
                    *weights.entry(key).or_insert(0.0) += weight;
                }
                Ok(if type_name == "MixHash" {
                    Value::mix_hash(weights)
                } else {
                    Value::mix(weights)
                })
            }
            "Bag" | "BagHash" => {
                let mut counts: HashMap<String, i64> = HashMap::new();
                for item in &items {
                    let (key, count) = match item {
                        Value::Pair(k, v) => {
                            let c = match v.as_ref() {
                                Value::Int(i) => *i,
                                Value::Num(n) => *n as i64,
                                _ => 1,
                            };
                            (k.clone(), c)
                        }
                        Value::ValuePair(k, v) => {
                            let c = match v.as_ref() {
                                Value::Int(i) => *i,
                                Value::Num(n) => *n as i64,
                                _ => 1,
                            };
                            (k.to_string_value(), c)
                        }
                        other => (other.to_string_value(), 1),
                    };
                    *counts.entry(key).or_insert(0) += count;
                }
                // Remove zero-count entries
                counts.retain(|_, v| *v > 0);
                Ok(if type_name == "BagHash" {
                    Value::bag_hash(counts)
                } else {
                    Value::bag(counts)
                })
            }
            "Set" | "SetHash" => {
                let mut elems = std::collections::HashSet::new();
                for item in &items {
                    match item {
                        Value::Pair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.clone());
                            }
                        }
                        Value::ValuePair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.to_string_value());
                            }
                        }
                        Value::Hash(h) => {
                            for (k, v) in h.iter() {
                                if v.truthy() {
                                    elems.insert(k.clone());
                                }
                            }
                        }
                        other => {
                            elems.insert(other.to_string_value());
                        }
                    }
                }
                Ok(if type_name == "SetHash" {
                    Value::set_hash(elems)
                } else {
                    Value::set(elems)
                })
            }
            _ => Err(RuntimeError::new(format!(
                "new-from-pairs not supported on {}",
                type_name
            ))),
        }
    }

    /// Handle the "bless" method: creates a new instance with attributes from named args.
    fn dispatch_bless(&mut self, target: &Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        // self.bless(:attr1($val1), :attr2($val2), ...)
        // Creates a new instance of the invocant's class with attributes from named args
        let class_name = match target {
            Value::Package(name) => *name,
            Value::Instance { class_name, .. } => *class_name,
            _ => {
                return Err(RuntimeError::new(
                    "bless can only be called on a class or instance",
                ));
            }
        };
        if matches!(
            class_name.resolve().as_str(),
            "Sub" | "Routine" | "Method" | "Code" | "Block"
        ) {
            return Err(RuntimeError::new(
                "getcodename requires a concrete code object",
            ));
        }
        // Initialize with default attribute values
        let mut attributes = HashMap::new();
        if self.classes.contains_key(&class_name.resolve()) {
            for (attr_name, _is_public, default, _is_rw, _, _, _) in
                self.collect_class_attributes(&class_name.resolve())
            {
                let val = if let Some(expr) = default {
                    self.eval_block_value(&[Stmt::Expr(expr)])?
                } else {
                    // Native types have zero/empty defaults instead of Nil
                    let type_constraint =
                        self.get_attr_type_constraint(&class_name.resolve(), &attr_name);
                    match type_constraint.as_deref() {
                        Some(
                            "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8"
                            | "uint16" | "uint32" | "uint64" | "byte" | "atomicint",
                        ) => Value::Int(0),
                        Some("num" | "num32" | "num64") => Value::Num(0.0),
                        Some("str") => Value::str("".to_string()),
                        _ => Value::Nil,
                    }
                };
                attributes.insert(attr_name, val);
            }
        }
        // Build a map of attribute sigils for proper container semantics
        let attr_sigils: HashMap<String, char> = if self.classes.contains_key(&class_name.resolve())
        {
            self.collect_class_attributes(&class_name.resolve())
                .into_iter()
                .map(|(name, _, _, _, _, sigil, _)| (name, sigil))
                .collect()
        } else {
            HashMap::new()
        };
        // Override with named args from bless call
        for arg in &args {
            if let Value::Pair(key, value) = arg {
                let sigil = attr_sigils.get(key.as_str()).copied().unwrap_or('$');
                let coerced = Self::coerce_attr_value_by_sigil(*value.clone(), sigil);
                attributes.insert(key.clone(), coerced);
            }
        }
        // Run BUILD/TWEAK submethods in MRO order (base-first)
        let mro = self.class_mro(&class_name.resolve());
        let is_6e = crate::parser::current_language_version().starts_with("6.e");
        for mro_class in mro.iter().rev() {
            if mro_class == "Any" || mro_class == "Mu" {
                continue;
            }
            // Skip role entries in MRO
            if self.roles.contains_key(mro_class) && !self.classes.contains_key(mro_class) {
                continue;
            }
            // Under v6.e+, call BUILD submethods from composed roles first
            if is_6e {
                let role_order = self.ordered_role_submethods_for_class(mro_class, "BUILD");
                for (role_name, method_def) in role_order {
                    let (_v, updated) = self.run_instance_method_resolved(
                        &class_name.resolve(),
                        &role_name,
                        method_def,
                        attributes.clone(),
                        Vec::new(),
                        Some(Value::make_instance(class_name, attributes.clone())),
                    )?;
                    attributes = updated;
                }
            }
            let has_build = self
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("BUILD"))
                .is_some();
            if has_build {
                let (_v, updated) = self.run_instance_method(
                    mro_class,
                    attributes.clone(),
                    "BUILD",
                    Vec::new(),
                    Some(Value::make_instance(class_name, attributes.clone())),
                )?;
                attributes = updated;
            }
        }
        for mro_class in mro.iter().rev() {
            if mro_class == "Any" || mro_class == "Mu" {
                continue;
            }
            // Skip role entries in MRO
            if self.roles.contains_key(mro_class) && !self.classes.contains_key(mro_class) {
                continue;
            }
            // Under v6.e+, call TWEAK submethods from composed roles first
            if is_6e {
                let role_order = self.ordered_role_submethods_for_class(mro_class, "TWEAK");
                for (role_name, method_def) in role_order {
                    let (_v, updated) = self.run_instance_method_resolved(
                        &class_name.resolve(),
                        &role_name,
                        method_def,
                        attributes.clone(),
                        Vec::new(),
                        Some(Value::make_instance(class_name, attributes.clone())),
                    )?;
                    attributes = updated;
                }
            }
            let has_tweak = self
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("TWEAK"))
                .is_some();
            if has_tweak {
                let (_v, updated) = self.run_instance_method(
                    mro_class,
                    attributes.clone(),
                    "TWEAK",
                    Vec::new(),
                    Some(Value::make_instance(class_name, attributes.clone())),
                )?;
                attributes = updated;
            }
        }
        Ok(Value::make_instance(class_name, attributes))
    }

    /// Dispatch Enum .new — should throw X::Constructor::BadType.
    /// Returns Some(err) if target is an enum, None otherwise.
    pub(super) fn dispatch_enum_new_check(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = args;
        let enum_name = match target {
            Value::Package(name) if name == "Bool" => Some("Bool".to_string()),
            Value::Bool(_) => Some("Bool".to_string()),
            Value::Package(name) if self.enum_types.contains_key(&name.resolve()) => {
                Some(name.resolve())
            }
            Value::Enum { enum_type, .. } => Some(enum_type.resolve()),
            _ => None,
        };
        if let Some(ename) = enum_name {
            let msg = format!(
                "Enum '{}' is insufficiently type-like to be instantiated.  Did you mean 'class'?",
                ename
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Constructor::BadType"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Some(Err(err));
        }
        None
    }

    /// Dispatch the CREATE method for CustomType and Package targets.
    pub(super) fn dispatch_create(&self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        match target {
            Value::CustomType {
                how,
                repr,
                name,
                id,
                ..
            } => Some(Ok(Value::CustomTypeInstance {
                type_id: *id,
                how: how.clone(),
                repr: repr.clone(),
                type_name: *name,
                attributes: std::sync::Arc::new(HashMap::new()),
                id: crate::value::next_instance_id(),
            })),
            Value::Package(class_name) => {
                Some(Ok(Value::make_instance(*class_name, HashMap::new())))
            }
            _ => None,
        }
    }

    /// Dispatch the "now" method for DateTime subclasses.
    pub(super) fn dispatch_datetime_now(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let Value::Package(class_name) = target else {
            return None;
        };
        if !self
            .class_mro(&class_name.resolve())
            .iter()
            .any(|name| name == "DateTime")
        {
            return None;
        }
        use crate::builtins::methods_0arg::temporal;
        let mut timezone = 0i64;
        let mut formatter: Option<Value> = None;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "timezone" => timezone = value.to_f64() as i64,
                    "formatter" => formatter = Some(*value.clone()),
                    _ => {}
                }
            }
        }
        let secs = crate::value::current_time_secs_f64() + timezone as f64;
        let total_i = secs.floor() as i64;
        let frac = secs - total_i as f64;
        let day_secs = total_i.rem_euclid(86400);
        let epoch_days = (total_i - day_secs) / 86400;
        let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
        let h = day_secs / 3600;
        let mi = (day_secs % 3600) / 60;
        let s = (day_secs % 60) as f64 + frac;
        let dt = temporal::make_datetime(y, m, d, h, mi, s, timezone);
        if let Some(formatter_value) = formatter
            && let Value::Instance {
                class_name,
                ref attributes,
                id,
            } = dt
        {
            let mut attrs = (**attributes).clone();
            attrs.insert("formatter".to_string(), formatter_value.clone());
            let dt_with_formatter = Value::make_instance_with_id(class_name, attrs, id);
            let saved_env = self.env().clone();
            let saved_readonly = self.save_readonly_vars();
            let rendered =
                match self.eval_call_on_value(formatter_value, vec![dt_with_formatter.clone()]) {
                    Ok(v) => v.to_string_value(),
                    Err(e) => return Some(Err(e)),
                };
            *self.env_mut() = saved_env;
            self.restore_readonly_vars(saved_readonly);
            if let Value::Instance {
                class_name,
                attributes,
                id,
            } = dt_with_formatter
            {
                let mut updated = (*attributes).clone();
                updated.insert("__formatter_rendered".to_string(), Value::str(rendered));
                return Some(Ok(Value::make_instance_with_id(class_name, updated, id)));
            }
        }
        if class_name.resolve() != "DateTime" {
            return Some(self.dispatch_new(target.clone(), vec![dt]));
        }
        Some(Ok(dt))
    }
}
