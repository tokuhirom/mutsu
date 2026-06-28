use super::vm_misc_ops::*;
use super::*;

impl Interpreter {
    pub(super) fn exec_type_check_op_inner(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
        bind_mode: bool,
    ) -> Result<(), RuntimeError> {
        let raw_constraint = Self::const_str(code, tc_idx);
        let var_name: Option<&str> = var_name_idx.map(|idx| Self::const_str(code, idx));
        // Apply `use variables :D/:U` pragma to the constraint
        let effective_constraint = loan_env!(self, apply_variables_pragma(raw_constraint));
        let constraint: &str = &effective_constraint;
        let (base_constraint, _) = crate::runtime::types::strip_type_smiley(constraint);
        let declared_constraint = base_constraint
            .split_once('(')
            .map_or(base_constraint, |(target, _)| target);
        let value = self.stack.last().expect("TypeCheck: empty stack").clone();
        if var_name.is_some_and(|name| name.starts_with('%')) {
            return Ok(());
        }
        // Lazy values cannot be stored in native typed arrays
        if var_name.is_some_and(|name| name.starts_with('@'))
            && crate::runtime::native_types::is_native_array_element_type(base_constraint)
            && crate::builtins::methods_0arg::is_value_lazy(&value)
        {
            let declared = format!("array[{}]", base_constraint);
            return Err(RuntimeError::typed(
                "X::Cannot::Lazy",
                [
                    (
                        "message".to_string(),
                        Value::str(format!("Cannot store a lazy list onto a {}", declared)),
                    ),
                    ("action".to_string(), Value::str_from("store")),
                ]
                .into_iter()
                .collect(),
            ));
        }
        if var_name.is_some_and(|name| name.starts_with('@'))
            && matches!(&value, Value::Array(..) | Value::Seq(_))
        {
            if !self.array_elements_match_constraint(constraint, &value) {
                return Err(self.typed_array_element_error(
                    var_name,
                    base_constraint,
                    constraint,
                    &value,
                ));
            }
            return Ok(());
        }
        // When the constraint is a container type (List, Array, Positional, Seq, Cool, Any, Mu),
        // an Array value directly satisfies it — do NOT descend into element-level matching.
        // Element-level matching is for declarations like `my Int @x = 1, 2, 3`.
        // Also skip element-level matching for subset types whose base type is a container type
        // (e.g., `subset NumArray of Array where { ... }`).
        let is_container_constraint = matches!(
            declared_constraint,
            "List" | "Array" | "Positional" | "Seq" | "Cool" | "Any" | "Mu" | "Iterable"
        ) || {
            let ultimate_base = self.resolve_subset_base_type(declared_constraint);
            matches!(
                ultimate_base.as_str(),
                "List"
                    | "Array"
                    | "Positional"
                    | "Seq"
                    | "Cool"
                    | "Any"
                    | "Mu"
                    | "Iterable"
                    | "Hash"
                    | "Map"
                    | "Pair"
            )
        };
        if let Value::Array(..) = &value
            && !is_container_constraint
            // Element-level matching is for `@`-sigil typed arrays (`my Int @a`),
            // whose constraint is the ELEMENT type. A `$`-scalar holding an array
            // (`my Array[Numeric] $x = …`, `my Array[Numeric] constant c .= new`)
            // matches the WHOLE value against the (parameterized) type, not its
            // elements — fall through to the whole-value type check below.
            && !var_name.is_some_and(|n| n.starts_with('$'))
        {
            if !self.array_elements_match_constraint(constraint, &value) {
                return Err(self.typed_array_element_error(
                    var_name,
                    base_constraint,
                    constraint,
                    &value,
                ));
            }
            return Ok(());
        }
        // For finite Range values assigned to typed arrays (my Int @a = ^5),
        // check if the range elements match the element type constraint.
        // Integer ranges always contain Int elements.
        {
            let is_finite_int_range = matches!(
                &value,
                Value::Range(a, b) | Value::RangeExcl(a, b) |
                Value::RangeExclStart(a, b) | Value::RangeExclBoth(a, b)
                if *b != i64::MAX && *a != i64::MIN
            );
            let is_finite_generic_range = matches!(
                &value,
                Value::GenericRange { end, .. }
                if !matches!(end.as_ref(), Value::Num(n) if n.is_infinite())
                    && !matches!(end.as_ref(), Value::Whatever | Value::HyperWhatever)
            );
            let range_ok = if is_finite_int_range {
                loan_env!(self, type_matches_value(constraint, &Value::Int(0)))
            } else if is_finite_generic_range {
                match &value {
                    Value::GenericRange { start, end, .. } => {
                        self.type_matches_value(constraint, start)
                            && self.type_matches_value(constraint, end)
                    }
                    _ => false,
                }
            } else {
                false
            };
            if range_ok {
                return Ok(());
            }
            // Infinite ranges and non-matching types fall through to normal check
        }
        if matches!(value, Value::Nil) && self.is_definite_constraint(constraint) {
            return Err(RuntimeError::new(format!(
                "X::Syntax::Variable::MissingInitializer: Variable definition of type {} needs to be given an initializer",
                constraint
            )));
        }
        // Native integer type check: validate value is an integer in range.
        // Native types cannot hold Nil/type objects — reject them.
        if crate::runtime::native_types::is_native_int_type(base_constraint) {
            if matches!(value, Value::Nil) {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox a type object (Nil) to {}.",
                    base_constraint
                )));
            }
            self.validate_native_int_assignment(base_constraint, &value)?;
            return Ok(());
        }
        // Native num/str types cannot hold type objects — reject Nil and Package values.
        if matches!(base_constraint, "num" | "num32" | "num64" | "str") {
            if matches!(value, Value::Nil | Value::Package(_)) {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox a type object to {}.",
                    base_constraint
                )));
            }
            return Ok(());
        }
        if runtime::is_known_type_constraint(base_constraint) {
            if !matches!(value, Value::Nil) && !self.type_matches_value(constraint, &value) {
                if base_constraint == "Int"
                    && matches!(value, Value::Num(f) if f.is_nan() || f.is_infinite())
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("value".to_string(), value.clone());
                    attrs.insert(
                        "vartype".to_string(),
                        Value::Package(Symbol::intern(base_constraint)),
                    );
                    let desc = if matches!(value, Value::Num(f) if f.is_nan()) {
                        "Cannot convert NaN to Int"
                    } else {
                        "Cannot assign a literal of type Num (Inf) to a variable of type Int"
                    };
                    attrs.insert("message".to_string(), Value::str(desc.to_string()));
                    return Err(RuntimeError::typed("X::Syntax::Number::LiteralType", attrs));
                }
                let coerced = match base_constraint {
                    "Str" => Some(Value::str(crate::runtime::utils::coerce_to_str(&value))),
                    _ => None,
                };
                if let Some(new_val) = coerced {
                    *self.stack.last_mut().unwrap() = new_val;
                } else {
                    // When assigning an unhandled Failure to a typed variable,
                    // explode the Failure first (Raku behavior)
                    if let Some(err) = self.failure_to_runtime_error_if_unhandled(&value) {
                        return Err(err);
                    }
                    if bind_mode {
                        return Err(crate::runtime::utils::type_check_binding_typed_error(
                            base_constraint,
                            &value,
                        ));
                    }
                    return Err(RuntimeError::typecheck_assignment(
                        base_constraint,
                        &value,
                        var_name,
                    ));
                }
            }
        } else if !self.has_type(declared_constraint)
            && !is_core_raku_type(declared_constraint)
            && !loan_env!(self, has_type_capture_binding(declared_constraint))
        {
            // Check if this is a suppressed nested class name that can be resolved
            if self.resolve_suppressed_type(declared_constraint).is_none() {
                // A `package`/`module` declared with this name exists but is not
                // type-like enough to constrain a variable (only `class`/`role`/
                // `enum`/`subset` are): throw X::Syntax::Variable::BadType, not the
                // generic "not declared" error.
                if self.is_declared_package(declared_constraint) {
                    let msg = format!(
                        "Package '{}' is insufficiently type-like to qualify a variable.  Did you mean 'class'?",
                        constraint
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("type".to_string(), Value::str(constraint.to_string()));
                    attrs.insert("message".to_string(), Value::str(msg));
                    return Err(RuntimeError::typed("X::Syntax::Variable::BadType", attrs));
                }
                // Unknown user-defined type — reject it.
                // In Raku this is a compile-time failure grouped into an
                // X::Comp::Group whose `.sorrows` holds the underlying
                // X::Undeclared (with type "Did you mean" suggestions).
                let suggestions = self.suggest_type_names(constraint);
                let mut undecl_msg = format!("Type '{}' is not declared.", constraint);
                if suggestions.len() == 1 {
                    undecl_msg.push_str(&format!(" Did you mean '{}'?", suggestions[0]));
                } else if suggestions.len() > 1 {
                    let quoted: Vec<String> =
                        suggestions.iter().map(|s| format!("'{}'", s)).collect();
                    undecl_msg.push_str(&format!(
                        " Did you mean any of these: {}?",
                        quoted.join(", ")
                    ));
                }
                let mut undecl_attrs = std::collections::HashMap::new();
                undecl_attrs.insert("what".to_string(), Value::str("type".to_string()));
                undecl_attrs.insert("symbol".to_string(), Value::str(constraint.to_string()));
                undecl_attrs.insert(
                    "suggestions".to_string(),
                    Value::array(suggestions.iter().cloned().map(Value::str).collect()),
                );
                undecl_attrs.insert("message".to_string(), Value::str(undecl_msg.clone()));
                let sorrow = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Undeclared"),
                    undecl_attrs,
                );
                // Group message mirrors Raku: the sorrow message plus "Malformed my".
                let group_msg = format!("{}\nMalformed my", undecl_msg);
                let mut group_attrs = std::collections::HashMap::new();
                group_attrs.insert("sorrows".to_string(), Value::array(vec![sorrow]));
                group_attrs.insert("worries".to_string(), Value::array(vec![]));
                group_attrs.insert("panic".to_string(), Value::Nil);
                group_attrs.insert("message".to_string(), Value::str(group_msg));
                return Err(RuntimeError::typed("X::Comp::Group", group_attrs));
            }
        }
        if !matches!(value, Value::Nil)
            && !self.type_matches_value(constraint, &value)
            && !self.is_container_subclass(constraint)
        {
            if bind_mode {
                return Err(crate::runtime::utils::type_check_binding_typed_error(
                    constraint, &value,
                ));
            }
            return Err(RuntimeError::typecheck_assignment(
                constraint, &value, var_name,
            ));
        }
        if !matches!(value, Value::Nil) {
            let coerced = loan_env!(
                self,
                try_coerce_value_for_constraint(constraint, value.clone())
            )?;
            *self.stack.last_mut().unwrap() = coerced;
        }
        Ok(())
    }

    pub(super) fn exec_indirect_type_lookup_op(&mut self) {
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        self.stack
            .push(loan_env!(self, resolve_indirect_type_name(&name)));
    }
}
