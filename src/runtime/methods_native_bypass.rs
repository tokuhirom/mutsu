use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Check if a metamodel class name is a HOW type.
    pub(super) fn is_metamodel_how(class_name: &Symbol) -> bool {
        let cn = class_name.resolve();
        cn == "Perl6::Metamodel::ClassHOW"
            || cn == "Perl6::Metamodel::SubsetHOW"
            || cn == "Perl6::Metamodel::EnumHOW"
            || cn == "Perl6::Metamodel::CurriedRoleHOW"
            || cn == "Perl6::Metamodel::ParametricRoleGroupHOW"
            || cn == "Perl6::Metamodel::CoercionHOW"
    }

    /// Check if a method name is a ClassHOW method.
    pub(super) fn is_classhow_method(method: &str) -> bool {
        matches!(
            method,
            "can"
                | "does"
                | "isa"
                | "lookup"
                | "find_method"
                | "add_attribute"
                | "add_method"
                | "add_multi_method"
                | "compose"
                | "archetypes"
                | "name"
                | "ver"
                | "auth"
                | "mro"
                | "mro_unhidden"
                | "methods"
                | "attributes"
                | "parents"
                | "roles"
                | "candidates"
                | "concretization"
                | "curried_role"
                | "enum_value_list"
                | "coerce"
                | "pun"
                | "language-revision"
                | "submethod_table"
        )
    }

    /// Check if a method on LazyList should force evaluation.
    pub(super) fn should_force_lazy_list(method: &str) -> bool {
        matches!(
            method,
            "list"
                | "Array"
                | "Numeric"
                | "Int"
                | "elems"
                | "hyper"
                | "race"
                | "first"
                | "grep"
                | "map"
                | "sort"
                | "reverse"
                | "join"
                | "head"
                | "tail"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "flat"
                | "unique"
                | "repeated"
                | "squish"
                | "classify"
                | "categorize"
                | "produce"
                | "rotor"
                | "batch"
                | "reduce"
                | "combinations"
                | "permutations"
                | "values"
                | "List"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "Seq"
                | "item"
                | "cache"
                | "pick"
                | "roll"
                | "keys"
                | "kv"
                | "pairs"
                | "antipairs"
        )
    }

    /// Determine whether to bypass the native method fast path.
    pub(super) fn should_bypass_native_fastpath(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
        skip_pseudo: bool,
        is_pseudo_method: bool,
    ) -> bool {
        let _ = args;
        skip_pseudo
            || method == "squish"
            || (matches!(
                method,
                "max"
                    | "min"
                    | "head"
                    | "flat"
                    | "sort"
                    | "comb"
                    | "words"
                    | "batch"
                    | "rotor"
                    | "rotate"
                    | "produce"
                    | "snip"
                    | "minmax"
                    | "start"
                    | "wait"
                    | "zip"
                    | "zip-latest"
            ) && matches!(target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "elems" && matches!(target, Value::Instance { .. }))
            || (matches!(method, "list" | "Array" | "Seq")
                && matches!(target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "Supply"
                && matches!(target, Value::Instance { class_name, .. }
                    if class_name == "Supplier" || class_name == "Supplier::Preserving"))
            || matches!(target, Value::Instance { class_name, .. }
                if self.is_native_method(&class_name.resolve(), method))
            || (matches!(target, Value::Instance { class_name, .. } if class_name == "IO::Handle")
                && matches!(method, "chomp" | "encoding" | "opened" | "DESTROY"))
            || (matches!(target, Value::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric")))
            || matches!(target, Value::Instance { class_name, .. } if self.has_user_method(&class_name.resolve(), "Bridge"))
            || (matches!(target, Value::Instance { class_name, .. } if class_name == "Proc::Async")
                && matches!(
                    method,
                    "start"
                        | "kill"
                        | "write"
                        | "close-stdin"
                        | "bind-stdin"
                        | "bind-stdout"
                        | "bind-stderr"
                        | "ready"
                        | "print"
                        | "say"
                        | "command"
                        | "started"
                        | "w"
                        | "pid"
                        | "stdout"
                        | "stderr"
                        | "Supply"
                ))
            || (matches!(method, "AT-KEY" | "keys")
                && matches!(target, Value::Instance { class_name, .. } if class_name == "Stash"))
            || (method == "keys"
                && args.is_empty()
                && (matches!(target, Value::Hash(_))
                    || matches!(target, Value::Mixin(inner, _) if matches!(inner.as_ref(), Value::Hash(_)))))
            || (!is_pseudo_method
                && matches!(target, Value::Instance { class_name, .. } if self.has_user_method(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target, Value::Instance { class_name, .. } if self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target, Value::Package(class_name) if self.has_user_method(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target, Value::Package(class_name) if self.has_class_level_attr(&class_name.resolve(), method) && !self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target, Value::Instance { class_name, .. } if self.has_class_level_attr(&class_name.resolve(), method) && !self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method && self.mixin_role_has_method(target, method))
    }

    /// Check if a Mixin's role mixins define the given method.
    /// Used so that role-method dispatch on punned role instances takes
    /// precedence over the built-in Cool fallbacks (e.g. `.uc`).
    pub(crate) fn mixin_role_has_method(&self, target: &Value, method: &str) -> bool {
        let Value::Mixin(_, mixins) = target else {
            return false;
        };
        for key in mixins.keys() {
            let Some(role_name) = key.strip_prefix("__mutsu_role__") else {
                continue;
            };
            if let Some(role) = self.roles.get(role_name)
                && role.methods.contains_key(method)
            {
                return true;
            }
        }
        false
    }

    /// Dispatch .raku/.perl on constrained Hash.
    pub(super) fn dispatch_constrained_hash_raku(
        &mut self,
        map: &std::collections::HashMap<String, Value>,
        info: &ContainerTypeInfo,
    ) -> Result<Value, RuntimeError> {
        let mut sorted_keys: Vec<&String> = map.keys().collect();
        sorted_keys.sort();
        // When key type is non-string (e.g. Int), use `key => value` format
        // instead of colonpair `:key(value)` format.
        let use_arrow = info.key_type.is_some()
            && info.key_type.as_deref() != Some("Str")
            && info.key_type.as_deref() != Some("Str(Any)");
        let parts: Vec<String> = sorted_keys
            .iter()
            .map(|k| {
                let v = &map[*k];
                let mut value_repr = || {
                    if matches!(v, Value::Nil) {
                        "Any".to_string()
                    } else {
                        self.call_method_with_values(v.clone(), "raku", vec![])
                            .map(|r| r.to_string_value())
                            .unwrap_or_else(|_| format!("{:?}", v))
                    }
                };
                if use_arrow {
                    format!("{} => {}", k, value_repr())
                } else if let Value::Bool(true) = v {
                    format!(":{}", k)
                } else if let Value::Bool(false) = v {
                    format!(":!{}", k)
                } else {
                    format!(":{}({})", k, value_repr())
                }
            })
            .collect();
        let key_suffix = if let Some(ref kt) = info.key_type {
            format!("{{{}}}", kt)
        } else {
            String::new()
        };
        let inner = parts.join(", ");
        let result = if inner.is_empty() {
            format!("(my {} %{})", info.value_type, key_suffix)
        } else {
            format!("(my {} %{} = {})", info.value_type, key_suffix, inner)
        };
        Ok(Value::str(result))
    }

    /// Dispatch Complex->Num conversion.
    pub(super) fn dispatch_complex_to_num(
        &mut self,
        r: f64,
        im: f64,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        let tolerance = self
            .get_dynamic_var("*TOLERANCE")
            .ok()
            .and_then(|v| match v {
                Value::Num(n) => Some(n),
                Value::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                Value::Int(n) => Some(n as f64),
                _ => None,
            })
            .unwrap_or(1e-15);
        if im.abs() > tolerance {
            let msg = format!(
                "Cannot convert {}{}{}i to Num: imaginary part not zero",
                r,
                if im >= 0.0 { "+" } else { "" },
                im
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("target".to_string(), Value::str_from("Num"));
            attrs.insert("source".to_string(), target.clone());
            let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        Ok(Value::Num(r))
    }
}
