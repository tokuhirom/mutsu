use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch .WHAT method
    pub(super) fn dispatch_what(
        &mut self,
        target: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(info) = self.container_type_metadata(target) {
            if let Some(declared) = info.declared_type {
                return Ok(Value::Package(Symbol::intern(&declared)));
            }
            match target {
                Value::Array(_, _) => {
                    return Ok(Value::Package(Symbol::intern(&format!(
                        "Array[{}]",
                        info.value_type
                    ))));
                }
                Value::Hash(_) => {
                    let name = if let Some(key_type) = info.key_type {
                        format!("Hash[{},{}]", info.value_type, key_type)
                    } else {
                        format!("Hash[{}]", info.value_type)
                    };
                    return Ok(Value::Package(Symbol::intern(&name)));
                }
                _ => {}
            }
        }
        let type_name: &str = match target {
            Value::Int(_) => "Int",
            Value::BigInt(_) => "Int",
            Value::Num(_) => "Num",
            Value::Str(_) => "Str",
            Value::Bool(_) => "Bool",
            Value::Range(_, _) => "Range",
            Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _)
            | Value::GenericRange { .. } => "Range",
            Value::Array(_, kind) if kind.is_real_array() => "Array",
            Value::Array(_, _) => "List",
            Value::LazyList(_) => "Seq",
            Value::Hash(_) => "Hash",
            Value::Rat(_, _) => "Rat",
            Value::FatRat(_, _) => "FatRat",
            Value::BigRat(_, _) => "Rat",
            Value::Complex(_, _) => "Complex",
            Value::Set(_) => "Set",
            Value::Bag(_) => "Bag",
            Value::Mix(_) => "Mix",
            Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
            Value::Enum { enum_type, .. } => {
                return Ok(Value::Package(Symbol::intern(&enum_type.resolve())));
            }
            Value::Nil => "Any",
            Value::Package(name) => {
                let resolved = name.resolve();
                let visible = if crate::value::is_internal_anon_type_name(&resolved) {
                    ""
                } else {
                    &resolved
                };
                return Ok(Value::Package(Symbol::intern(visible)));
            }
            Value::Routine { is_regex: true, .. } => "Regex",
            Value::Routine { .. } => "Sub",
            Value::Sub(data) => match data.env.get("__mutsu_callable_type") {
                Some(Value::Str(kind)) if kind.as_str() == "Method" => "Method",
                Some(Value::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
                _ => "Sub",
            },
            Value::WeakSub(_) => "Sub",
            Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
            Value::Instance { class_name, .. } => {
                let resolved = class_name.resolve();
                let visible = if crate::value::is_internal_anon_type_name(&resolved) {
                    ""
                } else {
                    &resolved
                };
                return Ok(Value::Package(Symbol::intern(visible)));
            }
            Value::Junction { .. } => "Junction",
            Value::Regex(_) | Value::RegexWithAdverbs { .. } => "Regex",
            Value::Version { .. } => "Version",
            Value::Slip(_) => "Slip",
            Value::Seq(_) => "Seq",
            Value::Promise(_) => "Promise",
            Value::Channel(_) => "Channel",
            Value::Whatever => "Whatever",
            Value::HyperWhatever => "HyperWhatever",
            Value::Capture { .. } => "Capture",
            Value::Uni { form, .. } => {
                if form.is_empty() {
                    "Uni"
                } else {
                    form.as_str()
                }
            }
            Value::Mixin(inner, mixins) => {
                if let Some(allo) = crate::value::types::allomorph_type_name(inner, mixins) {
                    return Ok(Value::Package(Symbol::intern(&allo)));
                }
                return self.call_method_with_values(inner.as_ref().clone(), "WHAT", args.clone());
            }
            Value::Proxy {
                subclass: Some((name, _)),
                ..
            } => {
                return Ok(Value::Package(*name));
            }
            Value::Proxy { .. } => "Proxy",
            Value::CustomType { name, .. } => {
                return Ok(Value::Package(*name));
            }
            Value::CustomTypeInstance { type_name: tn, .. } => {
                return Ok(Value::Package(*tn));
            }
            Value::ParametricRole {
                base_name,
                type_args,
            } => {
                let args_str: Vec<String> = type_args
                    .iter()
                    .map(|a| match a {
                        Value::Package(n) => n.resolve(),
                        Value::ParametricRole { .. } => {
                            // Recursively get the WHAT name for nested parametric roles
                            if let Ok(Value::Package(n)) =
                                self.call_method_with_values(a.clone(), "WHAT", Vec::new())
                            {
                                // Strip surrounding parens from (Name)
                                n.resolve()
                                    .trim_start_matches('(')
                                    .trim_end_matches(')')
                                    .to_string()
                            } else {
                                a.to_string_value()
                            }
                        }
                        _ => a.to_string_value(),
                    })
                    .collect();
                let name = format!("{}[{}]", base_name, args_str.join(","));
                return Ok(Value::Package(Symbol::intern(&name)));
            }
            Value::Scalar(inner) => {
                return self.call_method_with_values(*inner.clone(), "WHAT", args.clone());
            }
        };
        let visible_type_name = if crate::value::is_internal_anon_type_name(type_name) {
            ""
        } else {
            type_name
        };
        Ok(Value::Package(Symbol::intern(visible_type_name)))
    }

    /// Dispatch .HOW method
    pub(super) fn dispatch_how(
        &self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if !args.is_empty() {
            return Err(RuntimeError::new(
                "X::Syntax::Argument::MOPMacro: HOW does not take arguments",
            ));
        }
        // Return custom HOW for CustomType/CustomTypeInstance
        // Check rebless map first for reblessed instances
        if let Value::CustomTypeInstance { id, .. } = target
            && let Some(new_how) = self.rebless_map.get(id).cloned()
        {
            return Ok(new_how);
        }
        if let Value::CustomType { how, .. } | Value::CustomTypeInstance { how, .. } = target {
            return Ok(*how.clone());
        }
        // Return CurriedRoleHOW for parameterized roles
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = target
        {
            let args_str = type_args
                .iter()
                .map(|v| match v {
                    Value::Package(n) => n.resolve(),
                    other => other.to_string_value(),
                })
                .collect::<Vec<_>>()
                .join(",");
            let full_name = format!("{}[{}]", base_name, args_str);
            let mut attrs = HashMap::new();
            attrs.insert("name".to_string(), Value::str(full_name));
            return Ok(Value::make_instance(
                Symbol::intern("Perl6::Metamodel::CurriedRoleHOW"),
                attrs,
            ));
        }
        // Return a meta-object (ClassHOW) for any value
        let type_name = match target {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            _ => {
                // Get type name via WHAT logic
                let tn = match target {
                    Value::Int(_) | Value::BigInt(_) => "Int",
                    Value::Num(_) => "Num",
                    Value::Str(_) => "Str",
                    Value::Bool(_) => "Bool",
                    Value::Hash(_) => "Hash",
                    Value::Array(_, kind) if kind.is_real_array() => "Array",
                    Value::Array(_, _) => "List",
                    Value::Nil => "Any",
                    _ => "Mu",
                };
                tn.to_string()
            }
        };
        // Use appropriate HOW metaclass for each type kind
        let how_name = if self.roles.contains_key(&type_name) && !type_name.contains('[')
            || matches!(
                type_name.as_str(),
                "Numeric"
                    | "Real"
                    | "Stringy"
                    | "Positional"
                    | "Associative"
                    | "Callable"
                    | "Setty"
                    | "Baggy"
                    | "Mixy"
                    | "Dateish"
                    | "Iterable"
                    | "Iterator"
                    | "PositionalBindFailover"
            ) {
            "Perl6::Metamodel::ParametricRoleGroupHOW"
        } else if self.enum_types.contains_key(&type_name) {
            "Perl6::Metamodel::EnumHOW"
        } else if self.subsets.contains_key(&type_name)
            || matches!(type_name.as_str(), "UInt" | "NativeInt")
        {
            "Perl6::Metamodel::SubsetHOW"
        } else {
            "Perl6::Metamodel::ClassHOW"
        };
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(type_name));
        Ok(Value::make_instance(Symbol::intern(how_name), attrs))
    }

    /// Dispatch .WHO method
    pub(super) fn dispatch_who(&self, target: &Value) -> Result<Value, RuntimeError> {
        if let Value::Package(name) = target {
            return Ok(self.package_stash_value(&name.resolve()));
        }
        Ok(Value::Hash(Arc::new(HashMap::new())))
    }

    /// Dispatch .WHY method
    pub(super) fn dispatch_why(&self, target: &Value) -> Result<Value, RuntimeError> {
        // Return declarator doc comment attached to this type/package/sub
        let keys: Vec<String> = match target {
            Value::Package(name) => vec![name.resolve()],
            Value::Instance { class_name, .. } => vec![class_name.resolve()],
            Value::Sub(sub_data) => {
                let mut k = Vec::new();
                if sub_data.package != "" && sub_data.name != "" {
                    k.push(format!("{}::{}", sub_data.package, sub_data.name));
                }
                if sub_data.name != "" {
                    k.push(sub_data.name.resolve());
                }
                k
            }
            _ => vec![],
        };
        for key in keys {
            if let Some(doc) = self.doc_comments.get(&key) {
                return Ok(Value::str(doc.clone()));
            }
        }
        Ok(Value::Nil)
    }

    /// Dispatch .^name method
    pub(super) fn dispatch_caret_name(&self, target: &Value) -> Result<Value, RuntimeError> {
        Ok(Value::str(match target {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            Value::Promise(p) => p.class_name().resolve(),
            Value::ParametricRole {
                base_name,
                type_args,
            } => {
                let args_str = type_args
                    .iter()
                    .map(|v| match v {
                        Value::Package(n) => n.resolve(),
                        other => other.to_string_value(),
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                format!("{}[{}]", base_name, args_str)
            }
            other => value_type_name(other).to_string(),
        }))
    }

    /// Dispatch .^enum_value_list / .enum_value_list method
    pub(super) fn dispatch_enum_value_list(
        &self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let type_name_owned = match target {
            Value::Package(name) => Some(name.resolve()),
            Value::Str(name) => Some(name.to_string()),
            _ => None,
        };
        let type_name = type_name_owned.as_deref();
        if let Some(type_name) = type_name
            && let Some(variants) = self.enum_types.get(type_name)
        {
            let values: Vec<Value> = variants
                .iter()
                .enumerate()
                .map(|(index, (key, val, sv))| Value::Enum {
                    enum_type: Symbol::intern(type_name),
                    key: Symbol::intern(key),
                    value: *val,
                    index,
                    str_value: *sv,
                })
                .collect();
            return Some(Ok(Value::array(values)));
        }
        None
    }

    /// Dispatch .enums method
    pub(super) fn dispatch_enums(&self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        let type_name_owned = match target {
            Value::Package(name) => Some(name.resolve()),
            Value::Str(name) => Some(name.to_string()),
            _ => None,
        };
        let type_name = type_name_owned.as_deref();
        if let Some(type_name) = type_name
            && let Some(variants) = self.enum_types.get(type_name)
        {
            let mut map = HashMap::new();
            for (k, v, sv) in variants {
                if let Some(s) = sv {
                    map.insert(k.clone(), Value::str(s.resolve()));
                } else {
                    map.insert(k.clone(), Value::Int(*v));
                }
            }
            return Some(Ok(Value::hash(map)));
        }
        None
    }

    /// Dispatch .invert on enum type string
    pub(super) fn dispatch_invert_enum(
        &self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Str(type_name) = target
            && let Some(variants) = self.enum_types.get(type_name.as_str())
        {
            let mut result = Vec::new();
            for (k, v, sv) in variants {
                let key_val = if let Some(s) = sv {
                    s.resolve()
                } else {
                    v.to_string()
                };
                result.push(Value::Pair(key_val, Box::new(Value::str(k.clone()))));
            }
            return Some(Ok(Value::array(result)));
        }
        None
    }
}
