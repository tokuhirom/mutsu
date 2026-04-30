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
            Value::Set(_, false) => "Set",
            Value::Set(_, true) => "SetHash",
            Value::Bag(_, false) => "Bag",
            Value::Bag(_, true) => "BagHash",
            Value::Mix(_, false) => "Mix",
            Value::Mix(_, true) => "MixHash",
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
            Value::HyperSeq(_) => "HyperSeq",
            Value::RaceSeq(_) => "RaceSeq",
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
            Value::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    return self.call_method_with_values(cached.clone(), "WHAT", args.clone());
                }
                "Scalar"
            }
            Value::LazyIoLines { .. } => "Seq",
            Value::HashSlotRef { .. } => {
                return self.dispatch_what(&target.hash_slot_read(), args);
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
            Value::Mixin(inner, _) => match inner.as_ref() {
                Value::Instance { class_name, .. } => class_name.resolve(),
                _ => value_type_name(target).to_string(),
            },
            _ => {
                // Get type name via WHAT logic
                let tn = match target {
                    Value::Int(_) | Value::BigInt(_) => "Int",
                    Value::Num(_) => "Num",
                    Value::Str(_) => "Str",
                    Value::Bool(_) => "Bool",
                    Value::Rat(_, _) | Value::BigRat(_, _) => "Rat",
                    Value::FatRat(_, _) => "FatRat",
                    Value::Complex(_, _) => "Complex",
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
                    | "Rational"
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
        } else if crate::runtime::types::parse_coercion_type(&type_name).is_some() {
            "Perl6::Metamodel::CoercionHOW"
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
        // For instances, WHO returns the stash of their class
        if let Value::Instance { class_name, .. } = target {
            return Ok(self.package_stash_value(&class_name.resolve()));
        }
        if let Value::CustomType { name, .. } = target {
            return Ok(self.package_stash_value(&name.resolve()));
        }
        Ok(Value::Hash(Arc::new(HashMap::new())))
    }

    /// Dispatch .WHY method — returns a Pod::Block::Declarator instance
    pub(super) fn dispatch_why(&self, target: &Value) -> Result<Value, RuntimeError> {
        // Return declarator doc comment attached to this type/package/sub
        let keys: Vec<String> = match target {
            Value::Package(name) => vec![name.resolve()],
            Value::Instance {
                class_name,
                attributes,
                ..
            } => {
                if class_name == "Attribute" {
                    // Attribute objects: look up by "ClassName::$!attrname"
                    let mut k = Vec::new();
                    if let Some(Value::Str(attr_name)) = attributes.get("name") {
                        // Try __mutsu_attr_owner first, then package
                        let owner = attributes
                            .get("__mutsu_attr_owner")
                            .and_then(|v| match v {
                                Value::Str(s) => Some(s.to_string()),
                                _ => None,
                            })
                            .or_else(|| {
                                attributes.get("package").and_then(|v| match v {
                                    Value::Package(p) => Some(p.resolve()),
                                    Value::Str(s) => Some(s.to_string()),
                                    _ => None,
                                })
                            });
                        if let Some(owner) = owner {
                            k.push(format!("{}::{}", owner, attr_name));
                        }
                        k.push(attr_name.to_string());
                    }
                    k
                } else if class_name == "Parameter" {
                    // Parameter objects: look up by "owner_sub::param_name"
                    let mut k = Vec::new();
                    let param_name = attributes
                        .get("name")
                        .and_then(|v| match v {
                            Value::Str(s) => Some(s.to_string()),
                            _ => None,
                        })
                        .unwrap_or_default();
                    let sigil = attributes
                        .get("sigil")
                        .and_then(|v| match v {
                            Value::Str(s) => Some(s.to_string()),
                            _ => None,
                        })
                        .unwrap_or_default();
                    if let Some(Value::Str(owner)) = attributes.get("__mutsu_owner_sub") {
                        // Try scoped key with param name
                        if !param_name.is_empty() {
                            k.push(format!("{}::{}", owner, param_name));
                        }
                        // For anonymous params, try with just the sigil
                        if param_name.is_empty() || !param_name.starts_with(&sigil) {
                            k.push(format!("{}::{}", owner, sigil));
                        }
                    }
                    // Fallback: try plain param name
                    if !param_name.is_empty() {
                        k.push(param_name);
                    }
                    k
                } else {
                    vec![class_name.resolve()]
                }
            }
            Value::Sub(sub_data) => {
                let mut k = Vec::new();
                // Check for multi candidate index (from .candidates or routine_candidate_subs)
                let multi_idx = sub_data
                    .env
                    .get("__mutsu_multi_index")
                    .or_else(|| sub_data.env.get("__mutsu_lookup_candidate_idx"))
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i),
                        _ => None,
                    });
                if let Some(idx) = multi_idx {
                    if sub_data.package != "" && sub_data.name != "" {
                        k.push(format!(
                            "{}::{}/multi.{}",
                            sub_data.package, sub_data.name, idx
                        ));
                    }
                    if sub_data.name != "" {
                        k.push(format!("&{}/multi.{}", sub_data.name.resolve(), idx));
                    }
                }
                if sub_data.package != "" && sub_data.name != "" {
                    k.push(format!("{}::{}", sub_data.package, sub_data.name));
                }
                if sub_data.name != "" {
                    // Try &-prefixed key first (to disambiguate from package names)
                    k.push(format!("&{}", sub_data.name.resolve()));
                    k.push(sub_data.name.resolve());
                } else if !sub_data.is_bare_block {
                    // Anonymous sub (not bare block): try the &<anon> key
                    k.push("&<anon>".to_string());
                }
                k
            }
            Value::Routine { package, name, .. } => {
                let mut k = Vec::new();
                if *package != "" && *name != "" {
                    k.push(format!("{}::{}", package.resolve(), name.resolve()));
                }
                if *name != "" {
                    k.push(format!("&{}", name.resolve()));
                    k.push(name.resolve());
                }
                k
            }
            _ => vec![],
        };
        for key in keys {
            if let Some(doc) = self.doc_comments.get(&key) {
                return Ok(Self::make_pod_declarator(doc, target.clone()));
            }
        }
        // For anonymous bare blocks, try to find a doc comment by source line proximity
        if let Value::Sub(sub_data) = target
            && sub_data.name == ""
            && sub_data.is_bare_block
            && let Some(src_line) = sub_data.source_line
        {
            // Find the block:* doc comment whose source_line is closest
            // to (and at or after) the sub's source line
            let mut best_match: Option<&super::DocComment> = None;
            let mut best_dist = u32::MAX;
            for dc in self.doc_comments.values() {
                if dc.wherefore_name.starts_with("block:")
                    && let Some(dc_line) = dc.source_line
                {
                    let dist = if dc_line >= src_line {
                        dc_line - src_line
                    } else if src_line - dc_line <= 2 {
                        // Allow the sub to be 1-2 lines before the
                        // declaration (source_line might be off)
                        src_line - dc_line
                    } else {
                        continue;
                    };
                    if dist < best_dist {
                        best_dist = dist;
                        best_match = Some(dc);
                    }
                }
            }
            if let Some(dc) = best_match {
                return Ok(Self::make_pod_declarator(dc, target.clone()));
            }
        }
        Ok(Value::Nil)
    }

    /// Create a Pod::Block::Declarator instance from a DocComment
    pub(crate) fn make_pod_declarator(doc: &super::DocComment, wherefore: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "leading".to_string(),
            if let Some(ref leading) = doc.leading {
                Value::str(leading.clone())
            } else {
                Value::Nil
            },
        );
        attrs.insert(
            "trailing".to_string(),
            if let Some(ref trailing) = doc.trailing {
                Value::str(trailing.clone())
            } else {
                Value::Nil
            },
        );
        attrs.insert("WHEREFORE".to_string(), wherefore);
        // contents is leading + trailing joined by newline
        let contents = doc.contents();
        attrs.insert("contents".to_string(), Value::str(contents));
        Value::make_instance(Symbol::intern("Pod::Block::Declarator"), attrs)
    }

    /// Dispatch .^name method
    pub(super) fn dispatch_caret_name(&self, target: &Value) -> Result<Value, RuntimeError> {
        Ok(Value::str(match target {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            Value::Mixin(inner, _) => match inner.as_ref() {
                Value::Instance { class_name, .. } => class_name.resolve(),
                _ => value_type_name(target).to_string(),
            },
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
            Value::Sub(data) => {
                let base = value_type_name(target);
                // Check for return type to produce Sub+{Callable[Type]} format
                if let Some(Value::Str(ret)) = data.env.get("__mutsu_return_type") {
                    format!("{}+{{Callable[{}]}}", base, ret)
                } else {
                    base.to_string()
                }
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
                .map(|(index, (key, val))| Value::Enum {
                    enum_type: Symbol::intern(type_name),
                    key: Symbol::intern(key),
                    value: val.clone(),
                    index,
                })
                .collect();
            return Some(Ok(Value::array(values)));
        }
        None
    }

    /// Dispatch .enums method.
    /// Returns a Map (immutable Hash) of variant-name => value pairs.
    pub(super) fn dispatch_enums(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        let type_name_owned = match target {
            Value::Package(name) => Some(name.resolve()),
            Value::Str(name) => Some(name.to_string()),
            _ => None,
        };
        let type_name = type_name_owned.as_deref();
        // Built-in Bool enum
        let variants_owned;
        let variants_ref = if type_name == Some("Bool") {
            variants_owned = vec![
                ("False".to_string(), EnumValue::Int(0)),
                ("True".to_string(), EnumValue::Int(1)),
            ];
            Some(variants_owned.as_slice())
        } else if let Some(type_name) = type_name {
            self.enum_types.get(type_name).map(|v| v.as_slice())
        } else {
            None
        };
        if let Some(variants) = variants_ref {
            let mut map = HashMap::new();
            for (k, v) in variants {
                map.insert(k.clone(), v.to_value());
            }
            let result = Value::hash(map);
            // Mark as Map (immutable hash)
            self.register_container_type_metadata(
                &result,
                ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                },
            );
            return Some(Ok(result));
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
            for (k, v) in variants {
                result.push(Value::Pair(
                    v.to_str_value(),
                    Box::new(Value::str(k.clone())),
                ));
            }
            return Some(Ok(Value::array(result)));
        }
        None
    }
}
