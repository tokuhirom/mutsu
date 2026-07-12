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
                return Ok(Value::package(Symbol::intern(&declared)));
            }
            match target.view() {
                ValueView::Array(_, _) => {
                    return Ok(Value::package(Symbol::intern(&format!(
                        "Array[{}]",
                        info.value_type
                    ))));
                }
                ValueView::Hash(_) => {
                    let name = if let Some(key_type) = info.key_type {
                        format!("Hash[{},{}]", info.value_type, key_type)
                    } else {
                        format!("Hash[{}]", info.value_type)
                    };
                    return Ok(Value::package(Symbol::intern(&name)));
                }
                _ => {}
            }
        }
        let type_name: &str = match target.view() {
            ValueView::Int(_) => "Int",
            ValueView::BigInt(_) => "Int",
            ValueView::Num(_) => "Num",
            ValueView::Str(_) => "Str",
            ValueView::Bool(_) => "Bool",
            ValueView::Range(_, _) => "Range",
            ValueView::RangeExcl(_, _)
            | ValueView::RangeExclStart(_, _)
            | ValueView::RangeExclBoth(_, _)
            | ValueView::GenericRange { .. } => "Range",
            ValueView::Array(_, kind) if kind.is_real_array() => "Array",
            ValueView::Array(_, _) => "List",
            // A lazy list assigned into an `@` array reports `Array`; one coerced
            // via `.List` reports `List`; a bare Seq (scalar-held) reports `Seq`.
            ValueView::LazyList(ll) if ll.in_array_context() => "Array",
            ValueView::LazyList(ll) if ll.in_list_context() => "List",
            ValueView::LazyList(_) => "Seq",
            ValueView::Hash(_) => "Hash",
            ValueView::Rat(_, _) => "Rat",
            ValueView::FatRat(_, _) => "FatRat",
            ValueView::BigRat(_, _) => "Rat",
            ValueView::Complex(_, _) => "Complex",
            ValueView::Set(_, false) => "Set",
            ValueView::Set(_, true) => "SetHash",
            ValueView::Bag(_, false) => "Bag",
            ValueView::Bag(_, true) => "BagHash",
            ValueView::Mix(_, false) => "Mix",
            ValueView::Mix(_, true) => "MixHash",
            ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => "Pair",
            ValueView::Enum { enum_type, .. } => {
                return Ok(Value::package(Symbol::intern(&enum_type.resolve())));
            }
            ValueView::Nil => "Any",
            ValueView::Package(name) => {
                let resolved = name.resolve();
                let visible = if crate::value::is_internal_anon_type_name(&resolved) {
                    ""
                } else {
                    &resolved
                };
                return Ok(Value::package(Symbol::intern(visible)));
            }
            ValueView::Routine { is_regex: true, .. } => "Regex",
            ValueView::Routine { .. } => "Sub",
            // Keep in sync with `runtime::utils::value_type_name`: a bare/pointy
            // block (`{...}`, `-> {...}`) is a `Block`, not a `Sub`. `.WHAT`/`.^name`
            // previously reported `Sub` for these even though smartmatch already
            // treated them as `Block` (via `value_type_name`).
            ValueView::Sub(data) => match data.env.get("__mutsu_callable_type").map(Value::view) {
                Some(ValueView::Str(kind)) if kind.as_str() == "Method" => "Method",
                Some(ValueView::Str(kind)) if kind.as_str() == "Submethod" => "Submethod",
                Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
                Some(ValueView::Str(kind)) if kind.as_str() == "Block" => "Block",
                _ if data.is_bare_block => "Block",
                _ => "Sub",
            },
            ValueView::WeakSub(_) => "Sub",
            ValueView::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
            ValueView::Instance { class_name, .. } => {
                let resolved = class_name.resolve();
                let visible = if crate::value::is_internal_anon_type_name(&resolved) {
                    ""
                } else {
                    &resolved
                };
                return Ok(Value::package(Symbol::intern(visible)));
            }
            ValueView::Junction { .. } => "Junction",
            ValueView::Regex(_) | ValueView::RegexWithAdverbs { .. } => "Regex",
            ValueView::Version { .. } => "Version",
            ValueView::Slip(_) => "Slip",
            ValueView::Seq(_) => "Seq",
            ValueView::HyperSeq(_) => "HyperSeq",
            ValueView::RaceSeq(_) => "RaceSeq",
            ValueView::Promise(_) => "Promise",
            ValueView::Channel(_) => "Channel",
            ValueView::Whatever => "Whatever",
            ValueView::HyperWhatever => "HyperWhatever",
            ValueView::Capture { .. } => "Capture",
            ValueView::Uni(u) => {
                if u.form.is_empty() {
                    "Uni"
                } else {
                    u.form.as_str()
                }
            }
            ValueView::Mixin(inner, mixins) => {
                if let Some(allo) = crate::value::types::allomorph_type_name(inner, mixins) {
                    return Ok(Value::package(Symbol::intern(&allo)));
                }
                return self.call_method_with_values(inner.as_ref().clone(), "WHAT", args.clone());
            }
            ValueView::Proxy {
                subclass: Some((name, _)),
                ..
            } => {
                return Ok(Value::package(*name));
            }
            ValueView::Proxy { .. } => "Proxy",
            ValueView::CustomType(c) => {
                return Ok(Value::package(c.name));
            }
            ValueView::CustomTypeInstance(d) => {
                return Ok(Value::package(d.type_name));
            }
            ValueView::ParametricRole {
                base_name,
                type_args,
            } => {
                let args_str: Vec<String> = type_args
                    .iter()
                    .map(|a| match a.view() {
                        ValueView::Package(n) => n.resolve(),
                        ValueView::ParametricRole { .. } => {
                            // Recursively get the WHAT name for nested parametric roles
                            if let Ok(what) =
                                self.call_method_with_values(a.clone(), "WHAT", Vec::new())
                                && let ValueView::Package(n) = what.view()
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
                return Ok(Value::package(Symbol::intern(&name)));
            }
            ValueView::Scalar(inner) => {
                return self.call_method_with_values(inner.clone(), "WHAT", args.clone());
            }
            ValueView::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    return self.call_method_with_values(cached.clone(), "WHAT", args.clone());
                }
                "Scalar"
            }
            ValueView::LazyIoLines { .. } => "Seq",
            ValueView::HashEntryRef { .. } => {
                return self.dispatch_what(&target.hash_entry_read(), args);
            }
            ValueView::ContainerRef(_) => {
                return target.with_deref(|inner| self.dispatch_what(inner, args));
            }
        };
        let visible_type_name = if crate::value::is_internal_anon_type_name(type_name) {
            ""
        } else {
            type_name
        };
        Ok(Value::package(Symbol::intern(visible_type_name)))
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
        if let ValueView::CustomTypeInstance(d) = target.view()
            && let Some(new_how) = self.rebless_map.get(&d.id).cloned()
        {
            return Ok(new_how);
        }
        if let Some(how) = target.custom_how() {
            return Ok(how.clone());
        }
        // Return CurriedRoleHOW for parameterized roles
        if let ValueView::ParametricRole {
            base_name,
            type_args,
        } = target.view()
        {
            let args_str = type_args
                .iter()
                .map(|v| match v.view() {
                    ValueView::Package(n) => n.resolve(),
                    _ => v.to_string_value(),
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
        // Check for persistent HOW values (set by `$c.HOW does Role`)
        let how_lookup_name = match target.view() {
            ValueView::Package(name) => Some(name.resolve()),
            ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
            _ => None,
        };
        if let Some(ref name) = how_lookup_name
            && let Some(how_val) = self.registry().class_how_values.get(name)
        {
            return Ok(how_val.clone());
        }
        // Return a meta-object (ClassHOW) for any value
        let type_name = match target.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Mixin(inner, _) => match inner.as_ref().view() {
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => value_type_name(target).to_string(),
            },
            _ => {
                // Get type name via WHAT logic
                let tn = match target.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) => "Int",
                    ValueView::Num(_) => "Num",
                    ValueView::Str(_) => "Str",
                    ValueView::Bool(_) => "Bool",
                    ValueView::Rat(_, _) | ValueView::BigRat(_, _) => "Rat",
                    ValueView::FatRat(_, _) => "FatRat",
                    ValueView::Complex(_, _) => "Complex",
                    ValueView::Hash(_) => "Hash",
                    ValueView::Array(_, kind) if kind.is_real_array() => "Array",
                    ValueView::Array(_, _) => "List",
                    ValueView::Nil => "Any",
                    _ => "Mu",
                };
                tn.to_string()
            }
        };
        // Use appropriate HOW metaclass for each type kind
        let how_name = if self.registry().roles.contains_key(&type_name) && !type_name.contains('[')
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
        } else if self.registry().enum_types.contains_key(&type_name) {
            "Perl6::Metamodel::EnumHOW"
        } else if self.registry().subsets.contains_key(&type_name)
            || matches!(type_name.as_str(), "UInt" | "NativeInt")
        {
            "Perl6::Metamodel::SubsetHOW"
        } else if crate::runtime::types::parse_coercion_type(&type_name).is_some() {
            "Perl6::Metamodel::CoercionHOW"
        } else {
            "Perl6::Metamodel::ClassHOW"
        };
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(type_name.clone()));
        attrs.insert("__mutsu_how_target".to_string(), Value::str(type_name));
        Ok(Value::make_instance(Symbol::intern(how_name), attrs))
    }

    /// Dispatch .WHO method
    pub(super) fn dispatch_who(&self, target: &Value) -> Result<Value, RuntimeError> {
        if let ValueView::Package(name) = target.view() {
            return Ok(self.package_stash_value(&name.resolve()));
        }
        // For instances, WHO returns the stash of their class
        if let ValueView::Instance { class_name, .. } = target.view() {
            return Ok(self.package_stash_value(&class_name.resolve()));
        }
        if let ValueView::CustomType(c) = target.view() {
            return Ok(self.package_stash_value(&c.name.resolve()));
        }
        Ok(Value::hash_with_data(Value::hash_arc(HashMap::new())))
    }

    /// Dispatch .WHY method — returns a Pod::Block::Declarator instance
    pub(super) fn dispatch_why(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        // Return declarator doc comment attached to this type/package/sub
        let keys: Vec<String> = match target.view() {
            ValueView::Package(name) => vec![name.resolve()],
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                // Role candidate with index metadata
                if let Some(ValueView::Int(idx)) = attributes
                    .as_map()
                    .get("__mutsu_role_candidate_idx")
                    .map(Value::view)
                {
                    let base_name = attributes
                        .as_map()
                        .get("__mutsu_role_base_name")
                        .and_then(|v| match v.view() {
                            ValueView::Str(s) => Some(s.to_string()),
                            _ => None,
                        })
                        .unwrap_or_else(|| class_name.resolve());
                    let mut k = Vec::new();
                    if idx > 0 {
                        k.push(format!("{}/role.{}", base_name, idx));
                    }
                    k.push(base_name);
                    k
                } else if class_name == "Attribute" {
                    // Attribute objects: look up by "ClassName::$!attrname"
                    let mut k = Vec::new();
                    if let Some(ValueView::Str(attr_name)) =
                        attributes.as_map().get("name").map(Value::view)
                    {
                        // Try __mutsu_attr_owner first, then package
                        let owner = attributes
                            .as_map()
                            .get("__mutsu_attr_owner")
                            .and_then(|v| match v.view() {
                                ValueView::Str(s) => Some(s.to_string()),
                                _ => None,
                            })
                            .or_else(|| {
                                attributes
                                    .as_map()
                                    .get("package")
                                    .and_then(|v| match v.view() {
                                        ValueView::Package(p) => Some(p.resolve()),
                                        ValueView::Str(s) => Some(s.to_string()),
                                        _ => None,
                                    })
                            });
                        if let Some(owner) = owner {
                            k.push(format!("{}::{}", owner, *attr_name));
                        }
                        k.push(attr_name.to_string());
                    }
                    k
                } else if class_name == "Parameter" {
                    // Parameter objects: look up by "owner_sub::param_name"
                    let mut k = Vec::new();
                    let param_name = attributes
                        .as_map()
                        .get("name")
                        .and_then(|v| match v.view() {
                            ValueView::Str(s) => Some(s.to_string()),
                            _ => None,
                        })
                        .unwrap_or_default();
                    let sigil = attributes
                        .as_map()
                        .get("sigil")
                        .and_then(|v| match v.view() {
                            ValueView::Str(s) => Some(s.to_string()),
                            _ => None,
                        })
                        .unwrap_or_default();
                    if let Some(ValueView::Str(owner)) = attributes
                        .as_map()
                        .get("__mutsu_owner_sub")
                        .map(Value::view)
                    {
                        // Try scoped key with param name
                        if !param_name.is_empty() {
                            k.push(format!("{}::{}", *owner, param_name));
                        }
                        // For anonymous params, try with just the sigil
                        if param_name.is_empty() || !param_name.starts_with(&sigil) {
                            k.push(format!("{}::{}", *owner, sigil));
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
            ValueView::Sub(sub_data) => {
                let mut k = Vec::new();
                // Check for multi candidate index (from .candidates or routine_candidate_subs)
                let multi_idx = sub_data
                    .env
                    .get("__mutsu_multi_index")
                    .or_else(|| sub_data.env.get("__mutsu_lookup_candidate_idx"))
                    .and_then(|v| match v.view() {
                        ValueView::Int(i) => Some(i),
                        _ => None,
                    });
                if let Some(idx) = multi_idx {
                    if !sub_data.package.is_empty() && !sub_data.name.is_empty() {
                        k.push(format!(
                            "{}::{}/multi.{}",
                            sub_data.package, sub_data.name, idx
                        ));
                    }
                    if !sub_data.name.is_empty() {
                        k.push(format!("&{}/multi.{}", sub_data.name.resolve(), idx));
                    }
                }
                if !sub_data.package.is_empty() && !sub_data.name.is_empty() {
                    k.push(format!("{}::{}", sub_data.package, sub_data.name));
                }
                if !sub_data.name.is_empty() {
                    // Try &-prefixed key first (to disambiguate from package names)
                    k.push(format!("&{}", sub_data.name.resolve()));
                    k.push(sub_data.name.resolve());
                } else if !sub_data.is_bare_block {
                    // Anonymous sub (not bare block): try the &<anon> key
                    k.push("&<anon>".to_string());
                }
                k
            }
            ValueView::Routine { package, name, .. } => {
                let mut k = Vec::new();
                if !package.is_empty() && !name.is_empty() {
                    k.push(format!("{}::{}", package.resolve(), name.resolve()));
                }
                if !name.is_empty() {
                    k.push(format!("&{}", name.resolve()));
                    k.push(name.resolve());
                }
                k
            }
            _ => vec![],
        };
        // Try to find matching doc comment, checking cache first for each key
        for key in keys {
            if let Some(cached) = self.why_cache.get(&key) {
                return Ok(cached.clone());
            }
            if let Some(doc) = self.doc_comments.get(&key) {
                let pod = Self::make_pod_declarator(doc, target.clone());
                self.why_cache.insert(key, pod.clone());
                return Ok(pod);
            }
        }
        // For anonymous subs/bare blocks, try to find a doc comment by source line proximity
        if let ValueView::Sub(sub_data) = target.view()
            && sub_data.name.is_empty()
            && let Some(src_line) = sub_data.source_line
        {
            let prefix = if sub_data.is_bare_block {
                "block:"
            } else {
                "&<anon>"
            };
            // Find the doc comment whose source_line is closest
            // to (and at or after) the sub's source line
            let mut best_match: Option<&super::DocComment> = None;
            let mut best_dist = u32::MAX;
            for dc in self.doc_comments.values() {
                if dc.wherefore_name.starts_with(prefix)
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
        Ok(Value::NIL)
    }

    /// Create a Pod::Block::Declarator instance from a DocComment
    pub(crate) fn make_pod_declarator(doc: &super::DocComment, wherefore: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "leading".to_string(),
            if let Some(ref leading) = doc.leading {
                Value::str(leading.clone())
            } else {
                Value::NIL
            },
        );
        attrs.insert(
            "trailing".to_string(),
            if let Some(ref trailing) = doc.trailing {
                Value::str(trailing.clone())
            } else {
                Value::NIL
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
        Ok(Value::str(match target.view() {
            ValueView::Package(name) => {
                crate::value::user_facing_type_name(&name.resolve()).to_string()
            }
            ValueView::Instance { class_name, .. } => {
                crate::value::user_facing_type_name(&class_name.resolve()).to_string()
            }
            ValueView::Mixin(inner, _) => match inner.as_ref().view() {
                ValueView::Instance { class_name, .. } => {
                    crate::value::user_facing_type_name(&class_name.resolve()).to_string()
                }
                _ => value_type_name(target).to_string(),
            },
            ValueView::Promise(p) => p.class_name().resolve(),
            ValueView::ParametricRole {
                base_name,
                type_args,
            } => {
                let args_str = type_args
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Package(n) => n.resolve(),
                        _ => v.to_string_value(),
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                format!("{}[{}]", base_name, args_str)
            }
            ValueView::Sub(data) => {
                let base = value_type_name(target);
                // Check for return type to produce Sub+{Callable[Type]} format
                if let Some(ValueView::Str(ret)) =
                    data.env.get("__mutsu_return_type").map(Value::view)
                {
                    format!("{}+{{Callable[{}]}}", base, *ret)
                } else {
                    base.to_string()
                }
            }
            _ => {
                // Check container type metadata for typed Hash/Array
                if let Some(info) = self.container_type_metadata(target) {
                    // A declared type (e.g. an immutable `Map`) names the value
                    // directly, mirroring `.WHAT`.
                    if let Some(ref declared) = info.declared_type {
                        return Ok(Value::str(declared.clone()));
                    }
                    match target.view() {
                        ValueView::Hash(_) => {
                            if let Some(ref key_type) = info.key_type {
                                return Ok(Value::str(format!(
                                    "Hash[{},{}]",
                                    info.value_type, key_type
                                )));
                            } else if info.value_type != "Any" && info.value_type != "Mu" {
                                return Ok(Value::str(format!("Hash[{}]", info.value_type)));
                            }
                        }
                        ValueView::Array(_, kind)
                            if kind.is_real_array()
                                && info.value_type != "Any"
                                && info.value_type != "Mu" =>
                        {
                            return Ok(Value::str(format!("Array[{}]", info.value_type)));
                        }
                        _ => {}
                    }
                }
                value_type_name(target).to_string()
            }
        }))
    }

    /// Dispatch .^enum_value_list / .enum_value_list method
    pub(super) fn dispatch_enum_value_list(
        &self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let type_name_owned = match target.view() {
            ValueView::Package(name) => Some(name.resolve()),
            ValueView::Str(name) => Some(name.to_string()),
            _ => None,
        };
        let type_name = type_name_owned.as_deref();
        if let Some(type_name) = type_name
            && let Some(variants) = self.registry().enum_types.get(type_name)
        {
            let values: Vec<Value> = variants
                .iter()
                .enumerate()
                .map(|(index, (key, val))| {
                    Value::enum_parts(
                        Symbol::intern(type_name),
                        Symbol::intern(key),
                        val.clone(),
                        index,
                    )
                })
                .collect();
            return Some(Ok(Value::array(values)));
        }
        None
    }

    /// Dispatch .enums method.
    /// Returns a Map (immutable Hash) of variant-name => value pairs.
    pub(super) fn dispatch_enums(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        let type_name_owned = match target.view() {
            ValueView::Package(name) => Some(name.resolve()),
            ValueView::Str(name) => Some(name.to_string()),
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
            if let Some(v) = self.registry().enum_types.get(type_name) {
                variants_owned = v.clone();
                Some(variants_owned.as_slice())
            } else {
                None
            }
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
            let result = self.tag_container_metadata(
                result,
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
        if let ValueView::Str(type_name) = target.view()
            && let Some(variants) = self.registry().enum_types.get(type_name.as_str())
        {
            let mut result = Vec::new();
            for (k, v) in variants {
                result.push(Value::pair(v.to_str_value(), Value::str(k.clone())));
            }
            return Some(Ok(Value::array(result)));
        }
        None
    }
}
