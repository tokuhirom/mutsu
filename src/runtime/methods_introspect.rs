use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch .WHAT method
    pub(super) fn dispatch_what(
        &mut self,
        target: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // A `VarRef` is a transient binder wrapper, not a type of its own:
        // introspect the variable's value.
        if let ValueView::VarRef { value, .. } = target.view() {
            let inner = value.clone();
            return self.dispatch_what(&inner, args);
        }
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
                        if info.value_type.is_empty() {
                            // The `:{...}` / classify shape (of = Mu, no
                            // declared value type): rakudo parameterizes it as
                            // Hash[Mu,Mu,Any] — the third argument is the
                            // (Any) default that differs from the Mu of-type.
                            format!("Hash[Mu,{},Any]", key_type)
                        } else {
                            format!("Hash[{},{}]", info.value_type, key_type)
                        }
                    } else {
                        format!("Hash[{}]", info.value_type)
                    };
                    return Ok(Value::package(Symbol::intern(&name)));
                }
                _ => {}
            }
        }
        let type_name: &str = match target.view() {
            ValueView::VarRef { .. } => unreachable!("unwrapped above"),
            // `Buf`/`Blob` element storage never surfaces as a Raku-level value:
            // it lives in the buffer instance's attribute cell and only
            // `value::value_buf` reads it. Answer as the buffer it backs.
            ValueView::BufStorage(_) => "Buf",
            ValueView::RakuAst(node) => node.class.printed_name(),
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
            // `value_type_name` is the single oracle for what Raku type a
            // `LazyList` presents as (ADR-0038 S2) — defer to it instead of
            // keeping a third, drifted copy of the same context-marker table.
            ValueView::LazyList(_) => crate::runtime::value_type_name(target),
            ValueView::Hash(_) => "Hash",
            ValueView::Rat(_, _) => "Rat",
            ValueView::FatRat(_, _) => "FatRat",
            ValueView::BigRat(_, _) if target.is_bigfatrat() => "FatRat",
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
                let resolved = enum_type.resolve();
                // An anonymous enum (`enum <one two>`) has no type name: raku's
                // `.WHAT` is the empty type object `()`.
                let visible = if crate::value::is_internal_anon_type_name(&resolved) {
                    ""
                } else {
                    &resolved
                };
                return Ok(Value::package(Symbol::intern(visible)));
            }
            // `Nil.WHAT` IS the Nil type object — the same object the `Nil`
            // term denotes (`Nil.WHAT === Nil` is True), not a Package wrapper.
            ValueView::Nil => return Ok(Value::NIL),
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
            // Keep in sync with `value_type_name`: a builtin-method lookup
            // handle (package = owning type) is a Method, otherwise a Sub.
            ValueView::Routine { package, .. }
                if !package.with_str(|p| p == "GLOBAL" || p.is_empty()) =>
            {
                "Method"
            }
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
            // See the matching `value_type_name` arm — `.cache`/`.List` can
            // return a `List`-tagged handle over a not-yet-reified body.
            ValueView::Seq(_) => crate::runtime::value_type_name(target),
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
                // A role-mixed value's `.WHAT` is a distinct anonymous type
                // object per (base type, role set, role type-arguments)
                // composition — permanently cached and shared by every value
                // with that exact composition (ADR-0060), not the shared base
                // type and not forked per instance.
                return self.mixin_what_value(inner, mixins, &args);
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
                        ValueView::Package(n) => {
                            crate::value::user_facing_type_name(&n.resolve()).into_owned()
                        }
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

    /// A meta-object for one of the native `Perl6::Metamodel::*HOW` classes,
    /// tagged with the type it describes.
    fn native_how_instance(how_name: &str, type_name: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(type_name.to_string()));
        attrs.insert(
            "__mutsu_how_target".to_string(),
            Value::str(type_name.to_string()),
        );
        Value::make_instance(Symbol::intern(how_name), attrs)
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
                    ValueView::Package(n) => {
                        crate::value::user_facing_type_name(&n.resolve()).into_owned()
                    }
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
        // An INDIVIDUAL parametric role — one `role` declaration, as opposed to
        // the same-named role *group* the installed name resolves to — reports
        // `ParametricRoleHOW`. Two shapes carry that identity: the type object a
        // role declaration expression evaluates to (`(role R {...})`, a
        // candidate-keyed `Package`), and the candidate objects `.^candidates`
        // hands out.
        if self.is_individual_role_type_object(target) {
            let display = self.role_type_object_display_name(target);
            return Ok(Self::native_how_instance(
                "Perl6::Metamodel::ParametricRoleHOW",
                &display,
            ));
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
                    ValueView::Nil => "Nil",
                    _ => "Mu",
                };
                tn.to_string()
            }
        };
        // A role name only reports a role metaclass for the *type object*. An
        // INSTANCE of a role is an instance of the class the role was punned
        // into (`R.new` builds an anonymous class that does `R`), and an
        // ordinary class instance's metaclass is `ClassHOW` — the role group
        // lives on the name, not on the values made from it.
        let is_type_object = matches!(target.view(), ValueView::Package(_));
        // Use appropriate HOW metaclass for each type kind
        let how_name = if let Some(native) = self.registry().declared_native_how.get(&type_name) {
            // Minted at runtime by `Metamodel::<X>HOW.new_type(...)`; the
            // metaclass it was minted through is its metaclass.
            return Ok(Self::native_how_instance(native, &type_name));
        } else if let Some(kind) = self.registry().package_kinds.get(&type_name) {
            // A bare `package`/`module`/`grammar` reports its own metaclass
            // rather than the default `ClassHOW`.
            match kind {
                crate::ast::PackageKind::Package => "Perl6::Metamodel::PackageHOW",
                crate::ast::PackageKind::Module => "Perl6::Metamodel::ModuleHOW",
                crate::ast::PackageKind::Grammar => "Perl6::Metamodel::GrammarHOW",
            }
        } else if is_type_object
            && (self.registry().roles.contains_key(&type_name) && !type_name.contains('[')
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
                ))
        {
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
        let anonymous_mixin_layers = match type_name.as_str() {
            "Array" => 2,
            "Hash" | "Set" | "Bag" | "Mix" => 1,
            _ => 0,
        };
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(type_name.clone()));
        attrs.insert("__mutsu_how_target".to_string(), Value::str(type_name));
        let mut how = Value::make_instance(Symbol::intern(how_name), attrs);

        // Rakudo composes anonymous implementation roles into the metaobjects
        // for the mutable built-in collection classes.  Preserve that actual
        // MOP shape instead of special-casing `.^name`: wrapping the HOW in
        // ordinary mixin layers lets WHAT, name, identity, and method dispatch
        // all observe the same composed metaobject.
        for _ in 0..anonymous_mixin_layers {
            let mut mixins = HashMap::new();
            mixins.insert("__mutsu_role__<anon>".to_string(), Value::TRUE);
            how = Value::mixin(how, mixins);
        }
        Ok(how)
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
        // Builtin values (Int, Str, Array, ...) also answer .WHO with their
        // type's Stash, same as the type object (raku: 42.WHO.^name is Stash).
        Ok(self.package_stash_value(value_type_name(target)))
    }

    /// Dispatch .WHY method — returns a Pod::Block::Declarator instance
    pub(super) fn dispatch_why(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        let object_id = match target.view() {
            ValueView::Sub(data) => Some(data.id),
            ValueView::WeakSub(data) => data.upgrade().map(|data| data.id),
            ValueView::Instance { id, .. } => Some(id),
            _ => None,
        };
        if let Some(object_id) = object_id
            && let Some(pod) = self.why_object_cache.get(&object_id)
        {
            return Ok(pod.clone());
        }
        // Return declarator doc comment attached to this type/package/sub
        let keys: Vec<String> = match target.view() {
            ValueView::Package(name) => vec![name.resolve()],
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                // ADR-0019 Phase F box F1: `.^lookup`/`.^find_method`/
                // `.^methods` return a Method/Submethod `Instance` now
                // instead of a `Sub` (`todo/tickets/classhow-lookup-returns-
                // sub-not-method-instance.md`) -- mirror the `ValueView::Sub`
                // arm's doc-comment key shape below using the same
                // `__mutsu_lookup_class`/`__mutsu_lookup_method` attributes
                // `.wrap` already reads, so `Class.^find_method(name).WHY`
                // keeps finding the `#|` comment on the method declaration
                // (roast integration/advent2011-day10.t).
                if matches!(class_name.as_str(), "Method" | "Submethod" | "Regex") {
                    let am = attributes.as_map();
                    let method_name = am.get("__mutsu_lookup_method").map(|v| v.to_string_value());
                    let owner = am
                        .get("__mutsu_lookup_class")
                        .map(|v| v.to_string_value())
                        .or_else(|| {
                            am.get("package").and_then(|v| match v.view() {
                                ValueView::Package(p) => Some(p.resolve()),
                                _ => None,
                            })
                        });
                    let mut k = Vec::new();
                    // A specific multi candidate (`.candidates[N]`) has its
                    // own `#|` comment, distinct from its sibling candidates
                    // and the dispatcher itself -- try the
                    // `/multi.{idx}`-suffixed key first, mirroring the
                    // `ValueView::Sub` arm's `multi_idx` handling above. A
                    // non-multi method also carries `candidate_idx=0` (see
                    // `make_method_object_with_owner_ex`), but no doc comment
                    // is ever recorded under that suffixed key, so the lookup
                    // below just falls through to the plain key for it.
                    if let Some(ValueView::Int(idx)) =
                        am.get("__mutsu_lookup_candidate_idx").map(Value::view)
                        && let (Some(owner), Some(name)) = (&owner, &method_name)
                    {
                        k.push(format!("{}::{}/multi.{}", owner, name, idx));
                        k.push(format!("&{}/multi.{}", name, idx));
                    }
                    if let (Some(owner), Some(name)) = (&owner, &method_name) {
                        k.push(format!("{}::{}", owner, name));
                    }
                    if let Some(name) = &method_name {
                        k.push(format!("&{}", name));
                        k.push(name.clone());
                    }
                    k
                }
                // Role candidate with index metadata
                else if let Some(ValueView::Int(idx)) = attributes
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
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        // contents is leading + trailing joined by newline
        let contents = doc.contents();
        attrs.insert("contents".to_string(), Value::str(contents));
        Value::make_instance(Symbol::intern("Pod::Block::Declarator"), attrs)
    }

    /// Dispatch .^name method
    pub(super) fn dispatch_caret_name(&mut self, target: &Value) -> Result<Value, RuntimeError> {
        Ok(Value::str(match target.view() {
            ValueView::Package(name) => {
                let resolved = name.resolve();
                // `.^set_name` on a `Package` value (a user-declared class's
                // own type object, or a builtin's shared type object like
                // `Hash`/`Array`) persists a display-name override in
                // `type_metadata` (see `dispatch_classhow_method`'s
                // "set_name" handler); a plain `.^name` fast path must
                // consult it too, or the rename never becomes visible. For a
                // builtin type this is a genuine process-wide rename,
                // matching real Rakudo (`Hash.^set_name(...)` renames `Hash`
                // for every hash in the program, not just the caller's).
                self.type_metadata
                    .get(&resolved)
                    .and_then(|m| m.get("__set_name__"))
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| crate::value::user_facing_type_name(&resolved).to_string())
            }
            ValueView::Instance { class_name, .. } => {
                let resolved = class_name.resolve();
                self.type_metadata
                    .get(&resolved)
                    .and_then(|m| m.get("__set_name__"))
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| crate::value::user_facing_type_name(&resolved).to_string())
            }
            // An enum VALUE names its enum type (`Bob.^name` is `Names`), not the
            // underlying storage type — `value_type_name` reports "Int" for the
            // int-backed representation, which is only the `.WHICH`/`.Int` view.
            // An anonymous enum (`enum <one two>`) has no name: raku reports "".
            ValueView::Enum { enum_type, .. } => {
                let n = enum_type.resolve();
                if crate::value::is_internal_anon_type_name(&n) {
                    String::new()
                } else {
                    n
                }
            }
            // `.^set_name`, whether called directly on a role-mixed value
            // (`$obj.^set_name(...)`) or on its `.WHAT`
            // (`Hash::Restricted`'s `v.var.WHAT.^set_name(...)`), writes into
            // the SAME composition-keyed shared node (ADR-0060) — so the fast
            // `.^name` path resolves that same node here before falling back
            // to the synthesized `Base+{Role,...}` name.
            ValueView::Mixin(inner, mixins) => {
                let overrides = self.mixin_instance_composition_overrides(inner, mixins)?;
                match overrides.get("__mutsu_type_name__") {
                    Some(renamed) => renamed.to_string_value(),
                    None => crate::value::types::what_type_name(target),
                }
            }
            ValueView::Promise(p) => p.class_name().resolve(),
            ValueView::ParametricRole {
                base_name,
                type_args,
            } => {
                let args_str = type_args
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Package(n) => {
                            crate::value::user_facing_type_name(&n.resolve()).into_owned()
                        }
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
                                // See dispatch_what: the `:{...}` shape (no
                                // declared value type) is Hash[Mu,Mu,Any].
                                if info.value_type.is_empty() {
                                    return Ok(Value::str(format!("Hash[Mu,{},Any]", key_type)));
                                }
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
                self.builtin_display_name(value_type_name(target))
            }
        }))
    }

    /// Resolve a builtin type's `.^name` display name, honoring a global
    /// rename made via `Foo.^set_name(...)` on that type's shared `Package`
    /// value. `base` is the type's internal name (e.g. `"Hash"`), the same
    /// key `dispatch_classhow_method`'s `"set_name"` handler writes
    /// `__set_name__` under for a `ValueView::Package` — including builtins,
    /// since real Rakudo genuinely renames a builtin type process-wide when
    /// `.^set_name` is called directly on its shared type object (verified
    /// against `raku`: `Hash.^set_name("X")` makes every `%h.^name` report
    /// `"X"`, not just `Hash.^name` itself). Values whose `.^name` is
    /// resolved elsewhere (`Package`, `Instance`, `Mixin`, ...) have their
    /// own overrides and never reach this helper.
    pub(super) fn builtin_display_name(&self, base: &'static str) -> String {
        self.type_metadata
            .get(base)
            .and_then(|m| m.get("__set_name__"))
            .map(Value::to_string_value)
            .unwrap_or_else(|| base.to_string())
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
            // An enum value (e.g. `red` of `enum Color`) carries its enum type.
            ValueView::Enum { enum_type, .. } => Some(enum_type.resolve()),
            // A Bool value (True/False) is a member of the built-in Bool enum.
            ValueView::Bool(_) => Some("Bool".to_string()),
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
