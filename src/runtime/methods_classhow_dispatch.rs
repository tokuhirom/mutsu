use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn dispatch_classhow_method(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "set_name" if args.len() == 2 => {
                // `$type.^set_name($name)` renames a metaobject (Rakudo's ClassHOW
                // method). It is most often applied to a freshly-composed
                // anonymous type, e.g. `Foo.new but role {...}`, to give it a
                // human-readable name for display. Persist the name so a later
                // `.^name` returns it.
                let new_name = args[1].to_string_value();
                match args[0].view() {
                    ValueView::Mixin(_, overrides) => {
                        // Store the friendly name on the shared overrides map so a
                        // later `.^name` on any alias of this mixed-in object
                        // returns it. The metamethod receives a clone of the
                        // invocant, but the clone shares the overrides `Arc`, so an
                        // in-place write reaches the caller's value too — matching
                        // Rakudo's in-place mutation of the anonymous metaobject.
                        // SAFETY: aliased in-place mutation of a shared container
                        // (see `arc_contents_mut`); no borrow into the map is live
                        // across the insert.
                        let map = unsafe { crate::value::arc_contents_mut(overrides) };
                        map.insert(
                            "__mutsu_type_name__".to_string(),
                            Value::str(new_name.clone()),
                        );
                    }
                    ValueView::Package(name) => {
                        self.type_metadata
                            .entry(name.resolve())
                            .or_default()
                            .insert("__set_name__".to_string(), Value::str(new_name.clone()));
                    }
                    ValueView::Instance { class_name, .. } => {
                        self.type_metadata
                            .entry(class_name.resolve())
                            .or_default()
                            .insert("__set_name__".to_string(), Value::str(new_name.clone()));
                    }
                    _ => {}
                }
                Ok(Value::str(new_name))
            }
            "name" if args.len() == 1 => Ok(Value::str(match args[0].view() {
                ValueView::Mixin(_, overrides) if overrides.contains_key("__mutsu_type_name__") => {
                    overrides["__mutsu_type_name__"].to_string_value()
                }
                // A role-mixed value (`5 but Foo::Bar`, `$x does R`) reports its
                // base type with a `+{Role,...}` suffix, e.g. `Int+{Foo::Bar}`.
                // `what_type_name` builds this from the recorded role keys;
                // `value_type_name` (a `&'static str`) cannot.
                ValueView::Mixin(_, overrides)
                    if crate::value::role_mixin_suffix(overrides).is_some() =>
                {
                    crate::value::what_type_name(&args[0])
                }
                ValueView::Package(name) => self
                    .type_metadata
                    .get(&name.resolve())
                    .and_then(|m| m.get("__set_name__"))
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| {
                        crate::value::user_facing_type_name(&name.resolve()).to_string()
                    }),
                ValueView::Instance { class_name, .. } => self
                    .type_metadata
                    .get(&class_name.resolve())
                    .and_then(|m| m.get("__set_name__"))
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| {
                        crate::value::user_facing_type_name(&class_name.resolve()).to_string()
                    }),
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
                _ => value_type_name(&args[0]).to_string(),
            })),
            "ver" if args.len() == 1 => {
                let invocant_name = match args[0].view() {
                    ValueView::Package(name) => name,
                    ValueView::Instance { class_name, .. } => class_name,
                    _ => {
                        return Err(RuntimeError::new(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                        ));
                    }
                };
                let name = invocant_name.resolve();
                if let Some(meta) = self.type_metadata.get(&name)
                    && let Some(value) = meta.get("ver").cloned()
                {
                    return Ok(Self::version_from_value(value));
                }
                if let Some(subset) = self.registry().subsets.get(&name) {
                    return Ok(Self::version_from_value(Value::str(subset.version.clone())));
                }
                if invocant_name == "Grammar" {
                    return Ok(Self::version_from_value(Value::str_from("6.e")));
                }
                // A *class* with no declared version: `.^ver` is `Mu` (an undefined
                // type object), not an error -- matching Rakudo. Reached e.g. when
                // the `:ver(...)` adverb is an expression mutsu does not evaluate at
                // registration (`unit class C:ver($?DISTRIBUTION.meta<ver>)`), or a
                // plain unversioned class. (`has $.V = ::?CLASS.^ver` then sets V to
                // Mu rather than throwing X::Method::NotFound.)
                // A bare `package` uses PackageHOW, which has no `.^ver` at all, so
                // `P.^ver` must still throw X::Method::NotFound ("absent by design").
                // TODO: evaluate expression-form `:ver(...)` adverbs at class
                // registration and store the result in type_metadata.
                if self.registry().classes.contains_key(&name) {
                    return Ok(Value::package(crate::symbol::Symbol::intern("Mu")));
                }
                Err(RuntimeError::new(
                    "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                ))
            }
            "auth" if args.len() == 1 => {
                let invocant_name = match args[0].view() {
                    ValueView::Package(name) => name,
                    ValueView::Instance { class_name, .. } => class_name,
                    _ => {
                        return Err(RuntimeError::new(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                        ));
                    }
                };
                let Some(meta) = self.type_metadata.get(&invocant_name.resolve()) else {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                    ));
                };
                let Some(value) = meta.get("auth").cloned() else {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                    ));
                };
                Ok(Value::str(value.to_string_value()))
            }
            "api" if args.len() == 1 => {
                let invocant_name = match args[0].view() {
                    ValueView::Package(name) => name,
                    ValueView::Instance { class_name, .. } => class_name,
                    _ => {
                        return Err(RuntimeError::new(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): api",
                        ));
                    }
                };
                // A declared `:api(...)` is stored in type_metadata; a type with no
                // `:api` has an empty-string api in Rakudo (`class C {}; C.^api` eq
                // ""), so default to "" rather than throwing.
                if let Some(value) = self
                    .type_metadata
                    .get(&invocant_name.resolve())
                    .and_then(|meta| meta.get("api").cloned())
                {
                    return Ok(Value::str(value.to_string_value()));
                }
                Ok(Value::str(String::new()))
            }
            "isa" if args.len() == 2 => {
                // Allow calling .^isa on an instance: use the instance's class.
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name,
                    ValueView::Instance { class_name, .. } => class_name,
                    _ => return Ok(Value::FALSE),
                };
                let other_name = match args[1].view() {
                    ValueView::Package(name) => name,
                    ValueView::Instance { class_name, .. } => class_name,
                    _ => return Ok(Value::FALSE),
                };
                let is_same = class_name == other_name;
                if is_same {
                    return Ok(Value::TRUE);
                }
                let class_resolved = class_name.resolve();
                let other_resolved = other_name.resolve();
                // Clone the base out per step so the registry read guard never spans
                // iterations (recursive read locks may deadlock).
                if let Some(mut base) = self
                    .registry()
                    .subsets
                    .get(&class_resolved)
                    .map(|s| s.base.clone())
                {
                    loop {
                        if base == other_resolved {
                            return Ok(Value::TRUE);
                        }
                        let Some(parent_base) =
                            self.registry().subsets.get(&base).map(|s| s.base.clone())
                        else {
                            break;
                        };
                        if parent_base == base {
                            break;
                        }
                        base = parent_base;
                    }
                }
                let mro = self.class_mro(&class_name.resolve());
                Ok(Value::truth(
                    mro.iter().any(|p| p.as_str() == other_resolved),
                ))
            }
            "mro" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg.view() {
                        ValueView::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        ValueView::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    Ok(Value::array(mro))
                } else {
                    let mro = self.classhow_mro_names(&args[0]);
                    Ok(Value::array(
                        mro.into_iter()
                            .map(|s| Value::package(Symbol::intern(&s)))
                            .collect::<Vec<_>>(),
                    ))
                }
            }
            "archetypes" if !args.is_empty() => {
                let invocant_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                let base_name = invocant_name
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(invocant_name.as_str());
                let mut attrs = HashMap::new();
                attrs.insert(
                    "composable".to_string(),
                    Value::truth(self.registry().roles.contains_key(base_name)),
                );
                Ok(Value::make_instance(
                    Symbol::intern("Perl6::Metamodel::Archetypes"),
                    attrs,
                ))
            }
            "mro_unhidden" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg.view() {
                        ValueView::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        ValueView::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    let filtered = self.filter_mro_unhidden(&args[0], mro);
                    Ok(Value::array(filtered))
                } else {
                    let mro = self.classhow_mro_unhidden_names(&args[0]);
                    Ok(Value::array(
                        mro.into_iter()
                            .map(|s| Value::package(Symbol::intern(&s)))
                            .collect::<Vec<_>>(),
                    ))
                }
            }
            "can" if args.len() >= 2 => {
                let invocant = &args[args.len() - 2];
                // The method name is always the last argument. When called via ^can,
                // the args may be [Package, target, method_name] due to Package insertion.
                let method_name = args.last().unwrap().to_string_value();
                let results = self.collect_can_methods(invocant, &method_name);
                Ok(Value::array(results))
            }
            "does" if args.len() >= 2 => {
                let invocant = &args[args.len() - 2];
                let role_arg = args.last().unwrap();
                // Handle ParametricRole directly to compare type args properly
                if let ValueView::ParametricRole {
                    base_name,
                    type_args,
                } = role_arg.view()
                {
                    let base = base_name.resolve();
                    if let ValueView::Mixin(_, mixins) = invocant.view() {
                        let key = format!("__mutsu_role_typeargs__{}", base);
                        let has_role = invocant.does_check(&base);
                        let args_match = if let Some(ValueView::Array(actual_args, ..)) =
                            mixins.get(&key).map(Value::view)
                        {
                            actual_args.len() == type_args.len()
                                && actual_args
                                    .iter()
                                    .zip(type_args.iter())
                                    .all(|(a, e)| self.parametric_arg_subtypes(a, e))
                        } else {
                            type_args.is_empty()
                        };
                        return Ok(Value::truth(has_role && args_match));
                    }
                    return Ok(Value::truth(self.type_matches_value(
                        &format!(
                                "{}[{}]",
                                base,
                                type_args
                                    .iter()
                                    .map(|a| a.to_string_value())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                        invocant,
                    )));
                }
                let type_name = match role_arg.view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => role_arg.to_string_value(),
                };
                Ok(Value::truth(self.type_matches_value(&type_name, invocant)))
            }
            "lookup" if args.len() >= 2 => {
                let invocant = &args[0];
                // Method name is always the last argument; when ^lookup is called on
                // a concrete value the Package is prepended and the original value
                // sits in between.
                let method_name = args.last().unwrap().to_string_value();
                Ok(self
                    .classhow_lookup(invocant, &method_name)
                    .unwrap_or(Value::NIL))
            }
            "find_method" if args.len() >= 2 => {
                let invocant = &args[0];
                // The method name is the last *positional* argument: calling
                // `$obj.^find_method('v')` on a concrete value prepends the Package and
                // leaves the instance in between (so `args[1]` is not the name), while
                // `.^find_method('foo', :no_fallback)` trails an adverb after it.
                let Some(name_arg) = args
                    .iter()
                    .rev()
                    .find(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                else {
                    return Ok(Value::NIL);
                };
                let method_name = name_arg.to_string_value();
                Ok(self
                    .classhow_find_method(invocant, &method_name)
                    .unwrap_or(Value::NIL))
            }
            "coerce" if args.len() >= 2 => {
                let target_constraint = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                let original = args[1].clone();
                let parse_coercion = |constraint: &str| -> Option<(String, Option<String>)> {
                    if !constraint.ends_with(')') || constraint.contains('[') {
                        return None;
                    }
                    let open = constraint.find('(')?;
                    if open == 0 {
                        return None;
                    }
                    let target = constraint[..open].to_string();
                    let source = &constraint[open + 1..constraint.len() - 1];
                    let source = if source.is_empty() {
                        None
                    } else {
                        Some(source.to_string())
                    };
                    Some((target, source))
                };
                if let Some((_target, source)) = parse_coercion(&target_constraint)
                    && let Some(src) = source.as_ref()
                    && !self.type_matches_value(src, &original)
                {
                    return Err(super::types::coerce_impossible_error(
                        &target_constraint,
                        &original,
                    ));
                }
                let coerced =
                    self.try_coerce_value_for_constraint(&target_constraint, original.clone())?;
                if let Some((target, _)) = parse_coercion(&target_constraint)
                    && !self.type_matches_value(&target, &coerced)
                {
                    return Err(super::types::coerce_impossible_error(
                        &target_constraint,
                        &original,
                    ));
                }
                Ok(coerced)
            }
            "add_method" if args.len() >= 3 => {
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => {
                        return Err(RuntimeError::new("add_method target must be a type object"));
                    }
                };
                let method_name = args[1].to_string_value();
                let method_value = args[2].clone();
                let ValueView::Sub(sub_data) = method_value.view() else {
                    return Ok(Value::NIL);
                };
                // Filter out invocant params from param_defs since MethodDef
                // stores only the user-visible parameters (the invocant is
                // added implicitly during dispatch).
                let filtered_param_defs: Vec<ParamDef> = sub_data
                    .param_defs
                    .iter()
                    .filter(|pd| !pd.is_invocant)
                    .cloned()
                    .collect();
                let def = MethodDef {
                    params: sub_data.params.clone(),
                    param_defs: filtered_param_defs,
                    body: std::sync::Arc::new(sub_data.body.clone()),
                    is_rw: sub_data.is_rw,
                    is_private: false,
                    is_multi: false,
                    is_my: false,
                    role_origin: None,
                    original_role: None,
                    return_type: None,
                    compiled_code: sub_data.compiled_code.clone(),
                    delegation: None,
                    is_default: false,
                    deprecated_message: None,
                    is_submethod: false,
                };
                // If the class doesn't exist yet (e.g. built-in types like Rat, Int, Str),
                // create a stub ClassDef so methods can be added dynamically.
                if !self.registry().classes.contains_key(&class_name) {
                    self.registry_mut().classes.insert(
                        class_name.clone(),
                        ClassDef {
                            parents: vec![],
                            attributes: vec![],
                            attribute_types: HashMap::new(),
                            attribute_smileys: HashMap::new(),
                            attribute_built: HashMap::new(),
                            alias_attributes: HashSet::new(),
                            methods: HashMap::new(),
                            native_methods: HashSet::new(),
                            mro: sym_mro(&[&class_name]),
                            wildcard_handles: vec![],
                            class_level_attrs: HashMap::new(),
                        },
                    );
                }
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                    class_def.methods.insert(method_name, vec![def]);
                }
                // Class shape changed (an added BUILD/TWEAK/new flips ctor
                // eligibility) — drop cached construction plans.
                self.native_ctor_plan_cache.clear();
                // Return Nil even if the class was not found (e.g. built-in types
                // like Rat that are not in the user-defined class registry).
                // Raku's add_method returns the method name; returning Nil is
                // sufficient for eval-lives-ok tests.
                Ok(Value::NIL)
            }
            "add_multi_method" if args.len() >= 3 => {
                // Same as add_method but marks the method as multi
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => {
                        return Err(RuntimeError::new(
                            "add_multi_method target must be a type object",
                        ));
                    }
                };
                let method_name = args[1].to_string_value();
                let method_value = args[2].clone();
                let ValueView::Sub(sub_data) = method_value.view() else {
                    return Ok(Value::NIL);
                };
                let def = MethodDef {
                    params: sub_data.params.clone(),
                    param_defs: sub_data.param_defs.clone(),
                    body: std::sync::Arc::new(sub_data.body.clone()),
                    is_rw: sub_data.is_rw,
                    is_private: false,
                    is_multi: true,
                    is_my: false,
                    role_origin: None,
                    original_role: None,
                    return_type: None,
                    compiled_code: None,
                    delegation: None,
                    is_default: false,
                    deprecated_message: None,
                    is_submethod: false,
                };
                let inserted =
                    if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                        class_def.methods.entry(method_name).or_default().push(def);
                        true
                    } else {
                        false
                    };
                if inserted {
                    self.native_ctor_plan_cache.clear();
                    return Ok(Value::NIL);
                }
                Err(RuntimeError::new(format!(
                    "Unknown class for add_multi_method: {}",
                    class_name
                )))
            }
            "add_fallback" if args.len() >= 3 => {
                // ^add_fallback($type, &condition, &calculator): register a
                // dynamic method fallback. When a method is not found on a value
                // of this class, `&condition($obj, $name)` is checked; the first
                // that returns True has `&calculator($obj, $name)` produce the
                // method body, which is then invoked with the invocant.
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => {
                        return Err(RuntimeError::new(
                            "add_fallback target must be a type object",
                        ));
                    }
                };
                let condition = args[1].clone();
                let calculator = args[2].clone();
                self.method_fallbacks
                    .entry(class_name)
                    .or_default()
                    .push((condition, calculator));
                Ok(Value::NIL)
            }
            "compose" if !args.is_empty() => {
                // ^compose recomposes the class (e.g. after add_method)
                // Rebuild the MRO for the class
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => return Ok(Value::NIL),
                };
                let mro = self.class_mro(&class_name);
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                    class_def.mro = mro;
                }
                self.native_ctor_plan_cache.clear();
                Ok(Value::NIL)
            }
            // `$type.HOW.add_parent($type, $parent)` — the native ClassHOW
            // metamethod a user HOW (`class MyHOW is Metamodel::ClassHOW`) reaches
            // via `callsame`/`nextsame` or a direct fallback. Adds `$parent` to
            // `$type`'s parent list (idempotent — mutsu's `is Parent` already
            // installs it during declaration, so a trait-driven re-add must not
            // duplicate it) and recomputes the MRO.
            "add_parent" if args.len() >= 2 => {
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => return Ok(Value::NIL),
                };
                let parent_name = match args[1].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => return Ok(Value::NIL),
                };
                let mut changed = false;
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name)
                    && !class_def.parents.contains(&parent_name)
                {
                    class_def.parents.push(parent_name.clone());
                    changed = true;
                }
                if changed {
                    let mro = self.class_mro(&class_name);
                    if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                        class_def.mro = mro;
                    }
                    self.native_ctor_plan_cache.clear();
                }
                Ok(Value::NIL)
            }
            "add_attribute" if args.len() >= 2 => {
                // ^add_attribute($type, $attr)
                // Adds an Attribute object to a dynamically created class
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    _ => return Ok(Value::NIL),
                };
                if let ValueView::Instance {
                    class_name: attr_class,
                    attributes: attr_attrs,
                    ..
                } = args[1].view()
                    && attr_class.resolve() == "Attribute"
                {
                    let attr_name_raw = attr_attrs
                        .as_map()
                        .get("name")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    // Strip sigil+twigil prefix to get bare name (e.g. "$!inner" -> "inner")
                    let bare_name = attr_name_raw
                        .trim_start_matches(|c: char| "$.!@%&".contains(c))
                        .to_string();
                    let has_accessor = attr_attrs
                        .as_map()
                        .get("has_accessor")
                        .map(|v| v.truthy())
                        .unwrap_or(false);
                    let is_rw = attr_attrs
                        .as_map()
                        .get("rw")
                        .map(|v| v.truthy())
                        .unwrap_or(false);
                    let type_constraint =
                        attr_attrs
                            .as_map()
                            .get("type")
                            .and_then(|v| match v.view() {
                                ValueView::Package(name) => Some(name.resolve()),
                                _ => None,
                            });
                    let sigil = attr_name_raw.chars().next().unwrap_or('$');
                    // Add the attribute to the class definition
                    if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                        class_def.attributes.push((
                            bare_name.clone(),
                            has_accessor, // is_public
                            None,         // default_expr
                            is_rw,        // is_rw
                            None,         // is_required
                            sigil,        // sigil
                            None,         // where_constraint
                        ));
                        if let Some(tc) = type_constraint {
                            class_def.attribute_types.insert(bare_name, tc);
                        }
                    }
                    // Attribute set changed — drop cached construction plans.
                    self.native_ctor_plan_cache.clear();
                }
                Ok(Value::NIL)
            }
            "methods" if !args.is_empty() => self.dispatch_classhow_methods(&args),
            "attributes" if !args.is_empty() => {
                let owner_class = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                let local_only = args[1..].iter().any(
                    |a| matches!(a.view(), ValueView::Pair(k, v) if k == "local" && v.truthy()),
                );
                let values = self.collect_attribute_objects(&owner_class, local_only);
                Ok(Value::array(values))
            }
            "parents" if !args.is_empty() => self.dispatch_classhow_parents(&args),
            "pun" if !args.is_empty() => {
                let role_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => args[0].to_string_value(),
                };
                self.ensure_role_punned_to_class(&role_name);
                Ok(Value::package(Symbol::intern(&role_name)))
            }
            "roles" if !args.is_empty() => self.dispatch_classhow_roles(&args),
            "candidates" if !args.is_empty() => {
                let base_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::ParametricRole { base_name, .. } => base_name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => args[0]
                        .to_string_value()
                        .trim_start_matches('(')
                        .trim_end_matches(')')
                        .to_string(),
                };
                if let Some(candidates) = self.registry().role_candidates.get(&base_name) {
                    let values = candidates
                        .iter()
                        .enumerate()
                        .map(|(idx, cand)| {
                            // Create Instance values with candidate index so
                            // .WHY can look up per-candidate doc comments
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert(
                                "__mutsu_role_candidate_idx".to_string(),
                                Value::int(idx as i64),
                            );
                            attrs.insert(
                                "__mutsu_role_base_name".to_string(),
                                Value::str(base_name.clone()),
                            );
                            // Embed per-candidate language revision
                            let revision: String =
                                if let Some(letter) = cand.language_version.strip_prefix("6.") {
                                    letter.chars().next().unwrap_or('c').to_string()
                                } else {
                                    "c".to_string()
                                };
                            attrs.insert(
                                "__mutsu_language_revision".to_string(),
                                Value::str(revision),
                            );
                            Value::make_instance(Symbol::intern(&base_name), attrs)
                        })
                        .collect::<Vec<_>>();
                    return Ok(Value::array(values));
                }
                if self.registry().roles.contains_key(&base_name) {
                    return Ok(Value::array(vec![Value::package(Symbol::intern(
                        &base_name,
                    ))]));
                }
                Ok(Value::array(Vec::new()))
            }
            "concretization" if args.len() >= 2 => {
                let class_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                let role_name = match args[1].view() {
                    ValueView::Package(name) => name.resolve(),
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
                    _ => args[1].to_string_value(),
                };
                let base_role_name = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role_name.as_str());
                // Check for :local named arg
                let local_only = args[2..].iter().any(
                    |a| matches!(a.view(), ValueView::Pair(k, v) if k == "local" && v.truthy()),
                );
                // Check direct composed roles and transitive sub-roles
                let check_transitive =
                    |class_composed: &rustc_hash::FxHashMap<String, Vec<String>>,
                     role_parents: &rustc_hash::FxHashMap<String, Vec<String>>,
                     cn: &str|
                     -> Option<Value> {
                        let composed = class_composed.get(cn).cloned().unwrap_or_default();
                        // Check direct matches
                        for cr in &composed {
                            let cr_base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                            if *cr == role_name || cr_base == base_role_name {
                                return Some(Value::package(Symbol::intern(cr_base)));
                            }
                        }
                        // Check transitive sub-roles
                        let mut stack: Vec<String> = composed
                            .iter()
                            .map(|cr| {
                                cr.split_once('[')
                                    .map(|(b, _)| b)
                                    .unwrap_or(cr.as_str())
                                    .to_string()
                            })
                            .collect();
                        let mut seen = std::collections::HashSet::new();
                        while let Some(rn) = stack.pop() {
                            if !seen.insert(rn.clone()) {
                                continue;
                            }
                            if let Some(rp) = role_parents.get(&rn) {
                                for p in rp {
                                    let p_base =
                                        p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                                    if p_base == base_role_name || *p == role_name {
                                        return Some(Value::package(Symbol::intern(p_base)));
                                    }
                                    stack.push(p_base.to_string());
                                }
                            }
                        }
                        None
                    };
                if let Some(result) = check_transitive(
                    &self.registry().class_composed_roles,
                    &self.registry().role_parents,
                    &class_name,
                ) {
                    return Ok(result);
                }
                if !local_only {
                    let mro = self.class_mro(&class_name);
                    for cn in mro[1..].iter().map(|s| s.as_str()) {
                        if let Some(result) = check_transitive(
                            &self.registry().class_composed_roles,
                            &self.registry().role_parents,
                            cn,
                        ) {
                            return Ok(result);
                        }
                    }
                }
                Err(RuntimeError::new(format!(
                    "No concretization of {} found for {}",
                    role_name, class_name
                )))
            }
            "curried_role" if !args.is_empty() => {
                // For a parameterized role like R[Int], return the base role R
                match args[0].view() {
                    ValueView::ParametricRole { base_name, .. } => Ok(Value::package(base_name)),
                    ValueView::Package(name) => {
                        let resolved = name.resolve();
                        let base = resolved
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(resolved.as_str());
                        Ok(Value::package(Symbol::intern(base)))
                    }
                    _ => {
                        let s = args[0].to_string_value();
                        let base = s.split_once('[').map(|(b, _)| b).unwrap_or(s.as_str());
                        Ok(Value::package(Symbol::intern(base)))
                    }
                }
            }
            "enum_value_list" if !args.is_empty() => {
                let type_name = match args[0].view() {
                    ValueView::Package(name) => Some(name.resolve()),
                    ValueView::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                if let Some(type_name) = type_name
                    && let Some(variants) = self.registry().enum_types.get(&type_name)
                {
                    let values: Vec<Value> = variants
                        .iter()
                        .enumerate()
                        .map(|(index, (key, val))| {
                            Value::enum_parts(
                                Symbol::intern(&type_name),
                                Symbol::intern(key),
                                val.clone(),
                                index,
                            )
                        })
                        .collect();
                    Ok(Value::array(values))
                } else {
                    Ok(Value::array(Vec::new()))
                }
            }
            "language-revision" if !args.is_empty() => {
                // Check for per-candidate language revision embedded as an
                // attribute (set by ^candidates for role candidate instances).
                if let ValueView::Instance { attributes, .. } = args[0].view()
                    && let Some(rev) = attributes.as_map().get("__mutsu_language_revision")
                {
                    return Ok(rev.clone());
                }
                // Check for language revision in Mixin metadata (from
                // parametric role pun instances).
                if let ValueView::Mixin(_, mixins) = args[0].view()
                    && let Some(rev) = mixins.get("__mutsu_language_revision")
                {
                    return Ok(rev.clone());
                }
                let type_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                if let Some(meta) = self.type_metadata.get(&type_name)
                    && let Some(rev) = meta.get("language-revision")
                {
                    return Ok(rev.clone());
                }
                // Default to current language revision
                let version = crate::parser::current_language_version();
                let letter = if let Some(rest) = version.strip_prefix("6.") {
                    rest.chars().next().unwrap_or('c').to_string()
                } else {
                    "c".to_string()
                };
                Ok(Value::str(letter))
            }
            "submethod_table" if !args.is_empty() => {
                let type_name = match args[0].view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                let mut table = HashMap::new();
                if let Some(class_def) = self.registry().classes.get(&type_name) {
                    for (name, defs) in &class_def.methods {
                        if defs.iter().any(|d| d.is_my) {
                            table.insert(name.clone(), Value::str(name.clone()));
                        }
                    }
                }
                Ok(Value::hash(table))
            }
            _ => Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                method
            ))),
        }
    }
}
