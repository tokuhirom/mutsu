//! `Metamodel::*HOW` methods, part 6 (ADR-11276 slice 3G): one Interpreter method per
//! metamethod. The rows that reach them are `method_table/ctors_mop/class_how.rs`.

use super::*;

impl Interpreter {
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_concretization(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let class_name = self.mop_receiver_owner(&args[0]);
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

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_curried_role(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
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

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_language_revision(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
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
            let type_name = self.mop_receiver_owner(&args[0]);
            if let Some(meta) = self.types.type_metadata.get(&type_name)
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

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_method_table(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let type_name = match args[0].view() {
                ValueView::RakuAst(node) => node.class.printed_name().to_string(),
                _ => self.mop_receiver_owner(&args[0]),
            };
            Ok(Value::hash(self.class_method_table(&type_name)))
    }

        // Cost: O(m), m = methods declared directly on the class.
        // `Metamodel::MethodContainer.method_names`: the local method
        // names (own methods, accessors, submethods; not inherited).
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_method_names(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let type_name = self.mop_receiver_owner(&args[0]);
            let mut names: Vec<String> = self
                .class_method_table(&type_name)
                .keys()
                .map(|k| k.to_string())
                .collect();
            // Submethods are in the submethod table, not the method table.
            let registry = self.registry();
            for name in registry.owner_method_names(&type_name) {
                let name = name.resolve();
                if !names.contains(&name.to_string())
                    && registry
                        .user_method_overloads(&type_name, &name)
                        .is_some_and(|defs| defs.iter().any(|d| d.is_my))
                {
                    names.push(name.to_string());
                }
            }
            Ok(Value::array(names.into_iter().map(Value::str).collect()))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_private_method_table(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let type_name = self.mop_receiver_owner(&args[0]);
            Ok(Value::hash(self.class_private_method_table(&type_name)))
    }

        // Cost: O(m), m = methods declared directly on the class.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_private_methods(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let type_name = self.mop_receiver_owner(&args[0]);
            Ok(self.class_private_methods(&type_name))
    }

        // `Metamodel::ClassHOW.roles_to_compose`: the roles a class
        // still has queued for the native composer to flatten in,
        // as opposed to `.^roles` (already-composed roles). A custom
        // `compose` override that runs before `callsame` (AttrX::Lazy's
        // `LazyAttributeContainerHOW.compose`, which checks this to warn
        // about a name collision with a not-yet-composed role) observes
        // it empty even once real composition has finished (verified
        // against `raku`: `role R {}; class C does R {}; say
        // C.^roles_to_compose` is `()`, same as an empty class) -- mutsu
        // has no intermediate "queued, not yet flattened" state to
        // report, so this always answers empty.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_roles_to_compose(&mut self, _args: Vec<Value>) -> Result<Value, RuntimeError> {
        Ok(Value::array(Vec::new()))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_submethod_table(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // ADR-0019 F4c-1: enumerate via the canonical reverse index
            // instead of `class_def.methods.keys()` (zero-mismatch
            // shadow-checked across the full local `t/` suite).
            let type_name = self.mop_receiver_owner(&args[0]);
            let mut table = ValueMap::default();
            let registry = self.registry();
            for name in registry.owner_method_names(&type_name) {
                let name = name.resolve();
                if registry
                    .user_method_overloads(&type_name, &name)
                    .is_some_and(|defs| defs.iter().any(|d| d.is_my))
                {
                    table.insert(name.clone(), Value::str(name));
                }
            }
            Ok(Value::hash(table))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_nativesize(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let type_name = self.mop_receiver_owner(&args[0]);
            if let Some(decl) = self.native_decl(&type_name) {
                return Ok(decl.nativesize.map_or(Value::NIL, Value::int));
            }
            match native_types::native_type_bits(&type_name) {
                Some(bits) => Ok(Value::int(i64::from(bits))),
                None => Err(RuntimeError::meta_method_not_found(
                    "nativesize",
                    &type_name,
                )),
            }
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_unsigned(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let type_name = self.mop_receiver_owner(&args[0]);
            if let Some(decl) = self.native_decl(&type_name) {
                return Ok(Value::int(i64::from(decl.unsigned)));
            }
            if native_types::native_type_bits(&type_name).is_some() {
                Ok(Value::int(i64::from(!native_types::is_signed_native(
                    &type_name,
                ))))
            } else {
                Err(RuntimeError::meta_method_not_found("unsigned", &type_name))
            }
    }
}
