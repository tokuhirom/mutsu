//! `Metamodel::*HOW` methods, part 5 (ADR-11276 slice 3G): one Interpreter method per
//! metamethod. The rows that reach them are `method_table/ctors_mop/class_how.rs`.

use super::methods_classhow_dispatch::{unwrap_method_instance_callable};
use super::*;

impl Interpreter {
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_add_multi_method(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
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
            let method_value = unwrap_method_instance_callable(&args[2]);
            let ValueView::Sub(sub_data) = method_value.view() else {
                return Ok(Value::NIL);
            };
            let captured_env = if sub_data.env.is_empty() {
                None
            } else {
                let mut env = sub_data.env.clone();
                env.insert("__mutsu_declared_method_capture".to_string(), Value::int(1));
                Some(env)
            };
            let def = MethodDef {
                syms: Default::default(),
                lexical_package: sub_data.package,
                params: sub_data.params.to_vec(),
                param_defs: sub_data.param_defs.to_vec(),
                body: sub_data.body.clone(),
                is_rw: sub_data.is_rw,
                is_raw: sub_data.is_raw,
                is_private: false,
                is_multi: true,
                is_my: false,
                role_origin: None,
                original_role: None,
                return_type: None,
                compiled_code: None,
                compiled_fns: None,
                delegation: None,
                is_default: false,
                deprecated_message: None,
                is_submethod: false,
                is_hidden_from_backtrace: false,
                captured_env,
                source_file: sub_data.source_file.clone(),
                role_param_bindings: None,
                nested_capture_index: None,
                captured_readonly: None,
                routine_cell: Default::default(),
            };
            // `^add_multi_method` must still *error* for an unregistered
            // class -- existence keys off `classes.contains_key`, not the
            // method table (ADR-0019 F4c design note (0)(iii)).
            if self.registry().classes.contains_key(&class_name) {
                self.registry_mut().push_user_method(
                    Symbol::intern(&class_name),
                    Symbol::intern(&method_name),
                    def,
                );
                self.caches.native_ctor_plan_cache.clear();
                return Ok(Value::NIL);
            }
            Err(RuntimeError::new(format!(
                "Unknown class for add_multi_method: {}",
                class_name
            )))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_add_fallback(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
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
            crate::runtime::cow_table_mut(&mut self.types.method_fallbacks)
                .entry(class_name)
                .or_default()
                .push((condition, calculator));
            Ok(Value::NIL)
    }

        // Cost: O(d), d = target type's MRO depth.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_compose(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // ^compose recomposes the class (e.g. after add_method)
            // Rebuild the MRO for the class
            let class_name = match args[0].view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                _ => return Ok(Value::NIL),
            };
            // A `Metamodel::GrammarHOW.new_type` type given no parent
            // composes as a `Grammar` (rakudo's GrammarHOW default parent
            // type), so `.parse` and `~~ Grammar` work on it.
            let is_grammar_how = self
                .registry()
                .declared_native_how
                .get(&class_name)
                .is_some_and(|how| how == "Perl6::Metamodel::GrammarHOW");
            if is_grammar_how
                && let Some(class_def) = self.registry_mut().classes.get_mut(&class_name)
                && class_def.parents.is_empty()
            {
                class_def.parents.push("Grammar".to_string());
                class_def.mro = [].into();
            }
            let mro = self.class_mro(&class_name);
            if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                class_def.mro = mro;
            }
            // This is the native step a custom `compose` hook's
            // `callsame` reaches: from here on the class's auto-generated
            // accessors count as installed (`classes_composing_accessors`).
            self.types.classes_composing_accessors.remove(&class_name);
            self.caches.native_ctor_plan_cache.clear();
            // Rakudo returns the composed type object. MOP clients use
            // that result directly (for example Test::Mock calls
            // `$mocker.HOW.compose($mocker).CREATE`), so returning Nil
            // loses the freshly composed type and turns the following
            // call into an Any dispatch.
            Ok(args[0].clone())
    }

        // `$type.HOW.add_parent($type, $parent)` — the native ClassHOW
        // metamethod a user HOW (`class MyHOW is Metamodel::ClassHOW`) reaches
        // via `callsame`/`nextsame` or a direct fallback. Adds `$parent` to
        // `$type`'s parent list (idempotent — mutsu's `is Parent` already
        // installs it during declaration, so a trait-driven re-add must not
        // duplicate it) and recomputes the MRO.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_add_parent(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
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
                class_def.parents.push(parent_name);
                changed = true;
            }
            if changed {
                // `new_type` starts with an eagerly cached MRO. Adding a
                // parent must invalidate that cache before recomputing it,
                // otherwise the new parent is invisible to `^mro` and
                // role checks on instances of the dynamic type.
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                    class_def.mro = [].into();
                }
                let mro = self.class_mro(&class_name);
                if let Some(class_def) = self.registry_mut().classes.get_mut(&class_name) {
                    class_def.mro = mro;
                }
                self.caches.native_ctor_plan_cache.clear();
            }
            Ok(Value::NIL)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_add_attribute(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
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
                let attr_map = attr_attrs.as_map();
                let is_rw = attr_map
                    .get("rw")
                    .or_else(|| attr_map.get("is_rw"))
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
                    class_def.attributes.push(ClassAttributeDef {
                        name: bare_name.clone(),
                        is_public: has_accessor,
                        default: None,
                        captured_env: None,
                        captured_unit: None,
                        declaring_package: None,
                        is_rw,
                        is_required: None,
                        sigil,
                        type_constraint: type_constraint.clone(),
                        where_constraint: None,
                        declared_shape: None,
                        source_line: None,
                        source_file: None,
                        default_is_seed: false,
                    });
                    if let Some(tc) = type_constraint {
                        class_def.attribute_types.insert(bare_name, tc);
                    }
                }
                // Attribute set changed — drop cached construction plans.
                self.caches.native_ctor_plan_cache.clear();
            }
            Ok(Value::NIL)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_methods(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.dispatch_classhow_methods(&args)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_attributes(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let owner_class = self.mop_receiver_owner(&args[0]);
            let local_only = args[1..].iter().any(
                |a| matches!(a.view(), ValueView::Pair(k, v) if k == "local" && v.truthy()),
            );
            let values = self.collect_attribute_objects(&owner_class, local_only);
            Ok(Value::array(values))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_attribute_table(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        Ok(self.classhow_attribute_table(&args[0]))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_get_attribute_for_usage(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            self.classhow_get_attribute_for_usage(&args[0], &args[1])
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_parents(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.dispatch_classhow_parents(&args)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_pun(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // A curried role (`R[Int,Str].^pun`) puns to the same class its
            // `.new` constructs through, so an instance's `.WHAT` is `=:=`
            // its pun (Rake's tests check exactly that).
            if let ValueView::ParametricRole {
                base_name,
                type_args,
            } = args[0].view()
                && let Some(punned) =
                    self.ensure_parametric_role_pun_class(&base_name.resolve(), type_args)?
            {
                return Ok(Value::package(Symbol::intern(&punned)));
            }
            let role_name = match args[0].view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => args[0].to_string_value(),
            };
            self.punned_role_type_object(&role_name)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_roles(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.dispatch_classhow_roles(&args)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_candidates(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
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

    /// Whether `candidates` applies to these arguments (the arm's own guard).
    // Cost: O(1).
    pub(crate) fn mop_candidates_applies(&self, args: &[Value]) -> bool {
        self.is_role_reference_value(&args[0])
    }
}
