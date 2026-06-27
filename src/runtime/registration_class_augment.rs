use super::registration_class::apply_resolved_handles;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn augment_role_error(&self, name: &str) -> RuntimeError {
        // Built-in roles that are closed but not present in the user registry.
        const BUILTIN_ROLES: &[&str] = &[
            "Positional",
            "Associative",
            "Callable",
            "Iterable",
            "Numeric",
            "Real",
            "Stringy",
            "Mixy",
            "Setty",
            "Baggy",
            "Blob",
            "Buf",
        ];
        let exists = self.registry().roles.contains_key(name)
            || self.registry().classes.contains_key(name)
            || crate::runtime::utils::is_known_type_constraint(name)
            || BUILTIN_ROLES.contains(&name);
        if exists {
            let message = format!("Cannot augment {} because it is closed", name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(
                crate::symbol::Symbol::intern("X::Syntax::Augment::Illegal"),
                attrs,
            );
            let mut err = RuntimeError::new(message);
            err.exception = Some(Box::new(ex));
            err
        } else {
            let message = format!("You tried to augment role {}, but it does not exist", name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(message.clone()));
            attrs.insert("package-kind".to_string(), Value::str("role".to_string()));
            attrs.insert("package".to_string(), Value::str(name.to_string()));
            let ex = Value::make_instance(
                crate::symbol::Symbol::intern("X::Augment::NoSuchType"),
                attrs,
            );
            let mut err = RuntimeError::new(message);
            err.exception = Some(Box::new(ex));
            err
        }
    }

    /// Augment an existing class by adding methods (and attributes) from the body.
    /// This implements `augment class ClassName { ... }` (monkey-patching).
    pub(crate) fn augment_class(&mut self, name: &str, body: &[Stmt]) -> Result<(), RuntimeError> {
        self.clear_private_zeroarg_method_cache();
        // Check if the class exists (user-defined or builtin)
        let is_builtin = !self.registry().classes.contains_key(name);
        if is_builtin {
            // For builtin types, create a minimal class def so we can add methods
            const BUILTIN_TYPES: &[&str] = &[
                "Mu",
                "Any",
                "Cool",
                "Int",
                "Num",
                "Str",
                "Bool",
                "Rat",
                "FatRat",
                "Complex",
                "Array",
                "Hash",
                "List",
                "Map",
                "Set",
                "Bag",
                "Mix",
                "Range",
                "Pair",
                "IO",
                "IO::Path",
                "IO::Handle",
                "Regex",
                "Match",
                "Junction",
                "Exception",
                "Failure",
                "Version",
                "Nil",
                "Block",
                "Code",
                "Routine",
                "Sub",
                "Method",
                "Seq",
                "Slip",
                "Whatever",
                "WhateverCode",
                "HyperWhatever",
                "Callable",
                "Numeric",
                "Real",
                "Stringy",
                "Positional",
                "Associative",
                "Order",
                "Endian",
                "Proc",
                "Capture",
            ];
            if !BUILTIN_TYPES.contains(&name) {
                let message = format!("You tried to augment class {}, but it does not exist", name);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("message".to_string(), Value::str(message.clone()));
                attrs.insert("package-kind".to_string(), Value::str("class".to_string()));
                attrs.insert("package".to_string(), Value::str(name.to_string()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Augment::NoSuchType"),
                    attrs,
                );
                let mut err = RuntimeError::new(message);
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
            // Create a minimal entry for the builtin type
            self.registry_mut()
                .classes
                .entry(name.to_string())
                .or_default();
        }

        let saved_package = self.current_package();
        self.set_current_package(name.to_string());

        // Process body statements and add methods/attributes to the existing class
        let flattened_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        for stmt in flattened_body {
            match stmt {
                Stmt::MethodDecl {
                    name: method_name,
                    name_expr,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                    is_my,
                    is_submethod,
                    return_type,
                    is_default_candidate,
                    ..
                } => {
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.resolve()
                    };
                    let mut effective_param_defs =
                        Self::effective_method_param_defs(param_defs, false);
                    // Auto-detect @_ usage in methods without explicit signatures
                    if param_defs.is_empty() {
                        let (use_positional, _) = Self::auto_signature_uses(method_body);
                        if use_positional && !effective_param_defs.iter().any(|pd| pd.name == "@_")
                        {
                            let insert_pos = effective_param_defs
                                .iter()
                                .position(|pd| pd.name.starts_with('%') && pd.slurpy)
                                .unwrap_or(effective_param_defs.len());
                            effective_param_defs.insert(
                                insert_pos,
                                ParamDef {
                                    name: "@_".to_string(),
                                    default: None,
                                    multi_invocant: true,
                                    required: false,
                                    named: false,
                                    slurpy: true,
                                    double_slurpy: false,
                                    onearg: false,
                                    sigilless: false,
                                    type_constraint: None,
                                    literal_value: None,
                                    sub_signature: None,
                                    where_constraint: None,
                                    traits: Vec::new(),
                                    optional_marker: false,
                                    outer_sub_signature: None,
                                    code_signature: None,
                                    is_invocant: false,
                                    shape_constraints: None,
                                },
                            );
                        }
                    }
                    let effective_params: Vec<String> = effective_param_defs
                        .iter()
                        .map(|p| p.name.clone())
                        .collect();
                    let def = MethodDef {
                        params: effective_params,
                        param_defs: effective_param_defs,
                        body: std::sync::Arc::new(method_body.clone()),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        is_multi: *multi,
                        is_my: *is_my,
                        role_origin: None,
                        original_role: None,
                        return_type: return_type.clone(),
                        compiled_code: None,
                        delegation: None,
                        is_default: *is_default_candidate,
                        deprecated_message: None,
                        is_submethod: *is_submethod,
                    };
                    if let Some(class_def) = self.registry_mut().classes.get_mut(name) {
                        if *multi {
                            class_def
                                .methods
                                .entry(resolved_method_name)
                                .or_default()
                                .push(def);
                        } else {
                            // Check for duplicate non-multi method definition.
                            // Only error if the existing method was defined in
                            // this class (not composed from a role).
                            if let Some(existing) = class_def.methods.get(&resolved_method_name) {
                                let all_from_role =
                                    existing.iter().all(|m| m.role_origin.is_some());
                                if !all_from_role {
                                    return Err(RuntimeError::new(format!(
                                        "Package '{}' already has a method '{}' (did you mean to declare a multi method?)",
                                        name, resolved_method_name
                                    )));
                                }
                            }
                            class_def.methods.insert(resolved_method_name, vec![def]);
                        }
                    }
                }
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    is_rw,
                    is_readonly: _,
                    type_constraint,
                    type_smiley: _,
                    is_required,
                    sigil,
                    handles,
                    where_constraint,
                    is_alias,
                    is_our: _,
                    is_my: _,
                    is_default: _,
                    is_type: _,
                    deprecated_message: _,
                    is_built: _,
                    unknown_traits: _,
                } => {
                    let attr_name_str = attr_name.resolve();
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name_str)
                    } else {
                        format!("!{}", attr_name_str)
                    };
                    // Resolve handles before taking mutable borrow on class_def
                    let resolved = self.resolve_handle_specs_to_names(handles, &attr_var_name);
                    if let Some(class_def) = self.registry_mut().classes.get_mut(name) {
                        class_def.attributes.push((
                            attr_name_str.clone(),
                            *is_public,
                            default.clone(),
                            *is_rw,
                            is_required.clone(),
                            *sigil,
                            where_constraint.as_ref().map(|wc| wc.as_ref().clone()),
                        ));
                        if *is_alias {
                            class_def.alias_attributes.insert(attr_name_str.clone());
                        }
                        if let Some(tc) = type_constraint {
                            class_def
                                .attribute_types
                                .insert(attr_name_str.clone(), tc.clone());
                        }
                        apply_resolved_handles(
                            &resolved,
                            &mut class_def.methods,
                            &mut class_def.wildcard_handles,
                        );
                    }
                }
                // Sub declarations in the class body should persist across scope
                // boundaries so that methods can call them. Use run_block_raw
                // instead of eval_block_value to avoid function registry rollback.
                Stmt::SubDecl { .. } => {
                    let _ = self.run_block_raw(std::slice::from_ref(stmt));
                }
                other => {
                    let _ = self.eval_block_value(std::slice::from_ref(other));
                }
            }
        }

        self.set_current_package(saved_package);
        Ok(())
    }

    pub(crate) fn register_subset_decl(
        &mut self,
        name: &str,
        base: &str,
        predicate: Option<&Expr>,
        version: &str,
    ) {
        // When the predicate is `* ~~ <expr>` (Whatever on LHS of SmartMatch),
        // the parser doesn't wrap it as WhateverCode (to avoid breaking other
        // smartmatch semantics). Convert it here to a Lambda so the subset
        // check correctly evaluates `$_ ~~ <expr>` against the candidate value.
        let predicate = predicate.map(|pred| {
            if let Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::SmartMatch,
                right,
            } = pred
                && matches!(left.as_ref(), Expr::Whatever)
            {
                return Expr::Lambda {
                    param: "_".to_string(),
                    body: vec![Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Var("_".to_string())),
                        op: crate::token_kind::TokenKind::SmartMatch,
                        right: right.clone(),
                    })],
                    is_whatever_code: true,
                };
            }
            pred.clone()
        });
        // Drop any cached compiled predicate for this name so a redeclaration
        // recompiles against the new predicate (see `subset_predicate_cache`).
        self.subset_predicate_cache.remove(name);
        self.registry_mut().subsets.insert(
            name.to_string(),
            SubsetDef {
                base: base.to_string(),
                predicate,
                version: version.to_string(),
            },
        );
        self.env
            .insert(name.to_string(), Value::Package(Symbol::intern(name)));
    }

    pub(crate) fn register_cunion_class(&mut self, name: &str) {
        self.clear_private_zeroarg_method_cache();
        self.registry_mut().cunion_classes.insert(name.to_string());
    }

    pub(crate) fn construct_cunion_instance(
        &mut self,
        class_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::native_types::native_int_bounds;

        // Collect class attributes with their types
        let class_attrs = self.collect_class_attributes(class_name);

        // Parse named args
        let mut named_args: HashMap<String, Value> = HashMap::new();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                named_args.insert(key.clone(), *value.clone());
            }
        }

        // Find the widest provided value and convert to bytes
        let mut max_bytes = 0u32;
        let mut raw_value: u64 = 0;

        for (attr_name, _is_public, _default, _is_rw, _, _, _) in &class_attrs {
            if let Some(val) = named_args.get(attr_name) {
                let int_val = match val {
                    Value::Int(i) => *i as u64,
                    Value::BigInt(n) => {
                        use num_traits::ToPrimitive;
                        n.to_u64().unwrap_or(0)
                    }
                    _ => 0,
                };
                raw_value = int_val;
                // Find the type of this attribute to determine byte width
                if let Some(type_constraint) = self.get_attr_type_constraint(class_name, attr_name)
                {
                    let byte_width = Self::native_type_byte_width(&type_constraint);
                    if byte_width > max_bytes {
                        max_bytes = byte_width;
                    }
                }
            }
        }

        // Build attributes from shared bytes
        let bytes = raw_value.to_le_bytes();
        let mut attrs = HashMap::new();
        for (attr_name, _is_public, default, _is_rw, _, _, _) in &class_attrs {
            if let Some(type_constraint) = self.get_attr_type_constraint(class_name, attr_name) {
                let byte_width = Self::native_type_byte_width(&type_constraint);
                let val = match byte_width {
                    1 => Value::Int(bytes[0] as i64),
                    2 => Value::Int(u16::from_le_bytes([bytes[0], bytes[1]]) as i64),
                    4 => Value::Int(
                        u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as i64
                    ),
                    8 => {
                        let v = u64::from_le_bytes(bytes);
                        if let Some((_min, _max)) = native_int_bounds(&type_constraint) {
                            if type_constraint.starts_with('u') || type_constraint == "byte" {
                                // Unsigned: store as BigInt if needed
                                if v > i64::MAX as u64 {
                                    Value::bigint(num_bigint::BigInt::from(v as u128))
                                } else {
                                    Value::Int(v as i64)
                                }
                            } else {
                                // Signed: reinterpret as i64
                                Value::Int(v as i64)
                            }
                        } else {
                            Value::Int(v as i64)
                        }
                    }
                    _ => {
                        if let Some(v) = named_args.get(attr_name) {
                            v.clone()
                        } else if let Some(expr) = default {
                            self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                        } else {
                            Value::Int(0)
                        }
                    }
                };
                attrs.insert(attr_name.clone(), val);
            } else if let Some(v) = named_args.get(attr_name) {
                attrs.insert(attr_name.clone(), v.clone());
            } else if let Some(expr) = default {
                attrs.insert(
                    attr_name.clone(),
                    self.eval_block_value(&[Stmt::Expr(expr.clone())])?,
                );
            } else {
                attrs.insert(attr_name.clone(), Value::Int(0));
            }
        }

        Ok(Value::make_instance(Symbol::intern(class_name), attrs))
    }

    /// Get the type constraint for a class attribute, searching MRO.
    pub(super) fn get_attr_type_constraint(
        &self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<String> {
        if let Some(class_def) = self.registry().classes.get(class_name) {
            for (name, _is_public, _default, _is_rw, _, _, _) in &class_def.attributes {
                if name == attr_name {
                    return class_def.attribute_types.get(attr_name).cloned();
                }
            }
        }
        None
    }

    fn native_type_byte_width(type_name: &str) -> u32 {
        match type_name {
            "uint8" | "int8" | "byte" => 1,
            "uint16" | "int16" => 2,
            "uint32" | "int32" => 4,
            "uint64" | "int64" | "uint" | "int" => 8,
            _ => 0,
        }
    }

    pub(crate) fn ensure_role_punned_to_class(&mut self, role_name: &str) {
        if self.registry().classes.contains_key(role_name) {
            return;
        }
        self.clear_private_zeroarg_method_cache();
        let role_def = match self.registry().roles.get(role_name) {
            Some(r) => r.clone(),
            None => return,
        };
        // Collect attributes and methods from the role itself and all composed parent roles
        let mut all_attributes = role_def.attributes.clone();
        let mut all_methods: HashMap<String, Vec<MethodDef>> = role_def.methods.clone();
        let mut composed_roles_list = vec![role_name.to_string()];
        if let Some(parent_names) = self.registry().role_parents.get(role_name).cloned() {
            let mut role_stack: Vec<String> = parent_names;
            while let Some(parent_role_name) = role_stack.pop() {
                if !composed_roles_list.contains(&parent_role_name) {
                    composed_roles_list.push(parent_role_name.clone());
                    if let Some(parent_role) = self.registry().roles.get(&parent_role_name).cloned()
                    {
                        for attr in &parent_role.attributes {
                            // ClassAttributeDef is a tuple; field 0 is the attribute name
                            if !all_attributes.iter().any(|a| a.0 == attr.0) {
                                all_attributes.push(attr.clone());
                            }
                        }
                        for (method_name, method_defs) in &parent_role.methods {
                            all_methods
                                .entry(method_name.clone())
                                .or_default()
                                .extend(method_defs.clone());
                        }
                    }
                    // Also recurse into grandparent roles
                    if let Some(grandparents) =
                        self.registry().role_parents.get(&parent_role_name).cloned()
                    {
                        for gp_name in &grandparents {
                            if !composed_roles_list.contains(gp_name) {
                                role_stack.push(gp_name.clone());
                            }
                        }
                    }
                }
            }
        }
        let punned_class = ClassDef {
            parents: Vec::new(),
            attributes: all_attributes,
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            attribute_built: HashMap::new(),
            methods: all_methods,
            native_methods: HashSet::new(),
            mro: vec![role_name.to_string(), "Any".to_string(), "Mu".to_string()],
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: HashMap::new(),
        };
        self.registry_mut()
            .classes
            .insert(role_name.to_string(), punned_class);
        // Register the role and its composed roles
        self.registry_mut()
            .class_composed_roles
            .insert(role_name.to_string(), composed_roles_list);
        // When punning a bare role (no type params), update the language
        // revision metadata from the matching candidate so that
        // `.^language-revision` on the pun instance returns the correct
        // revision (not the last-registered candidate's revision).
        let candidate_lang_version =
            self.registry()
                .role_candidates
                .get(role_name)
                .and_then(|candidates| {
                    candidates
                        .iter()
                        .find(|c| c.type_params.is_empty())
                        .map(|c| c.language_version.clone())
                });
        if let Some(version) = candidate_lang_version {
            self.store_language_revision_from_version(role_name, &version);
        }
    }
}
