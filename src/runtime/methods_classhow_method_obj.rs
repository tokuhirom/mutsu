use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn collect_class_methods(
        &self,
        class_name: &str,
        include_private: bool,
        result: &mut Vec<Value>,
    ) {
        if let Some(class_def) = self.registry().classes.get(class_name) {
            // First add accessor methods for public attributes (in order)
            for (attr_name, is_public, ..) in &class_def.attributes {
                if *is_public && !class_def.methods.contains_key(attr_name) {
                    result.push(self.make_native_method_object(attr_name));
                }
            }
            // Then add explicit methods
            for (method_name, overloads) in &class_def.methods {
                if overloads.is_empty() {
                    continue;
                }
                // Skip private methods unless :private
                let first = &overloads[0];
                if first.is_private && !include_private {
                    continue;
                }
                let is_multi = overloads.len() > 1;
                let return_type = first.return_type.clone();
                let method_obj = self.make_method_object_with_candidates(
                    method_name,
                    first,
                    is_multi,
                    return_type,
                    Some(overloads),
                );
                result.push(method_obj);
            }
            // Also include native (built-in) methods
            for native_name in &class_def.native_methods {
                let method_obj = self.make_native_method_object(native_name);
                result.push(method_obj);
            }
        }
    }

    /// Collect methods from a runtime-mixed-in role definition.
    pub(super) fn collect_role_methods(
        &self,
        role_name: &str,
        include_private: bool,
        result: &mut Vec<Value>,
    ) {
        if let Some(role_def) = self.registry().roles.get(role_name) {
            // Add accessor methods for public attributes
            for (attr_name, is_public, ..) in &role_def.attributes {
                if *is_public && !role_def.methods.contains_key(attr_name) {
                    result.push(self.make_native_method_object(attr_name));
                }
            }
            // Add explicit methods
            for (method_name, overloads) in &role_def.methods {
                if overloads.is_empty() {
                    continue;
                }
                let first = &overloads[0];
                if first.is_private && !include_private {
                    continue;
                }
                let is_multi = overloads.len() > 1;
                let return_type = first.return_type.clone();
                let method_obj = self.make_method_object_with_candidates(
                    method_name,
                    first,
                    is_multi,
                    return_type,
                    Some(overloads),
                );
                result.push(method_obj);
            }
        }
    }

    pub(super) fn make_native_method_object(&self, name: &str) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::str(name.to_string()));
        attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
        let sig_attrs = {
            let mut sa = std::collections::HashMap::new();
            sa.insert("params".to_string(), Value::array(Vec::new()));
            sa
        };
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Signature"), sig_attrs),
        );
        attrs.insert("returns".to_string(), Value::Package(Symbol::intern("Mu")));
        attrs.insert("of".to_string(), Value::Package(Symbol::intern("Mu")));
        Value::make_instance(Symbol::intern("Method"), attrs)
    }

    pub(super) fn make_method_object(
        &self,
        name: &str,
        method_def: &MethodDef,
        is_dispatcher: bool,
        return_type: Option<String>,
    ) -> Value {
        self.make_method_object_with_candidates(name, method_def, is_dispatcher, return_type, None)
    }

    /// Build a Method object. When `is_dispatcher` is true and `overloads`
    /// is supplied, a `candidates` attribute holding a Method object per
    /// overload is attached so that `.candidates` works on a multi method.
    pub(super) fn make_method_object_with_candidates(
        &self,
        name: &str,
        method_def: &MethodDef,
        is_dispatcher: bool,
        return_type: Option<String>,
        overloads: Option<&[MethodDef]>,
    ) -> Value {
        let mut attrs = std::collections::HashMap::new();

        // Store the display name (with ! prefix for private methods)
        let display_name = if method_def.is_private {
            format!("!{}", name)
        } else {
            name.to_string()
        };
        attrs.insert("name".to_string(), Value::str(display_name));
        attrs.insert("is_dispatcher".to_string(), Value::Bool(is_dispatcher));

        // Build a Signature object for this method, threading the return type
        // so that `.signature.returns` reflects a `--> Type` declaration.
        let param_defs = &method_def.param_defs;
        let sig_info =
            crate::value::signature::param_defs_to_sig_info(param_defs, return_type.clone());
        attrs.insert(
            "signature".to_string(),
            crate::value::signature::make_signature_value(sig_info),
        );

        // Return type
        let rt = return_type.unwrap_or_else(|| "Mu".to_string());
        attrs.insert("returns".to_string(), Value::Package(Symbol::intern(&rt)));
        attrs.insert("of".to_string(), Value::Package(Symbol::intern(&rt)));

        // For a multi method dispatcher, attach one Method object per
        // candidate so that `.candidates` returns them. A non-multi (single)
        // method is its own sole candidate.
        let candidates: Vec<Value> = match overloads {
            Some(defs) if is_dispatcher => defs
                .iter()
                .map(|def| self.make_method_object(name, def, false, def.return_type.clone()))
                .collect(),
            _ => Vec::new(),
        };
        if !candidates.is_empty() {
            attrs.insert("candidates".to_string(), Value::array(candidates));
        }

        Value::make_instance(Symbol::intern("Method"), attrs)
    }

    pub(super) fn classhow_methods_tree(
        &self,
        class_name: &str,
        include_private: bool,
    ) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();

        // First: own methods
        self.collect_class_methods(class_name, include_private, &mut result);

        // Then: each parent's tree as a nested array
        if let Some(class_def) = self.registry().classes.get(class_name) {
            for parent in &class_def.parents {
                let subtree = self.classhow_methods_tree(parent, include_private)?;
                result.push(subtree);
            }
        }

        Ok(Value::array(result))
    }

    /// Collect all methods named `method_name` across the MRO of `target`.
    /// Returns a list of callable Sub values, one per class in the MRO that
    /// defines the method. This implements `.can(method-name)`.
    pub(super) fn collect_can_methods(&mut self, target: &Value, method_name: &str) -> Vec<Value> {
        let class_name = match target {
            Value::Instance { class_name, .. } => class_name.resolve(),
            Value::Package(name) => name.resolve(),
            Value::Enum { enum_type, .. } => enum_type.resolve(),
            _ => utils::value_type_name(target).to_string(),
        };
        let mro = self.classhow_mro_unhidden_names(target);
        let mut results = Vec::new();
        for cn in &mro {
            if let Some(class_def) = self.registry().classes.get(cn)
                && let Some(defs) = class_def.methods.get(method_name)
            {
                for def in defs.iter().filter(|def| !def.is_my || cn == &class_name) {
                    // Prepend "self" to params so the method can be called
                    // as $meth($invocant) — the first argument binds as self.
                    let mut params = vec!["self".to_string()];
                    params.extend(def.params.iter().filter(|p| p.as_str() != "self").cloned());
                    let mut param_defs = vec![crate::ast::ParamDef {
                        name: "self".to_string(),
                        default: None,
                        multi_invocant: true,
                        required: false,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        onearg: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        is_invocant: true,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        shape_constraints: None,
                    }];
                    param_defs.extend(
                        def.param_defs
                            .iter()
                            .filter(|p| p.name.as_str() != "self")
                            .cloned(),
                    );
                    results.push(Value::make_sub(
                        Symbol::intern(cn),
                        Symbol::intern(method_name),
                        params,
                        param_defs,
                        (*def.body).clone(),
                        def.is_rw,
                        crate::env::Env::new(),
                    ));
                }
            }
        }
        // Check for auto-generated attribute accessors (has $.x creates an accessor method).
        if results.is_empty() {
            let class_attrs = self.collect_class_attributes(&class_name);
            for (attr_name, is_public, _, is_rw, ..) in &class_attrs {
                if *is_public && attr_name == method_name {
                    results.push(Value::Routine {
                        package: Symbol::intern(&class_name),
                        name: Symbol::intern(method_name),
                        is_regex: false,
                    });
                    // Tag the routine with rw status if needed — currently Routine
                    // doesn't carry rw info, but we at least return a truthy result.
                    let _ = is_rw; // suppress unused warning
                    break;
                }
            }
        }
        // Also check for native/builtin methods if no user-defined methods found.
        // For built-in types, probe the native method dispatch to see if the method exists.
        if results.is_empty() {
            let method_sym = Symbol::intern(method_name);
            let has_native =
                // Check 0-arg builtins
                crate::builtins::native_method_0arg(target, method_sym).is_some()
                // Check 1-arg builtins with a dummy arg
                || crate::builtins::native_method_1arg(
                    target,
                    method_sym,
                    &Value::Nil,
                )
                .is_some()
                // Check user-defined classes and their native_methods set
                || {
                    let pkg = Value::Package(Symbol::intern(&class_name));
                    self.classhow_find_method(&pkg, method_name).is_some()
                };
            if has_native {
                results.push(Value::Routine {
                    package: Symbol::intern(&class_name),
                    name: Symbol::intern(method_name),
                    is_regex: false,
                });
            }
        }
        // Grammars inherit parse/subparse/parsefile from the built-in Grammar
        // type. These are dispatched natively (no ClassDef.methods entry), so the
        // MRO walk above misses them — probe the grammar-ness of the target.
        if results.is_empty()
            && matches!(method_name, "parse" | "subparse" | "parsefile")
            && self.package_looks_like_grammar(&class_name)
        {
            results.push(Value::Routine {
                package: Symbol::intern(&class_name),
                name: Symbol::intern(method_name),
                is_regex: false,
            });
        }
        // Built-in exception instances (X::...) expose their attributes as
        // accessor methods (e.g. X::Undeclared.suggestions, .symbol). The HOW
        // metamodel has no user class def for these, so probe the instance's own
        // attribute map directly.
        if results.is_empty()
            && let Value::Instance {
                class_name: cn,
                attributes,
                ..
            } = target
            && cn.resolve().starts_with("X::")
            && attributes.contains_key(method_name)
        {
            results.push(Value::Routine {
                package: Symbol::intern(&class_name),
                name: Symbol::intern(method_name),
                is_regex: false,
            });
        }
        // For enum values, also check enum-specific methods (key, value, Int, Str, etc.)
        if results.is_empty()
            && matches!(target, Value::Enum { .. })
            && self
                .dispatch_enum_method(target, method_name, &[])
                .is_some()
        {
            results.push(Value::Routine {
                package: Symbol::intern(&class_name),
                name: Symbol::intern(method_name),
                is_regex: false,
            });
        }
        results
    }
}
