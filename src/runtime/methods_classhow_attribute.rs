use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn collect_attribute_objects(
        &self,
        class_name: &str,
        local_only: bool,
    ) -> Vec<Value> {
        // For Attribute itself, return BOOTSTRAPATTR instances for its well-known attributes
        if class_name == "Attribute" && !self.registry().classes.contains_key("Attribute") {
            return Self::make_bootstrapattr_list();
        }
        if local_only {
            if let Some(class_def) = self.registry().classes.get(class_name) {
                class_def
                    .attributes
                    .iter()
                    .map(|attr| self.make_attribute_object(attr, class_name))
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            let mro = if let Some(class_def) = self.registry().classes.get(class_name)
                && !class_def.mro.is_empty()
            {
                class_def.mro.clone()
            } else {
                vec![class_name.to_string()]
            };
            let mut result = Vec::new();
            let mut seen_names = std::collections::HashSet::new();
            for cn in &mro {
                if cn == "Any" || cn == "Mu" {
                    continue;
                }
                if let Some(cd) = self.registry().classes.get(cn) {
                    for attr in &cd.attributes {
                        if seen_names.insert(attr.0.clone()) {
                            result.push(self.make_attribute_object(attr, cn));
                        }
                    }
                }
            }
            result
        }
    }

    /// Create a list of BOOTSTRAPATTR instances for the Attribute class's own attributes.
    fn make_bootstrapattr_list() -> Vec<Value> {
        // Raku's Attribute class has these well-known attributes (in order).
        // We model a subset that matches what the roast tests check.
        let bootstrapattrs: &[(&str, &str)] = &[
            ("name", "str"),
            ("type", "Mu"),
            ("build", "Mu"),
            ("package", "Mu"),
            ("inlined", "int"),
            ("has_accessor", "int"),
            ("rw", "int"),
            ("is_built", "int"),
            ("is_bound", "int"),
            ("required", "Mu"),
            ("container_descriptor", "Mu"),
            ("auto_viv_container", "Mu"),
            ("positional_delegate", "int"),
            ("associative_delegate", "int"),
            ("why", "Mu"),
            ("container_initializer", "Mu"),
            ("original", "Mu"),
            ("compose_order", "int"),
            ("composed", "int"),
            ("is_required", "int"),
            ("dimensions", "Mu"),
        ];
        bootstrapattrs
            .iter()
            .map(|(attr_name, type_name)| {
                let mut meta = HashMap::new();
                meta.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                meta.insert(
                    "type".to_string(),
                    Value::Package(Symbol::intern(type_name)),
                );
                meta.insert(
                    "__mutsu_attr_name".to_string(),
                    Value::str(attr_name.to_string()),
                );
                meta.insert("__mutsu_is_bootstrapattr".to_string(), Value::Bool(true));
                Value::make_instance(Symbol::intern("Attribute"), meta)
            })
            .collect()
    }

    fn make_attribute_object(&self, attr: &super::ClassAttributeDef, owner: &str) -> Value {
        let (ref attr_name, is_public, ref default, is_rw, _, sigil, _) = *attr;
        let full_name = format!("{}!{}", sigil, attr_name);
        let raw_type_name = self
            .registry()
            .classes
            .get(owner)
            .and_then(|cd| cd.attribute_types.get(attr_name))
            .cloned();
        // For @ sigil, the exposed type is Positional[T]; for % it is Associative[T]
        let type_name = match sigil {
            '@' => {
                if let Some(ref inner) = raw_type_name {
                    format!("Positional[{}]", inner)
                } else {
                    "Positional".to_string()
                }
            }
            '%' => {
                if let Some(ref inner) = raw_type_name {
                    format!("Associative[{}]", inner)
                } else {
                    "Associative".to_string()
                }
            }
            _ => raw_type_name.unwrap_or_else(|| "Mu".to_string()),
        };
        let mut meta = HashMap::new();
        meta.insert("name".to_string(), Value::str(full_name));
        meta.insert(
            "__mutsu_attr_name".to_string(),
            Value::str(attr_name.clone()),
        );
        meta.insert(
            "__mutsu_attr_owner".to_string(),
            Value::str(owner.to_string()),
        );
        meta.insert("is_public".to_string(), Value::Bool(is_public));
        meta.insert("is_rw".to_string(), Value::Bool(is_rw));
        meta.insert("sigil".to_string(), Value::str(sigil.to_string()));
        meta.insert(
            "type".to_string(),
            Value::Package(Symbol::intern(&type_name)),
        );
        meta.insert("has_accessor".to_string(), Value::Bool(is_public));
        if let Some(default_expr) = default {
            meta.insert("__mutsu_has_build".to_string(), Value::Bool(true));
            if let crate::ast::Expr::Literal(v) = default_expr {
                meta.insert("build".to_string(), v.clone());
            } else {
                // Wrap the default expression in a Sub closure so .build returns Code
                let sub_data = crate::value::SubData {
                    package: Symbol::intern("GLOBAL"),
                    name: Symbol::intern("<attribute-build>"),
                    params: Vec::new(),
                    param_defs: Vec::new(),
                    body: vec![crate::ast::Stmt::Expr(default_expr.clone())],
                    is_rw: false,
                    is_raw: false,
                    env: self.env().clone(),
                    assumed_positional: Vec::new(),
                    assumed_named: HashMap::new(),
                    id: crate::value::next_instance_id(),
                    empty_sig: false,
                    is_bare_block: false,
                    compiled_code: None,
                    deprecated_message: None,
                    source_line: None,
                    source_file: None,
                    owned_captures: Vec::new(),
                    upvalues: Vec::new(),
                };
                meta.insert(
                    "build".to_string(),
                    Value::Sub(std::sync::Arc::new(sub_data)),
                );
            }
        }
        Value::make_instance(Symbol::intern("Attribute"), meta)
    }

    /// Build a minimal Attribute introspection object for a `has` declaration
    /// that is being processed at class-registration time, so it can be passed
    /// to a user-defined `trait_mod:<is>`. Unlike `make_attribute_object`, this
    /// does not require the owning class to already be present in `self.registry().classes`.
    pub(crate) fn make_trait_attribute_object(
        &self,
        attr_name: &str,
        sigil: char,
        is_public: bool,
        owner: &str,
        type_constraint: Option<&str>,
    ) -> Value {
        let full_name = format!("{}!{}", sigil, attr_name);
        let type_name = match sigil {
            '@' => type_constraint
                .map(|t| format!("Positional[{}]", t))
                .unwrap_or_else(|| "Positional".to_string()),
            '%' => type_constraint
                .map(|t| format!("Associative[{}]", t))
                .unwrap_or_else(|| "Associative".to_string()),
            _ => type_constraint.unwrap_or("Mu").to_string(),
        };
        let mut meta = HashMap::new();
        meta.insert("name".to_string(), Value::str(full_name));
        meta.insert(
            "__mutsu_attr_name".to_string(),
            Value::str(attr_name.to_string()),
        );
        meta.insert(
            "__mutsu_attr_owner".to_string(),
            Value::str(owner.to_string()),
        );
        meta.insert("is_public".to_string(), Value::Bool(is_public));
        meta.insert("has_accessor".to_string(), Value::Bool(is_public));
        meta.insert("sigil".to_string(), Value::str(sigil.to_string()));
        meta.insert(
            "type".to_string(),
            Value::Package(Symbol::intern(&type_name)),
        );
        Value::make_instance(Symbol::intern("Attribute"), meta)
    }

    /// Dispatch unknown attribute traits to user-defined `trait_mod:<...>` subs,
    /// or raise X::Comp::Trait::Unknown if no handler is registered. Called at
    /// class registration for each `has` declaration that carries unknown traits.
    pub(crate) fn apply_attribute_traits(
        &mut self,
        unknown_traits: &[(String, String, Option<crate::ast::Expr>)],
        attr_name_str: &str,
        sigil: char,
        is_public: bool,
        owner: &str,
        type_constraint: Option<&str>,
    ) -> Result<(), RuntimeError> {
        for (kind, trait_name, trait_arg) in unknown_traits {
            // `has $.x does Foo` — record the role so construction mixes it into
            // the attribute's value (its container does the role). Not a
            // `trait_mod:<does>` dispatch.
            if kind == "does" {
                self.registry_mut()
                    .class_attribute_does_roles
                    .entry((owner.to_string(), attr_name_str.to_string()))
                    .or_default()
                    .push(trait_name.clone());
                continue;
            }
            let trait_mod_name = format!("trait_mod:<{}>", kind);
            let has_handler =
                self.has_proto(&trait_mod_name) || self.has_multi_candidates(&trait_mod_name);
            if has_handler {
                let attr_obj = self.make_trait_attribute_object(
                    attr_name_str,
                    sigil,
                    is_public,
                    owner,
                    type_constraint,
                );
                let trait_arg_val = if let Some(arg_expr) = trait_arg {
                    Some(self.eval_block_value(&[crate::ast::Stmt::Expr(arg_expr.clone())])?)
                } else {
                    None
                };
                let type_obj = self.resolve_type_object(trait_name);
                let mut args = vec![attr_obj];
                if let Some(type_val) = type_obj {
                    args.push(type_val);
                    if let Some(arg_val) = trait_arg_val {
                        args.push(arg_val);
                    }
                } else {
                    let named_val = Value::Pair(
                        trait_name.clone(),
                        Box::new(trait_arg_val.unwrap_or(Value::Bool(true))),
                    );
                    args.push(named_val);
                }
                self.call_function(&trait_mod_name, args)?;
                continue;
            }
            let msg = format!(
                "Can't use unknown trait '{}' -> '{}' in an attribute declaration.",
                kind, trait_name
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("type".to_string(), Value::str(kind.clone()));
            attrs.insert("subtype".to_string(), Value::str(trait_name.clone()));
            attrs.insert("declaring".to_string(), Value::str("attribute".to_string()));
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Comp::Trait::Unknown"),
                attrs,
            )));
            return Err(err);
        }
        Ok(())
    }
}
