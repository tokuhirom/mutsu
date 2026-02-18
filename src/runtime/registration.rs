use super::*;

impl Interpreter {
    pub(crate) fn has_function(&self, name: &str) -> bool {
        let fq = format!("{}::{}", self.current_package, name);
        self.functions.contains_key(&fq) || self.functions.contains_key(name)
    }

    /// Check if a multi-dispatched function with the given name exists (any arity).
    pub(crate) fn has_multi_function(&self, name: &str) -> bool {
        let fq_slash = format!("{}::{}/", self.current_package, name);
        self.functions.keys().any(|k| k.starts_with(&fq_slash))
    }

    pub(crate) fn register_sub_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
    ) {
        let def = FunctionDef {
            package: self.current_package.clone(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
        };
        if multi {
            let arity = param_defs.iter().filter(|p| !p.slurpy && !p.named).count();
            let type_sig: Vec<&str> = param_defs
                .iter()
                .filter(|p| !p.slurpy && !p.named)
                .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                .collect();
            let has_types = type_sig.iter().any(|t| *t != "Any");
            if has_types {
                let typed_fq = format!(
                    "{}::{}/{}:{}",
                    self.current_package,
                    name,
                    arity,
                    type_sig.join(",")
                );
                self.functions.insert(typed_fq, def.clone());
            }
            let fq = format!("{}::{}/{}", self.current_package, name, arity);
            if !has_types {
                self.functions.insert(fq, def);
            } else {
                self.functions.entry(fq).or_insert(def);
            }
        } else {
            let fq = format!("{}::{}", self.current_package, name);
            self.functions.insert(fq, def);
        }
    }

    pub(crate) fn register_token_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
    ) {
        let def = FunctionDef {
            package: self.current_package.clone(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
        };
        self.insert_token_def(name, def, multi);
    }

    pub(crate) fn register_proto_decl(&mut self, name: &str) {
        let key = format!("{}::{}", self.current_package, name);
        self.proto_subs.insert(key);
    }

    pub(crate) fn register_proto_token_decl(&mut self, name: &str) {
        let key = format!("{}::{}", self.current_package, name);
        self.proto_tokens.insert(key);
    }

    pub(crate) fn register_enum_decl(
        &mut self,
        name: &str,
        variants: &[(String, Option<Expr>)],
    ) -> Result<(), RuntimeError> {
        let mut enum_variants = Vec::new();
        let mut next_value: i64 = 0;
        for (key, value_expr) in variants {
            let val = if let Some(expr) = value_expr {
                let v = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                match v {
                    Value::Int(i) => i,
                    _ => next_value,
                }
            } else {
                next_value
            };
            enum_variants.push((key.clone(), val));
            next_value = val + 1;
        }
        self.enum_types
            .insert(name.to_string(), enum_variants.clone());
        self.env
            .insert(name.to_string(), Value::Str(name.to_string()));
        for (index, (key, val)) in enum_variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: name.to_string(),
                key: key.clone(),
                value: *val,
                index,
            };
            self.env
                .insert(format!("{}::{}", name, key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
        Ok(())
    }

    pub(crate) fn register_class_decl(
        &mut self,
        name: &str,
        parents: &[String],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut class_def = ClassDef {
            parents: parents.to_vec(),
            attributes: Vec::new(),
            methods: HashMap::new(),
            native_methods: HashSet::new(),
            mro: Vec::new(),
        };
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    is_rw: _,
                } => {
                    class_def
                        .attributes
                        .push((attr_name.clone(), *is_public, default.clone()));
                }
                Stmt::MethodDecl {
                    name: method_name,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                } => {
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: method_body.clone(),
                    };
                    if *multi {
                        class_def
                            .methods
                            .entry(method_name.clone())
                            .or_default()
                            .push(def);
                    } else {
                        class_def.methods.insert(method_name.clone(), vec![def]);
                    }
                }
                Stmt::DoesDecl { name: role_name } => {
                    let role =
                        self.roles.get(role_name).cloned().ok_or_else(|| {
                            RuntimeError::new(format!("Unknown role: {}", role_name))
                        })?;
                    for attr in &role.attributes {
                        if !class_def.attributes.iter().any(|(n, _, _)| n == &attr.0) {
                            class_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        class_def
                            .methods
                            .entry(mname)
                            .or_default()
                            .extend(overloads);
                    }
                }
                _ => {
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                }
            }
        }
        self.classes.insert(name.to_string(), class_def);
        let mut stack = Vec::new();
        let _ = self.compute_class_mro(name, &mut stack)?;
        Ok(())
    }

    pub(crate) fn register_role_decl(
        &mut self,
        name: &str,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut role_def = RoleDef {
            attributes: Vec::new(),
            methods: HashMap::new(),
        };
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    is_rw: _,
                } => {
                    role_def
                        .attributes
                        .push((attr_name.clone(), *is_public, default.clone()));
                }
                Stmt::MethodDecl {
                    name: method_name,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                } => {
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: method_body.clone(),
                    };
                    if *multi {
                        role_def
                            .methods
                            .entry(method_name.clone())
                            .or_default()
                            .push(def);
                    } else {
                        role_def.methods.insert(method_name.clone(), vec![def]);
                    }
                }
                _ => {
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                }
            }
        }
        self.roles.insert(name.to_string(), role_def);
        Ok(())
    }

    pub(crate) fn register_subset_decl(
        &mut self,
        name: &str,
        base: &str,
        predicate: Option<&Expr>,
    ) {
        self.subsets.insert(
            name.to_string(),
            SubsetDef {
                base: base.to_string(),
                predicate: predicate.cloned(),
            },
        );
    }
}
