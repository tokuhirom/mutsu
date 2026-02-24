use super::*;

impl Interpreter {
    fn is_stub_routine_body(body: &[Stmt]) -> bool {
        body.len() == 1
            && matches!(
                &body[0],
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
            )
    }

    fn validate_private_access_in_stmts(
        &self,
        caller_class: &str,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.validate_private_access_in_stmt(caller_class, stmt)?;
        }
        Ok(())
    }

    fn validate_private_access_in_stmt(
        &self,
        caller_class: &str,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
                self.validate_private_access_in_expr(caller_class, e)?
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                self.validate_private_access_in_expr(caller_class, expr)?
            }
            Stmt::Say(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for e in exprs {
                    self.validate_private_access_in_expr(caller_class, e)?;
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.validate_private_access_in_expr(caller_class, cond)?;
                self.validate_private_access_in_stmts(caller_class, then_branch)?;
                self.validate_private_access_in_stmts(caller_class, else_branch)?;
            }
            Stmt::While { cond, body, .. } => {
                self.validate_private_access_in_expr(caller_class, cond)?;
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::For { iterable, body, .. } => {
                self.validate_private_access_in_expr(caller_class, iterable)?;
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => {
                if let Some(init) = init.as_ref() {
                    self.validate_private_access_in_stmt(caller_class, init)?;
                }
                if let Some(cond) = cond.as_ref() {
                    self.validate_private_access_in_expr(caller_class, cond)?;
                }
                if let Some(step) = step.as_ref() {
                    self.validate_private_access_in_expr(caller_class, step)?;
                }
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::Block(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::RoleDecl { body, .. }
            | Stmt::SubDecl { body, .. }
            | Stmt::TokenDecl { body, .. }
            | Stmt::RuleDecl { body, .. }
            | Stmt::ProtoDecl { body, .. }
            | Stmt::Package { body, .. }
            | Stmt::React { body }
            | Stmt::When { body, .. }
            | Stmt::Given { body, .. }
            | Stmt::Phaser { body, .. }
            | Stmt::Subtest { body, .. } => {
                self.validate_private_access_in_stmts(caller_class, body)?
            }
            Stmt::Whenever { supply, body, .. } => {
                self.validate_private_access_in_expr(caller_class, supply)?;
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::MethodDecl { body, .. } => {
                self.validate_private_access_in_stmts(caller_class, body)?;
            }
            Stmt::TempMethodAssign {
                method_args, value, ..
            } => {
                for e in method_args {
                    self.validate_private_access_in_expr(caller_class, e)?;
                }
                self.validate_private_access_in_expr(caller_class, value)?;
            }
            Stmt::Let { index, value, .. } => {
                if let Some(index) = index.as_ref() {
                    self.validate_private_access_in_expr(caller_class, index)?;
                }
                if let Some(value) = value.as_ref() {
                    self.validate_private_access_in_expr(caller_class, value)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn validate_private_access_in_expr(
        &self,
        caller_class: &str,
        expr: &Expr,
    ) -> Result<(), RuntimeError> {
        match expr {
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
            } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
                if *modifier == Some('!')
                    && let Some((owner_class, _)) = name.split_once("::")
                    && owner_class != caller_class
                    && !self
                        .class_trusts
                        .get(owner_class)
                        .is_some_and(|trusted| trusted.contains(caller_class))
                {
                    return Err(RuntimeError::new("X::Method::Private::Permission"));
                }
            }
            Expr::HyperMethodCall { target, args, .. } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::Call { args, .. }
            | Expr::ArrayLiteral(args)
            | Expr::BracketArray(args)
            | Expr::CaptureLiteral(args)
            | Expr::StringInterpolation(args) => {
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::Reduction { expr, .. } => {
                self.validate_private_access_in_expr(caller_class, expr)?;
            }
            Expr::Binary { left, right, .. }
            | Expr::MetaOp { left, right, .. }
            | Expr::HyperOp { left, right, .. } => {
                self.validate_private_access_in_expr(caller_class, left)?;
                self.validate_private_access_in_expr(caller_class, right)?;
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.validate_private_access_in_expr(caller_class, cond)?;
                self.validate_private_access_in_expr(caller_class, then_expr)?;
                self.validate_private_access_in_expr(caller_class, else_expr)?;
            }
            Expr::Index { target, index } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                self.validate_private_access_in_expr(caller_class, index)?;
            }
            Expr::IndexAssign {
                target,
                index,
                value,
            } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                self.validate_private_access_in_expr(caller_class, index)?;
                self.validate_private_access_in_expr(caller_class, value)?;
            }
            Expr::AssignExpr { expr, .. } => {
                self.validate_private_access_in_expr(caller_class, expr)?
            }
            Expr::DoBlock { body, .. }
            | Expr::Block(body)
            | Expr::Gather(body)
            | Expr::AnonSub(body)
            | Expr::AnonSubParams { body, .. }
            | Expr::Lambda { body, .. } => {
                self.validate_private_access_in_stmts(caller_class, body)?
            }
            Expr::Try { body, catch } => {
                self.validate_private_access_in_stmts(caller_class, body)?;
                if let Some(catch) = catch.as_ref() {
                    self.validate_private_access_in_stmts(caller_class, catch)?;
                }
            }
            Expr::DoStmt(stmt) => self.validate_private_access_in_stmt(caller_class, stmt)?,
            Expr::CallOn { target, args } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                for arg in args {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::InfixFunc { left, right, .. } => {
                self.validate_private_access_in_expr(caller_class, left)?;
                for arg in right {
                    self.validate_private_access_in_expr(caller_class, arg)?;
                }
            }
            Expr::Exists(inner) => self.validate_private_access_in_expr(caller_class, inner)?,
            _ => {}
        }
        Ok(())
    }

    pub(crate) fn has_function(&self, name: &str) -> bool {
        let fq = format!("{}::{}", self.current_package, name);
        self.functions.contains_key(&fq) || self.functions.contains_key(name)
    }

    /// Check if a multi-dispatched function with the given name exists (any arity).
    pub(crate) fn has_multi_function(&self, name: &str) -> bool {
        let fq_slash = format!("{}::{}/", self.current_package, name);
        self.functions.keys().any(|k| k.starts_with(&fq_slash))
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
        is_test_assertion: bool,
        supersede: bool,
    ) -> Result<(), RuntimeError> {
        let new_def = FunctionDef {
            package: self.current_package.clone(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
            is_test_assertion,
        };
        let single_key = format!("{}::{}", self.current_package, name);
        let multi_prefix = format!("{}::{}/", self.current_package, name);
        let has_single = self.functions.contains_key(&single_key);
        let has_multi = self.functions.keys().any(|k| k.starts_with(&multi_prefix));
        let has_proto = self.proto_subs.contains(&single_key);
        if self.env.contains_key(&format!("&{}", name)) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared as code variable",
                name
            )));
        }
        if let Some(existing) = self.functions.get(&single_key) {
            let same = existing.package == new_def.package
                && existing.name == new_def.name
                && existing.params == new_def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", new_def.param_defs)
                && format!("{:?}", existing.body) == format!("{:?}", new_def.body);
            if same {
                return Ok(());
            }
        }
        let existing_is_stub = self
            .functions
            .get(&single_key)
            .is_some_and(|existing| Self::is_stub_routine_body(&existing.body));
        if multi {
            if has_single && !has_proto && !supersede {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared as non-multi",
                    name
                )));
            }
        } else if !supersede && ((has_multi && !has_proto) || (has_single && !existing_is_stub)) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared",
                name
            )));
        }
        let def = new_def;
        if multi {
            let arity = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
                .count();
            let type_sig: Vec<&str> = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
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
                match self.functions.entry(fq.clone()) {
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(def);
                    }
                    std::collections::hash_map::Entry::Occupied(_) => {
                        let mut idx = 1usize;
                        loop {
                            let key = format!("{}__m{}", fq, idx);
                            if let std::collections::hash_map::Entry::Vacant(entry) =
                                self.functions.entry(key)
                            {
                                entry.insert(def);
                                break;
                            }
                            idx += 1;
                        }
                    }
                }
            } else {
                self.functions.entry(fq).or_insert(def);
            }
        } else {
            let fq = format!("{}::{}", self.current_package, name);
            self.functions.insert(fq, def);
        }
        Ok(())
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
            is_test_assertion: false,
        };
        self.insert_token_def(name, def, multi);
    }

    pub(crate) fn register_proto_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let key = format!("{}::{}", self.current_package, name);
        if self.functions.contains_key(&key) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared",
                name
            )));
        }
        if self.proto_subs.contains(&key) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared as proto",
                name
            )));
        }
        self.proto_subs.insert(key);
        let fq = format!("{}::{}", self.current_package, name);
        self.proto_functions.insert(
            fq,
            FunctionDef {
                package: self.current_package.clone(),
                name: name.to_string(),
                params: params.to_vec(),
                param_defs: param_defs.to_vec(),
                body: body.to_vec(),
                is_test_assertion: false,
            },
        );
        Ok(())
    }

    /// Register a sub under GLOBAL:: (used for `is export` trait).
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl_as_global(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
        is_test_assertion: bool,
        supersede: bool,
    ) -> Result<(), RuntimeError> {
        let def = FunctionDef {
            package: "GLOBAL".to_string(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
            is_test_assertion,
        };
        let single_key = format!("GLOBAL::{}", name);
        let multi_prefix = format!("GLOBAL::{}/", name);
        let has_single = self.functions.contains_key(&single_key);
        let has_multi = self.functions.keys().any(|k| k.starts_with(&multi_prefix));
        let has_proto = self.proto_subs.contains(&single_key);
        if let Some(existing) = self.functions.get(&single_key) {
            let same = existing.package == def.package
                && existing.name == def.name
                && existing.params == def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", def.param_defs)
                && format!("{:?}", existing.body) == format!("{:?}", def.body);
            if same {
                return Ok(());
            }
        }
        let existing_is_stub = self
            .functions
            .get(&single_key)
            .is_some_and(|existing| Self::is_stub_routine_body(&existing.body));
        if multi {
            if has_single && !has_proto && !supersede {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared as non-multi",
                    name
                )));
            }
            let arity = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
                .count();
            let type_sig: Vec<&str> = param_defs
                .iter()
                .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
                .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                .collect();
            let has_types = type_sig.iter().any(|t| *t != "Any");
            if has_types {
                let typed_fq = format!("GLOBAL::{}/{}:{}", name, arity, type_sig.join(","));
                self.functions.insert(typed_fq, def.clone());
            }
            let fq = format!("GLOBAL::{}/{}", name, arity);
            if !has_types {
                match self.functions.entry(fq.clone()) {
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(def);
                    }
                    std::collections::hash_map::Entry::Occupied(_) => {
                        let mut idx = 1usize;
                        loop {
                            let key = format!("{}__m{}", fq, idx);
                            if let std::collections::hash_map::Entry::Vacant(entry) =
                                self.functions.entry(key)
                            {
                                entry.insert(def);
                                break;
                            }
                            idx += 1;
                        }
                    }
                }
            } else {
                self.functions.entry(fq).or_insert(def);
            }
        } else {
            if has_multi && !has_proto && !supersede {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared",
                    name
                )));
            }
            if has_single && !supersede && !existing_is_stub {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared",
                    name
                )));
            }
            let fq = format!("GLOBAL::{}", name);
            self.functions.insert(fq, def);
        }
        Ok(())
    }

    /// Register a proto sub under GLOBAL:: (used for `is export` trait).
    pub(crate) fn register_proto_decl_as_global(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let key = format!("GLOBAL::{}", name);
        if self.functions.contains_key(&key) {
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared",
                name
            )));
        }
        if self.proto_subs.contains(&key) {
            if self.current_package == "GLOBAL" {
                // `is export` on a GLOBAL proto hits both local/global registration paths.
                return Ok(());
            }
            return Err(RuntimeError::new(format!(
                "X::Redeclaration: '{}' already declared as proto",
                name
            )));
        }
        self.proto_subs.insert(key.clone());
        self.proto_functions.insert(
            key,
            FunctionDef {
                package: "GLOBAL".to_string(),
                name: name.to_string(),
                params: params.to_vec(),
                param_defs: param_defs.to_vec(),
                body: body.to_vec(),
                is_test_assertion: false,
            },
        );
        Ok(())
    }

    pub(crate) fn register_proto_token_decl(&mut self, name: &str) {
        let key = format!("{}::{}", self.current_package, name);
        self.proto_tokens.insert(key);
    }

    pub(crate) fn register_enum_decl(
        &mut self,
        name: &str,
        variants: &[(String, Option<Expr>)],
    ) -> Result<Value, RuntimeError> {
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
        let is_anonymous = name.is_empty();
        let enum_type_name = if is_anonymous { "__ANON_ENUM__" } else { name };
        self.enum_types
            .insert(enum_type_name.to_string(), enum_variants.clone());
        if !is_anonymous {
            self.env
                .insert(name.to_string(), Value::Str(name.to_string()));
        }
        for (index, (key, val)) in enum_variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: enum_type_name.to_string(),
                key: key.clone(),
                value: *val,
                index,
            };
            if !is_anonymous {
                self.env
                    .insert(format!("{}::{}", name, key), enum_val.clone());
            }
            self.env.insert(key.clone(), enum_val);
        }
        // For anonymous enums, return a Map (Hash) of key => value pairs
        if is_anonymous {
            let mut map = HashMap::new();
            for (key, val) in &enum_variants {
                map.insert(key.clone(), Value::Int(*val));
            }
            Ok(Value::hash(map))
        } else {
            Ok(Value::Nil)
        }
    }

    pub(crate) fn register_class_decl(
        &mut self,
        name: &str,
        parents: &[String],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // Validate that all parent classes exist
        // Allow inheriting from built-in types that may not be in the classes HashMap
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
            "Proc::Async",
            "Supply",
            "Supplier",
        ];
        for parent in parents {
            if parent == name {
                return Err(RuntimeError::new(format!(
                    "X::Inheritance::SelfInherit: class '{}' cannot inherit from itself",
                    name
                )));
            }
            if !self.classes.contains_key(parent)
                && !BUILTIN_TYPES.contains(&parent.as_str())
                && !self.roles.contains_key(parent)
            {
                return Err(RuntimeError::new(format!(
                    "X::Inheritance::UnknownParent: class '{}' specifies unknown parent class '{}'",
                    name, parent
                )));
            }
        }
        let mut class_def = ClassDef {
            parents: parents.to_vec(),
            attributes: Vec::new(),
            methods: HashMap::new(),
            native_methods: HashSet::new(),
            mro: Vec::new(),
        };
        for stmt in body {
            if let Stmt::TrustsDecl {
                name: trusted_class,
            } = stmt
            {
                self.class_trusts
                    .entry(name.to_string())
                    .or_default()
                    .insert(trusted_class.clone());
            }
        }
        // Detect stub class: `class Foo { ... }` — body is a stub operator call.
        // Register the class but skip body execution.
        let is_stub = body.len() == 1
            && matches!(&body[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                if fn_name == "__mutsu_stub_die");
        // Make the class visible while its body executes so introspection calls
        // like `A.^add_method(...)` inside the declaration can resolve `A`.
        self.classes.insert(name.to_string(), class_def.clone());
        if is_stub {
            self.classes.insert(name.to_string(), class_def);
            let mut stack = Vec::new();
            let _ = self.compute_class_mro(name, &mut stack)?;
            return Ok(());
        }
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
                    name_expr,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                } => {
                    self.validate_private_access_in_stmts(name, method_body)?;
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.clone()
                    };
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: method_body.clone(),
                        is_rw: *is_rw,
                        is_private: *is_private,
                    };
                    if *multi {
                        class_def
                            .methods
                            .entry(resolved_method_name)
                            .or_default()
                            .push(def);
                    } else {
                        class_def.methods.insert(resolved_method_name, vec![def]);
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
                Stmt::TrustsDecl {
                    name: trusted_class,
                } => {
                    self.class_trusts
                        .entry(name.to_string())
                        .or_default()
                        .insert(trusted_class.clone());
                }
                // our &baz ::= &bar  — alias a method under a new name
                Stmt::VarDecl {
                    name: var_name,
                    expr: Expr::CodeVar(source_name),
                    ..
                } if var_name.starts_with('&') => {
                    let alias = var_name.trim_start_matches('&').to_string();
                    if let Some(overloads) = class_def.methods.get(source_name).cloned() {
                        class_def.methods.insert(alias, overloads);
                    }
                    // Also execute the statement so the code variable is set
                    self.classes.insert(name.to_string(), class_def.clone());
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                    if let Some(updated) = self.classes.get(name).cloned() {
                        class_def = updated;
                    }
                }
                _ => {
                    self.classes.insert(name.to_string(), class_def.clone());
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                    if let Some(updated) = self.classes.get(name).cloned() {
                        class_def = updated;
                    }
                }
            }
            self.classes.insert(name.to_string(), class_def.clone());
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
                    name_expr,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                } => {
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.clone()
                    };
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: method_body.clone(),
                        is_rw: *is_rw,
                        is_private: *is_private,
                    };
                    if *multi {
                        role_def
                            .methods
                            .entry(resolved_method_name)
                            .or_default()
                            .push(def);
                    } else {
                        role_def.methods.insert(resolved_method_name, vec![def]);
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
