use super::*;

/// Parse role type arguments from a string like "Str:D(Numeric), Bool(Any)".
fn parse_role_type_args(input: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, ch) in input.char_indices() {
        match ch {
            '(' | '[' => depth += 1,
            ')' | ']' => depth -= 1,
            ',' if depth == 0 => {
                args.push(input[start..i].trim().to_string());
                start = i + 1;
            }
            _ => {}
        }
    }
    let last = input[start..].trim();
    if !last.is_empty() {
        args.push(last.to_string());
    }
    args
}

/// Substitute type parameters in a method definition.
/// E.g., if type_subs = [("T", "Str:D(Numeric)")], then any param with
/// type_constraint "T" becomes "Str:D(Numeric)".
fn substitute_type_params_in_method(
    method: &MethodDef,
    type_subs: &[(String, String)],
) -> MethodDef {
    let new_param_defs = method
        .param_defs
        .iter()
        .map(|pd| {
            if let Some(tc) = &pd.type_constraint {
                for (param_name, replacement) in type_subs {
                    if tc == param_name {
                        let mut new_pd = pd.clone();
                        new_pd.type_constraint = Some(replacement.clone());
                        return new_pd;
                    }
                }
            }
            pd.clone()
        })
        .collect();
    MethodDef {
        params: method.params.clone(),
        param_defs: new_param_defs,
        body: method.body.clone(),
        is_rw: method.is_rw,
        is_private: method.is_private,
        return_type: method.return_type.clone(),
    }
}

impl Interpreter {
    fn has_explicit_named_slurpy(param_defs: &[ParamDef]) -> bool {
        param_defs
            .iter()
            .any(|pd| pd.slurpy && (pd.name.starts_with('%') || pd.double_slurpy))
    }

    fn implicit_method_named_slurpy_param() -> ParamDef {
        ParamDef {
            name: "%_".to_string(),
            default: None,
            multi_invocant: true,
            required: false,
            named: false,
            slurpy: true,
            double_slurpy: false,
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
        }
    }

    fn effective_method_param_defs(
        param_defs: &[ParamDef],
        class_is_hidden: bool,
    ) -> Vec<ParamDef> {
        let mut defs = param_defs.to_vec();
        if !class_is_hidden && !Self::has_explicit_named_slurpy(&defs) {
            defs.push(Self::implicit_method_named_slurpy_param());
        }
        defs
    }

    fn is_stub_routine_body(body: &[Stmt]) -> bool {
        body.len() == 1
            && matches!(
                &body[0],
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
            )
    }

    fn is_stub_method_def(def: &MethodDef) -> bool {
        Self::is_stub_routine_body(&def.body)
    }

    fn method_positional_signature(def: &MethodDef) -> Vec<String> {
        def.param_defs
            .iter()
            .filter(|pd| !(pd.named || (pd.slurpy && pd.name.starts_with('%'))))
            .map(|pd| {
                if pd.slurpy {
                    format!("*{}", pd.type_constraint.as_deref().unwrap_or("Any"))
                } else {
                    pd.type_constraint.as_deref().unwrap_or("Any").to_string()
                }
            })
            .collect()
    }

    fn method_signatures_match(required: &MethodDef, candidate: &MethodDef) -> bool {
        Self::method_positional_signature(required) == Self::method_positional_signature(candidate)
            && required.is_private == candidate.is_private
    }

    fn stub_is_nullary(def: &MethodDef) -> bool {
        def.param_defs.iter().all(|pd| pd.named || pd.slurpy)
    }

    fn inherited_matching_method_count(
        &mut self,
        class_name: &str,
        method_name: &str,
        required: &MethodDef,
    ) -> usize {
        let mut count = 0usize;
        let mro = self.class_mro(class_name);
        for parent in mro.into_iter().skip(1) {
            let Some(class_def) = self.classes.get(&parent) else {
                continue;
            };
            let Some(defs) = class_def.methods.get(method_name) else {
                continue;
            };
            for def in defs {
                if Self::is_stub_method_def(def) {
                    continue;
                }
                if Self::method_signatures_match(required, def) {
                    count += 1;
                }
            }
        }
        count
    }

    fn accessor_matches_stub(
        &mut self,
        class_name: &str,
        method_name: &str,
        required: &MethodDef,
    ) -> bool {
        if !Self::stub_is_nullary(required) {
            return false;
        }
        self.collect_class_attributes(class_name)
            .iter()
            .any(|(attr_name, is_public, _, _)| *is_public && attr_name == method_name)
    }

    fn resolve_class_stub_requirements(
        &mut self,
        class_name: &str,
        class_def: &mut ClassDef,
    ) -> Result<(), RuntimeError> {
        let method_names: Vec<String> = class_def.methods.keys().cloned().collect();
        for method_name in method_names {
            let Some(all_defs) = class_def.methods.get(&method_name).cloned() else {
                continue;
            };
            let mut stubs = Vec::new();
            let mut concrete = Vec::new();
            for def in all_defs {
                if Self::is_stub_method_def(&def) {
                    stubs.push(def);
                } else {
                    concrete.push(def);
                }
            }
            if stubs.is_empty() {
                continue;
            }
            for required in &stubs {
                let local_matches = concrete
                    .iter()
                    .filter(|candidate| Self::method_signatures_match(required, candidate))
                    .count();
                if local_matches > 1 {
                    return Err(RuntimeError::new(format!(
                        "X::Role::Composition::Conflict: multiple candidates for required method '{}'",
                        method_name
                    )));
                }
                if local_matches == 0 {
                    let inherited_matches =
                        self.inherited_matching_method_count(class_name, &method_name, required);
                    let accessor_match =
                        usize::from(self.accessor_matches_stub(class_name, &method_name, required));
                    let total = inherited_matches + accessor_match;
                    if total == 0 {
                        return Err(RuntimeError::new(format!(
                            "X::Role::Composition::Unimplemented: required method '{}'",
                            method_name
                        )));
                    }
                    if total > 1 {
                        return Err(RuntimeError::new(format!(
                            "X::Role::Composition::Conflict: multiple inherited candidates for required method '{}'",
                            method_name
                        )));
                    }
                }
            }
            if concrete.is_empty() {
                class_def.methods.remove(&method_name);
            } else {
                class_def.methods.insert(method_name, concrete);
            }
        }
        Ok(())
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
                ..
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
                quoted: _,
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
            | Expr::AnonSub { body, .. }
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
            Expr::Exists { target, arg, .. } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                if let Some(a) = arg {
                    self.validate_private_access_in_expr(caller_class, a)?;
                }
            }
            Expr::ZenSlice(inner) => {
                self.validate_private_access_in_expr(caller_class, inner)?;
            }
            _ => {}
        }
        Ok(())
    }

    pub(crate) fn has_function(&self, name: &str) -> bool {
        self.has_declared_function(name)
    }

    pub(crate) fn has_declared_function(&self, name: &str) -> bool {
        let fq = format!("{}::{}", self.current_package, name);
        self.functions.contains_key(&fq) || self.functions.contains_key(name)
    }

    pub(crate) fn is_implicit_zero_arg_builtin(name: &str) -> bool {
        matches!(name, "dir")
    }

    /// Check if a multi-dispatched function with the given name exists (any arity).
    pub(crate) fn has_multi_function(&self, name: &str) -> bool {
        let fq_slash = format!("{}::{}/", self.current_package, name);
        self.functions.keys().any(|k| k.starts_with(&fq_slash))
    }

    fn malformed_return_value_compile_error() -> RuntimeError {
        let mut err = RuntimeError::new("Malformed return value");
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::Str("Malformed return value".to_string()),
        );
        err.exception = Some(Box::new(Value::make_instance(
            "X::AdHoc".to_string(),
            attrs,
        )));
        err
    }

    fn body_contains_non_nil_return(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            match stmt {
                Stmt::Return(expr) => {
                    if !matches!(expr, Expr::Literal(Value::Nil)) {
                        return true;
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if Self::body_contains_non_nil_return(then_branch)
                        || Self::body_contains_non_nil_return(else_branch)
                    {
                        return true;
                    }
                }
                Stmt::While { body, .. }
                | Stmt::React { body }
                | Stmt::SyntheticBlock(body)
                | Stmt::Block(body)
                | Stmt::Subtest { body, .. } => {
                    if Self::body_contains_non_nil_return(body) {
                        return true;
                    }
                }
                Stmt::For { body, .. } => {
                    if Self::body_contains_non_nil_return(body) {
                        return true;
                    }
                }
                Stmt::Loop { init, body, .. } => {
                    if let Some(init) = init
                        && Self::body_contains_non_nil_return(std::slice::from_ref(init.as_ref()))
                    {
                        return true;
                    }
                    if Self::body_contains_non_nil_return(body) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        return_type: Option<&String>,
        associativity: Option<&String>,
        body: &[Stmt],
        multi: bool,
        is_rw: bool,
        is_test_assertion: bool,
        supersede: bool,
        custom_traits: &[String],
    ) -> Result<(), RuntimeError> {
        if let Some(spec) = return_type
            && self.is_definite_return_spec(spec)
            && Self::body_contains_non_nil_return(body)
        {
            return Err(Self::malformed_return_value_compile_error());
        }
        // Auto-detect @_ / %_ usage for subs without explicit signatures
        let (effective_param_defs, empty_sig) = if param_defs.is_empty() && params.is_empty() {
            let (use_positional, use_named) = Self::auto_signature_uses(body);
            let mut defs = Vec::new();
            if use_positional {
                defs.push(ParamDef {
                    name: "@_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            if use_named {
                defs.push(ParamDef {
                    name: "%_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            // If neither @_ nor %_ is used, this is a true empty signature
            let is_empty = defs.is_empty();
            (defs, is_empty)
        } else {
            (param_defs.to_vec(), false)
        };
        let new_def = FunctionDef {
            package: self.current_package.clone(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: effective_param_defs,
            body: body.to_vec(),
            is_test_assertion,
            is_rw,
            is_method: false,
            empty_sig,
            return_type: return_type.cloned(),
        };
        let single_key = format!("{}::{}", self.current_package, name);
        let multi_prefix = format!("{}::{}/", self.current_package, name);
        let has_single = self.functions.contains_key(&single_key);
        let has_multi = self.functions.keys().any(|k| k.starts_with(&multi_prefix));
        let has_proto = self.proto_subs.contains(&single_key);
        let code_var_key = format!("&{}", name);
        if let Some(existing) = self.env.get(&code_var_key) {
            // Mixin values in &name come from trait_mod and should not block registration
            if !matches!(existing, Value::Mixin(..)) {
                return Err(RuntimeError::new(format!(
                    "X::Redeclaration: '{}' already declared as code variable",
                    name
                )));
            }
        }
        if let Some(existing) = self.functions.get(&single_key) {
            let same = existing.package == new_def.package
                && existing.name == new_def.name
                && existing.params == new_def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", new_def.param_defs)
                && format!("{:?}", existing.body) == format!("{:?}", new_def.body);
            if same {
                let callable_key =
                    format!("__mutsu_callable_id::{}::{}", self.current_package, name);
                self.env.insert(
                    callable_key,
                    Value::Int(crate::value::next_instance_id() as i64),
                );
                return Ok(());
            }
            let same_signature = existing.package == new_def.package
                && existing.name == new_def.name
                && existing.params == new_def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", new_def.param_defs);
            if body.is_empty() && same_signature {
                let callable_key =
                    format!("__mutsu_callable_id::{}::{}", self.current_package, name);
                self.env.insert(
                    callable_key,
                    Value::Int(crate::value::next_instance_id() as i64),
                );
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
        if let Some(assoc) = associativity {
            self.operator_assoc.insert(name.to_string(), assoc.clone());
            self.operator_assoc
                .insert(format!("{}::{}", self.current_package, name), assoc.clone());
        }
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
        let callable_key = format!("__mutsu_callable_id::{}::{}", self.current_package, name);
        self.env.insert(
            callable_key,
            Value::Int(crate::value::next_instance_id() as i64),
        );
        // Apply custom trait_mod:<is> for each non-builtin trait (only if trait_mod:<is> is defined)
        if !custom_traits.is_empty()
            && (self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>"))
        {
            for trait_name in custom_traits {
                let sub_val = Value::make_sub(
                    self.current_package.clone(),
                    name.to_string(),
                    params.to_vec(),
                    param_defs.to_vec(),
                    body.to_vec(),
                    is_rw,
                    self.env.clone(),
                );
                let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                let result = self.call_function("trait_mod:<is>", vec![sub_val, named_arg])?;
                // If the trait_mod returned a modified sub (e.g. with CALL-ME mixed in),
                // store it in the env so function dispatch can find it.
                if matches!(result, Value::Mixin(..)) {
                    self.env.insert(format!("&{}", name), result);
                }
            }
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
            is_rw: false,
            is_method: false,
            empty_sig: false,
            return_type: None,
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
                is_rw: false,
                is_method: false,
                empty_sig: false,
                return_type: None,
            },
        );
        Ok(())
    }

    /// Register a sub under GLOBAL:: (used for `is export` trait).
    #[allow(dead_code)]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl_as_global(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        return_type: Option<&String>,
        associativity: Option<&String>,
        body: &[Stmt],
        multi: bool,
        is_rw: bool,
        is_test_assertion: bool,
        supersede: bool,
    ) -> Result<(), RuntimeError> {
        if let Some(spec) = return_type
            && self.is_definite_return_spec(spec)
            && Self::body_contains_non_nil_return(body)
        {
            return Err(Self::malformed_return_value_compile_error());
        }
        // Auto-detect @_ / %_ usage for subs without explicit signatures
        let (effective_param_defs, empty_sig) = if param_defs.is_empty() && params.is_empty() {
            let (use_positional, use_named) = Self::auto_signature_uses(body);
            let mut defs = Vec::new();
            if use_positional {
                defs.push(ParamDef {
                    name: "@_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            if use_named {
                defs.push(ParamDef {
                    name: "%_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
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
                });
            }
            let is_empty = defs.is_empty();
            (defs, is_empty)
        } else {
            (param_defs.to_vec(), false)
        };
        let def = FunctionDef {
            package: "GLOBAL".to_string(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: effective_param_defs,
            body: body.to_vec(),
            is_test_assertion,
            is_rw,
            is_method: false,
            empty_sig,
            return_type: return_type.cloned(),
        };
        let single_key = format!("GLOBAL::{}", name);
        let multi_prefix = format!("GLOBAL::{}/", name);
        let has_single = self.functions.contains_key(&single_key);
        let has_multi = self.functions.keys().any(|k| k.starts_with(&multi_prefix));
        let has_proto = self.proto_subs.contains(&single_key);
        if let Some(assoc) = associativity {
            self.operator_assoc.insert(name.to_string(), assoc.clone());
            self.operator_assoc
                .insert(format!("GLOBAL::{}", name), assoc.clone());
        }
        if let Some(existing) = self.functions.get(&single_key) {
            let same = existing.package == def.package
                && existing.name == def.name
                && existing.params == def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", def.param_defs)
                && format!("{:?}", existing.body) == format!("{:?}", def.body);
            if same {
                return Ok(());
            }
            let same_signature = existing.package == def.package
                && existing.name == def.name
                && existing.params == def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", def.param_defs);
            if body.is_empty() && same_signature {
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
        let callable_key = format!("__mutsu_callable_id::GLOBAL::{}", name);
        self.env.insert(
            callable_key,
            Value::Int(crate::value::next_instance_id() as i64),
        );
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
                is_rw: false,
                is_method: false,
                empty_sig: false,
                return_type: None,
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
        is_hidden: bool,
        hidden_parents: &[String],
        does_parents: &[String],
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
            "Setty",
            "Baggy",
            "Mixy",
        ];
        for parent in parents {
            // Strip type arguments for validation (e.g., "R[Str:D(Numeric)]" -> "R")
            let base_parent = if let Some(bracket) = parent.find('[') {
                &parent[..bracket]
            } else {
                parent.as_str()
            };
            if base_parent == name {
                return Err(RuntimeError::new(format!(
                    "X::Inheritance::SelfInherit: class '{}' cannot inherit from itself",
                    name
                )));
            }
            if !self.classes.contains_key(base_parent)
                && !BUILTIN_TYPES.contains(&base_parent)
                && !self.roles.contains_key(base_parent)
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
            wildcard_handles: Vec::new(),
        };
        if is_hidden {
            self.hidden_classes.insert(name.to_string());
        } else {
            self.hidden_classes.remove(name);
        }
        if hidden_parents.is_empty() {
            self.hidden_defer_parents.remove(name);
        } else {
            self.hidden_defer_parents
                .insert(name.to_string(), hidden_parents.iter().cloned().collect());
        }
        // Compose roles listed in the parents (from "does Role" or "is Role" in class header)
        let mut composed_roles_list = Vec::new();
        let mut punned_roles = Vec::new();
        for parent in parents {
            // Strip role type arguments (e.g., "R[Str:D(Numeric)]" -> "R")
            let base_role_name = if let Some(bracket) = parent.find('[') {
                &parent[..bracket]
            } else {
                parent.as_str()
            };
            if let Some(role) = self.roles.get(base_role_name).cloned() {
                if role.is_stub_role {
                    return Err(RuntimeError::new("X::Role::Parametric::NoSuchCandidate"));
                }
                // Check if this role was specified via `is` (punning) vs `does` (composition)
                let is_punned = !does_parents.contains(parent);
                if is_punned {
                    punned_roles.push(parent.clone());
                }
                composed_roles_list.push(parent.clone());
                // Collect type parameter substitutions if this is a parametric role
                let type_subs: Vec<(String, String)> =
                    if let Some(role_type_params) = self.role_type_params.get(base_role_name) {
                        if let Some(bracket_start) = parent.find('[') {
                            let args_str = &parent[bracket_start + 1..parent.len() - 1];
                            let type_args = parse_role_type_args(args_str);
                            role_type_params
                                .iter()
                                .zip(type_args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect()
                        } else {
                            Vec::new()
                        }
                    } else {
                        Vec::new()
                    };
                for attr in &role.attributes {
                    if !class_def.attributes.iter().any(|(n, _, _, _)| n == &attr.0) {
                        class_def.attributes.push(attr.clone());
                    }
                }
                for (mname, overloads) in &role.methods {
                    let composed: Vec<MethodDef> = if type_subs.is_empty() {
                        overloads.clone()
                    } else {
                        overloads
                            .iter()
                            .map(|md| substitute_type_params_in_method(md, &type_subs))
                            .collect()
                    };
                    class_def
                        .methods
                        .entry(mname.clone())
                        .or_default()
                        .extend(composed);
                }
            }
        }
        // Handle role punning: `is Role` creates a punned class from the role
        for punned_role in &punned_roles {
            let base_role = punned_role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(punned_role.as_str());
            // Create a punned class entry if one doesn't already exist
            if !self.classes.contains_key(punned_role.as_str())
                && !self.classes.contains_key(base_role)
            {
                // Collect class parents and composed roles recursively from role hierarchy
                let mut punned_class_parents = Vec::new();
                let mut punned_composed_roles = Vec::new();
                let mut role_stack = vec![base_role.to_string()];
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                // It's a role - add as composed role and recurse
                                if !punned_composed_roles.contains(&rp) {
                                    punned_composed_roles.push(rp.clone());
                                }
                                role_stack.push(rp_base.to_string());
                            } else if self.classes.contains_key(rp_base)
                                && !punned_class_parents.contains(&rp)
                            {
                                // It's a class - add as parent
                                punned_class_parents.push(rp);
                            }
                        }
                    }
                }
                let punned_class = ClassDef {
                    parents: punned_class_parents,
                    attributes: Vec::new(),
                    methods: HashMap::new(),
                    native_methods: HashSet::new(),
                    mro: Vec::new(),
                    wildcard_handles: Vec::new(),
                };
                self.classes.insert(base_role.to_string(), punned_class);
                if !punned_composed_roles.is_empty() {
                    self.class_composed_roles
                        .insert(base_role.to_string(), punned_composed_roles);
                }
                // Propagate hidden status from role to punned class
                if let Some(role_def) = self.roles.get(base_role)
                    && role_def.is_hidden
                {
                    self.hidden_classes.insert(base_role.to_string());
                }
                // Recompute MRO for the punned class
                let mro = self.class_mro(base_role);
                if let Some(cd) = self.classes.get_mut(base_role) {
                    cd.mro = mro;
                }
            }
        }
        // Clear stale composed roles from previous registration
        self.class_composed_roles.remove(name);
        if !composed_roles_list.is_empty() {
            // Propagate role parent classes to the class (recursively through sub-roles)
            // When a role `R is C1` is composed into a class, C1 becomes a parent
            {
                let mut role_stack: Vec<String> = composed_roles_list
                    .iter()
                    .map(|r| {
                        r.split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(r.as_str())
                            .to_string()
                    })
                    .collect();
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                // It's a sub-role, recurse
                                role_stack.push(rp_base.to_string());
                            } else if self.classes.contains_key(rp_base)
                                && !class_def.parents.contains(&rp)
                            {
                                class_def.parents.push(rp);
                            }
                        }
                    }
                }
            }
            self.class_composed_roles
                .insert(name.to_string(), composed_roles_list.clone());
            // Propagate `hides` from composed roles (and sub-roles) to the class
            {
                let mut role_stack: Vec<String> = composed_roles_list
                    .iter()
                    .map(|r| {
                        r.split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(r.as_str())
                            .to_string()
                    })
                    .collect();
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(hides_list) = self.role_hides.get(&role_name).cloned() {
                        for hidden in hides_list {
                            self.hidden_defer_parents
                                .entry(name.to_string())
                                .or_default()
                                .insert(hidden);
                        }
                    }
                    // Recurse into sub-roles
                    if let Some(rparents) = self.role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
        }
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
        let saved_package = self.current_package.clone();
        self.current_package = name.to_string();
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    handles,
                    is_rw,
                } => {
                    class_def.attributes.push((
                        attr_name.clone(),
                        *is_public,
                        default.clone(),
                        *is_rw,
                    ));
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name)
                    } else {
                        format!("!{}", attr_name)
                    };
                    for handle_name in handles {
                        if handle_name == "*" {
                            // Wildcard delegation: forward unknown methods to this attribute
                            class_def.wildcard_handles.push(attr_var_name.clone());
                        } else {
                            class_def
                                .methods
                                .entry(handle_name.clone())
                                .or_default()
                                .push(MethodDef {
                                    params: Vec::new(),
                                    param_defs: Vec::new(),
                                    body: vec![Stmt::Expr(Expr::MethodCall {
                                        target: Box::new(Expr::Var(attr_var_name.clone())),
                                        name: handle_name.clone(),
                                        args: Vec::new(),
                                        modifier: None,
                                        quoted: false,
                                    })],
                                    is_rw: false,
                                    is_private: false,
                                    return_type: None,
                                });
                        }
                    }
                }
                Stmt::MethodDecl {
                    name: method_name,
                    name_expr,
                    params: _,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                    is_our,
                    return_type,
                } => {
                    self.validate_private_access_in_stmts(name, method_body)?;
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.clone()
                    };
                    let effective_param_defs =
                        Self::effective_method_param_defs(param_defs, is_hidden);
                    let effective_params: Vec<String> = effective_param_defs
                        .iter()
                        .map(|p| p.name.clone())
                        .collect();
                    let def = MethodDef {
                        params: effective_params.clone(),
                        param_defs: effective_param_defs.clone(),
                        body: method_body.clone(),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        return_type: return_type.clone(),
                    };
                    if *multi {
                        class_def
                            .methods
                            .entry(resolved_method_name.clone())
                            .or_default()
                            .push(def);
                    } else {
                        class_def
                            .methods
                            .insert(resolved_method_name.clone(), vec![def]);
                    }
                    // `our method` also registers as a package-scoped sub
                    if *is_our {
                        let qualified_name = format!("{}::{}", name, resolved_method_name);
                        // Prepend "self" as first param so the first argument
                        // gets bound as `self` when calling this as a function.
                        let mut our_params = vec!["self".to_string()];
                        our_params.extend(
                            effective_params
                                .iter()
                                .filter(|p| p.as_str() != "self")
                                .cloned(),
                        );
                        let self_param = crate::ast::ParamDef {
                            name: "self".to_string(),
                            default: None,
                            multi_invocant: true,
                            required: false,
                            named: false,
                            slurpy: false,
                            double_slurpy: false,
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
                        };
                        let mut our_param_defs = vec![self_param];
                        our_param_defs.extend(
                            effective_param_defs
                                .iter()
                                .filter(|p| !p.is_invocant)
                                .cloned(),
                        );
                        let func_def = crate::ast::FunctionDef {
                            package: name.to_string(),
                            name: resolved_method_name.clone(),
                            params: our_params,
                            param_defs: our_param_defs,
                            body: method_body.clone(),
                            is_test_assertion: false,
                            is_rw: *is_rw,
                            is_method: true,
                            empty_sig: false,
                            return_type: None,
                        };
                        self.functions.insert(qualified_name, func_def);
                    }
                }
                Stmt::DoesDecl { name: role_name } => {
                    let role =
                        self.roles.get(role_name).cloned().ok_or_else(|| {
                            RuntimeError::new(format!("Unknown role: {}", role_name))
                        })?;
                    if role.is_stub_role {
                        return Err(RuntimeError::new("X::Role::Parametric::NoSuchCandidate"));
                    }
                    for attr in &role.attributes {
                        if !class_def.attributes.iter().any(|(n, _, _, _)| n == &attr.0) {
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
                    if !class_def.parents.iter().any(|p| p == role_name) {
                        // Keep role composition visible in MRO introspection.
                        class_def.parents.insert(0, role_name.clone());
                        class_def.mro.clear();
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
        self.current_package = saved_package;
        self.resolve_class_stub_requirements(name, &mut class_def)?;
        self.classes.insert(name.to_string(), class_def);
        let mut stack = Vec::new();
        let _ = self.compute_class_mro(name, &mut stack)?;
        Ok(())
    }

    pub(crate) fn register_role_decl(
        &mut self,
        name: &str,
        type_params: &[String],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // Clean up stale punned class entry from previous registration
        // (e.g., role R was previously used as `is R` creating a punned class)
        if self.roles.contains_key(name) {
            self.classes.remove(name);
            self.hidden_classes.remove(name);
            self.class_composed_roles.remove(name);
        }
        // Clear stale role parents and hides
        self.role_parents.remove(name);
        self.role_hides.remove(name);
        let mut role_def = RoleDef {
            attributes: Vec::new(),
            methods: HashMap::new(),
            is_stub_role: false,
            is_hidden: false,
        };
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    handles,
                    is_rw,
                } => {
                    role_def.attributes.push((
                        attr_name.clone(),
                        *is_public,
                        default.clone(),
                        *is_rw,
                    ));
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name)
                    } else {
                        format!("!{}", attr_name)
                    };
                    for handle_name in handles {
                        role_def
                            .methods
                            .entry(handle_name.clone())
                            .or_default()
                            .push(MethodDef {
                                params: Vec::new(),
                                param_defs: Vec::new(),
                                body: vec![Stmt::Expr(Expr::MethodCall {
                                    target: Box::new(Expr::Var(attr_var_name.clone())),
                                    name: handle_name.clone(),
                                    args: Vec::new(),
                                    modifier: None,
                                    quoted: false,
                                })],
                                is_rw: false,
                                is_private: false,
                                return_type: None,
                            });
                    }
                }
                Stmt::DoesDecl { name: role_name } => {
                    if role_name == "__mutsu_role_hidden__" {
                        role_def.is_hidden = true;
                        continue;
                    }
                    if let Some(hidden_name) = role_name.strip_prefix("__mutsu_role_hides__") {
                        // Track hidden class relationship for this role
                        self.role_hides
                            .entry(name.to_string())
                            .or_default()
                            .push(hidden_name.to_string());
                        continue;
                    }
                    if self.classes.contains_key(role_name) {
                        self.role_parents
                            .entry(name.to_string())
                            .or_default()
                            .push(role_name.clone());
                        continue;
                    }
                    let role =
                        self.roles.get(role_name).cloned().ok_or_else(|| {
                            RuntimeError::new(format!("Unknown role: {}", role_name))
                        })?;
                    if role.is_stub_role {
                        return Err(RuntimeError::new("X::Role::Parametric::NoSuchCandidate"));
                    }
                    self.role_parents
                        .entry(name.to_string())
                        .or_default()
                        .push(role_name.clone());
                    for attr in &role.attributes {
                        if !role_def.attributes.iter().any(|(n, _, _, _)| n == &attr.0) {
                            role_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        role_def.methods.entry(mname).or_default().extend(overloads);
                    }
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
                    is_our: _,
                    return_type,
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
                        return_type: return_type.clone(),
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
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn" =>
                {
                    role_def.is_stub_role = true;
                }
                _ => {
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                }
            }
        }
        self.roles.insert(name.to_string(), role_def);
        if !type_params.is_empty() {
            self.role_type_params
                .insert(name.to_string(), type_params.to_vec());
        }
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
        self.env
            .insert(name.to_string(), Value::Package(name.to_string()));
    }
}
