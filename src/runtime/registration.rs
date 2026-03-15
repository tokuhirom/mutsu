use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn validate_callable_param_return_redeclaration(
        param_defs: &[ParamDef],
    ) -> Result<(), RuntimeError> {
        for pd in param_defs {
            if pd.type_constraint.is_some()
                && pd
                    .code_signature
                    .as_ref()
                    .is_some_and(|(_, ret)| ret.is_some())
            {
                return Err(RuntimeError::new(
                    "X::Redeclaration: only one way of specifying sub-signature return type allowed",
                ));
            }
        }
        Ok(())
    }

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

    pub(super) fn effective_method_param_defs(
        param_defs: &[ParamDef],
        class_is_hidden: bool,
    ) -> Vec<ParamDef> {
        let mut defs = param_defs.to_vec();
        if !class_is_hidden && !Self::has_explicit_named_slurpy(&defs) {
            defs.push(Self::implicit_method_named_slurpy_param());
        }
        defs
    }

    pub(super) fn is_stub_routine_body(body: &[Stmt]) -> bool {
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
            .any(|(attr_name, is_public, ..)| *is_public && attr_name == method_name)
    }

    pub(super) fn resolve_class_stub_requirements(
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

    pub(super) fn validate_private_access_in_stmts(
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
            Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
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
                    && let Some((owner_class, _)) = name.resolve().split_once("::")
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
            Expr::HyperMethodCallDynamic {
                target,
                name_expr,
                args,
                ..
            } => {
                self.validate_private_access_in_expr(caller_class, target)?;
                self.validate_private_access_in_expr(caller_class, name_expr)?;
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

    /// Validate that all `self!method()` calls in the class body reference
    /// private methods that actually exist on the class (compile-time check).
    pub(super) fn validate_private_method_existence(
        &self,
        class_name: &str,
    ) -> Result<(), RuntimeError> {
        let class_def = match self.classes.get(class_name) {
            Some(cd) => cd.clone(),
            None => return Ok(()),
        };
        for overloads in class_def.methods.values() {
            for method_def in overloads {
                self.check_private_calls_exist(class_name, &class_def, &method_def.body)?;
            }
        }
        Ok(())
    }

    fn check_private_calls_exist(
        &self,
        class_name: &str,
        class_def: &ClassDef,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.check_private_calls_exist_stmt(class_name, class_def, stmt)?;
        }
        Ok(())
    }

    fn check_private_calls_exist_stmt(
        &self,
        class_name: &str,
        class_def: &ClassDef,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
                self.check_private_calls_exist_expr(class_name, class_def, e)?;
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, expr)?;
            }
            Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for e in exprs {
                    self.check_private_calls_exist_expr(class_name, class_def, e)?;
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                self.check_private_calls_exist_expr(class_name, class_def, cond)?;
                self.check_private_calls_exist(class_name, class_def, then_branch)?;
                self.check_private_calls_exist(class_name, class_def, else_branch)?;
            }
            Stmt::While { cond, body, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, cond)?;
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            Stmt::For { iterable, body, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, iterable)?;
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            Stmt::Block(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::When { body, .. }
            | Stmt::Given { body, .. }
            | Stmt::Phaser { body, .. } => {
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn check_private_calls_exist_expr(
        &self,
        class_name: &str,
        class_def: &ClassDef,
        expr: &Expr,
    ) -> Result<(), RuntimeError> {
        match expr {
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                ..
            } => {
                self.check_private_calls_exist_expr(class_name, class_def, target)?;
                for arg in args {
                    self.check_private_calls_exist_expr(class_name, class_def, arg)?;
                }
                // Check self!method() calls
                if *modifier == Some('!')
                    && matches!(target.as_ref(), Expr::BareWord(w) if w == "self")
                {
                    let method_name = name.resolve();
                    // Skip owner-qualified calls (e.g., Class::method)
                    if !method_name.contains("::") {
                        let has_method = class_def
                            .methods
                            .get(&method_name)
                            .is_some_and(|overloads| overloads.iter().any(|md| md.is_private));
                        if !has_method {
                            return Err(super::methods_signature::make_method_not_found_error(
                                &method_name,
                                class_name,
                                true,
                            ));
                        }
                    }
                }
            }
            Expr::Binary { left, right, .. }
            | Expr::MetaOp { left, right, .. }
            | Expr::HyperOp { left, right, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, left)?;
                self.check_private_calls_exist_expr(class_name, class_def, right)?;
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::Reduction { expr, .. }
            | Expr::AssignExpr { expr, .. } => {
                self.check_private_calls_exist_expr(class_name, class_def, expr)?;
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.check_private_calls_exist_expr(class_name, class_def, cond)?;
                self.check_private_calls_exist_expr(class_name, class_def, then_expr)?;
                self.check_private_calls_exist_expr(class_name, class_def, else_expr)?;
            }
            Expr::Index { target, index } => {
                self.check_private_calls_exist_expr(class_name, class_def, target)?;
                self.check_private_calls_exist_expr(class_name, class_def, index)?;
            }
            Expr::Call { args, .. }
            | Expr::ArrayLiteral(args)
            | Expr::BracketArray(args)
            | Expr::StringInterpolation(args) => {
                for arg in args {
                    self.check_private_calls_exist_expr(class_name, class_def, arg)?;
                }
            }
            Expr::Block(body)
            | Expr::AnonSub { body, .. }
            | Expr::AnonSubParams { body, .. }
            | Expr::Lambda { body, .. }
            | Expr::Gather(body) => {
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            Expr::DoBlock { body, .. } => {
                self.check_private_calls_exist(class_name, class_def, body)?;
            }
            Expr::DoStmt(stmt) => {
                self.check_private_calls_exist_stmt(class_name, class_def, stmt)?;
            }
            Expr::Try { body, catch } => {
                self.check_private_calls_exist(class_name, class_def, body)?;
                if let Some(catch) = catch.as_ref() {
                    self.check_private_calls_exist(class_name, class_def, catch)?;
                }
            }
            Expr::CallOn { target, args } => {
                self.check_private_calls_exist_expr(class_name, class_def, target)?;
                for arg in args {
                    self.check_private_calls_exist_expr(class_name, class_def, arg)?;
                }
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
        self.functions.contains_key(&Symbol::intern(&fq))
            || self.functions.contains_key(&Symbol::intern(name))
    }

    pub(crate) fn is_implicit_zero_arg_builtin(name: &str) -> bool {
        matches!(name, "dir" | "lines")
    }

    /// Check if a multi-dispatched function with the given name exists (any arity).
    pub(crate) fn has_multi_function(&self, name: &str) -> bool {
        let fq_slash = format!("{}::{}/", self.current_package, name);
        self.functions
            .keys()
            .any(|k| k.resolve().starts_with(&fq_slash))
    }

    pub(super) fn malformed_return_value_compile_error() -> RuntimeError {
        let mut err = RuntimeError::new("Malformed return value");
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from("Malformed return value"),
        );
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::AdHoc"),
            attrs,
        )));
        err
    }

    pub(super) fn body_contains_non_nil_return(stmts: &[Stmt]) -> bool {
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
}
