use super::registration_class::{AttrValidationCtx, language_revision_letter};
use super::*;

impl Interpreter {
    pub(crate) fn validate_attr_declared_in_class(
        ctx: &AttrValidationCtx<'_>,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        for stmt in stmts {
            Self::validate_attr_in_stmt(ctx, stmt)?;
        }
        Ok(())
    }

    pub(crate) fn validate_attr_in_stmt(
        ctx: &AttrValidationCtx<'_>,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) => {
                Self::validate_attr_in_expr(ctx, e)?;
            }
            Stmt::VarDecl { expr, .. } => {
                Self::validate_attr_in_expr(ctx, expr)?;
            }
            Stmt::Assign { name, expr, .. } => {
                // Check if assigning to an undeclared private attribute ($!attr = ...)
                if let Some(attr_name) = name.strip_prefix('!')
                    && !attr_name.is_empty()
                    && !ctx.attrs.contains(attr_name)
                {
                    return Err(Self::undeclared_attr_error(ctx, attr_name, "!"));
                }
                Self::validate_attr_in_expr(ctx, expr)?;
            }
            Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for e in exprs {
                    Self::validate_attr_in_expr(ctx, e)?;
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                Self::validate_attr_in_expr(ctx, cond)?;
                Self::validate_attr_declared_in_class(ctx, then_branch)?;
                Self::validate_attr_declared_in_class(ctx, else_branch)?;
            }
            Stmt::While { cond, body, .. } => {
                Self::validate_attr_in_expr(ctx, cond)?;
                Self::validate_attr_declared_in_class(ctx, body)?;
            }
            Stmt::For { iterable, body, .. } => {
                Self::validate_attr_in_expr(ctx, iterable)?;
                Self::validate_attr_declared_in_class(ctx, body)?;
            }
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => {
                if let Some(init) = init.as_ref() {
                    Self::validate_attr_in_stmt(ctx, init)?;
                }
                if let Some(cond) = cond.as_ref() {
                    Self::validate_attr_in_expr(ctx, cond)?;
                }
                if let Some(step) = step.as_ref() {
                    Self::validate_attr_in_expr(ctx, step)?;
                }
                Self::validate_attr_declared_in_class(ctx, body)?;
            }
            Stmt::Given { topic, body, .. } => {
                Self::validate_attr_in_expr(ctx, topic)?;
                Self::validate_attr_declared_in_class(ctx, body)?;
            }
            Stmt::When { cond, body, .. } => {
                Self::validate_attr_in_expr(ctx, cond)?;
                Self::validate_attr_declared_in_class(ctx, body)?;
            }
            Stmt::Default(body) => {
                Self::validate_attr_declared_in_class(ctx, body)?;
            }
            _ => {}
        }
        Ok(())
    }

    pub(crate) fn undeclared_attr_error(
        ctx: &AttrValidationCtx<'_>,
        attr_name: &str,
        twigil: &str,
    ) -> RuntimeError {
        let symbol = format!("${}{}", twigil, attr_name);
        let message = format!(
            "Attribute {} not declared in {} {}",
            symbol, ctx.pkg_kind, ctx.pkg_name
        );
        let mut attrs = HashMap::new();
        attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
        attrs.insert(
            "package-name".to_string(),
            Value::str(ctx.pkg_name.to_string()),
        );
        attrs.insert(
            "package-kind".to_string(),
            Value::str(ctx.pkg_kind.to_string()),
        );
        attrs.insert("what".to_string(), Value::str("attribute".to_string()));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Attribute::Undeclared"), attrs);
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        err
    }

    pub(crate) fn validate_attr_in_expr(
        ctx: &AttrValidationCtx<'_>,
        expr: &Expr,
    ) -> Result<(), RuntimeError> {
        match expr {
            Expr::Var(name) => {
                // $! by itself is the error variable, not an attribute
                if let Some(attr_name) = name.strip_prefix('!')
                    && !attr_name.is_empty()
                    && !ctx.attrs.contains(attr_name)
                {
                    return Err(Self::undeclared_attr_error(ctx, attr_name, "!"));
                }
                // $.attr is compiled as self.attr() — undeclared attributes will
                // fail at runtime with "No such method", no compile-time check needed.
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                Self::validate_attr_in_expr(ctx, target)?;
                for arg in args {
                    Self::validate_attr_in_expr(ctx, arg)?;
                }
            }
            Expr::Call { args, .. }
            | Expr::ArrayLiteral(args)
            | Expr::BracketArray(args, _)
            | Expr::StringInterpolation(args) => {
                for arg in args {
                    Self::validate_attr_in_expr(ctx, arg)?;
                }
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::Reduction { expr, .. } => {
                Self::validate_attr_in_expr(ctx, expr)?;
            }
            Expr::Binary { left, right, .. }
            | Expr::MetaOp { left, right, .. }
            | Expr::HyperOp { left, right, .. } => {
                Self::validate_attr_in_expr(ctx, left)?;
                Self::validate_attr_in_expr(ctx, right)?;
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::validate_attr_in_expr(ctx, cond)?;
                Self::validate_attr_in_expr(ctx, then_expr)?;
                Self::validate_attr_in_expr(ctx, else_expr)?;
            }
            Expr::Index { target, index, .. } => {
                Self::validate_attr_in_expr(ctx, target)?;
                Self::validate_attr_in_expr(ctx, index)?;
            }
            Expr::IndexAssign {
                target,
                index,
                value,
                ..
            } => {
                Self::validate_attr_in_expr(ctx, target)?;
                Self::validate_attr_in_expr(ctx, index)?;
                Self::validate_attr_in_expr(ctx, value)?;
            }
            Expr::AssignExpr { name, expr, .. } => {
                // Check if assigning to an undeclared private attribute ($!attr = ...)
                if let Some(attr_name) = name.strip_prefix('!')
                    && !attr_name.is_empty()
                    && !ctx.attrs.contains(attr_name)
                {
                    return Err(Self::undeclared_attr_error(ctx, attr_name, "!"));
                }
                // $.attr assignment is compiled as self.attr = ... — undeclared
                // attributes will fail at runtime, no compile-time check needed.
                Self::validate_attr_in_expr(ctx, expr)?;
            }
            Expr::DoBlock { body, .. }
            | Expr::Block(body)
            | Expr::Gather(body)
            | Expr::AnonSub { body, .. }
            | Expr::AnonSubParams { body, .. }
            | Expr::Lambda { body, .. } => {
                Self::validate_attr_declared_in_class(ctx, body)?;
            }
            Expr::Try { body: _, catch } => {
                // Skip attribute validation inside try blocks — accessing an
                // undeclared attribute will produce a runtime error that the
                // try block can catch.
                if let Some(catch) = catch.as_ref() {
                    Self::validate_attr_declared_in_class(ctx, catch)?;
                }
            }
            Expr::DoStmt(stmt) => {
                Self::validate_attr_in_stmt(ctx, stmt)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Store a specific language version as type metadata for ^language-revision.
    pub(crate) fn store_language_revision_from_version(&mut self, name: &str, version: &str) {
        let revision = language_revision_letter(version);
        let meta = self.type_metadata.entry(name.to_string()).or_default();
        meta.insert("language-revision".to_string(), Value::str(revision));
    }
}
