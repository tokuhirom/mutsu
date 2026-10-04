use super::registration_class::{AttrValidationCtx, language_revision_letter};
use super::*;
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt, walk_stmts};

impl Interpreter {
    /// Reject a `$!attr` read or assignment in `stmts` (a method body) that
    /// names no attribute the package declares (`X::Attribute::Undeclared`).
    // Cost: O(n), n = size of the AST of `stmts`.
    pub(crate) fn validate_attr_declared_in_class(
        ctx: &AttrValidationCtx<'_>,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut scan = AttrScan { ctx, err: None };
        walk_stmts(&mut scan, stmts);
        scan.err.map_or(Ok(()), Err)
    }

    pub(crate) fn undeclared_attr_error(
        ctx: &AttrValidationCtx<'_>,
        attr_name: &str,
        twigil: &str,
    ) -> RuntimeError {
        Self::undeclared_attr_symbol_error(ctx, format!("${twigil}{attr_name}"))
    }

    /// [`Self::undeclared_attr_error`] for an attribute spelled `symbol`
    /// (`$!x`, `@!a`, `%!h`).
    fn undeclared_attr_symbol_error(ctx: &AttrValidationCtx<'_>, symbol: String) -> RuntimeError {
        // `ctx.pkg_name` is the class's REGISTRY storage name, which for a
        // `my`-scoped declaration is mangled (ADR-0047 P1: `Foo\u{0}<id>`).
        // The exception's `.package-name` and message must show the
        // user-facing bare name, like every other class-name-in-a-message
        // site.
        let pkg_name = crate::value::user_facing_type_name(ctx.pkg_name);
        let message = format!(
            "Attribute {} not declared in {} {}",
            symbol, ctx.pkg_kind, pkg_name
        );
        let mut attrs = HashMap::new();
        attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
        attrs.insert("package-name".to_string(), Value::str(pkg_name.to_string()));
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

    /// Store a specific language version as type metadata for ^language-revision.
    pub(crate) fn store_language_revision_from_version(&mut self, name: &str, version: &str) {
        let revision = language_revision_letter(version);
        let meta = crate::runtime::cow_table_mut(&mut self.types.type_metadata)
            .entry(name.to_string())
            .or_default();
        meta.insert("language-revision".to_string(), Value::str(revision));
    }
}

/// The walk of [`Interpreter::validate_attr_declared_in_class`] (ADR-0137).
struct AttrScan<'a, 'c> {
    ctx: &'a AttrValidationCtx<'c>,
    err: Option<RuntimeError>,
}

impl<'ast> Visit<'ast> for AttrScan<'_, '_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.err.is_none() {
            match stmt {
                // A nested type declares its own attributes, and its methods
                // are validated against them when it registers.
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => {}
                _ => walk_stmt(self, stmt),
            }
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.err.is_none() {
            match expr {
                // Not checked inside a `try` body: the undeclared attribute is
                // left to fail at run time, where the `try` catches it (the
                // `$!an_A` subtests of roast/S12-attributes/trusts.t). Its
                // `catch` half is still checked.
                Expr::Try { body: _, catch } => {
                    if let Some(catch) = catch {
                        walk_stmts(self, catch);
                    }
                }
                _ => walk_expr(self, expr),
            }
        }
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        // A `$!attr` / `@!attr` / `%!attr` / `&!attr` read, or an assignment to one. The
        // AST spells a `$` variable without its sigil and an `@`/`%` one
        // without it as a variable but with it as an assignment target.
        // `$!` by itself is the error variable, not an attribute. `$.attr`
        // compiles to `self.attr`, which an undeclared attribute fails at run
        // time ("No such method"), so it needs no check here.
        if self.err.is_some() {
            return;
        }
        let (sigil, rest) = match kind {
            NameKind::Var => ('$', name),
            NameKind::ArrayVar => ('@', name),
            NameKind::HashVar => ('%', name),
            NameKind::CodeVar => ('&', name),
            NameKind::AssignTarget => match name.strip_prefix(['@', '%']) {
                Some(rest) => (name.chars().next().unwrap_or('$'), rest),
                None => ('$', name),
            },
            _ => return,
        };
        if let Some(attr_name) = rest.strip_prefix('!')
            && !attr_name.is_empty()
            && !self.ctx.attrs.contains(attr_name)
        {
            self.err = Some(Interpreter::undeclared_attr_symbol_error(
                self.ctx,
                format!("{sigil}!{attr_name}"),
            ));
        }
    }
}
