//! Compile-time checks of private-method calls in a class's method bodies.
//!
//! Rakudo resolves `$obj!Owner::meth` and `self!meth` while compiling the
//! method, so an untrusted qualified call or a call to a private method the
//! class does not have is a compile-time error wherever in the body it is
//! written. Both checks walk the body with the typed AST visitor (ADR-0137),
//! so every position is searched — a named argument, a hash value, a
//! parameter default of a nested closure — not only the forms an older
//! hand-rolled walker happened to list.

use super::methods_signature_errors::{make_method_not_found_error, make_private_permission_error};
use super::*;
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};

impl Interpreter {
    /// Reject a qualified private call (`$o!Owner::meth`) in `stmts` whose
    /// owner does not trust `caller_class`.
    // Cost: O(n), n = size of the AST of `stmts` (one trust lookup per
    // qualified private call).
    pub(super) fn validate_private_access_in_stmts(
        &self,
        caller_class: &str,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut scan = PrivateAccess {
            interp: self,
            caller_class,
            nested: Vec::new(),
            err: None,
        };
        walk_stmts(&mut scan, stmts);
        scan.err.map_or(Ok(()), Err)
    }

    /// Validate that all `self!method()` calls in the class body reference
    /// private methods that actually exist on the class (compile-time check).
    // Cost: O(m * n), m = methods of the class, n = size of one method body.
    pub(super) fn validate_private_method_existence(
        &self,
        class_name: &str,
    ) -> Result<(), RuntimeError> {
        if !self.registry().classes.contains_key(class_name) {
            return Ok(());
        }
        // ADR-0019 F4c-1: enumerate via the canonical reverse index instead
        // of `class_def.methods.values()` (zero-mismatch shadow-checked
        // across the full local `t/` suite before this cutover).
        let registry = self.registry();
        for method_name in registry.owner_method_names(class_name) {
            let method_name = method_name.resolve();
            let Some(overloads) = registry.user_method_overloads(class_name, &method_name) else {
                continue;
            };
            for method_def in &overloads {
                self.check_private_calls_exist(class_name, &method_def.body)?;
            }
        }
        Ok(())
    }

    /// Validate `self!method()` private calls in freshly compiled statements
    /// (e.g. an EVAL'd string) against the class of the lexical `self` in scope.
    /// Raku resolves private method dispatch at compile time, so a call to a
    /// nonexistent private method is an error even when a preceding `return`
    /// would short-circuit it at runtime — this reproduces that for EVAL bodies
    /// running inside a method.
    // Cost: O(n), n = size of the AST of `stmts`.
    pub(crate) fn validate_private_calls_against_self(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let class_name = match self.env.get("self").map(Value::view) {
            Some(ValueView::Instance { class_name, .. }) => class_name.resolve(),
            _ => return Ok(()),
        };
        if !self.registry().classes.contains_key(&class_name) {
            return Ok(());
        }
        self.check_private_calls_exist(&class_name, stmts)
    }

    // Cost: O(n), n = size of the AST of `stmts`.
    fn check_private_calls_exist(
        &self,
        class_name: &str,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut scan = PrivateCallsExist {
            interp: self,
            class_name,
            err: None,
        };
        walk_stmts(&mut scan, stmts);
        scan.err.map_or(Ok(()), Err)
    }
}

/// The trust check of [`Interpreter::validate_private_access_in_stmts`].
struct PrivateAccess<'a> {
    interp: &'a Interpreter,
    caller_class: &'a str,
    /// Classes declared inside the walked body, with the `trusts` list written
    /// in their own body. They are not registered yet while the enclosing
    /// class registers, so `class_trusts` cannot answer for them.
    nested: Vec<(String, Vec<String>)>,
    err: Option<RuntimeError>,
}

impl<'ast> Visit<'ast> for PrivateAccess<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.err.is_some() {
            return;
        }
        match stmt {
            // A nested class is its own caller: its methods' private calls are
            // checked against it when it registers. Remember its `trusts`
            // list so a call into it from this body can be decided now.
            Stmt::ClassDecl { name, body, .. } => {
                let trusts = body
                    .iter()
                    .filter_map(|s| match s {
                        Stmt::TrustsDecl { name } => Some(name.resolve()),
                        _ => None,
                    })
                    .collect();
                self.nested.push((name.resolve(), trusts));
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.err.is_some() {
            return;
        }
        match expr {
            // Not checked inside a `try` body: the untrusted call is left to
            // fail at run time, where the `try` catches it. Its CATCH-like
            // `catch` half is still checked.
            Expr::Try { body: _, catch } => {
                if let Some(catch) = catch {
                    walk_stmts(self, catch);
                }
            }
            Expr::MethodCall {
                name,
                modifier: Some('!'),
                ..
            } => {
                walk_expr(self, expr);
                if self.err.is_some() {
                    return;
                }
                // Split at the LAST `::`: the owner class of a qualified private
                // call may itself be a nested name (`$c!Cookie::Jar::Cookie::match`
                // is owner `Cookie::Jar::Cookie`, not `Cookie::Jar`).
                let name = name.resolve();
                if let Some((owner_class, method_name)) = name.rsplit_once("::") {
                    // `owner_class` is the short name as written in source
                    // (`Renderer`), while `caller_class` is always the fully
                    // qualified registered name (`Outer::Inner::Renderer`).
                    // Canonicalize before comparing, the same way an ordinary
                    // bareword type reference resolves against its enclosing
                    // package chain — otherwise a perfectly legal self-call
                    // written from inside a `module` false-positives here.
                    let (canonical_owner, trusted) =
                        match self.nested.iter().find(|(n, _)| n == owner_class) {
                            Some((_, trusts)) => (
                                owner_class.to_string(),
                                trusts.iter().any(|t| {
                                    // `caller_class` may be a mangled `my class`
                                    // storage name; `t` is the name as written.
                                    let caller =
                                        crate::value::user_facing_type_name(self.caller_class);
                                    *t == caller || caller.rsplit("::").next() == Some(t.as_str())
                                }),
                            ),
                            None => self.interp.resolve_and_check_private_owner(
                                Some(self.caller_class),
                                owner_class,
                            ),
                        };
                    if !trusted {
                        self.err = Some(make_private_permission_error(
                            method_name,
                            &canonical_owner,
                            self.caller_class,
                        ));
                    }
                }
            }
            _ => walk_expr(self, expr),
        }
    }
}

/// The existence check of [`Interpreter::validate_private_method_existence`]:
/// every unqualified `self!meth` names a private method of `class_name`.
struct PrivateCallsExist<'a> {
    interp: &'a Interpreter,
    class_name: &'a str,
    err: Option<RuntimeError>,
}

impl<'ast> Visit<'ast> for PrivateCallsExist<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.err.is_some() {
            return;
        }
        match stmt {
            // `self` inside a nested class or role body is that type's
            // invocant, not `class_name`'s.
            Stmt::ClassDecl {
                name: nested_name,
                body,
                does_parents,
                ..
            } if does_parents.is_empty() => {
                // The nested class is not registered while the enclosing
                // class registers, so check its methods against the private
                // methods its own body declares.
                let own: Vec<String> = body
                    .iter()
                    .filter_map(|s| match s {
                        Stmt::MethodDecl {
                            name,
                            is_private: true,
                            ..
                        } => Some(name.resolve()),
                        _ => None,
                    })
                    .collect();
                for s in body {
                    if let Stmt::MethodDecl { body, .. } = s {
                        let mut scan = NestedPrivateCalls {
                            class_name: crate::qualified::qualified(
                                Symbol::intern(self.class_name),
                                *nested_name,
                            )
                            .resolve(),
                            own: &own,
                            err: None,
                        };
                        walk_stmts(&mut scan, body);
                        if let Some(err) = scan.err {
                            self.err = Some(err);
                            return;
                        }
                    }
                }
            }
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.err.is_some() {
            return;
        }
        walk_expr(self, expr);
        if self.err.is_some() {
            return;
        }
        if let Expr::MethodCall {
            target,
            name,
            modifier: Some('!'),
            ..
        } = expr
            && matches!(target.as_ref(), Expr::BareWord(w) if w == "self")
        {
            let method_name = name.resolve();
            // Skip owner-qualified calls (e.g., Class::method)
            if method_name.contains("::") {
                return;
            }
            let has_method = self
                .interp
                .registry()
                .user_method_overloads(self.class_name, &method_name)
                .is_some_and(|overloads| overloads.iter().any(|md| md.is_private));
            if !has_method {
                self.err = Some(make_method_not_found_error(
                    &method_name,
                    self.class_name,
                    true,
                ));
            }
        }
    }
}

/// The existence check for a class declared inside a method body: every
/// unqualified `self!meth` in its methods names a private method its own body
/// declares.
struct NestedPrivateCalls<'a> {
    class_name: String,
    own: &'a [String],
    err: Option<RuntimeError>,
}

impl<'ast> Visit<'ast> for NestedPrivateCalls<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.err.is_some() {
            return;
        }
        match stmt {
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.err.is_some() {
            return;
        }
        walk_expr(self, expr);
        if self.err.is_some() {
            return;
        }
        if let Expr::MethodCall {
            target,
            name,
            modifier: Some('!'),
            ..
        } = expr
            && matches!(target.as_ref(), Expr::BareWord(w) if w == "self")
        {
            let method_name = name.resolve();
            if !crate::qualified::is_qualified(*name) && !self.own.contains(&method_name) {
                self.err = Some(make_method_not_found_error(
                    &method_name,
                    &self.class_name,
                    true,
                ));
            }
        }
    }
}
