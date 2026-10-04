//! The type-name scans behind the compile-time type checks of a unit
//! (`eval_check.rs`): which types it declares, which `use lib` paths it adds,
//! and the checks over parameter types, type arguments, `trusts` targets and
//! inheritance from a type capture. Every walk is the typed AST visitor
//! (ADR-0137).
//!
//! A type capture (`::T`) is in scope for the body of whatever declares it —
//! a routine, method or block signature, or a role's type parameters — so
//! the scans that judge a type name keep a stack of the captures in scope.

use super::*;
use crate::ast::{ParamDef, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_param, walk_stmt, walk_stmts};

/// The signatures a statement introduces for its own body.
fn stmt_signatures(stmt: &Stmt) -> Vec<&[ParamDef]> {
    match stmt {
        Stmt::SubDecl {
            param_defs,
            signature_alternates,
            ..
        } => std::iter::once(param_defs.as_slice())
            .chain(signature_alternates.iter().map(|(_, defs)| defs.as_slice()))
            .collect(),
        Stmt::MethodDecl { param_defs, .. }
        | Stmt::TokenDecl { param_defs, .. }
        | Stmt::RuleDecl { param_defs, .. }
        | Stmt::ProtoDecl { param_defs, .. }
        | Stmt::Whenever { param_defs, .. } => vec![param_defs.as_slice()],
        Stmt::RoleDecl {
            type_param_defs, ..
        } => vec![type_param_defs.as_slice()],
        Stmt::For {
            param_def,
            params_def,
            ..
        } => param_def
            .as_ref()
            .as_ref()
            .map(std::slice::from_ref)
            .into_iter()
            .chain(std::iter::once(params_def.as_slice()))
            .collect(),
        _ => Vec::new(),
    }
}

/// The type captures (`::T`) a list of signatures declares.
fn signature_captures<'a>(sigs: impl IntoIterator<Item = &'a [ParamDef]>) -> Vec<String> {
    sigs.into_iter()
        .flatten()
        .filter_map(|pd| pd.captured_type_name())
        .filter(|name| !name.is_empty() && !crate::qualified::is_qualified_str(name))
        .map(str::to_string)
        .collect()
}

/// The type captures in scope at the current node.
#[derive(Default)]
pub(super) struct Captures {
    names: Vec<String>,
}

impl Captures {
    // Cost: O(k), k = captures in scope.
    pub(super) fn contains(&self, name: &str) -> bool {
        self.names.iter().any(|n| n == name)
    }

    /// Runs `f` with `added` pushed onto the captures in scope.
    fn scoped<V>(
        v: &mut V,
        captures: impl Fn(&mut V) -> &mut Captures,
        added: Vec<String>,
        f: impl FnOnce(&mut V),
    ) {
        let mark = captures(v).names.len();
        captures(v).names.extend(added);
        f(v);
        captures(v).names.truncate(mark);
    }
}

/// Every type, package and class name a unit declares, anywhere in it.
#[derive(Default)]
pub(super) struct DeclaredTypes {
    /// Type-like names (class, role, grammar, enum and its keys, subset,
    /// constants).
    pub(super) types: HashSet<String>,
    /// `module`/`package` names: not type-like.
    pub(super) packages: HashSet<String>,
    /// Plain classes: type-like but not parametric.
    pub(super) classes: HashSet<String>,
    /// A `use`d module computes its import set in a `sub EXPORT` hook, so the
    /// names the unit imports are not known before it runs (#11062).
    pub(super) imports_through_export_hook: bool,
}

/// Record a declared type/package name under every spelling it is reachable by.
///
/// `class GLOBAL::Foo` declares `Foo` in the global namespace (class/role
/// registration strips the prefix), so a later `sub f(Foo $x)` must not be
/// rejected as an invalid typename. The prefixed spelling is kept too — a
/// parameter may legitimately be written `GLOBAL::Foo`.
fn insert_declared_name(out: &mut HashSet<String>, name: &str) {
    if let Some(stripped) = name.strip_prefix("GLOBAL::") {
        out.insert(stripped.to_string());
    }
    out.insert(name.to_string());
}

/// Adds the type names a `use`d module declares to the set, and returns whether
/// the module computes its import set in a `sub EXPORT` hook.
pub(super) type HarvestFn<'a> = dyn Fn(&str, &mut HashSet<String>) -> bool + 'a;

/// Collects [`DeclaredTypes`], and — given an interpreter — the types each
/// `use`d module's source declares (`harvest`). Declarations are collected
/// scope-blind: a type declared anywhere only ever widens what the checks
/// accept.
pub(super) struct TypeDecls<'a> {
    pub(super) harvest: Option<&'a HarvestFn<'a>>,
    pub(super) out: DeclaredTypes,
}

impl<'ast> Visit<'ast> for TypeDecls<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        let out = &mut self.out;
        match stmt {
            Stmt::Use { module, .. } => {
                if let Some(harvest) = self.harvest
                    && harvest(module, &mut out.types)
                {
                    out.imports_through_export_hook = true;
                }
            }
            // `need` loads without importing, so an export hook never runs.
            Stmt::Need { module } => {
                if let Some(harvest) = self.harvest {
                    harvest(module, &mut out.types);
                }
            }
            Stmt::ClassDecl { name, body, .. } => {
                // A plain `class` is type-like but NOT parametric (parameterizing
                // it with `[T]`/`of T` is X::NotParametric); roles ARE parametric.
                // So is a class with its own `method ^parameterize` (upstream
                // NativeCall's `CArray`), whose `C[T]` calls that meta-method.
                insert_declared_name(&mut out.types, &name.resolve());
                let parametric = body.iter().any(
                    |s| matches!(s, Stmt::MethodDecl { name, .. } if *name == "^parameterize"),
                );
                if !parametric {
                    insert_declared_name(&mut out.classes, &name.resolve());
                }
            }
            Stmt::RoleDecl { name, .. } | Stmt::SubsetDecl { name, .. } => {
                insert_declared_name(&mut out.types, &name.resolve());
            }
            Stmt::EnumDecl { name, variants, .. } => {
                insert_declared_name(&mut out.types, &name.resolve());
                // Enum values are valid value-params (`sub f(SomeEnumValue)`),
                // and serve as suggestions for a mistyped one.
                for (vname, _) in variants {
                    out.types.insert(vname.clone());
                }
            }
            Stmt::Package { name, kind, .. } => {
                // `module`/`package` are not type-like (a parameter typed by one
                // is X::Parameter::BadType); `grammar` is a real type.
                if matches!(
                    kind,
                    crate::ast::PackageKind::Module | crate::ast::PackageKind::Package
                ) {
                    insert_declared_name(&mut out.packages, &name.resolve());
                } else {
                    insert_declared_name(&mut out.types, &name.resolve());
                }
            }
            // `constant HANDLE = uint32;` aliases a type, and the alias is usable
            // wherever a type name is (`sub GetProcessHeap(--> HANDLE)`, which is
            // how C bindings spell their platform types). A constant bound to a
            // *value* is just as valid there: rakudo turns `sub f(TAU)` /
            // `sub f(TAU $x)` into a value constraint (the value's type plus a
            // smartmatch against it), which the binder resolves from the
            // constant at dispatch time -- `secp256k1`'s
            // `multi infix:<*>(Int $n, G)` special-cases its generator point
            // that way. So every sigilless `constant` name is recorded; whether
            // it names a type or a value is decided when it is bound.
            Stmt::VarDecl {
                name,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(t, _)| t == "__constant")
                && name.starts_with(|c: char| c.is_ascii_uppercase() || c.is_ascii_lowercase())
                && !name.starts_with(['$', '@', '%', '&']) =>
            {
                insert_declared_name(&mut out.types, name);
            }
            _ => {}
        }
        walk_stmt(self, stmt);
    }
}

/// The literal paths every `use lib ...` in a unit adds to the search path.
pub(super) struct UseLibDirs<'a> {
    pub(super) file: Option<&'a str>,
    pub(super) program: Option<&'a str>,
    pub(super) out: Vec<String>,
}

impl UseLibDirs<'_> {
    /// Folds each path of one `use lib` argument.
    fn push_paths(&mut self, expr: &Expr) {
        for arg in crate::parser::use_lib_args(expr) {
            if let Some(path) = crate::parser::fold_use_lib_path(arg, self.file, self.program) {
                self.out.push(path);
            }
        }
    }
}

impl<'ast> Visit<'ast> for UseLibDirs<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::Use {
            module,
            arg: Some(arg),
            ..
        } = stmt
            && module == "lib"
        {
            self.push_paths(arg);
        }
        walk_stmt(self, stmt);
    }
}

/// Validates the parameter and return types of every `sub` against the
/// unit's declared types, with the enclosing type captures in scope.
pub(super) struct SubParamTypes<'a> {
    pub(super) interp: &'a Interpreter,
    pub(super) declared: &'a DeclaredTypes,
    pub(super) captures: Captures,
    pub(super) error: Option<RuntimeError>,
}

impl SubParamTypes<'_> {
    fn validate_sub(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        let Stmt::SubDecl {
            param_defs,
            return_type,
            custom_traits,
            ..
        } = stmt
        else {
            return Ok(());
        };
        let in_scope: HashSet<String> = self.captures.names.iter().cloned().collect();
        let d = self.declared;
        self.interp.validate_param_type_constraints(
            param_defs,
            &d.types,
            &d.packages,
            &d.classes,
            &in_scope,
        )?;
        let via_trait = custom_traits
            .iter()
            .any(|(t, _)| t == "__return_via_trait" || t == "__return_via_of");
        self.interp.validate_return_type_constraint(
            return_type.as_deref(),
            param_defs,
            &d.types,
            via_trait,
            &in_scope,
        )
    }
}

impl<'ast> Visit<'ast> for SubParamTypes<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.error.is_some() {
            return;
        }
        match stmt {
            // Deliberate stop (kept from the original walk): a type body's
            // routines may forward-reference the enclosing type, and a role's
            // routines its type parameters, before either is registered.
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::AugmentClass { .. } => {}
            _ => {
                if let Err(e) = self.validate_sub(stmt) {
                    self.error = Some(e);
                    return;
                }
                let added = signature_captures(stmt_signatures(stmt));
                Captures::scoped(self, |v| &mut v.captures, added, |v| walk_stmt(v, stmt));
            }
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.error.is_some() {
            return;
        }
        let added = expr_captures(expr);
        Captures::scoped(self, |v| &mut v.captures, added, |v| walk_expr(v, expr));
    }
}

/// The type captures an expression's signature declares for its body.
fn expr_captures(expr: &Expr) -> Vec<String> {
    match expr {
        Expr::AnonSubParams { param_defs, .. } => signature_captures([param_defs.as_slice()]),
        _ => Vec::new(),
    }
}

/// Rejects `class C is T {}` where `T` is a type capture in scope.
#[derive(Default)]
pub(super) struct CaptureInheritance {
    captures: Captures,
    pub(super) error: Option<RuntimeError>,
}

impl CaptureInheritance {
    fn check_parents(&mut self, name: &str, parents: &[String]) {
        for parent in parents {
            let base = crate::qualified::type_capture_name(parent).unwrap_or(parent);
            if !self.captures.contains(base) {
                continue;
            }
            let child_display = if name.starts_with("__ANON_CLASS_") {
                "<anon>".to_string()
            } else {
                name.to_string()
            };
            let msg = format!(
                "{base} does not support inheritance, so {child_display} cannot inherit from it"
            );
            let mut attrs = ValueMap::default();
            attrs.insert("child-typename".to_string(), Value::str(child_display));
            attrs.insert(
                "parent".to_string(),
                Value::package(crate::symbol::Symbol::intern(base)),
            );
            attrs.insert("message".to_string(), Value::str(msg));
            self.error = Some(RuntimeError::typed("X::Inheritance::Unsupported", attrs));
            return;
        }
    }
}

impl<'ast> Visit<'ast> for CaptureInheritance {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.error.is_some() {
            return;
        }
        if let Stmt::ClassDecl { name, parents, .. } = stmt {
            self.check_parents(&name.resolve(), parents);
            if self.error.is_some() {
                return;
            }
        }
        let added = signature_captures(stmt_signatures(stmt));
        Captures::scoped(self, |v| &mut v.captures, added, |v| walk_stmt(v, stmt));
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.error.is_some() {
            return;
        }
        let added = expr_captures(expr);
        Captures::scoped(self, |v| &mut v.captures, added, |v| walk_expr(v, expr));
    }
}

/// Finds the first type argument (`Array[Numerix]`) that names an undeclared
/// type, in a variable, attribute, parameter or return type.
pub(super) struct TypeArgs<'a> {
    pub(super) interp: &'a Interpreter,
    pub(super) declared: &'a HashSet<String>,
    captures: Captures,
    pub(super) found: Option<String>,
}

impl<'a> TypeArgs<'a> {
    pub(super) fn new(interp: &'a Interpreter, declared: &'a HashSet<String>) -> Self {
        TypeArgs {
            interp,
            declared,
            captures: Captures::default(),
            found: None,
        }
    }

    fn check(&mut self, tc: Option<&str>) {
        if self.found.is_none()
            && let Some(tc) = tc
        {
            self.found = self
                .interp
                .first_undeclared_type_arg(tc, self.declared, &self.captures);
        }
    }
}

impl<'ast> Visit<'ast> for TypeArgs<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        match stmt {
            Stmt::VarDecl {
                type_constraint, ..
            }
            | Stmt::HasDecl {
                type_constraint, ..
            } => self.check(type_constraint.as_deref()),
            _ => {}
        }
        let added = signature_captures(stmt_signatures(stmt));
        Captures::scoped(
            self,
            |v| &mut v.captures,
            added,
            |v| {
                // A return type sees the signature's own captures.
                if let Stmt::SubDecl { return_type, .. } | Stmt::MethodDecl { return_type, .. } =
                    stmt
                {
                    v.check(return_type.as_deref());
                }
                walk_stmt(v, stmt)
            },
        );
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found.is_some() {
            return;
        }
        let added = expr_captures(expr);
        Captures::scoped(self, |v| &mut v.captures, added, |v| walk_expr(v, expr));
    }

    fn visit_param(&mut self, param: &'ast ParamDef) {
        self.check(param.type_constraint.as_deref());
        walk_param(self, param);
    }
}

/// Finds the first `trusts T` whose target names no declared or known type.
pub(super) struct Trusts<'a> {
    pub(super) interp: &'a Interpreter,
    pub(super) declared: &'a HashSet<String>,
    pub(super) found: Option<String>,
}

impl<'ast> Visit<'ast> for Trusts<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        if let Stmt::TrustsDecl { name } = stmt {
            let target = name.resolve();
            let known = self.declared.contains(target.as_str())
                || self.interp.has_type(&target)
                || self.interp.is_resolvable_type(&target);
            if !known {
                self.found = Some(target.to_string());
                return;
            }
        }
        walk_stmt(self, stmt);
    }
}

/// Walks `stmts` with `v` (shorthand for the entry points).
pub(super) fn scan<'ast, V: Visit<'ast>>(mut v: V, stmts: &'ast [Stmt]) -> V {
    walk_stmts(&mut v, stmts);
    v
}
