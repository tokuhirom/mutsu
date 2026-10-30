//! The source form of a signature declaration, `my ($a, @b) = …`
//! (`ParsedStmt::SignatureDecl`, ADR-10723 Stage 2).

use super::{Expr, Stmt};

/// `my|our|state [TYPE] (VARS) [is default(EXPR)] [= RHS | := RHS]`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct SignatureDecl {
    pub(crate) vars: Vec<SignatureVar>,
    /// Source order and nesting of the flattened `vars` when a declaration
    /// contains a nested group. Absent for a flat declarator list.
    pub(crate) group_shape: Option<Vec<SignatureGroup>>,
    pub(crate) is_state: bool,
    pub(crate) is_our: bool,
    /// The declaration's type (`my Int ($a, $b)`), applying to every element
    /// that has none of its own.
    pub(crate) type_constraint: Option<String>,
    /// A group `is default(EXPR)` trait.
    pub(crate) group_default: Option<Expr>,
    /// A nested group (`my ($a, ($b, $c)) = …`), whose leaves `vars` holds
    /// flattened.
    pub(crate) has_nested_group: bool,
    /// The initializer, absent for a bare declaration.
    pub(crate) init: Option<SignatureInit>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum SignatureGroup {
    Var(usize),
    Group(Vec<SignatureGroup>),
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct SignatureInit {
    /// `:=` (or `::=`) rather than `=`.
    pub(crate) is_binding: bool,
    /// The right-hand side as written, before the expansion wraps it.
    pub(crate) rhs: Expr,
}

/// One element of a declarator list.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct SignatureVar {
    /// Full variable name including sigil prefix for @/%/& (e.g. "@y", "x",
    /// "%h"); a `$` element carries no sigil.
    pub(crate) name: String,
    /// A slurpy element (`*@rest`).
    pub(crate) is_slurpy: bool,
    /// An optional element (`$x?`).
    pub(crate) is_optional: bool,
    /// A named element (`:@even`).
    pub(crate) is_named: bool,
    /// The element's own default (`$x = 5` inside the group).
    pub(crate) default: Option<Expr>,
    /// The element's own type (`Foo $d`).
    pub(crate) per_var_type_constraint: Option<String>,
    /// A `where` constraint (`$a where 2`).
    pub(crate) where_constraint: Option<Expr>,
    /// A sigilless element (`\c`).
    pub(crate) sigilless: bool,
    /// A literal match element (`"foo"`).
    pub(crate) literal_value: Option<Expr>,
    /// A parameter trait written on the element (`$a is rw`): the declarator
    /// list is a signature, so `is rw` / `is raw` / `is copy` / `is readonly`
    /// decide how a `:=` bind treats the element.
    pub(crate) param_trait: Option<ParamTrait>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ParamTrait {
    Rw,
    Raw,
    Copy,
    Readonly,
}

impl SignatureVar {
    /// A plain element named by its full spelling (`$a`, `@b`).
    pub(crate) fn plain(spelling: &str) -> Self {
        let name = spelling.strip_prefix('$').unwrap_or(spelling).to_string();
        SignatureVar {
            name,
            is_slurpy: false,
            is_optional: false,
            is_named: false,
            default: None,
            per_var_type_constraint: None,
            where_constraint: None,
            sigilless: false,
            literal_value: None,
            param_trait: None,
        }
    }

    /// The element's source spelling, with its sigil (`$a`, `@b`).
    pub(crate) fn spelling(&self) -> String {
        if self.name.starts_with(['@', '%', '&']) {
            self.name.clone()
        } else {
            format!("${}", self.name)
        }
    }
}

/// The expansion of a bare group declaration `my ($a, @b);`: plain
/// declarations.
pub(crate) fn is_group_declaration(inner: &[Stmt]) -> bool {
    inner.iter().any(|s| matches!(s, Stmt::VarDecl { .. }))
        && inner.iter().all(|s| matches!(s, Stmt::VarDecl { .. }))
}

/// The statements of the scope `stmts` makes up, with every
/// [`Stmt::SyntheticBlock`] opened up into the statements it holds, as a
/// pass that collects the scope's own declarations reads them. A synthetic
/// block is a parser desugaring, not a scope: `my T $x .= new;` and
/// `my ($a, $b) = ...;` declare in the enclosing scope.
// Cost: O(n), n = `stmts` plus the statements nested in their synthetic
// blocks.
pub(crate) fn scope_statements(stmts: &[Stmt]) -> Vec<&Stmt> {
    struct ScopeStatements<'ast>(Vec<&'ast Stmt>);
    impl<'ast> crate::ast_visit::Visit<'ast> for ScopeStatements<'ast> {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if matches!(stmt, Stmt::SyntheticBlock(_)) {
                crate::ast_visit::walk_stmt(self, stmt);
            } else {
                self.0.push(stmt);
            }
        }
    }
    let mut collector = ScopeStatements(Vec::with_capacity(stmts.len()));
    crate::ast_visit::walk_stmts(&mut collector, stmts);
    collector.0
}

/// Whether `body` is the desugaring of a `my (...) = RHS` / `my (...) := RHS`
/// destructuring declaration: it opens with the staging-temp declaration,
/// wrapped in a bound block in binding mode.
// Cost: O(s), s = statements of `body` before the staging temporary.
pub(crate) fn is_destructure_block(body: &[Stmt]) -> bool {
    fn is_tmp_decl(stmt: &Stmt) -> bool {
        matches!(stmt, Stmt::VarDecl { name, .. }
            if name == "@__destructure_tmp__" || name == "%__destructure_tmp__")
    }
    // Plain targets are predeclared (default-initialized) ahead of the temp so
    // the RHS sees them; skip those leading declarations.
    let is_predecl =
        |s: &Stmt| matches!(s, Stmt::VarDecl { name, .. } if !name.contains("__destructure_tmp__"));
    match body.iter().find(|s| !is_predecl(s)) {
        Some(Stmt::SyntheticBlock(inner)) => {
            matches!(inner.as_slice(), [decl] if is_tmp_decl(decl))
        }
        Some(first) => is_tmp_decl(first),
        None => false,
    }
}
