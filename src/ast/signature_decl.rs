//! The source form of a signature declaration, `my ($a, @b) = …`.
//!
//! The parser expands a declarator list into ordinary declarations reading a
//! staging temporary (`parser::stmt::decl::destructure::desugar`). The
//! expansion is all the compiler needs, but it is not what the source said:
//! RakuAST models the declaration as one `VarDeclaration::Signature` node
//! (ADR-10723 Stage 1). So the expansion carries this record, as a
//! [`Stmt::SourceForm`] marker that is its first
//! statement, and the RakuAST layer reads the declaration from it instead of
//! reverse-engineering the expansion; `rakuast::lower` hands it back to the same
//! expansion function the parser uses.

use super::{Expr, Stmt};

/// A parsed source construct that the parser expanded, recorded inside its
/// expansion. The compiler skips it.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum SourceForm {
    /// Traits written on a routine, in source order; opens its body.
    RoutineTraits(Vec<super::routine_trait::RoutineTrait>),
    SignatureDecl(SignatureDecl),
    /// `:(...) := RHS`: the parser expands the signature bind into temporary
    /// declarations and assignments; the model layer keeps the written bind.
    SignatureBind {
        signature: Expr,
        rhs: Expr,
    },
    /// An inline `subset` term expands to a declaration followed by a read of
    /// its generated name. Keep the declaration as written for RakuAST.
    InlineSubsetTerm(Box<Stmt>),
    /// `has Type ($.a, $.b)`: one visible RakuAST signature declaration,
    /// whose parser expansion installs each listed attribute separately.
    AttributeGroup(Vec<Stmt>),
    /// A declaration parsed inside a public attribute's colon argument list.
    /// The nested declarations execute, but Rakudo's RakuAST gist displays the
    /// declaration that introduced the colon list.
    AttributeColonArglist(Vec<Stmt>),
    MethodAssignDecl(super::method_assign_decl::MethodAssignDecl),
    /// `supply { BODY }`: the body as written, before the expansion rewrites
    /// its `emit` / `done` onto the on-demand emitter. It opens the emitter
    /// lambda's body.
    SupplyBlock(Vec<Stmt>),
    /// A `start { BODY }` whose placeholder is rejected by Raku. The parser
    /// keeps this marker when using a desugared block to defer that diagnostic
    /// until compilation instead of letting `make_anon_sub` consume it.
    StartBlock,
    /// `take VALUE and|or|xor|andthen|orelse|notandthen TAIL`: the parser
    /// expands the value-producing control call into a temporary and a
    /// `Stmt::Take`, while the model layer keeps the written infix form.
    TakeLogicalTail {
        value: Expr,
        op: crate::token_kind::TokenKind,
        right: Expr,
    },
    /// `with COND -> PARAM { BODY }`: the written parameter and body, which
    /// open the then-branch of the expansion `with_then_branch` builds.
    WithPointy {
        param_name: String,
        param_def: Option<super::ParamDef>,
        body: Vec<Stmt>,
    },
    /// `given EXPR -> PARAM { BODY }`: the written parameter and body, which
    /// follow the parameter bind at the head of the expansion
    /// `given_pointy_body` builds.
    GivenPointy {
        param_def: super::ParamDef,
        body: Vec<Stmt>,
    },
    /// `if COND -> PARAMS { BODY }` (and `elsif`): the written parameters and
    /// body, which open the then-branch of the expansion
    /// `lower_if_clause_binding` builds.
    IfPointy {
        param_defs: Vec<super::ParamDef>,
        body: Vec<Stmt>,
    },
    /// `(LVALUES) »=» VALUE`: the written target, value and dwim flags, which
    /// open the expansion `hyper_assignment_expr` builds for a literal list of
    /// lvalues.
    HyperAssign {
        target: Expr,
        value: Expr,
        dwim_left: bool,
        dwim_right: bool,
    },
    /// `my $x OP= EXPR`: a declaration initialized by a compound assignment.
    /// It opens the expansion `decl_compound_assign::expand` builds (the
    /// declaration, then the assignment, whose `Expr::CompoundAssign` keeps
    /// the written operator and right-hand side).
    DeclarationCompoundAssign,
}

/// Open the parser expansion of one parenthesized `has` declaration with its
/// source declarations. The marker lets RakuAST keep the single signature
/// declaration that Rakudo exposes while the compiler still installs each
/// attribute separately.
// Cost: O(a), a = total size of the grouped declarations and their expressions.
pub(crate) fn attribute_group_expansion(attributes: Vec<Stmt>) -> Stmt {
    let mut statements = Vec::with_capacity(attributes.len() + 1);
    statements.push(Stmt::SourceForm(Box::new(SourceForm::AttributeGroup(
        attributes.clone(),
    ))));
    statements.extend(attributes);
    Stmt::SyntheticBlock(statements)
}

/// Open the class-body expansion produced by a colon argument list.
// Cost: O(a), a = total size of the nested declarations and their expressions.
pub(crate) fn attribute_colon_expansion(attributes: Vec<Stmt>) -> Stmt {
    let mut statements = Vec::with_capacity(attributes.len() + 1);
    statements.push(Stmt::SourceForm(Box::new(
        SourceForm::AttributeColonArglist(attributes.clone()),
    )));
    statements.extend(attributes);
    Stmt::SyntheticBlock(statements)
}

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
/// declarations, opened by the expansion's source-form record.
pub(crate) fn is_group_declaration(inner: &[Stmt]) -> bool {
    inner.iter().any(|s| matches!(s, Stmt::VarDecl { .. }))
        && inner
            .iter()
            .all(|s| matches!(s, Stmt::VarDecl { .. } | Stmt::SourceForm(_)))
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
