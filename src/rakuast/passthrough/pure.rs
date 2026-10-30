//! Which trees a round-trip leaf may carry (see the parent module): the
//! families whose parser tree is what the lowering would hand the compiler.
//!
//! The test is a [`Visit`] over the tree. Each hook rules on the node it is
//! given and lets the visitor's `walk_*` functions recurse, so a variant the
//! test does not name is refused, never skipped.

use crate::ast::parsed::{ParsedExpr, ParsedStmt};
use crate::ast::{CallArg, Expr, ParamDef, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_param, walk_stmt};
use crate::value::ValueView;

/// Whether `stmt` may be carried, and if so whether it holds a spelling or a
/// kept construct to undo first.
// Cost: O(s), s = nodes of `stmt`.
pub(super) fn analyse_stmt(stmt: &Stmt) -> Option<bool> {
    let mut purity = Purity::default();
    purity.visit_stmt(stmt);
    purity.pure.then_some(purity.fixup)
}

/// [`analyse_stmt`] for an expression.
// Cost: O(e), e = nodes of `expr`.
pub(super) fn analyse_expr(expr: &Expr) -> Option<bool> {
    let mut purity = Purity::default();
    purity.visit_expr(expr);
    purity.pure.then_some(purity.fixup)
}

struct Purity {
    pure: bool,
    fixup: bool,
}

impl Default for Purity {
    fn default() -> Self {
        Purity {
            pure: true,
            fixup: false,
        }
    }
}

impl<'ast> Visit<'ast> for Purity {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if !self.pure {
            return;
        }
        if !self.stmt_allowed(stmt) {
            self.pure = false;
            return;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.pure {
            return;
        }
        if !self.expr_allowed(expr) {
            self.pure = false;
            return;
        }
        // A regex without code holds no expression; one with code is refused.
        if matches!(
            expr,
            Expr::RegexLiteral { .. } | Expr::MatchRegexTree { .. }
        ) {
            return;
        }
        walk_expr(self, expr);
    }

    fn visit_param(&mut self, param: &'ast ParamDef) {
        if !self.pure {
            return;
        }
        if !pure_param(param) {
            self.pure = false;
            return;
        }
        walk_param(self, param);
    }
}

impl Purity {
    // Cost: O(1) for the node itself.
    fn stmt_allowed(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::SetLine(..)
            | Stmt::Last(_)
            | Stmt::Next(_)
            | Stmt::Redo(_)
            | Stmt::Block(_)
            | Stmt::For { .. }
            | Stmt::Expr(_)
            | Stmt::Say(_)
            | Stmt::Put(_)
            | Stmt::Print(_)
            | Stmt::Note(_)
            | Stmt::Return(_)
            | Stmt::Die(_)
            | Stmt::Fail(_)
            | Stmt::Take(..) => true,
            // A routine's written traits are read by the conversion of the
            // routine that holds them, not carried on their own.
            Stmt::Ast(node) => {
                self.fixup = true;
                !matches!(node.as_ref(), ParsedStmt::RoutineTraits(_))
            }
            Stmt::Assign {
                name,
                target_is_sigilless: false,
                ..
            } => declared_name(name),
            Stmt::VarDecl {
                name,
                anonymous: false,
                is_export: false,
                export_tags,
                custom_traits,
                // A `constant` is a distinct RakuAST node; the scope scans
                // of `bareword` must see it, so it is converted, not carried.
                constant: None,
                bind: None,
                marks,
                where_constraint: None,
                ..
            } => {
                declared_name(name)
                    && export_tags.is_empty()
                    && custom_traits.is_empty()
                    && marks.is_plain()
            }
            // A plain named routine whose signature and body are themselves
            // carried: nothing in it is read by node class.
            Stmt::SubDecl {
                name_expr: None,
                associativity: None,
                precedence_trait: None,
                signature_alternates,
                custom_traits,
                is_test_assertion: false,
                supersede: false,
                is_hidden_from_backtrace: false,
                ..
            } => {
                signature_alternates.is_empty() && custom_traits.is_empty()
            }
            // A plain method, with the same conditions.
            Stmt::MethodDecl {
                name_expr: None,
                value_prototype: None,
                deprecated_message: None,
                handles,
                custom_traits,
                is_export: false,
                is_hidden_from_backtrace: false,
                export_tags,
                nested_capture_index: None,
                ..
            } => handles.is_empty() && custom_traits.is_empty() && export_tags.is_empty(),
            Stmt::Call { args, .. } => args
                .iter()
                .all(|a| matches!(a, CallArg::Positional(_) | CallArg::Named { .. })),
            _ => false,
        }
    }

    // Cost: O(1) for the node itself (a regex: O(n), n = its nodes).
    fn expr_allowed(&mut self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(v) => !matches!(
                v.view(),
                ValueView::Regex(_)
                    | ValueView::RegexWithAdverbs(_)
                    | ValueView::Sub(_)
                    | ValueView::WeakSub(_)
                    | ValueView::Instance { .. }
            ),
            Expr::Var(name) | Expr::ArrayVar(name) | Expr::HashVar(name) | Expr::CodeVar(name) => {
                plain_name(name)
            }
            // A type name: the spelled parse leaves a lowercase bareword to
            // the lowering, which resolves it against what the unit imported.
            Expr::BareWord(word) => {
                word.starts_with(|c: char| c.is_uppercase())
                    && word
                        .chars()
                        .all(|c| c.is_alphanumeric() || matches!(c, ':' | '-' | '_'))
            }
            // A `;` that ends no statement is no statement of the lowered
            // tree (`sub f { 1;; }` still returns 1); only the lowering of the
            // `Statement::Empty` node knows to drop it.
            Expr::Spelled(spelled) => {
                self.fixup = true;
                !is_empty_statement(spelled)
            }
            Expr::Ast(node) => {
                self.fixup = true;
                matches!(
                    node.as_ref(),
                    ParsedExpr::Assign(_) | ParsedExpr::WordList(_) | ParsedExpr::WordsAtom(_)
                )
            }
            // The lowering plants the priming scopes of a `*` itself (a second pass
            // over a tree the parser already marked wraps it again), and a `||`
            // dimension splat is a spelled form it rewrites.
            Expr::Unary { op, .. } => !matches!(op, crate::token_kind::TokenKind::OrOr),
            Expr::AnonSubParams { custom_traits, .. } => custom_traits.as_slice().is_empty(),
            Expr::RegexLiteral { tree, .. } | Expr::MatchRegexTree { tree, .. } => !has_code(tree),
            Expr::StringInterpolation(_)
            | Expr::Call { .. }
            | Expr::UserRoutineCall { .. }
            | Expr::MethodCall { .. }
            | Expr::HyperMethodCall { .. }
            | Expr::DynamicMethodCall { .. }
            | Expr::CallOn { .. }
            | Expr::Binary { .. }
            | Expr::Ternary { .. }
            | Expr::Index { .. }
            | Expr::ArrayLiteral(_)
            | Expr::BracketArray(..)
            | Expr::Grouped(_)
            | Expr::AnonSub { .. }
            | Expr::DoBlock { .. }
            | Expr::Lambda { .. }
            | Expr::Hash(..)
            | Expr::PositionalPair(_)
            | Expr::PostfixOp { .. }
            | Expr::DoStmt(_)
            | Expr::Exists { .. }
            | Expr::Try { .. }
            | Expr::IndirectTypeLookup(_)
            | Expr::IndirectTypeLookupTail { .. }
            | Expr::Eager(_)
            | Expr::Itemize(_)
            | Expr::Contextualizer { .. }
            | Expr::Reduction { .. }
            | Expr::MetaOp { .. }
            | Expr::HyperOp { .. }
            | Expr::InfixFunc { .. }
            | Expr::Gather(_)
            | Expr::Once { .. }
            | Expr::PhaserExpr { .. } => true,
            _ => false,
        }
    }
}

/// A parameter with a name and, at most, a type: nothing the lowering reads
/// the signature of.
// Cost: O(1).
/// Whether a spelling, or one it wraps, is the `;` of a `Statement::Empty`.
// Cost: O(d), d = nesting depth of the spellings.
fn is_empty_statement(spelled: &crate::ast::spelled::Spelled) -> bool {
    let mut spelled = spelled;
    loop {
        if matches!(
            spelled.spelling,
            crate::ast::spelled::Spelling::EmptyStatement
        ) {
            return true;
        }
        match &spelled.expr {
            Expr::Spelled(inner) => spelled = inner,
            _ => return false,
        }
    }
}

fn pure_param(param: &ParamDef) -> bool {
    param.sub_signature.is_none()
        && param.outer_sub_signature.is_none()
        && param.code_signature.is_none()
        && param.shape_constraints.is_none()
        && param.type_capture.is_none()
        && param.no_traits()
        && param.trait_args.is_empty()
}

/// A variable name the parser's tree and the lowering agree on: not a
/// placeholder, a match variable or a desugaring's temporary.
// Cost: O(1).
fn plain_name(name: &str) -> bool {
    if name == "_" {
        return true;
    }
    let mut chars = name.chars();
    match chars.next() {
        Some('*' | '?') => chars.next().is_some_and(|c| c.is_alphabetic()),
        Some(c) => c.is_alphabetic(),
        None => false,
    }
}

/// Whether a regex holds code (a block, an assertion, an interpolation): the
/// regex `Value` keeps its own copy of that code, which still carries the
/// spelled parse's annotations.
// Cost: O(n), n = nodes of the regex.
fn has_code(tree: &crate::regex_tree::RegexTree) -> bool {
    struct Code(bool);
    impl<'ast> Visit<'ast> for Code {
        fn visit_stmt(&mut self, _stmt: &'ast Stmt) {
            self.0 = true;
        }
        fn visit_expr(&mut self, _expr: &'ast Expr) {
            self.0 = true;
        }
    }
    let mut code = Code(false);
    crate::ast_visit::walk_regex_tree(&mut code, tree);
    code.0
}

/// The name a declaration or assignment carries: the sigil of an array or hash
/// variable is part of it. A code variable is not carried: `lower` resolves
/// the calls that follow `my &emit` against the declaration it sees.
// Cost: O(1).
fn declared_name(name: &str) -> bool {
    plain_name(name.strip_prefix(['@', '%']).unwrap_or(name))
}
