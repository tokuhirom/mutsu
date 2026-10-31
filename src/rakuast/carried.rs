//! Which trees the executing finalization keeps as the parser built them
//! (ADR-15334 Stage 2): the families whose parser tree is what the lowering
//! would hand the compiler. Anything else is converted and lowered where it
//! stands (`finalize`).

use crate::ast::parsed::{CompoundAssign, CompoundMeta, ParsedExpr, ParsedStmt};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::Visit;
use crate::value::ValueView;

/// Whether the node `stmt` itself (not its operands) is kept as the parser
/// built it.
// Cost: O(1).
pub(super) fn stmt_carried(stmt: &Stmt) -> bool {
    match stmt {
        // Its statement form is read by the conversion too.
        Stmt::Expr(Expr::Ast(node)) if matches!(node.as_ref(), ParsedExpr::CompoundAssign(c) if !compound_carried(c)) => {
            false
        }
        // A named routine whose signature and body are themselves carried;
        // an operator's precedence, a symbolic name and a signature
        // alternative are still read by node class.
        Stmt::SubDecl {
            name_expr,
            associativity,
            precedence_trait,
            signature_alternates,
            ..
        } => {
            name_expr.is_none()
                && associativity.is_none()
                && precedence_trait.is_none()
                && signature_alternates.is_empty()
        }
        Stmt::MethodDecl {
            name_expr,
            value_prototype,
            deprecated_message,
            nested_capture_index,
            ..
        } => {
            name_expr.is_none()
                && value_prototype.is_none()
                && deprecated_message.is_none()
                && nested_capture_index.is_none()
        }
        // A method declared in a nested block of a package body is the
        // package's and closes over the block's lexicals: the conversion of
        // the whole package places it, so the package is not kept.
        Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::Package { .. } => {
            !holds_nested_method(stmt)
        }
        Stmt::Ast(node) if matches!(node.as_ref(), ParsedStmt::PackageHeader { .. }) => {
            !holds_nested_method(stmt)
        }
        // `temp Type.meth = v` is expanded (save, assign, restore) by the
        // conversion of the temporizer, not by the compiler.
        Stmt::TempMethodAssign { .. } => false,
        _ => true,
    }
}

/// Whether a method declared in a nested block is somewhere in `stmt`.
// Cost: O(n), n = nodes of `stmt`.
fn holds_nested_method(stmt: &Stmt) -> bool {
    struct Nested(bool);
    impl<'ast> Visit<'ast> for Nested {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if self.0 {
                return;
            }
            if matches!(
                stmt,
                Stmt::MethodDecl {
                    nested_capture_index: Some(_),
                    ..
                }
            ) {
                self.0 = true;
                return;
            }
            crate::ast_visit::walk_stmt(self, stmt);
        }
    }
    let mut nested = Nested(false);
    nested.visit_stmt(stmt);
    nested.0
}

/// Whether the node `expr` itself (not its operands) is kept as the parser
/// built it.
// Cost: O(1) for the node itself (a regex: O(n), n = its nodes).
pub(super) fn expr_carried(expr: &Expr) -> bool {
    match expr {
        Expr::Literal(v) => !matches!(
            v.view(),
            ValueView::Regex(_)
                | ValueView::RegexWithAdverbs(_)
                | ValueView::Sub(_)
                | ValueView::WeakSub(_)
                | ValueView::Instance { .. }
        ),
        // A desugaring's temporary (`__ANON_ARRAY__`) is the compiler's own
        // name, carried as the parser built it.
        Expr::Var(name) | Expr::ArrayVar(name) | Expr::HashVar(name) | Expr::CodeVar(name) => {
            plain_name(name) || name.starts_with("__")
        }
        // A type name: the spelled parse leaves a lowercase bareword to
        // the lowering, which resolves it against what the unit imported.
        Expr::BareWord(word) => {
            // `Pointer[int8]` is a parameterized type name as the parser wrote it.
            word.starts_with(|c: char| c.is_uppercase())
                && (word.ends_with(']') && word.contains('[')
                    || word
                        .chars()
                        .all(|c| c.is_alphanumeric() || matches!(c, ':' | '-' | '_')))
        }
        // A `;` that ends no statement is no statement of the lowered
        // tree (`sub f { 1;; }` still returns 1); only the lowering of the
        // `Statement::Empty` node knows to drop it.
        Expr::Spelled(spelled) => !is_empty_statement(spelled),
        Expr::Ast(node) => match node.as_ref() {
            ParsedExpr::CompoundAssign(compound) => compound_carried(compound),
            ParsedExpr::Assign(_) | ParsedExpr::WordList(_) | ParsedExpr::WordsAtom(_) => true,
            _ => false,
        },
        // The lowering plants the priming scopes of a `*` itself (a second pass
        // over a tree the parser already marked wraps it again), and a `||`
        // dimension splat is a spelled form it rewrites.
        // The marker a `my T $x .= new` declaration carries as its initializer.
        Expr::CompoundAssign { op, .. } => {
            op == crate::ast::method_assign_decl::DECL_INITIALIZER_OP
        }
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

/// Whether a spelling, or one it wraps, is the `;` of a `Statement::Empty`.
// Cost: O(d), d = nesting depth of the spellings.
pub(super) fn is_empty_statement(spelled: &crate::ast::spelled::Spelled) -> bool {
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

/// Whether a compound assignment is lowered from its typed node: a scalar `Z+=`
/// and a bracketed metaoperator assignment (`@a X= 10`) are read by the
/// conversion.
// Cost: O(1).
fn compound_carried(compound: &CompoundAssign) -> bool {
    matches!(
        compound.meta,
        CompoundMeta::Plain
            | CompoundMeta::Bracketed(_)
            | CompoundMeta::Set
            | CompoundMeta::Dotty
            | CompoundMeta::TopicDotty
            | CompoundMeta::Reverse
    )
}
