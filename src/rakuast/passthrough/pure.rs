//! Which trees a round-trip leaf may carry (see the parent module): the
//! families whose parser tree is what the lowering would hand the compiler.
//!
//! The test is a [`Visit`] over the tree. Each hook rules on the node it is
//! given and lets the visitor's `walk_*` functions recurse, so a variant the
//! test does not name is refused, never skipped.

use crate::ast::parsed::{CompoundAssign, CompoundMeta, ParsedExpr, ParsedStmt};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use crate::value::ValueView;

/// Whether `stmt` may be carried, and if so whether it holds a spelling or a
/// kept construct to undo first.
// Cost: O(s), s = nodes of `stmt`.
pub(super) fn analyse_stmt(stmt: &Stmt) -> Option<bool> {
    // An empty statement is no statement of the lowered tree; inside a carried
    // body `Kept` drops it, on its own the conversion does.
    if matches!(stmt, Stmt::Expr(Expr::Spelled(spelled)) if is_empty_statement(spelled)) {
        return None;
    }
    // The traits a routine was written with are read by the conversion of the
    // routine that holds them; inside a carried body `Kept` drops them.
    if matches!(stmt, Stmt::Ast(node) if matches!(node.as_ref(), ParsedStmt::RoutineTraits(_))) {
        return None;
    }
    let mut purity = Purity::default();
    purity.visit_stmt(stmt);
    purity.pure.then_some(purity.fixup)
}

/// [`analyse_stmt`] for an expression.
// Cost: O(e), e = nodes of `expr`.
pub(super) fn analyse_expr(expr: &Expr) -> Option<bool> {
    let mut purity = Purity::default();
    if !purity.expr_allowed(expr) {
        return None;
    }
    purity.visit_expr(expr);
    purity.pure.then_some(purity.fixup)
}

/// Whether the node itself (not its operands) is carried as the parser built it.
// Cost: O(1) (a regex: O(n), n = its nodes).
pub(super) fn expr_node_allowed(expr: &Expr) -> bool {
    Purity::default().expr_allowed(expr)
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
        // An expression the leaf cannot carry is converted where it stands
        // when the tree is lowered (`Kept`), so it does not hold the rest of
        // the statement back.
        if !self.expr_allowed(expr) {
            self.fixup = true;
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
}

impl Purity {
    // Cost: O(1) for the node itself.
    fn stmt_allowed(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            // Its statement form is read by the conversion too.
            Stmt::Expr(Expr::Ast(node)) if matches!(node.as_ref(), ParsedExpr::CompoundAssign(c) if !compound_carried(c)) => {
                false
            }
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
            Stmt::Ast(_) => {
                self.fixup = true;
                true
            }
            Stmt::Assign { .. } => true,
            // An anonymous declaration (`my @`) is named by a desugaring's
            // temporary; the conversion reads it as the declaration it is.
            // The `state` declaration the parser puts at the top of a block for
            // each bare `$` it holds is carried with the `$` it declares.
            Stmt::VarDecl { .. } => true,
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
            // `temp Type.meth = v` is expanded (save, assign, restore) by the
            // conversion of the temporizer, not by the compiler.
            Stmt::TempMethodAssign { .. } => false,
            _ => true,
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
            Expr::Spelled(spelled) => {
                self.fixup = true;
                !is_empty_statement(spelled)
            }
            Expr::Ast(node) => {
                self.fixup = true;
                match node.as_ref() {
                    ParsedExpr::CompoundAssign(compound) => compound_carried(compound),
                    ParsedExpr::Assign(_) | ParsedExpr::WordList(_) | ParsedExpr::WordsAtom(_) => {
                        true
                    }
                    _ => false,
                }
            }
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
}

/// A parameter with a name and, at most, a type: nothing the lowering reads
/// the signature of.
// Cost: O(1).
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
