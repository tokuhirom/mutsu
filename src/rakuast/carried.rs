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
            plain_name(name) || name.starts_with("__") || context_free_name(expr, name)
        }
        // `*` alone: the priming scope that curries it is planted by the
        // enclosing `WhateverCurry`, so the term itself lowers to itself.
        Expr::Whatever => true,
        // The `*` of a priming scope (`* + 1`): `WhateverCode::Argument` reads back as it.
        Expr::WhateverArg => true,
        // A type name: the spelled parse leaves a lowercase bareword to
        // the lowering, which resolves it against what the unit imported.
        Expr::BareWord(word) => {
            // `self` is a keyword: nothing the unit declares changes it.
            word == "self" ||
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
            ParsedExpr::Assign(_)
            | ParsedExpr::WordList(_)
            | ParsedExpr::WordsAtom(_)
            | ParsedExpr::SubscriptAdverbs(_)
            | ParsedExpr::Supply(_)
            | ParsedExpr::HyperPrefix(_)
            | ParsedExpr::AtomicVar(_)
            | ParsedExpr::ObjectHash(_)
            | ParsedExpr::HyperAssign(_)
            | ParsedExpr::InlineSubset(_) => true,
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
        Expr::Subst {
            replacement,
            replacement_thunk,
            ..
        }
        | Expr::NonDestructiveSubst {
            replacement,
            replacement_thunk,
            ..
        } => {
            replacement_thunk.is_some() || super::substitution::is_plain_qq_source(replacement)
        }
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
        // The priming scope the parser planted: the conversion marks the node it
        // wraps and the lowering puts the marker back around it.
        | Expr::WhateverCurry(_)
        // `tr///`, and `s///` / `S///` whose replacement is a constant or already
        // a per-match thunk, read back as the parser built them
        // (`finalization_is_the_ordinary_tree`). An interpolating quote-form
        // replacement becomes the thunk in the conversion.
        | Expr::Transliterate { .. }
        | Expr::ZenSlice(_)
        | Expr::CaptureVar(_)
        | Expr::PseudoStash(_)
        | Expr::MultiDimIndex { .. }
        | Expr::AssignExpr { .. }
        | Expr::HyperMethodCallDynamic { .. }
        | Expr::SymbolicDeref { .. }
        | Expr::ChainedCompare { .. }
        | Expr::Feed { .. }
        | Expr::ControlFlow { .. }
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

/// A routine name the conversion reads as a lexical `&name`: an identifier
/// (`&f`, `&foo-bar`), not a package-qualified or operator name.
// Cost: O(k), k = length of the name.
fn plain_code_name(name: &str) -> bool {
    name.starts_with(|c: char| c.is_alphabetic() || c == '_')
        && name
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''))
}

/// A variable the lowering hands back unchanged whatever the unit declares:
/// the match and error variables (`$/`, `$!`), an attribute (`$!x`), a
/// placeholder (`$^a`) and a positional capture (`$0`). A code variable is carried when
/// it is a plain identifier (`&f`); a qualified or operator name is still read
/// by the conversion.
// Cost: O(1).
fn context_free_name(expr: &Expr, name: &str) -> bool {
    // A placeholder (`$^a`, `@:n`, `&^cb`) lowers back to the same caret- or
    // colon-prefixed name whatever the unit declares.
    if let Some(bare) = name.strip_prefix(['^', ':'])
        && crate::ast::placeholders::is_placeholder_name(bare)
    {
        return true;
    }
    if matches!(expr, Expr::CodeVar(_)) {
        return plain_code_name(name);
    }
    // A positional capture (`$0`) reads back as `Var::PositionalCapture`, whose
    // index is the canonical integer.
    if matches!(expr, Expr::Var(_))
        && !name.is_empty()
        && name.bytes().all(|b| b.is_ascii_digit())
        && name.parse::<i64>().is_ok_and(|n| n.to_string() == name)
    {
        return true;
    }
    // The user lexical `$self` (its env key carries the sigil, ADR-0061) and a
    // Pod variable (`$=pod`, `$=finish`) read back as the same name.
    if matches!(expr, Expr::Var(_))
        && (name == crate::env::LEX_SELF
            || name.strip_prefix('=').is_some_and(|rest| {
                !rest.is_empty() && rest.chars().all(|c| c.is_alphanumeric() || c == '_')
            }))
    {
        return true;
    }
    match name {
        "/" | "!" => true,
        _ => name.strip_prefix('!').is_some_and(|rest| {
            rest.chars()
                .next()
                .is_some_and(|c| c.is_alphabetic() || c == '_')
        }),
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
