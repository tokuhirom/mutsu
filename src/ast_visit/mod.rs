//! A typed, read-only AST visitor (ADR-0137).
//!
//! Analyses that ask a question about a subtree ("does this body mention
//! `return`?", "which mentions of `foo` does this routine make?") implement
//! [`Visit`] and override the hooks for the node kinds they care about; the
//! `walk_*` functions supply the default recursion into every child.
//!
//! Two properties make the walk a sound basis for such a question:
//!
//! - **Exhaustive.** Every `walk_*` function destructures every variant with
//!   every field named — no `_ =>` arm, no `..` rest pattern. A new AST
//!   variant or field is a compile error here, so it cannot be silently
//!   skipped by every analysis at once.
//! - **Names are typed.** Every string field that holds an identifier (a
//!   routine, variable, type, method, trait, module, label or operator name,
//!   or source text compiled later) is reported through
//!   [`Visit::visit_name`] together with the [`NameKind`] of its position.
//!   Literal data (string literals, hash-literal keys, named-argument keys,
//!   `tr///` tables, version strings, messages) is not a name and is never
//!   reported, so a string literal `"return"` cannot look like a `return`.

mod visit_mut;
mod walk_decl;
mod walk_expr;
mod walk_mut_decl;
mod walk_mut_expr;
mod walk_mut_stmt;
mod walk_stmt;

pub(crate) use visit_mut::{VisitMut, walk_param_mut, walk_regex_node_mut};
use visit_mut::{walk_call_arg_mut, walk_handle_spec_mut, walk_regex_tree_mut};
use visit_mut::{exprs_mut, params_mut, traits_mut};
pub(crate) use walk_expr::walk_expr;
pub(crate) use walk_mut_expr::walk_expr_mut;
pub(crate) use walk_mut_stmt::{walk_stmt_mut, walk_stmts_mut};
pub(crate) use walk_stmt::{walk_stmt, walk_stmts};

use crate::ast::{CallArg, Expr, HandleSpec, ParamDef, Stmt};
use crate::regex_tree::{RegexNode, RegexTree, SubruleArgs};
use crate::value::{Value, ValueView};

/// The position an identifier string was found in. See the variants for
/// which AST fields report which kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NameKind {
    /// The callee of a by-name call: `Expr::Call` / `Stmt::Call`.
    Call,
    /// The callee of an `Expr::UserRoutineCall`.
    UserRoutineCall,
    /// The name of a `sub` declaration (`Stmt::SubDecl`).
    SubDecl,
    /// The name of any other declaration: method, token/rule/regex, proto,
    /// package, class, role, enum (and its keys), subset, augment, and the
    /// routines a nested-method capture lists.
    Decl,
    /// The name of an attribute declaration (`Stmt::HasDecl`).
    Attribute,
    /// `&name` (`Expr::CodeVar`).
    CodeVar,
    /// `$name` (`Expr::Var`).
    Var,
    /// `$<name>` / `$0` (`Expr::CaptureVar`).
    CaptureVar,
    /// `@name` (`Expr::ArrayVar`).
    ArrayVar,
    /// `%name` (`Expr::HashVar`).
    HashVar,
    /// The declared name of a `Stmt::VarDecl` (with its sigil).
    VarDecl,
    /// The target of a `Stmt::Assign` / `Expr::AssignExpr`.
    AssignTarget,
    /// The variable of a `Stmt::MarkReadonly`.
    MarkReadonly,
    /// The variable of a `Stmt::MarkBoundContainer`.
    MarkBoundContainer,
    /// The variable of a `Stmt::MarkSigilless` / `MarkSigillessReadonly`.
    Sigilless,
    /// The variable of a `Stmt::Let` (`let` / `temp`) or the variable of a
    /// `Stmt::TempMethodAssign`.
    TempTarget,
    /// The name of a [`ParamDef`].
    Param,
    /// A parameter named outside a [`ParamDef`]: `Expr::Lambda`'s param,
    /// a `for`/`whenever` loop's params, a routine's flat `params` list,
    /// `if`/`with`'s binding variable.
    BlockParam,
    /// A type name: a type constraint, return type, parent, role, base
    /// type, `handles` type, type capture, a literal type object.
    Type,
    /// A method name: `Expr::MethodCall` / `HyperMethodCall`, a
    /// `TempMethodAssign`'s method, a `handles` forwarding name.
    Method,
    /// A trait name (routine, variable, parameter, class or attribute trait).
    Trait,
    /// A module name or an import tag (`use`/`no`/`need`/`import`, export
    /// tags).
    Module,
    /// A loop or statement label.
    Label,
    /// A bare term: `Expr::BareWord`, a shadowable term keyword, an exported
    /// term.
    Term,
    /// An operator or operator-like name (reductions, meta- and hyper-ops,
    /// infix functions, compound assignment, associativity/precedence
    /// traits).
    Operator,
    /// A name a regex resolves at match time: a subrule, a `<&callable>`,
    /// an interpolated variable, a named capture or alias, an adverb.
    Regex,
    /// Source text that is parsed again later (a `s///` pattern or
    /// replacement, a heredoc body, a regex code block's text, an argument
    /// list's text).
    Source,
    /// A sigil or name used for a symbolic or pseudo-package lookup
    /// (`SymbolicDeref`, `PseudoStash`, `IndirectCodeLookup`).
    Symbolic,
    /// A routine named by a literal `Routine` value.
    LiteralRoutine,
}

/// A read-only AST visitor. Every hook defaults to plain recursion, so an
/// implementation overrides only the hooks it needs and calls the matching
/// `walk_*` function to keep descending.
pub(crate) trait Visit {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_param(&mut self, param: &ParamDef) {
        walk_param(self, param);
    }

    fn visit_regex_node(&mut self, node: &RegexNode) {
        walk_regex_node(self, node);
    }

    /// An identifier found at a position of kind `kind`. See [`NameKind`].
    fn visit_name(&mut self, _name: &str, _kind: NameKind) {}
}

// Cost: O(n), n = size of the parameter's subtree (default, `where`, traits,
// sub-signatures).
pub(crate) fn walk_param<V: Visit + ?Sized>(v: &mut V, p: &ParamDef) {
    let ParamDef {
        name,
        default,
        multi_invocant: _,
        required: _,
        named: _,
        named_alias: _,
        slurpy: _,
        double_slurpy: _,
        onearg: _,
        sigilless: _,
        type_constraint,
        type_capture,
        literal_value,
        sub_signature,
        where_constraint,
        traits,
        trait_args,
        optional_marker: _,
        outer_sub_signature,
        code_signature,
        is_invocant: _,
        shape_constraints,
        block_param: _,
        code: _,
    } = p;
    v.visit_name(name, NameKind::Param);
    if let Some(e) = default {
        v.visit_expr(e);
    }
    names(v, type_constraint.iter(), NameKind::Type);
    names(v, type_capture.iter(), NameKind::Type);
    if let Some(value) = literal_value {
        walk_literal(v, value);
    }
    for sub in [sub_signature, outer_sub_signature].into_iter().flatten() {
        params(v, sub);
    }
    if let Some(e) = where_constraint {
        v.visit_expr(e);
    }
    names(v, traits.iter(), NameKind::Trait);
    for (trait_name, arg) in trait_args {
        v.visit_name(trait_name, NameKind::Trait);
        v.visit_expr(arg);
    }
    if let Some((sig, ret)) = code_signature {
        params(v, sig);
        names(v, ret.iter(), NameKind::Type);
    }
    if let Some(dims) = shape_constraints {
        exprs(v, dims);
    }
}

/// Reports the names a literal value carries: a type object's or a
/// routine's. Literal data (strings, numbers, ...) is not a name.
// Cost: O(1).
pub(crate) fn walk_literal<V: Visit + ?Sized>(v: &mut V, value: &Value) {
    match value.view() {
        ValueView::Package(name) => v.visit_name(name.as_str(), NameKind::Type),
        ValueView::Routine { name, .. } => v.visit_name(name.as_str(), NameKind::LiteralRoutine),
        _ => {}
    }
}

// Cost: O(n), n = size of the argument's subtree.
pub(crate) fn walk_call_arg<V: Visit + ?Sized>(v: &mut V, arg: &CallArg) {
    match arg {
        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => v.visit_expr(e),
        // The key of a named argument is data, like a hash-literal key.
        CallArg::Named { name: _, value } => {
            if let Some(e) = value {
                v.visit_expr(e);
            }
        }
    }
}

// Cost: O(n), n = size of the spec's subtree.
pub(crate) fn walk_handle_spec<V: Visit + ?Sized>(v: &mut V, spec: &HandleSpec) {
    match spec {
        HandleSpec::Name(name) => v.visit_name(name, NameKind::Method),
        HandleSpec::Expr(e) => v.visit_expr(e),
        HandleSpec::Rename { exposed, target } => {
            v.visit_name(exposed, NameKind::Method);
            v.visit_name(target, NameKind::Method);
        }
        HandleSpec::Type(name) => v.visit_name(name, NameKind::Type),
        HandleSpec::Regex(pattern) => v.visit_name(pattern, NameKind::Source),
        HandleSpec::Wildcard => {}
    }
}

// Cost: O(n), n = size of the regex tree.
pub(crate) fn walk_regex_tree<V: Visit + ?Sized>(v: &mut V, tree: &RegexTree) {
    let RegexTree {
        body,
        match_immediately: _,
        adverbs,
        declaration_kind: _,
    } = tree;
    v.visit_regex_node(body);
    for adverb in adverbs {
        // The argument is the adverb's literal value.
        v.visit_name(&adverb.name, NameKind::Regex);
    }
}

fn walk_subrule_args<V: Visit + ?Sized>(v: &mut V, args: &SubruleArgs) {
    let SubruleArgs {
        args,
        source,
        literal_hash_indices: _,
        colonpair_values: _,
        colonpair_variables: _,
        colonpair_trues: _,
        colonpair_falses: _,
    } = args;
    exprs(v, args);
    names(v, source.iter(), NameKind::Source);
}

// Cost: O(n), n = size of the node's subtree.
pub(crate) fn walk_regex_node<V: Visit + ?Sized>(v: &mut V, node: &RegexNode) {
    match node {
        RegexNode::Literal(_) | RegexNode::Quote(_) => {}
        RegexNode::Sequence(nodes)
        | RegexNode::Alternation(nodes)
        | RegexNode::SequentialAlternation(nodes) => {
            for n in nodes {
                v.visit_regex_node(n);
            }
        }
        RegexNode::Group(inner)
        | RegexNode::CapturingGroup(inner)
        | RegexNode::WithWhitespace(inner) => v.visit_regex_node(inner),
        RegexNode::NamedCapture {
            name,
            array: _,
            regex,
        } => {
            v.visit_name(name, NameKind::Regex);
            v.visit_regex_node(regex);
        }
        RegexNode::Subrule {
            name,
            capturing: _,
            args,
        } => {
            v.visit_name(name, NameKind::Regex);
            if let Some(args) = args {
                walk_subrule_args(v, args);
            }
        }
        RegexNode::SubruleAlias {
            alias,
            name,
            capturing: _,
            args,
        } => {
            v.visit_name(alias, NameKind::Regex);
            v.visit_name(name, NameKind::Regex);
            if let Some(args) = args {
                walk_subrule_args(v, args);
            }
        }
        RegexNode::Lookaround {
            assertion,
            negated: _,
            is_behind: _,
        }
        | RegexNode::NamedLookaround {
            assertion,
            is_behind: _,
            capturing: _,
        } => v.visit_regex_node(assertion),
        RegexNode::Interpolation {
            name,
            sequential: _,
        }
        | RegexNode::RegexValueInterpolation {
            name,
            sequential: _,
            sigil: _,
        }
        | RegexNode::ArrayInterpolation {
            name,
            sequential: _,
        }
        | RegexNode::ArrayLookaround { name, negated: _ } => v.visit_name(name, NameKind::Regex),
        RegexNode::Callable {
            name,
            args,
            arg_source,
        } => {
            v.visit_name(name, NameKind::Regex);
            exprs(v, args);
            names(v, arg_source.iter(), NameKind::Source);
        }
        RegexNode::CodeAssertion {
            code,
            negated: _,
            body,
        }
        | RegexNode::CodeBlock { code, body }
        | RegexNode::InterpolatedBlock {
            code,
            body,
            sequential: _,
        } => {
            v.visit_name(code, NameKind::Source);
            walk_stmts(v, body);
        }
        RegexNode::Quantified {
            atom,
            quantifier: _,
        } => v.visit_regex_node(atom),
        RegexNode::AnchorBeginningOfString
        | RegexNode::AnchorBeginningOfLine
        | RegexNode::AnchorEndOfString
        | RegexNode::AnchorEndOfLine
        | RegexNode::CharClassDigit => {}
    }
}

fn exprs<V: Visit + ?Sized>(v: &mut V, items: &[Expr]) {
    for e in items {
        v.visit_expr(e);
    }
}

fn params<V: Visit + ?Sized>(v: &mut V, items: &[ParamDef]) {
    for p in items {
        v.visit_param(p);
    }
}

fn names<'a, V: Visit + ?Sized>(
    v: &mut V,
    items: impl IntoIterator<Item = &'a String>,
    kind: NameKind,
) {
    for s in items {
        v.visit_name(s, kind);
    }
}

/// Custom traits: `(name, optional argument)`.
fn traits<V: Visit + ?Sized>(v: &mut V, items: &[(String, Option<Expr>)]) {
    for (name, arg) in items {
        v.visit_name(name, NameKind::Trait);
        if let Some(e) = arg {
            v.visit_expr(e);
        }
    }
}

/// `word` occurs in `s` not flanked by an identifier character.
// Cost: O(n * m), n = `s.len()`, m = `word.len()`.
pub(crate) fn contains_word(s: &str, word: &str) -> bool {
    let is_ident = |c: char| c.is_alphanumeric() || c == '_';
    s.match_indices(word).any(|(i, _)| {
        !s[..i].chars().next_back().is_some_and(is_ident)
            && !s[i + word.len()..].chars().next().is_some_and(is_ident)
    })
}

#[cfg(test)]
#[path = "ast_visit_tests.rs"]
mod tests;
