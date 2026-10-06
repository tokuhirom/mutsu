//! A typed, mutable AST visitor for rewriting passes (ADR-10499).
//!
//! [`VisitMut`] is the in-place counterpart of [`super::Visit`]: a pass
//! overrides the hooks for the node kinds it rewrites and calls the matching
//! `walk_*_mut` function to keep descending. The `walk_*_mut` functions mirror
//! the read-only walkers child for child and obey the same rule — every
//! variant destructured with every field named, no `_ =>` arm, no `..` — so a
//! new AST variant or field is a compile error here rather than a position
//! every rewriting pass silently skips.
//!
//! There is no by-value fold: a rewrite that returns a new tree clones its
//! input and runs a `VisitMut` over the clone (ADR-10499 §2).

use super::walk_mut_expr::walk_expr_mut;
use super::walk_mut_stmt::walk_stmts_mut;
use crate::ast::{CallArg, Expr, HandleSpec, ParamCode, ParamDef, Stmt};
use crate::regex_tree::{RegexNode, RegexTree, SubruleArgs};

/// A mutable AST visitor. Every hook defaults to plain recursion, so an
/// implementation overrides only the hooks it needs and calls the matching
/// `walk_*_mut` function to keep descending.
pub(crate) trait VisitMut {
    /// A statement list: a block, routine, loop or branch body, a regex code
    /// block. Every `walk_*_mut` hands a body here rather than to
    /// [`walk_stmts_mut`], so a pass that must see a body as a list — to
    /// insert, remove or reorder statements, or to look at a statement's
    /// neighbour — overrides this one hook (ADR-10499 §4).
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        walk_stmts_mut(self, body);
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        super::walk_mut_stmt::walk_stmt_mut(self, stmt);
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        walk_expr_mut(self, expr);
    }

    fn visit_param_mut(&mut self, param: &mut ParamDef) {
        walk_param_mut(self, param);
    }

    fn visit_regex_node_mut(&mut self, node: &mut RegexNode) {
        walk_regex_node_mut(self, node);
    }
}

/// Visits a parameter's expressions and sub-signatures, then gives it a
/// fresh, empty [`ParamCode`] slot: chunks compiled from the old default,
/// `where` or shape expressions must not outlive a rewrite of them
/// (ADR-0133, ADR-10499 §3).
// Cost: O(n), n = size of the parameter's subtree.
pub(crate) fn walk_param_mut<V: VisitMut + ?Sized>(v: &mut V, p: &mut ParamDef) {
    let ParamDef {
        name: _,
        default,
        multi_invocant: _,
        required: _,
        named: _,
        named_alias: _,
        slurpy: _,
        double_slurpy: _,
        onearg: _,
        sigilless: _,
        type_constraint: _,
        type_capture: _,
        literal_value: _,
        sub_signature,
        where_constraint,
        traits: _,
        trait_args,
        optional_marker: _,
        outer_sub_signature,
        code_signature,
        is_invocant: _,
        shape_constraints,
        block_param: _,
        code,
    } = p;
    if let Some(e) = default {
        v.visit_expr_mut(e);
    }
    for sub in [sub_signature, outer_sub_signature].into_iter().flatten() {
        params_mut(v, sub);
    }
    if let Some(e) = where_constraint {
        v.visit_expr_mut(e);
    }
    for (_trait_name, arg) in trait_args {
        v.visit_expr_mut(arg);
    }
    if let Some((sig, _ret)) = code_signature {
        params_mut(v, sig);
    }
    if let Some(dims) = shape_constraints {
        exprs_mut(v, dims);
    }
    *code = ParamCode::default();
}

// Cost: O(n), n = size of the argument's subtree.
pub(crate) fn walk_call_arg_mut<V: VisitMut + ?Sized>(v: &mut V, arg: &mut CallArg) {
    match arg {
        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => v.visit_expr_mut(e),
        CallArg::Named { name: _, value } => {
            if let Some(e) = value {
                v.visit_expr_mut(e);
            }
        }
    }
}

// Cost: O(n), n = size of the spec's subtree.
pub(crate) fn walk_handle_spec_mut<V: VisitMut + ?Sized>(v: &mut V, spec: &mut HandleSpec) {
    match spec {
        HandleSpec::Expr(e) => v.visit_expr_mut(e),
        HandleSpec::Name(_)
        | HandleSpec::Rename {
            exposed: _,
            target: _,
        }
        | HandleSpec::Type(_)
        | HandleSpec::Regex(_)
        | HandleSpec::Wildcard => {}
    }
}

// Cost: O(n), n = size of the regex tree.
pub(crate) fn walk_regex_tree_mut<V: VisitMut + ?Sized>(v: &mut V, tree: &mut RegexTree) {
    let RegexTree {
        body,
        match_immediately: _,
        adverbs: _,
        declaration_kind: _,
    } = tree;
    v.visit_regex_node_mut(body);
}

fn walk_subrule_args_mut<V: VisitMut + ?Sized>(v: &mut V, args: &mut SubruleArgs) {
    let SubruleArgs {
        args,
        source: _,
        literal_hash_indices: _,
        colonpair_values: _,
        colonpair_variables: _,
        colonpair_trues: _,
        colonpair_falses: _,
    } = args;
    exprs_mut(v, args);
}

// Cost: O(n), n = size of the node's subtree.
pub(crate) fn walk_regex_node_mut<V: VisitMut + ?Sized>(v: &mut V, node: &mut RegexNode) {
    match node {
        RegexNode::Literal(_) | RegexNode::Quote(_) => {}
        RegexNode::Sequence(nodes)
        | RegexNode::Alternation(nodes)
        | RegexNode::SequentialAlternation(nodes) => {
            for n in nodes {
                v.visit_regex_node_mut(n);
            }
        }
        RegexNode::Group(inner)
        | RegexNode::CapturingGroup(inner)
        | RegexNode::WithWhitespace(inner) => v.visit_regex_node_mut(inner),
        RegexNode::NamedCapture {
            name: _,
            array: _,
            regex,
        } => v.visit_regex_node_mut(regex),
        RegexNode::Subrule {
            name: _,
            capturing: _,
            args,
        }
        | RegexNode::SubruleAlias {
            alias: _,
            name: _,
            capturing: _,
            args,
        } => {
            if let Some(args) = args {
                walk_subrule_args_mut(v, args);
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
        } => v.visit_regex_node_mut(assertion),
        RegexNode::Interpolation {
            name: _,
            sequential: _,
        }
        | RegexNode::RegexValueInterpolation {
            name: _,
            sequential: _,
            sigil: _,
        }
        | RegexNode::ArrayInterpolation {
            name: _,
            sequential: _,
        }
        | RegexNode::ArrayLookaround {
            name: _,
            negated: _,
        } => {}
        RegexNode::Callable {
            name: _,
            args,
            arg_source: _,
        } => exprs_mut(v, args),
        RegexNode::CodeAssertion {
            code: _,
            negated: _,
            body,
        }
        | RegexNode::CodeBlock { code: _, body }
        | RegexNode::InterpolatedBlock {
            code: _,
            body,
            sequential: _,
        } => v.visit_stmts_mut(body),
        RegexNode::Quantified { atom, quantifier } => {
            v.visit_regex_node_mut(atom);
            if let Some(separator) = &mut quantifier.separator {
                v.visit_regex_node_mut(&mut separator.node);
            }
        }
        RegexNode::AnchorBeginningOfString
        | RegexNode::AnchorBeginningOfLine
        | RegexNode::AnchorEndOfString
        | RegexNode::AnchorEndOfLine
        | RegexNode::AnchorLeftWordBoundary
        | RegexNode::AnchorRightWordBoundary
        | RegexNode::MatchFrom
        | RegexNode::MatchTo
        | RegexNode::AssertionPass
        | RegexNode::AssertionFail
        | RegexNode::CharClass(_)
        | RegexNode::CharClassAssertion(_)
        | RegexNode::InternalModifier { .. } => {}
        RegexNode::Extension(extension) => {
            for child in extension.children_mut() {
                v.visit_regex_node_mut(child);
            }
            if let Some((_, body)) = extension.code_mut() {
                v.visit_stmts_mut(body);
            }
        }
    }
}

pub(super) fn exprs_mut<V: VisitMut + ?Sized>(v: &mut V, items: &mut [Expr]) {
    for e in items {
        v.visit_expr_mut(e);
    }
}

pub(super) fn params_mut<V: VisitMut + ?Sized>(v: &mut V, items: &mut [ParamDef]) {
    for p in items {
        v.visit_param_mut(p);
    }
}

/// Custom traits: `(name, optional argument)`.
pub(super) fn traits_mut<V: VisitMut + ?Sized>(v: &mut V, items: &mut [(String, Option<Expr>)]) {
    for (_name, arg) in items {
        if let Some(e) = arg {
            v.visit_expr_mut(e);
        }
    }
}
