//! Splitting a `my`/`our` declaration out of a statement modifier: the
//! declaration takes effect unconditionally, only its initializer is gated.

use crate::ast::{Expr, Stmt};
use crate::value::Value;

/// A `my`/`our` declaration carrying a conditional statement modifier
/// (`my $x = 5 if COND`, `my $x unless COND`) keeps its *declaration* lexically
/// unconditional — in Raku declarations take effect at compile time regardless
/// of the runtime modifier; only the *initializer* is gated. So split such a
/// declaration into an always-run declaration plus a conditional assignment:
///
///   `my $x = INIT if COND`  ->  `my $x; ($x = INIT) if COND`
///   `my $x if COND`         ->  `my $x`            (no init to gate)
///
/// `effective_cond` is the condition the surrounding modifier would test (already
/// negated for `unless`). Returns `None` for anything that is not a hoistable
/// scalar/array/hash `my`/`our` declaration (e.g. `state`, routines), leaving the
/// generic modifier wrapping in place.
pub(super) fn try_split_decl_modifier(stmt: &Stmt, effective_cond: &Expr) -> Option<Stmt> {
    let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        is_export,
        export_tags,
        custom_traits,
        where_constraint,
    } = stmt
    else {
        return None;
    };
    // `state` has once-only initialization semantics that this split would
    // break, so leave it to the generic modifier wrapping.
    if *is_state {
        return None;
    }
    // A `constant` binding is resolved at compile time and does not respect a
    // runtime statement modifier at all -- `raku` evaluates `constant $w = 11
    // if False;` unconditionally (confirmed against real raku: `$w` reads back
    // `11` even under a `False` condition, with no warning). So drop the
    // modifier entirely rather than gating the initializer like a normal `my`
    // variable would: keep the original declaration (with its real
    // initializer) as-is.
    if custom_traits.iter().any(|(n, _)| n == "__constant") {
        return Some(stmt.clone());
    }
    // Only sigil'd variables are hoistable here; a sigilless `\x` or other form
    // is left untouched.
    let is_array = name.starts_with('@');
    let is_hash = name.starts_with('%');
    let has_init = custom_traits.iter().any(|(n, _)| n == "__has_initializer");
    let decl = Stmt::VarDecl {
        name: name.clone(),
        expr: super::decl::default_decl_expr(is_array, is_hash, None, type_constraint.as_deref()),
        type_constraint: type_constraint.clone(),
        is_state: false,
        is_our: *is_our,
        is_dynamic: *is_dynamic,
        is_export: *is_export,
        export_tags: export_tags.clone(),
        custom_traits: custom_traits
            .iter()
            .filter(|(n, _)| n != "__has_initializer")
            .cloned()
            .collect(),
        where_constraint: where_constraint.clone(),
    };
    if !has_init {
        // No initializer to gate: the declaration is simply unconditional.
        return Some(decl);
    }
    let init = Stmt::If {
        cond: effective_cond.clone(),
        then_branch: vec![Stmt::Assign {
            name: name.clone(),
            expr: expr.clone(),
            op: crate::ast::AssignOp::Assign,
            target_is_sigilless: false,
        }],
        else_branch: Vec::new(),
        binding_var: None,
        is_statement_modifier: true,
        is_unless: false,
        with_kind: None,
    };
    Some(Stmt::SyntheticBlock(vec![decl, init]))
}

/// [`try_split_decl_modifier`] for the topicalizing modifiers (`with`,
/// `without`): `my @a = .list with $x` declares `@a` unconditionally and
/// runs only the initializer under `given $x { if .defined { … } }`, so
/// the declaration is split into the always-run declaration and the
/// statement that belongs in the modifier's body (the gated assignment, or
/// `None` when there is no initializer). `None` for anything the split
/// does not apply to.
pub(super) fn split_decl_for_topic_modifier(stmt: &Stmt) -> Option<(Stmt, Option<Stmt>)> {
    let Stmt::VarDecl { custom_traits, .. } = stmt else {
        return None;
    };
    if custom_traits.iter().any(|(n, _)| n == "__constant") {
        return None;
    }
    let always = Expr::Literal(Value::TRUE);
    match try_split_decl_modifier(stmt, &always)? {
        Stmt::SyntheticBlock(mut parts) if parts.len() == 2 => {
            let Some(Stmt::If { then_branch, .. }) = parts.pop() else {
                return None;
            };
            let decl = parts.pop()?;
            let assign = then_branch.into_iter().next()?;
            Some((decl, Some(assign)))
        }
        decl @ Stmt::VarDecl { .. } => Some((decl, None)),
        _ => None,
    }
}
