//! Splitting a `my`/`our` declaration out of a statement modifier: the
//! declaration takes effect unconditionally, only its initializer is gated.

use crate::ast::{Expr, SignatureDecl, SourceForm, Stmt};
use crate::symbol::Symbol;
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
///
/// `is_unless` is whether the modifier was written `unless`; the gated
/// assignment keeps it, as the generic modifier wrapping does, so the
/// RakuAST conversion can tell `unless C` from `if !C`.
// Cost: O(n), n = size of the declaration's initializer (it is cloned).
pub(crate) fn try_split_decl_modifier(
    stmt: &Stmt,
    effective_cond: &Expr,
    is_unless: bool,
) -> Option<Stmt> {
    if let Stmt::SyntheticBlock(parts) = stmt {
        return split_signature_decl(parts, effective_cond, is_unless);
    }
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
        is_unless,
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
    match try_split_decl_modifier(stmt, &always, false)? {
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

/// `my ($a, @b) = RHS if COND`: the list declaration is unconditional, only the
/// list assignment is gated. Only a declarator list of plain `$`/`@`/`%`
/// elements is split; anything else keeps the generic modifier wrapping.
fn split_signature_decl(parts: &[Stmt], effective_cond: &Expr, is_unless: bool) -> Option<Stmt> {
    let Some(Stmt::SourceForm(form)) = parts.first() else {
        return None;
    };
    let SourceForm::SignatureDecl(decl) = &**form else {
        return None;
    };
    let init = decl.init.as_ref()?;
    if decl.is_state
        || decl.is_our
        || decl.has_nested_group
        || decl.type_constraint.is_some()
        || decl.group_default.is_some()
        || !decl.vars.iter().all(|v| {
            !v.is_slurpy
                && !v.is_optional
                && !v.is_named
                && v.default.is_none()
                && v.per_var_type_constraint.is_none()
                && v.where_constraint.is_none()
                && !v.sigilless
                && v.literal_value.is_none()
                && v.param_trait.is_none()
        })
    {
        return None;
    }
    let bare = SignatureDecl {
        init: None,
        ..decl.clone()
    };
    // A `:=` list declaration is gated whole: its binding statements must run
    // as written (aliasing the elements, arity checks, read-only marks), after
    // the unconditional bare declaration. A statement modifier opens no scope,
    // so the names declared inside the gated body are the declared ones.
    if init.is_binding {
        let gated = Stmt::If {
            cond: effective_cond.clone(),
            then_branch: rebind_staged_decls(&decl)?,
            else_branch: Vec::new(),
            binding_var: None,
            is_statement_modifier: true,
            is_unless,
            with_kind: None,
        };
        return Some(Stmt::SyntheticBlock(vec![
            super::decl::destructure::desugar::signature_decl(bare),
            gated,
        ]));
    }
    let targets = decl
        .vars
        .iter()
        .map(|v| match v.name.split_at(1) {
            ("@", n) => Expr::ArrayVar(n.to_string()),
            ("%", n) => Expr::HashVar(n.to_string()),
            _ => Expr::Var(v.name.clone()),
        })
        .collect();
    let rhs = Expr::Call {
        name: Symbol::intern("__mutsu_list_assign_rhs"),
        args: vec![init.rhs.clone()],
        listop: false,
    };
    let assign = Stmt::Expr(Expr::Call {
        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
        args: vec![
            Expr::ArrayLiteral(targets),
            Expr::ArrayLiteral(Vec::new()),
            rhs,
        ],
        listop: false,
    });
    let gated = Stmt::If {
        cond: effective_cond.clone(),
        then_branch: vec![assign],
        else_branch: Vec::new(),
        binding_var: None,
        is_statement_modifier: true,
        is_unless,
        with_kind: None,
    };
    Some(Stmt::SyntheticBlock(vec![
        super::decl::destructure::desugar::signature_decl(bare),
        gated,
    ]))
}

/// The statements of an initialized `:=` list declaration with every element
/// declaration turned into a plain (re)bind of the already declared name, so
/// that gating them behind a statement modifier leaves the unconditional bare
/// declaration as the only one. Only the staging temp keeps its declaration.
/// `None` for an element form this rewrite does not cover (sigilless or
/// `is rw` binds), which keeps the generic modifier wrapping.
fn rebind_staged_decls(decl: &SignatureDecl) -> Option<Vec<Stmt>> {
    let Stmt::SyntheticBlock(stmts) =
        super::decl::destructure::desugar::signature_decl(decl.clone())
    else {
        return None;
    };
    let mut out = Vec::new();
    let last = stmts.len().saturating_sub(1);
    for (i, stmt) in stmts.into_iter().enumerate() {
        match stmt {
            // The block's trailing result expression: the gated statement is
            // sunk, and the staged list would only warn as a useless use.
            Stmt::Expr(_) if i == last => {}
            Stmt::SourceForm(_) => {}
            Stmt::VarDecl {
                name,
                expr,
                custom_traits,
                ..
            } if name == "@__destructure_tmp__" => {
                out.push(Stmt::VarDecl {
                    name,
                    expr,
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits,
                    where_constraint: None,
                });
            }
            Stmt::VarDecl {
                name,
                expr,
                custom_traits,
                ..
            } if custom_traits.is_empty() => {
                out.push(Stmt::Assign {
                    name,
                    expr,
                    op: crate::ast::AssignOp::Assign,
                    target_is_sigilless: false,
                });
            }
            Stmt::SyntheticBlock(inner) => match inner.as_slice() {
                [
                    Stmt::MarkBind,
                    Stmt::VarDecl {
                        name,
                        expr,
                        custom_traits,
                        ..
                    },
                ] if custom_traits.is_empty() => {
                    out.push(Stmt::Assign {
                        name: name.clone(),
                        expr: expr.clone(),
                        op: crate::ast::AssignOp::Bind,
                        target_is_sigilless: false,
                    });
                }
                _ => return None,
            },
            other @ (Stmt::MarkReadonly(..) | Stmt::If { .. }) => out.push(other),
            _ => return None,
        }
    }
    Some(out)
}
