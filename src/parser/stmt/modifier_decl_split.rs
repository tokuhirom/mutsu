//! Splitting a `my`/`our` declaration out of a statement modifier: the
//! declaration takes effect unconditionally, only its initializer is gated.

use crate::ast::{Expr, SignatureDecl, Stmt};
use crate::symbol::Symbol;
use crate::value::RuntimeError;
use crate::value::Value;

/// Split a declaration under `if` / `unless` into its unconditional
/// declaration and gated initializer (`rakuast::modifier_lower`).
// Cost: O(n), n = declaration and condition nodes.
pub(crate) fn conditional_declaration(
    stmt: &Stmt,
    condition: &Expr,
    is_unless: bool,
) -> Result<Option<Stmt>, RuntimeError> {
    if !is_declaration(stmt) {
        return Ok(None);
    }
    super::control::source_body::check(condition, &[], std::slice::from_ref(stmt))?;
    let effective = if is_unless {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Bang,
            expr: Box::new(condition.clone()),
            word: false,
        }
    } else {
        condition.clone()
    };
    try_split_decl_modifier(stmt, &effective, is_unless)
}

/// Topicalize only a declaration's initializer, preserving list binding and
/// keeping all declared names available even when the condition is undefined.
// Cost: O(n), n = declaration and topic nodes.
pub(crate) fn topical_declaration(
    stmt: &Stmt,
    topic: &Expr,
    is_without: bool,
) -> Result<Option<Stmt>, RuntimeError> {
    if !is_declaration(stmt) {
        return Ok(None);
    }
    super::control::source_body::check(topic, &[], std::slice::from_ref(stmt))?;
    let Some((decl, assignment)) = split_decl_for_topic_modifier(stmt)? else {
        return Ok(None);
    };
    let defined = Expr::MethodCall {
        target: Box::new(Expr::Var("_".into())),
        name: Symbol::intern("defined"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
        sugar: false,
    };
    let cond = if is_without {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Bang,
            expr: Box::new(defined),
            word: false,
        }
    } else {
        defined
    };
    let expanded = Stmt::SyntheticBlock(vec![
        decl,
        Stmt::Given {
            topic: topic.clone(),
            body: vec![Stmt::If {
                cond,
                then_branch: assignment.into_iter().collect(),
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            }],
            is_statement_modifier: true,
            with_kind: Some(if is_without {
                crate::ast::GivenWithKind::Without
            } else {
                crate::ast::GivenWithKind::With
            }),
        },
    ]);
    Ok(Some(expanded))
}

/// The declaration of a signature declaration the parser holds as written
/// (`desugar::pending`).
// Cost: O(1).
fn written_signature_declaration(stmt: &Stmt) -> Option<&SignatureDecl> {
    match stmt {
        Stmt::Ast(node) => match node.as_ref() {
            crate::ast::parsed::ParsedStmt::SignatureDecl(decl) => Some(decl),
            _ => None,
        },
        _ => None,
    }
}

fn is_declaration(stmt: &Stmt) -> bool {
    matches!(stmt, Stmt::VarDecl { .. }) || written_signature_declaration(stmt).is_some()
}

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
fn try_split_decl_modifier(
    stmt: &Stmt,
    effective_cond: &Expr,
    is_unless: bool,
) -> Result<Option<Stmt>, RuntimeError> {
    if let Some(decl) = written_signature_declaration(stmt) {
        return split_signature_decl(decl, effective_cond, is_unless);
    }
    Ok(split_simple_declaration(stmt, effective_cond, is_unless))
}

fn split_simple_declaration(stmt: &Stmt, effective_cond: &Expr, is_unless: bool) -> Option<Stmt> {
    let Stmt::VarDecl {
        anonymous,
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        is_export,
        export_tags,
        custom_traits,
        constant,
        has_initializer,
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
    if constant.is_some() {
        return Some(stmt.clone());
    }
    // Only sigil'd variables are hoistable here; a sigilless `\x` or other form
    // is left untouched.
    let is_array = name.starts_with('@');
    let is_hash = name.starts_with('%');
    // A `:=` declaration (`my $x := EXPR with $t`) carries its bound
    // expression the way an `=` one carries its initializer.
    let is_bind = custom_traits.iter().any(|(n, _)| n == "__scalar_bind");
    let has_init = is_bind || *has_initializer;
    let decl = Stmt::VarDecl {
        anonymous: *anonymous,
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
            .filter(|(n, _)| n != "__scalar_bind")
            .cloned()
            .collect(),
        has_initializer: false,
        constant: None,
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
            op: if is_bind {
                crate::ast::AssignOp::Bind
            } else {
                crate::ast::AssignOp::Assign
            },
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
fn split_decl_for_topic_modifier(
    stmt: &Stmt,
) -> Result<Option<(Stmt, Option<Stmt>)>, RuntimeError> {
    if matches!(stmt, Stmt::VarDecl { constant, .. } if constant.is_some())
    {
        return Ok(None);
    }
    let always = Expr::Literal(Value::TRUE);
    Ok(match try_split_decl_modifier(stmt, &always, false)? {
        None => None,
        Some(Stmt::SyntheticBlock(mut parts)) if parts.len() == 2 => {
            let Some(Stmt::If { then_branch, .. }) = parts.pop() else {
                return Ok(None);
            };
            let Some(decl) = parts.pop() else {
                return Ok(None);
            };
            Some((decl, Some(Stmt::SyntheticBlock(then_branch))))
        }
        Some(decl @ Stmt::VarDecl { .. }) => Some((decl, None)),
        _ => None,
    })
}

/// `my ($a, @b) = RHS if COND`: the list declaration is unconditional, only the
/// list assignment is gated. Only a declarator list of plain `$`/`@`/`%`
/// elements is split; anything else keeps the generic modifier wrapping.
fn split_signature_decl(
    decl: &SignatureDecl,
    effective_cond: &Expr,
    is_unless: bool,
) -> Result<Option<Stmt>, RuntimeError> {
    let Some(init) = decl.init.as_ref() else {
        return Ok(None);
    };
    if decl.is_state
        || decl.is_our
        || decl.has_nested_group
        || decl.type_constraint.is_some()
        || decl.group_default.is_some()
        || !decl.vars.iter().all(|v| {
            !v.is_slurpy
                && !v.is_optional
                && v.default.is_none()
                && v.per_var_type_constraint.is_none()
                && v.where_constraint.is_none()
                && !v.sigilless
                && v.literal_value.is_none()
                && v.param_trait.is_none()
        })
    {
        return Ok(None);
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
        let Some(then_branch) = rebind_staged_decls(decl)? else {
            return Ok(None);
        };
        let gated = Stmt::If {
            cond: effective_cond.clone(),
            then_branch,
            else_branch: Vec::new(),
            binding_var: None,
            is_statement_modifier: true,
            is_unless,
            with_kind: None,
        };
        return Ok(Some(Stmt::SyntheticBlock(vec![
            super::decl::destructure::desugar::signature_decl(bare)?,
            gated,
        ])));
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
    Ok(Some(Stmt::SyntheticBlock(vec![
        super::decl::destructure::desugar::signature_decl(bare)?,
        gated,
    ])))
}

/// The statements of an initialized `:=` list declaration with every element
/// declaration turned into a plain (re)bind of the already declared name, so
/// that gating them behind a statement modifier leaves the unconditional bare
/// declaration as the only one. Only the staging temp keeps its declaration.
/// `None` for an element form this rewrite does not cover (sigilless or
/// `is rw` binds), which keeps the generic modifier wrapping.
fn rebind_staged_decls(decl: &SignatureDecl) -> Result<Option<Vec<Stmt>>, RuntimeError> {
    let Stmt::SyntheticBlock(stmts) =
        super::decl::destructure::desugar::signature_decl(decl.clone())?
    else {
        return Ok(None);
    };
    let mut out = Vec::new();
    let last = stmts.len().saturating_sub(1);
    for (i, stmt) in stmts.into_iter().enumerate() {
        match stmt {
            // The block's trailing result expression: the gated statement is
            // sunk, and the staged list would only warn as a useless use.
            Stmt::Expr(_) if i == last => {}
            Stmt::VarDecl {
                name,
                expr,
                custom_traits,
                ..
            } if matches!(
                name.as_str(),
                "@__destructure_tmp__" | "%__destructure_tmp__"
            ) =>
            {
                out.push(Stmt::VarDecl {
                    anonymous: false,
                    name,
                    expr,
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits,
                    has_initializer: false,
                    constant: None,
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
                _ => return Ok(None),
            },
            other @ (Stmt::MarkReadonly(..) | Stmt::If { .. }) => out.push(other),
            _ => return Ok(None),
        }
    }
    Ok(Some(out))
}
