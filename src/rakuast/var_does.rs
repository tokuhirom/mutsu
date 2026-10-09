//! `my %h does Role` across the RakuAST boundary: a `VarDeclaration::Simple`
//! with a `Trait::Does` (and the initializer after it), which the parser
//! spells as a declaration, an in-place mixin and the initializer
//! ([`crate::ast::var_does`]).

use super::lower::{lower_expr, named_child_or_positional, rakuast_node_of, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::value::{RuntimeError, Value};

/// A declaration node without its `Trait::Does` entries, and the role types
/// they named; `None` when it has none.
// Cost: O(t), t = traits of the declaration.
pub(super) fn split(
    node: &RakuAstNode,
) -> Result<Option<(RakuAstNode, Vec<RakuAstNode>)>, RuntimeError> {
    let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok(None);
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Ok(None);
    };
    let mut roles = Vec::new();
    let mut kept = Vec::new();
    for item in items {
        match rakuast_node_of(item) {
            Some(t) if t.class == RakuAstClass::TraitDoes => {
                roles.push(named_child_or_positional(t)?.clone());
            }
            _ => kept.push(item.clone()),
        }
    }
    if roles.is_empty() {
        return Ok(None);
    }
    let fields = node
        .fields
        .iter()
        .filter_map(|f| {
            if f.name != Some("traits") {
                return Some(f.clone());
            }
            (!kept.is_empty()).then(|| RakuAstField {
                name: f.name,
                value: RakuAstFieldValue::List(kept.clone()),
            })
        })
        .collect();
    Ok(Some((
        RakuAstNode {
            class: node.class,
            fields,
        },
        roles,
    )))
}

/// The parser's expansion of `declaration` (lowered without its role) mixed
/// with `roles`.
// Cost: O(n), n = size of the declaration and the role.
pub(super) fn lower_expansion(
    node: &RakuAstNode,
    declaration: Stmt,
    roles: &[RakuAstNode],
) -> Result<Stmt, RuntimeError> {
    let [role] = roles else {
        return Err(unsupported(node));
    };
    let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        is_export,
        export_tags,
        mut custom_traits,
        where_constraint,
    } = declaration
    else {
        return Err(unsupported(node));
    };
    // The role is mixed into the fresh container, then the initializer is
    // assigned: the declaration itself carries none.
    let has_initializer = custom_traits.iter().any(|(t, _)| t == "__has_initializer");
    if custom_traits
        .iter()
        .any(|(t, _)| t == crate::ast::shaped_decl::SHAPED_DECL)
    {
        return Err(unsupported(node));
    }
    custom_traits.retain(|(t, _)| t != "__has_initializer");
    let (default, init) = if has_initializer {
        let default = match name.chars().next() {
            Some('@') => Expr::Literal(Value::real_array(Vec::new())),
            Some('%') => Expr::Hash(Vec::new(), crate::ast::HashSpelling::Composer),
            _ => Expr::Literal(Value::NIL),
        };
        let init = Stmt::Assign {
            name: name.clone(),
            expr,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        };
        (default, Some(init))
    } else {
        (expr, None)
    };
    let role = lower_expr(role)?;
    let declared = Stmt::VarDecl {
        name: name.clone(),
        expr: default,
        type_constraint,
        is_state,
        is_our,
        is_dynamic,
        is_export,
        export_tags,
        custom_traits,
        where_constraint,
    };
    Ok(crate::ast::var_does::expand(
        &name, declared, "does", role, init,
    ))
}
