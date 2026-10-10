//! Written control signatures and their shared parser expansions.

use super::convert::{convert_expr, node_field, pointy_block};
use super::lower::{
    lower_block, lower_expr, name_for_unpack_params, named_child, signature_positional_params,
};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{ControlPointyKind, Expr, ParamDef, SourceForm, Stmt};
use crate::value::RuntimeError;

// Cost: O(1), bounded class names.
pub(super) fn constructor_schema(name: &str) -> Option<(RakuAstClass, &'static [&'static str])> {
    use RakuAstClass::*;
    let class = match name {
        "RakuAST::Statement::If" => StatementIf,
        "RakuAST::Statement::With" => StatementWith,
        "RakuAST::Statement::Unless" => StatementUnless,
        "RakuAST::Statement::Without" => StatementWithout,
        "RakuAST::Statement::Elsif" => StatementElsif,
        "RakuAST::Statement::Orwith" => StatementOrwith,
        "RakuAST::Statement::Loop::While" => StatementLoopWhile,
        "RakuAST::Statement::Loop::Until" => StatementLoopUntil,
        _ => return None,
    };
    Some((
        class,
        if matches!(
            class,
            StatementIf | StatementWith | StatementElsif | StatementOrwith
        ) {
            &["condition", "then"]
        } else {
            &["condition", "body"]
        },
    ))
}

// Cost: O(a + l), a = arguments, l = labels and continuation clauses.
pub(super) fn construct(
    name: &str,
    args: &[crate::value::Value],
) -> Option<Result<crate::value::Value, RuntimeError>> {
    use super::{RakuAstField, RakuAstFieldValue};
    use crate::value::Value;
    let (class, schema) = constructor_schema(name)?;
    Some((|| {
        let mut fields = Vec::new();
        for &field in schema {
            let value = super::named_arg(args, field).ok_or_else(|| {
                RuntimeError::new(format!("{name}.new requires a `{field}` argument"))
            })?;
            super::require_any_rakuast(&value, name, field)?;
            fields.push(RakuAstField {
                name: Some(field),
                value: RakuAstFieldValue::Node(value),
            });
        }
        let chain = matches!(
            class,
            RakuAstClass::StatementIf | RakuAstClass::StatementWith
        );
        if chain && let Some(value) = super::named_arg(args, "else") {
            super::require_any_rakuast(&value, name, "else")?;
            fields.push(RakuAstField {
                name: Some("else"),
                value: RakuAstFieldValue::Node(value),
            });
        }
        for field in ["labels", "elsifs"] {
            if field == "elsifs" && !chain {
                continue;
            }
            if let Some(value) = super::named_arg(args, field) {
                let items = value.as_list_items().ok_or_else(|| {
                    RuntimeError::new(format!("{name}.new expects a list for `{field}`"))
                })?;
                let mut list = Vec::new();
                list.try_reserve(items.len())
                    .map_err(|_| RuntimeError::new("RakuAST: control field list is too large"))?;
                for item in items {
                    let item = item.with_deref(|value| value.descalarize().clone());
                    if field == "labels" {
                        super::require_rakuast_class(&item, RakuAstClass::Label, name)?;
                    } else {
                        super::require_any_rakuast(&item, name, field)?;
                    }
                    list.push(item);
                }
                fields.push(RakuAstField {
                    name: Some(field),
                    value: RakuAstFieldValue::List(list),
                });
            }
        }
        Ok(Value::rakuast(Box::new(RakuAstNode { class, fields })))
    })())
}

// Cost: O(1).
pub(super) fn source(stmt: &Stmt) -> Option<&SourceForm> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    match stmts.first() {
        Some(Stmt::SourceForm(form))
            if matches!(form.as_ref(), SourceForm::ControlPointy { .. }) =>
        {
            Some(form)
        }
        _ => None,
    }
}

// Cost: O(n), n = size of the written clause.
pub(super) fn convert(form: &SourceForm) -> Result<RakuAstNode, RuntimeError> {
    let SourceForm::ControlPointy {
        kind,
        label,
        condition,
        param_defs,
        body,
    } = form
    else {
        return Err(RuntimeError::new(
            "RakuAST: expected a control signature source form",
        ));
    };
    let mut fields = super::convert::label_fields(label);
    fields.extend([
        node_field(Some("condition"), convert_expr(condition)?),
        node_field(Some("body"), pointy_block(param_defs, body, None)?),
    ]);
    Ok(RakuAstNode {
        class: match kind {
            ControlPointyKind::While => RakuAstClass::StatementLoopWhile,
            ControlPointyKind::Until => RakuAstClass::StatementLoopUntil,
            ControlPointyKind::Unless => RakuAstClass::StatementUnless,
        },
        fields,
    })
}

// Cost: O(p), p = size of the signature.
pub(super) fn params(block: &RakuAstNode) -> Result<Vec<ParamDef>, RuntimeError> {
    let (mut names, mut defs) = signature_positional_params(block)?;
    name_for_unpack_params(&mut names, &mut defs);
    Ok(defs)
}

// Cost: O(n), n = size of the written clause.
pub(super) fn lower_unless(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let block = named_child(node, "body")?;
    let defs = if block.class == RakuAstClass::PointyBlock {
        Some(params(block)?)
    } else {
        None
    };
    Ok(crate::parser::unless_clause(
        lower_expr(named_child(node, "condition")?)?,
        defs,
        lower_block(block)?,
    ))
}

/// Expand the same with-family pointy signature on both entry paths.
// Cost: O(n), n = size of the signature and body.
pub(super) fn with_body(
    cond: &Expr,
    tmp: &Expr,
    block: &RakuAstNode,
) -> Result<Vec<Stmt>, RuntimeError> {
    let mut defs = params(block)?;
    if defs.len() != 1 {
        return Err(RuntimeError::new(
            "RakuAST: with-family pointy block requires one parameter",
        ));
    }
    let Some(def) = defs.pop() else {
        return Err(RuntimeError::new(
            "RakuAST: missing with-family pointy parameter",
        ));
    };
    Ok(crate::parser::with_then_branch(
        cond,
        tmp,
        &Some(def.name.clone()),
        &Some(def),
        lower_block(block)?,
    ))
}
