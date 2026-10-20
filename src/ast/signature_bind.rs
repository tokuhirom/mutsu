//! Source form and executable expansion for binding a signature literal.

use std::sync::atomic::{AtomicU64, Ordering};

use super::{AssignOp, Expr, SourceForm, Stmt};
use crate::value::{Value, ValueView};

static NEXT_TEMP: AtomicU64 = AtomicU64::new(0);

#[derive(Debug)]
struct ParamInfo {
    sigiled_name: String,
    is_named: bool,
    named_keys: Vec<String>,
}

/// Record a signature literal bind and build the statements used by execution.
// Cost: O(p + e), p = signature parameters, e = expression nodes in RHS.
pub(crate) fn expand(signature: Expr, rhs: Expr) -> Option<Stmt> {
    let params = param_infos(&signature)?;
    let static_named = static_named_map(&rhs);
    let id = NEXT_TEMP.fetch_add(1, Ordering::Relaxed);
    let temp_name = format!("__sig_bind_tmp_{id}");
    let bound_name = format!("{temp_name}_bound");
    let decl = |name: String, expr: Expr| Stmt::VarDecl {
        anonymous: false,
        name,
        expr,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    };
    let mut statements = Vec::with_capacity(params.len() + 3);
    statements.push(Stmt::SourceForm(Box::new(SourceForm::SignatureBind {
        signature: signature.clone(),
        rhs: rhs.clone(),
    })));
    statements.push(decl(temp_name.clone(), rhs));
    statements.push(decl(
        bound_name.clone(),
        Expr::Call {
            name: crate::symbol::Symbol::intern("__mutsu_sig_bind"),
            args: vec![
                signature,
                Expr::Var(temp_name),
                Expr::Literal(Value::array(
                    params
                        .iter()
                        .filter(|param| !param.sigiled_name.is_empty())
                        .map(|param| Value::str(bind_var_name(param)))
                        .collect(),
                )),
            ],
            listop: false,
        },
    ));
    for param in params {
        if param.sigiled_name.is_empty() {
            continue;
        }
        let name = bind_var_name(&param);
        let static_source = if param.is_named {
            let key = param
                .named_keys
                .first()
                .cloned()
                .unwrap_or_else(|| name.clone());
            static_named.as_ref().and_then(|map| map.get(&key).cloned())
        } else {
            None
        };
        let value = static_source.unwrap_or_else(|| Expr::Index {
            target: Box::new(Expr::Var(bound_name.clone())),
            index: Box::new(Expr::Literal(Value::str(name.clone()))),
            is_positional: false,
            spelling: Default::default(),
        });
        statements.push(Stmt::Assign {
            name,
            expr: value,
            op: AssignOp::Bind,
            target_is_sigilless: false,
        });
    }
    Some(Stmt::SyntheticBlock(statements))
}

/// Whether an expression is a signature literal with binding metadata.
// Cost: O(p), p = signature parameters.
pub(crate) fn supports(expr: &Expr) -> bool {
    param_infos(expr).is_some()
}

/// Extract parameter names and named-argument metadata from a Signature value.
// Cost: O(p), p = signature parameters.
fn param_infos(expr: &Expr) -> Option<Vec<ParamInfo>> {
    let Expr::Literal(literal) = expr else {
        return None;
    };
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = literal.view()
    else {
        return None;
    };
    if class_name != "Signature" {
        return None;
    }
    let attributes = attributes.as_map();
    let params = attributes.get("params")?;
    let ValueView::Array(params, ..) = params.view() else {
        return None;
    };
    Some(
        params
            .iter()
            .map(|param| match param.view() {
                ValueView::Instance { attributes, .. } => {
                    let attrs = attributes.as_map();
                    let sigiled_name = match attrs.get("name").map(Value::view) {
                        Some(ValueView::Str(name)) => name.to_string(),
                        _ => String::new(),
                    };
                    let is_named = matches!(
                        attrs.get("named").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    let named_keys = match attrs.get("named_names").map(Value::view) {
                        Some(ValueView::Array(names, ..)) => names
                            .iter()
                            .filter_map(|value| match value.view() {
                                ValueView::Str(name) => Some(name.to_string()),
                                _ => None,
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    ParamInfo {
                        sigiled_name,
                        is_named,
                        named_keys,
                    }
                }
                _ => ParamInfo {
                    sigiled_name: String::new(),
                    is_named: false,
                    named_keys: Vec::new(),
                },
            })
            .collect(),
    )
}

/// Extract a static key-to-value map from literal named arguments, if present.
// Cost: O(n), n = RHS expression nodes.
fn static_named_map(rhs: &Expr) -> Option<std::collections::HashMap<String, Expr>> {
    use std::collections::HashMap;
    let items = match rhs {
        Expr::CaptureLiteral(items, _) | Expr::ArrayLiteral(items) => items,
        _ => return None,
    };
    let mut map = HashMap::new();
    for item in items {
        match item {
            Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
                ..
            } => {
                let key = match left.as_ref() {
                    Expr::Literal(literal) => match literal.view() {
                        ValueView::Str(key) => key.to_string(),
                        _ => return None,
                    },
                    Expr::BareWord(key) => key.clone(),
                    _ => return None,
                };
                map.insert(key, (**right).clone());
            }
            Expr::Literal(literal) => {
                if let ValueView::Pair(key, value) = literal.view() {
                    map.insert(key.to_string(), Expr::Literal(value.clone()));
                } else {
                    return None;
                }
            }
            _ => return None,
        }
    }
    Some(map)
}

/// The binder's key for one variable: scalars omit `$`, other sigils stay.
// Cost: O(n), n = variable-name length.
fn bind_var_name(info: &ParamInfo) -> String {
    info.sigiled_name
        .strip_prefix('$')
        .unwrap_or(&info.sigiled_name)
        .to_string()
}
