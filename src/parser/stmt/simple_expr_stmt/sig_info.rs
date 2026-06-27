use crate::ast::Expr;
use crate::value::Value;

/// Extract variable names from a Signature literal expression for signature binding.
/// Returns None if the expression is not a Signature literal.
/// Returns Some(Vec<String>) where each string is either a variable name (e.g., "f")
/// or empty string for anonymous params.
#[derive(Debug, Clone)]
pub(super) struct SigParamInfo {
    /// Sigiled variable name (e.g. "$t", "@a") or empty if anonymous.
    pub(super) sigiled_name: String,
    /// True if this is a named parameter.
    pub(super) is_named: bool,
    /// Named keys (e.g. ["type"]) for named parameters; empty otherwise.
    pub(super) named_keys: Vec<String>,
}

pub(super) fn extract_signature_param_infos(expr: &Expr) -> Option<Vec<SigParamInfo>> {
    let Expr::Literal(Value::Instance {
        class_name,
        attributes,
        ..
    }) = expr
    else {
        return None;
    };
    if class_name != "Signature" {
        return None;
    }
    let map = attributes.as_map();
    let params = map.get("params")?;
    let Value::Array(param_list, ..) = params else {
        return None;
    };
    let mut infos = Vec::new();
    for param in param_list.iter() {
        let Value::Instance { attributes, .. } = param else {
            infos.push(SigParamInfo {
                sigiled_name: String::new(),
                is_named: false,
                named_keys: Vec::new(),
            });
            continue;
        };
        let sigiled_name = match attributes.as_map().get("name") {
            Some(Value::Str(name)) => name.to_string(),
            _ => String::new(),
        };
        let is_named = matches!(attributes.as_map().get("named"), Some(Value::Bool(true)));
        let named_keys = match attributes.as_map().get("named_names") {
            Some(Value::Array(arr, ..)) => arr
                .iter()
                .filter_map(|v| match v {
                    Value::Str(s) => Some(s.to_string()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        };
        infos.push(SigParamInfo {
            sigiled_name,
            is_named,
            named_keys,
        });
    }
    Some(infos)
}

/// If `rhs` is a static expression we can statically index by named key,
/// extract a map from key → value-expression. Used to desugar
/// `:(:type($t)) := (type => "foo")` into `$t := "foo"`.
pub(super) fn extract_static_named_map(
    rhs: &Expr,
) -> Option<std::collections::HashMap<String, Expr>> {
    use std::collections::HashMap;
    let items: &[Expr] = match rhs {
        Expr::CaptureLiteral(items) => items,
        Expr::ArrayLiteral(items) => items,
        _ => return None,
    };
    let mut map = HashMap::new();
    for item in items {
        match item {
            Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
            } => {
                let key = match left.as_ref() {
                    Expr::Literal(Value::Str(s)) => s.to_string(),
                    Expr::BareWord(s) => s.clone(),
                    _ => return None,
                };
                map.insert(key, (**right).clone());
            }
            Expr::Literal(Value::Pair(k, v)) => {
                map.insert(k.to_string(), Expr::Literal((**v).clone()));
            }
            _ => return None,
        }
    }
    Some(map)
}
