//! `constant X = EXPR` in RakuAST: `VarDeclaration::Constant`.

use super::view::{
    build_type_node, leaf_field, node_field, statement_expression, unsupported, view_expr,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::spelled::Spelling;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// `constant X = 5` -> `VarDeclaration::Constant(name => "X", initializer =>
/// Initializer::Assign(...))`. Measured against rakudo 2026.07: the `name` is a
/// plain string (not a `Name` node), the package-scoped default spelling emits
/// no `scope`, and `my constant Y = 7` emits `scope => "my"`.
///
/// A sigilled constant (`constant @a = 1, 2`) keeps its sigil in `name`. A
/// typed one (`our Mu constant X = 1`) has the `type` between `scope` and
/// `name`.
pub(super) fn view_node(
    names: &NameScope,
    name: &str,
    expr: &Expr,
    constant: crate::ast::ConstantSigil,
    custom_traits: &[(String, Option<Expr>)],
    type_constraint: &Option<String>,
    (is_our, export_trait): (bool, Option<Value>),
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let sigil = constant.as_str();
    // rakudo's `name` carries the sigil (`"@a"`, `"$x"`). The parser keeps
    // it in the name for `@` / `%` / `&` and strips a `$`.
    let name = match sigil {
        // `constant term:<$bar>`: a sigiled name without a sigil of its own
        // is a term, which rakudo names `term:<$bar>`.
        "" if name.starts_with(['$', '@', '%', '&']) => format!("term:<{name}>"),
        "" => name.to_string(),
        "$" => format!("${name}"),
        sigil @ ("@" | "%" | "&") if name.starts_with(sigil) => name.to_string(),
        _ => return Err(unsupported("sigilled constant")),
    };
    if !custom_traits.is_empty() {
        return Err(unsupported("constant with traits"));
    }
    let mut fields = Vec::new();
    // The package-scoped default (`constant X`) prints no scope; `my constant`
    // does. mutsu records the default as `is_our`.
    if !is_our {
        fields.push(leaf_field(Some("scope"), Value::str_from("my")));
    }
    if let Some(type_name) = type_constraint {
        fields.push(node_field(Some("type"), build_type_node(names, type_name)?));
    }
    fields.push(leaf_field(Some("name"), Value::str(name)));
    // `constant X is export = …`: the trait sits between the name and the
    // initializer (measured on rakudo 2026.09).
    if let Some(export) = export_trait {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![export]),
        });
    }
    // `constant X := EXPR` keeps its `Initializer::Bind`.
    let (class, expr) = match expr {
        Expr::Spelled(s) if matches!(s.spelling, Spelling::ConstantBind) => {
            (RakuAstClass::InitializerBind, &s.expr)
        }
        _ => (RakuAstClass::InitializerAssign, expr),
    };
    let initializer = node_field(None, view_expr(names, expr)?);
    let initializer = RakuAstNode {
        origin: Default::default(),
        class,
        fields: vec![initializer],
    };
    fields.push(node_field(Some("initializer"), initializer));
    Ok(Some(statement_expression(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarDeclarationConstant,
        fields,
    })))
}
