//! Regex declaration nodes and their parser-owned reflection provenance.
use super::convert::{leaf_field, name_from_identifier, node_field, regex_node, signature};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::regex_tree::RegexTree;
use crate::value::{RuntimeError, Value};

/// `[my|our] token NAME(SIG) { … }`: scope precedes the method-style signature
/// and body; the default `has` scope is omitted.
// Cost: O(r + p), r = size of the regex, p = size of the parameters.
pub(super) fn convert(
    class: RakuAstClass,
    name: &str,
    tree: &RegexTree,
    scope: Option<&str>,
    multiness: Option<&str>,
    param_defs: &[ParamDef],
    export_trait: Option<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    let _underscore_scope = super::underscore_bindings::Scope::parameters(param_defs);
    with_body(
        class,
        Some(name),
        scope,
        multiness,
        param_defs,
        regex_node(&tree.body)?,
        export_trait.into_iter().collect(),
    )
}

/// [`convert`] over an already converted body (the `{*}` of a proto).
// Cost: O(p), p = size of the parameters.
pub(super) fn with_body(
    class: RakuAstClass,
    name: Option<&str>,
    scope: Option<&str>,
    multiness: Option<&str>,
    param_defs: &[ParamDef],
    body: RakuAstNode,
    traits: Vec<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = Vec::with_capacity(6);
    if let Some(scope) = scope {
        fields.push(leaf_field(Some("scope"), Value::str_from(scope)));
    }
    if let Some(multiness) = multiness {
        fields.push(leaf_field(Some("multiness"), Value::str_from(multiness)));
    }
    if let Some(name) = name {
        fields.push(node_field(Some("name"), name_from_identifier(name)));
    }
    if !param_defs.is_empty() {
        fields.push(node_field(
            Some("signature"),
            signature(param_defs, true, None)?,
        ));
    }
    if !traits.is_empty() {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        });
    }
    fields.push(node_field(Some("body"), body));
    Ok(RakuAstNode { class, fields })
}

const SOURCE: &str = "declaration-source";

/// Declaration reflection refers to the parsed source even after public AST
/// edits; execution always uses the current lowered tree (Rakudo oracle).
// Cost: O(s), s = declaration source length.
pub(super) fn retain_source(node: &mut RakuAstNode, body: &[Stmt]) {
    if let [Stmt::Expr(Expr::Literal(value))] = body {
        retain_value_source(node, value);
    }
}

// Cost: O(s), s = declaration source length.
pub(super) fn retain_value_source(node: &mut RakuAstNode, value: &Value) {
    if let Some(source) = value.regex_declared_source() {
        node.fields
            .push(leaf_field(Some(SOURCE), Value::str(source.to_string())));
    }
}

// Cost: O(1).
pub(super) fn is_source_field(field: &RakuAstField) -> bool {
    field.name == Some(SOURCE)
}

/// Retain only reflection provenance; never parse or execute it again.
// Cost: O(f + s), f = field count, s = source length.
pub(super) fn restore_source(node: &RakuAstNode, value: Value) -> Result<Value, RuntimeError> {
    if !node.fields.iter().any(is_source_field) {
        return Ok(value);
    }
    let source = super::lower::leaf_str(node, SOURCE)?;
    Ok(value.with_regex_declared_source(&source))
}
