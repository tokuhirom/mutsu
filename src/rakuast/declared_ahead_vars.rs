//! The unit-level lexicals a package declared ahead of `EVAL` sees
//! (ADR-17472, #17522).
//!
//! Rakudo declares a package at BEGIN time inside its unit's own lexpad, so a
//! method reading an earlier `my $kind` closes over the variable the unit's
//! `EVAL` later fills. mutsu declares the package ahead in a unit of its own
//! (`Interpreter::package_node_meta_object`). To give the two the same
//! variable, each earlier top-level `my` of the unit is bound, in that ahead
//! unit, to a shared container recorded on the unit source; when the unit is
//! `EVAL`ed, its own declaration of the variable binds the same container and
//! then assigns its initializer through it. The method then reads what the
//! `EVAL` stored, as in rakudo.
//!
//! Containers are keyed by the declaration's start offset, like the declared
//! types (`declared_ahead`), and a variable gets one only once some package
//! of its unit has been declared ahead.

use super::infix_origins::{child, leaf_text};
use super::view::{leaf_field, node_field};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The `[name, from]` rows of a unit's top-level `my`/`our` declarations.
pub(super) const VARS_ATTR: &str = "__mutsu_unit_vars";
/// The shared containers of the declarations, keyed by start offset.
const CELLS_ATTR: &str = "__mutsu_unit_var_cells";

/// The scope keyword and the `sigil + name` of a simple declaration.
// Cost: O(f), f = the node's fields.
fn scope_and_name(decl: &RakuAstNode) -> Option<(String, String)> {
    let leaf = |name: &str| {
        decl.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name == Some(name) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        })
    };
    let sigil = leaf("sigil")?;
    if leaf("twigil").is_some_and(|t| !t.is_empty()) {
        return None;
    }
    let name = child(decl, "desigilname").and_then(leaf_text)?;
    let scope = leaf("scope").unwrap_or_else(|| "my".to_string());
    matches!(sigil.as_str(), "$" | "@" | "%").then(|| (scope, format!("{sigil}{name}")))
}

/// Record the top-level declaration `decl` of a unit `.AST` parsed.
// Cost: O(f), f = the node's fields.
pub(super) fn note_unit_var(rows: &mut Vec<Value>, decl: &RakuAstNode) {
    let Some((scope, name)) = scope_and_name(decl) else {
        return;
    };
    if scope != "my" && scope != "our" {
        return;
    }
    let Some((_, from, _)) = super::declared_ahead::origin_span(decl) else {
        return;
    };
    rows.push(Value::array(vec![
        Value::str(scope),
        Value::str(name),
        Value::int(from),
    ]));
}

/// One earlier top-level declaration of a package's unit.
pub(crate) struct UnitVar {
    pub(crate) scope: String,
    pub(crate) name: String,
    /// The container a `my` shares with the unit's `EVAL`; `None` for `our`.
    pub(crate) cell: Option<Value>,
}

/// The top-level declarations before the package node `node` in its unit,
/// each `my` with its shared container (made now if none yet).
// Cost: O(v), v = top-level declarations of the unit.
pub(crate) fn unit_vars_before(node: &RakuAstNode) -> Vec<UnitVar> {
    let Some((source, at, _)) = super::declared_ahead::origin_span(node) else {
        return Vec::new();
    };
    let ValueView::Instance { attributes, .. } = source.view() else {
        return Vec::new();
    };
    let Some(rows) = attributes.as_map().get(VARS_ATTR).cloned() else {
        return Vec::new();
    };
    let mut cells = match attributes.as_map().get(CELLS_ATTR).map(Value::view) {
        Some(ValueView::Hash(map)) => (**map).clone(),
        _ => Default::default(),
    };
    let mut out = Vec::new();
    for row in rows.as_list_items().unwrap_or_default().iter() {
        let Some(cells_row) = row.as_list_items() else {
            continue;
        };
        let [scope, name, from] = &cells_row[..] else {
            continue;
        };
        let ValueView::Int(from) = from.view() else {
            continue;
        };
        if from >= at {
            continue;
        }
        let (scope, name) = (scope.to_string_value(), name.to_string_value());
        // TODO: an `our` is declared again so the package compiles, but it
        // is not shared: binding the package symbol to a container loops
        // through the package store's write-through, and the ahead unit's
        // closure keeps its own binding of the name (#17508's family).
        let cell = (scope == "my").then(|| {
            cells
                .entry(from.to_string())
                .or_insert_with(|| fresh_container(&name))
                .clone()
        });
        out.push(UnitVar { scope, name, cell });
    }
    attributes.insert(CELLS_ATTR.to_string(), Value::hash_bare_values(cells));
    out
}

/// An empty container for the variable `name`, shared by reference.
// Cost: O(1).
fn fresh_container(name: &str) -> Value {
    let inner = if name.starts_with('@') {
        Value::real_array(Vec::new())
    } else if name.starts_with('%') {
        Value::hash_bare_values(crate::value::HashData::default())
    } else {
        Value::package(crate::symbol::wk::any())
    };
    Value::container_ref(crate::gc::Gc::new(crate::value::ContainerCell::new(inner)))
}

/// The shared container of the top-level declaration `decl`, when a package of
/// its unit was declared ahead.
// Cost: O(1) average (one hash probe).
fn cell_of(decl: &RakuAstNode) -> Option<Value> {
    let (source, from, _) = super::declared_ahead::origin_span(decl)?;
    let ValueView::Instance { attributes, .. } = source.view() else {
        return None;
    };
    let cells = attributes.as_map().get(CELLS_ATTR)?.clone();
    let ValueView::Hash(map) = cells.view() else {
        return None;
    };
    map.get(from.to_string().as_str()).cloned()
}

/// `decl` with its initializer replaced by a bind to `cell`, and without its
/// origin (so it is not rewritten again).
// Cost: O(f), f = the node's fields.
pub(crate) fn bound_to(decl: &RakuAstNode, cell: Value) -> RakuAstNode {
    let mut bound = decl.clone();
    bound.origin = Default::default();
    bound.fields.retain(|f| f.name != Some("initializer"));
    let literal = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Literal,
        fields: vec![leaf_field(None, cell)],
    };
    bound.fields.push(node_field(
        Some("initializer"),
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::InitializerBind,
            fields: vec![node_field(None, literal)],
        },
    ));
    bound
}

/// The unit's own declaration of a variable a package declared ahead shares:
/// a bind of the shared container, then the assignment of the declaration's
/// initializer through it. `None` for any other node.
// Cost: O(f), f = the node's fields.
pub(super) fn rewrite(decl: &RakuAstNode) -> Option<(RakuAstNode, Option<RakuAstNode>)> {
    if decl.class != RakuAstClass::VarDeclarationSimple {
        return None;
    }
    let cell = cell_of(decl)?;
    let (_, name) = scope_and_name(decl)?;
    let assign = child(decl, "initializer")
        .filter(|init| init.class == RakuAstClass::InitializerAssign)
        .and_then(|init| match &init.fields.first()?.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::RakuAst(expr) => Some(expr.clone()),
                _ => None,
            },
            _ => None,
        })
        .map(|expr| RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(
                    Some("left"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::VarLexical,
                        fields: vec![leaf_field(None, Value::str(name.clone()))],
                    },
                ),
                node_field(
                    Some("infix"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::Infix,
                        fields: vec![leaf_field(None, Value::str("=".to_string()))],
                    },
                ),
                node_field(Some("right"), expr),
            ],
        });
    Some((bound_to(decl, cell), assign))
}

/// The statement `statement` (`Statement::Expression` of a declaration) with
/// its declaration bound to `cell`.
// Cost: O(f), f = the node's fields.
pub(crate) fn bind_statement(statement: &Value, cell: Value) -> Value {
    let Some(stmt) = super::declared_ahead::node_of(statement) else {
        return statement.clone();
    };
    let mut stmt = stmt.clone();
    for field in &mut stmt.fields {
        if field.name != Some("expression") {
            continue;
        }
        if let RakuAstFieldValue::Node(v) = &field.value
            && let ValueView::RakuAst(decl) = v.view()
        {
            field.value =
                RakuAstFieldValue::Node(Value::rakuast(Box::new(bound_to(decl, cell.clone()))));
        }
    }
    Value::rakuast(Box::new(stmt))
}
