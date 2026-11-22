//! The `Method::Initializer` thunk Rakudo builds for an attribute's default
//! (#18621), measured on rakudo 2026.11.
//!
//! An attribute declared with a default (`has $.x = EXPR`) gets a generated
//! method taking `(Mu $_)` that computes the default. Its `Trait::WillBuild`
//! holds:
//!
//! - the written term itself, when the default is a literal Rakudo keeps as
//!   written (`= 5`, `= "s"`, `= True`; [`super::attribute_decl::literal_default`]).
//!   The thunk is then empty, and the declaration visits it between its
//!   `initializer` and its traits.
//! - otherwise the thunk, whose body returns the default
//!   (`Call::Name(return, ArgList(EXPR))`), or for a `.=` default
//!   (`has R $.r .= new(...)`) calls the method on the declared type. The
//!   declaration's `initializer` is then not visited: the thunk holds the
//!   expression, with the origins the initializer's copy has.
//!
//! None of the generated nodes has an origin. The thunk is not one of the
//! declaration's constructor fields: `.raku`, equality and `EVAL` stay what
//! the source wrote.

use super::view::{leaf_field, node_field};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

fn plain(class: RakuAstClass, fields: Vec<super::RakuAstField>) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    }
}

/// A `Method::Initializer` whose body holds `statement` (none: empty).
// Cost: O(1).
fn thunk(statement: Option<RakuAstNode>) -> Value {
    let mu = plain(
        RakuAstClass::TypeSimple,
        vec![node_field(None, super::view::name_from_identifier("Mu"))],
    );
    let topic = plain(
        RakuAstClass::ParameterTargetVar,
        vec![leaf_field(Some("name"), Value::str_from("$_"))],
    );
    let parameter = plain(
        RakuAstClass::Parameter,
        vec![
            node_field(Some("type"), mu),
            node_field(Some("target"), topic),
            leaf_field(Some("optional"), Value::FALSE),
        ],
    );
    let signature = plain(
        RakuAstClass::Signature,
        vec![super::RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(parameter))]),
        }],
    );
    let list = plain(
        RakuAstClass::StatementList,
        statement.map(|s| node_field(None, s)).into_iter().collect(),
    );
    let body = plain(RakuAstClass::Blockoid, vec![node_field(None, list)]);
    Value::rakuast(Box::new(plain(
        RakuAstClass::MethodInitializer,
        vec![
            node_field(Some("signature"), signature),
            node_field(Some("body"), body),
        ],
    )))
}

/// What an attribute's `Trait::WillBuild` holds for its default `value`: the
/// value itself when it is a literal, else the thunk computing it (`call` for
/// a `.=` default, whose call is the body as it is; any other is returned).
// Cost: O(1).
pub(super) fn will_build_value(value: Value, call: bool) -> Value {
    let ValueView::RakuAst(node) = value.view() else {
        return value;
    };
    if super::attribute_decl::literal_default(node) {
        return value;
    }
    let expression = if call {
        RakuAstNode::clone(node)
    } else {
        let name = super::view::name_from_identifier("return");
        let args = plain(RakuAstClass::ArgList, vec![node_field(None, node.clone())]);
        plain(
            RakuAstClass::CallName,
            vec![node_field(Some("name"), name), node_field(Some("args"), args)],
        )
    };
    thunk(Some(super::view::statement_expression(expression)))
}

/// The thunk `node`'s `Trait::WillBuild` holds, if it holds one.
// Cost: O(t), t = traits of `node`.
fn held_thunk(node: &RakuAstNode) -> Option<&RakuAstNode> {
    let traits = node.fields.iter().find(|f| f.name == Some("traits"))?;
    let RakuAstFieldValue::List(items) = &traits.value else {
        return None;
    };
    items.iter().find_map(|item| match item.view() {
        ValueView::RakuAst(t) if t.class == RakuAstClass::TraitWillBuild => {
            super::infix_origins::child_at(t, 0)
                .filter(|held| held.class == RakuAstClass::MethodInitializer)
        }
        _ => None,
    })
}

/// Whether `node` is an attribute declaration whose default is computed by
/// its thunk: Rakudo then visits the thunk, not the `initializer`.
// Cost: O(t), t = traits of `node`.
pub(crate) fn is_thunked(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::VarDeclarationSimple && held_thunk(node).is_some()
}

/// `my $.q = 5` / `our $.q`: the accessor of a class attribute, a
/// `Method::ClassAccessor` named after it that returns the variable, which
/// the declaration visits after its fields. Its invocant is typed by the
/// package the declaration recorded (`origin.3`).
// Cost: O(1).
fn class_accessor(node: &RakuAstNode) -> Option<Value> {
    let package = node.origin.3.as_ref()?;
    let scope = super::decl_origins::leaf(node, "scope");
    if node.class != RakuAstClass::VarDeclarationSimple || scope.as_deref() != Some("our") && scope.is_some() {
        return None;
    }
    let desigil = super::infix_origins::child(node, "desigilname")
        .and_then(super::infix_origins::leaf_text)?;
    let sigil = super::decl_origins::leaf(node, "sigil")?;
    let var = plain(
        RakuAstClass::VarLexical,
        vec![leaf_field(None, Value::str(format!("{sigil}.{desigil}")))],
    );
    let list = plain(
        RakuAstClass::StatementList,
        vec![node_field(None, super::view::statement_expression(var))],
    );
    let mut accessor = plain(
        RakuAstClass::MethodClassAccessor,
        vec![
            node_field(Some("name"), super::view::name_from_identifier(&desigil)),
            node_field(Some("body"), plain(RakuAstClass::Blockoid, vec![node_field(None, list)])),
        ],
    );
    accessor.origin.3 = Some(package.clone());
    Some(Value::rakuast(Box::new(accessor)))
}

/// `has $.x = 5`: the empty thunk a declaration with a literal default visits
/// right after its `initializer`, and `my $.q`: the class accessor it visits
/// last (the `children` of `node` are `out`).
// Cost: O(t + c), t = traits of `node`, c = its children.
pub(crate) fn insert_visit(node: &RakuAstNode, out: &mut Vec<Value>) {
    if let Some(accessor) = class_accessor(node) {
        out.push(accessor);
        return;
    }
    if node.class != RakuAstClass::VarDeclarationSimple
        || held_thunk(node).is_some()
        || !super::decl_origins::leaf(node, "scope").is_some_and(|s| s == "has" || s == "HAS")
    {
        return;
    }
    let has_will_build = node.fields.iter().any(|f| {
        f.name == Some("traits")
            && matches!(&f.value, RakuAstFieldValue::List(items) if items.iter().any(|t|
                matches!(t.view(), ValueView::RakuAst(t) if t.class == RakuAstClass::TraitWillBuild)))
    });
    let initializer = super::infix_origins::child(node, "initializer");
    let Some(initializer) = initializer.filter(|_| has_will_build) else {
        return;
    };
    let at = out.iter().position(|child| {
        matches!(child.view(), ValueView::RakuAst(c) if std::ptr::eq(c, initializer) || *c == *initializer)
    });
    if let Some(at) = at {
        out.insert(at + 1, thunk(None));
    }
}

/// The expression a thunk's body computes: the `.=` call itself, or the
/// argument of its `return`.
// Cost: O(1).
fn body_expression(thunk: &mut RakuAstNode, f: impl FnOnce(&mut RakuAstNode)) {
    use super::infix_origins::{with_child, with_child_at};
    with_child(thunk, "body", |body| {
        with_child_at(body, 0, |list| {
            with_child_at(list, 0, |statement| {
                with_child(statement, "expression", |expression| {
                    if expression.class == RakuAstClass::CallName {
                        with_child(expression, "args", |args| with_child_at(args, 0, f));
                    } else {
                        f(expression);
                    }
                })
            })
        })
    });
}

/// Apply `f` to the expression the thunk in `node`'s `Trait::WillBuild`
/// computes.
// Cost: O(t), t = traits of `node`.
pub(super) fn with_thunk_expression(node: &mut RakuAstNode, f: impl FnOnce(&mut RakuAstNode)) {
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("traits")) else {
        return;
    };
    let RakuAstFieldValue::List(traits) = &mut field.value else {
        return;
    };
    let Some(value) = traits.iter_mut().find(|v| {
        matches!(v.view(), ValueView::RakuAst(t) if t.class == RakuAstClass::TraitWillBuild)
    }) else {
        return;
    };
    let ValueView::RakuAst(trait_node) = value.view() else {
        return;
    };
    let mut trait_node = RakuAstNode::clone(trait_node);
    super::infix_origins::with_child_at(&mut trait_node, 0, |held| {
        if held.class == RakuAstClass::MethodInitializer {
            body_expression(held, f);
        }
    });
    *value = Value::rakuast(Box::new(trait_node));
}
