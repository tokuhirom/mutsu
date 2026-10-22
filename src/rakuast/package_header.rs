//! The header of a `class` / `grammar` / `module` / `package` in RakuAST,
//! measured on rakudo 2026.09: the adverbs of the name and `is export`.
//!
//! `class A:ver<1.0>:auth<me> is export { }` is a `Class` whose `name` is
//! `Name.from-identifier("A", colonpairs => (ColonPair::Value(key => "ver",
//! value => QuotedString(<words val>, "1.0")), ...))` and whose `traits` hold
//! `Trait::Is(name => export)` (with `argument => (:tag, ...)` for tags); the
//! parser instead wraps the declaration in meta setters and an export
//! registration (`ast::package_header`). [`apply`] turns the parser's header
//! into those fields, [`strip`] takes them off a node again for the lowering.

use super::convert::{leaf_field, node_field, unsupported};
use super::lower::{list_field, named_child};
use super::routine_traits::{IsTraits, export_argument, trait_is};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::package_header::Header;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) fn words_value(text: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(vec![
                    Value::str_from("words"),
                    Value::str_from("val"),
                ]),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(RakuAstNode {
                    class: RakuAstClass::StrLiteral,
                    fields: vec![leaf_field(None, Value::str(text.to_string()))],
                }))]),
            },
        ],
    }
}

/// The header's fields added to the declaration expression `node`.
// Cost: O(n), n = size of the adverbs and the node's field list.
pub(super) fn apply(node: &RakuAstNode, header: &Header) -> Result<RakuAstNode, RuntimeError> {
    let mut node = node.clone();
    if !header.adverbs.is_empty() {
        let field = node
            .fields
            .iter_mut()
            .find(|f| f.name == Some("name"))
            .ok_or_else(|| unsupported("a declaration with adverbs but no name"))?;
        let RakuAstFieldValue::Node(name) = &field.value else {
            return Err(unsupported("declaration name"));
        };
        let ValueView::RakuAst(name) = name.view() else {
            return Err(unsupported("declaration name"));
        };
        if name.fields.iter().any(|f| f.name == Some("colonpairs")) {
            return Err(unsupported("a name that already has colonpairs"));
        }
        let mut name = name.clone();
        let mut pairs = Vec::new();
        for (key, value) in &header.adverbs {
            let value = match value {
                // Keep the parser's compact representation for angle words:
                // `:ver<1.2>` is a words quote in Rakudo's RakuAST.
                Expr::Literal(value) if let ValueView::Str(text) = value.view() => {
                    node_field(Some("value"), words_value(&text))
                }
                value => node_field(Some("value"), super::convert::convert_expr(value)?),
            };
            pairs.push(Value::rakuast(Box::new(RakuAstNode {
                class: RakuAstClass::ColonPairValue,
                fields: vec![leaf_field(Some("key"), Value::str(key.clone())), value],
            })));
        }
        name.fields.push(RakuAstField {
            name: Some("colonpairs"),
            value: RakuAstFieldValue::List(pairs),
        });
        *field = node_field(Some("name"), name);
    }
    if let Some(tags) = &header.export_tags
        && !super::package_traits::has_export(&node)
    {
        let export = Value::rakuast(Box::new(trait_is("export", export_argument(tags))));
        match node.fields.iter_mut().find(|f| f.name == Some("traits")) {
            Some(field) => {
                let RakuAstFieldValue::List(list) = &mut field.value else {
                    return Err(unsupported("declaration traits"));
                };
                list.push(export);
            }
            None => {
                let at = node
                    .fields
                    .iter()
                    .position(|f| matches!(f.name, Some("body" | "term")))
                    .unwrap_or(node.fields.len());
                node.fields.insert(
                    at,
                    RakuAstField {
                        name: Some("traits"),
                        value: RakuAstFieldValue::List(vec![export]),
                    },
                );
            }
        }
    }
    Ok(node)
}

/// `node` without the adverbs and the `is export` it carries, and what they
/// said.
// Cost: O(n), n = size of the node's field list and traits.
pub(super) fn strip(
    node: &RakuAstNode,
    strip_export: bool,
) -> Result<(RakuAstNode, Header), RuntimeError> {
    let mut header = Header::default();
    let mut stripped = node.clone();
    // The adverbs of the name.
    if let Some(field) = stripped.fields.iter_mut().find(|f| f.name == Some("name"))
        && let RakuAstFieldValue::Node(name) = &field.value
        && let ValueView::RakuAst(name) = name.view()
        && name.class == RakuAstClass::Name
        && name.fields.iter().any(|f| f.name == Some("colonpairs"))
    {
        for pair in list_field(name, "colonpairs")? {
            let ValueView::RakuAst(pair) = pair.view() else {
                return Err(super::lower::unsupported(node));
            };
            if pair.class != RakuAstClass::ColonPairValue {
                return Err(super::lower::unsupported(node));
            }
            let key = super::lower::leaf_str(pair, "key")?;
            let value = named_child(pair, "value")?;
            header.adverbs.push((key, super::lower::lower_expr(value)?));
        }
        let mut bare = name.clone();
        bare.fields.retain(|f| f.name != Some("colonpairs"));
        *field = node_field(Some("name"), bare);
    }
    // `is export`, which the parser keeps as a registration, not a trait.
    if strip_export
        && let Some(field) = stripped
            .fields
            .iter_mut()
            .find(|f| f.name == Some("traits"))
        && let RakuAstFieldValue::List(list) = &mut field.value
    {
        let mut kept = Vec::new();
        for item in list.iter() {
            let ValueView::RakuAst(item_node) = item.view() else {
                kept.push(item.clone());
                continue;
            };
            let mut flags = IsTraits::default();
            if item_node.class == RakuAstClass::TraitIs
                && item_node.fields.iter().any(|f| f.name == Some("name"))
                && flags.read(item_node)?
                && !flags.export_tags.is_empty()
            {
                header.export_tags = Some(flags.export_tags);
            } else {
                kept.push(item.clone());
            }
        }
        *list = kept;
    }
    stripped.fields.retain(|f| {
        !(f.name == Some("traits")
            && matches!(&f.value, RakuAstFieldValue::List(list) if list.is_empty()))
    });
    Ok((stripped, header))
}

/// The export tags a lexical `my class ... is export` carries as an internal
/// marker in its `custom_traits` (see `parser::export_type_marker`).
// Cost: O(t), t = custom traits and tags.
pub(super) fn lexical_export_tags(custom_traits: &[(String, Option<Expr>)]) -> Option<Vec<String>> {
    let (_, Some(Expr::ArrayLiteral(tags))) = custom_traits
        .iter()
        .find(|(t, _)| t == crate::parser::EXPORT_TYPE_MARKER)?
    else {
        return None;
    };
    tags.iter()
        .map(|tag| match tag {
            Expr::Literal(v) => v.as_str().map(str::to_string),
            _ => None,
        })
        .collect()
}

/// Lower a package-like declaration node with `lower`, then put its header
/// back the way the parser spells it: the registration and meta setters around
/// the declaration, or, for a lexical class, the marker in its traits.
// Cost: O(n), n = size of the node.
pub(super) fn lower_with_header(
    node: &RakuAstNode,
    lower: impl FnOnce(&RakuAstNode) -> Result<crate::ast::Stmt, RuntimeError>,
) -> Result<crate::ast::Stmt, RuntimeError> {
    use crate::ast::Stmt;
    let (stripped, mut header) = strip(node, node.class != RakuAstClass::Role)?;
    let mut stmt = lower(&stripped)?;
    if super::routine_source::has_supersede(node) {
        match &mut stmt {
            Stmt::ClassDecl { custom_traits, .. } => {
                custom_traits.push(crate::parser::declare_how_marker("class".to_string()));
            }
            Stmt::RoleDecl { custom_traits, .. } => {
                custom_traits.push(crate::parser::declare_how_marker("role".to_string()));
            }
            _ => return Err(super::lower::unsupported(node)),
        }
    }
    if let Stmt::Package { body, .. } = &mut stmt {
        super::routine_source::retain(node, body)?;
    }
    let name = match &stmt {
        Stmt::ClassDecl { name, .. } | Stmt::Package { name, .. } | Stmt::RoleDecl { name, .. } => {
            name.resolve()
        }
        _ => return Err(super::lower::unsupported(node)),
    };
    if let Stmt::ClassDecl {
        is_lexical: true,
        custom_traits,
        ..
    } = &mut stmt
        && let Some(tags) = header.export_tags.take()
    {
        custom_traits.push(crate::parser::export_type_marker(&tags));
    }
    if let Stmt::RoleDecl { custom_traits, .. } = &mut stmt
        && custom_traits
            .iter()
            .any(|(name, _)| name == super::convert::MY_SCOPED)
        && let Some(tags) = header.export_tags.take()
    {
        custom_traits.push(crate::parser::export_type_marker(&tags));
    }
    Ok(crate::ast::package_header::wrap(stmt, &name, header))
}

/// The `is export` trait node of an enum or subset that is exported, from its
/// recorded tags (bare `is export` is the `DEFAULT` tag).
// Cost: O(t), t = tags.
pub(super) fn export_trait_value(is_export: bool, export_tags: &[String]) -> Option<Value> {
    if !is_export {
        return None;
    }
    let tags: Vec<String> = if export_tags.is_empty() {
        vec!["DEFAULT".to_string()]
    } else {
        export_tags.to_vec()
    };
    Some(Value::rakuast(Box::new(trait_is(
        "export",
        export_argument(&tags),
    ))))
}

/// The `scope => "my"` leading field of a lexical enum or subset.
// Cost: O(1).
pub(super) fn my_scope_field(is_my: bool) -> Option<RakuAstField> {
    is_my.then(|| leaf_field(Some("scope"), Value::str_from("my")))
}

/// The scope and export a lowered enum or subset node says, with the node as
/// the lowering reads it: the `traits` of its `is export` taken off.
// Cost: O(n), n = size of the node's field list and traits.
pub(super) fn strip_scope_and_export(
    node: &RakuAstNode,
) -> Result<(RakuAstNode, bool, bool, Vec<String>), RuntimeError> {
    let is_my = match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => false,
        Some(_) => match super::lower::leaf_str(node, "scope")?.as_str() {
            "my" => true,
            "our" => false,
            _ => return Err(super::lower::unsupported(node)),
        },
    };
    let (stripped, header) = strip(node, true)?;
    let (is_export, tags) = match header.export_tags {
        None => (false, Vec::new()),
        // The parser keeps a bare `is export` as an empty tag list.
        Some(tags) if tags == ["DEFAULT"] => (true, Vec::new()),
        Some(tags) => (true, tags),
    };
    Ok((stripped, is_my, is_export, tags))
}
