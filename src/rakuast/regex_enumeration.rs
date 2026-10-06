//! `RakuAST::Regex::Assertion::CharClass` with its `CharClassElement::*` and
//! `CharClassEnumerationElement::*` entries, in both directions
//! (`crate::regex_tree::CharClassElement`).
//!
//! Measured on rakudo 2026.09: the assertion holds its elements
//! positionally; an `Enumeration` holds `negated` and an `elements` list of
//! `Character`s (positional string), `Range`s (`from` / `to` codepoints) and
//! `CharClass::*` nodes; a `Rule` holds `negated` and `name`; a `Property`
//! holds `negated`, `inverted` (its `!`), `property` and, for `<:Script<Latin>>`
//! a `predicate` of words (a `QuotedString`) or, for `<:Nv(1)>`, of a
//! parenthesised expression.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::regex_tree::{CharClassElement, EnumerationElement, PropertyPredicate};
use crate::value::{RuntimeError, Value, ValueView};

fn named(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}

fn positional(value: Value) -> RakuAstField {
    RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(value),
    }
}

fn node(class: RakuAstClass, fields: Vec<RakuAstField>) -> Value {
    Value::rakuast(Box::new(RakuAstNode { class, fields }))
}

/// The `Assertion::CharClass` node for a tree assertion.
// Cost: O(n), n = total number of entries.
pub(super) fn convert(elements: &[CharClassElement]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::RegexAssertionCharClass,
        fields: elements
            .iter()
            .map(|element| convert_element(element).map(positional))
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn convert_element(element: &CharClassElement) -> Result<Value, RuntimeError> {
    Ok(match element {
        CharClassElement::Enumeration { negated, elements } => {
            let mut fields = Vec::new();
            if *negated {
                fields.push(named("negated", Value::truth(true)));
            }
            fields.push(RakuAstField {
                name: Some("elements"),
                value: RakuAstFieldValue::List(elements.iter().map(convert_entry).collect()),
            });
            node(RakuAstClass::RegexCharClassElementEnumeration, fields)
        }
        CharClassElement::Rule { name, negated } => {
            let mut fields = Vec::new();
            if *negated {
                fields.push(named("negated", Value::truth(true)));
            }
            fields.push(named("name", Value::str(name.clone())));
            node(RakuAstClass::RegexCharClassElementRule, fields)
        }
        CharClassElement::Property {
            name,
            negated,
            inverted,
            predicate,
        } => {
            let mut fields = Vec::new();
            if *negated {
                fields.push(named("negated", Value::truth(true)));
            }
            if *inverted {
                fields.push(named("inverted", Value::truth(true)));
            }
            fields.push(named("property", Value::str(name.clone())));
            if let Some(predicate) = predicate {
                fields.push(named("predicate", convert_predicate(predicate)?));
            }
            node(RakuAstClass::RegexCharClassElementProperty, fields)
        }
    })
}

/// `<Latin>` as a words `QuotedString`, `(1)` as a parenthesised expression.
// Cost: O(|predicate|).
fn convert_predicate(predicate: &PropertyPredicate) -> Result<Value, RuntimeError> {
    match predicate {
        PropertyPredicate::Words(text) => Ok(Value::rakuast(Box::new(super::convert::word_quote(
            text.trim(),
        )))),
        PropertyPredicate::Args(text) => {
            let expr = crate::parser::parse_adverb_argument(text)
                .ok_or_else(|| super::convert::unsupported("property predicate"))?;
            let value = super::convert::convert_expr(&expr)?;
            // What cannot be spelled again cannot be lowered: refuse it here.
            if predicate_text(&value).is_none() {
                return Err(super::convert::unsupported("property predicate"));
            }
            let semilist = RakuAstNode {
                class: RakuAstClass::SemiList,
                fields: vec![positional(Value::rakuast(Box::new(
                    super::convert::statement_expression(value),
                )))],
            };
            Ok(node(
                RakuAstClass::CircumfixParentheses,
                vec![positional(Value::rakuast(Box::new(semilist)))],
            ))
        }
    }
}

/// The text of a predicate argument that is a number or a plain string; `None`
/// for any other expression.
// Cost: O(|text|).
fn predicate_text(value: &RakuAstNode) -> Option<String> {
    match value.class {
        RakuAstClass::IntLiteral => {
            Some(super::lower::positional_leaf(value).ok()?.to_string_value())
        }
        RakuAstClass::QuotedString => {
            let segments = value.fields.iter().find(|f| f.name == Some("segments"))?;
            let RakuAstFieldValue::List(items) = &segments.value else {
                return None;
            };
            let [only] = items.as_slice() else {
                return None;
            };
            let ValueView::RakuAst(segment) = only.view() else {
                return None;
            };
            let text = super::lower::positional_leaf(segment)
                .ok()?
                .to_string_value();
            let escaped: String = text
                .chars()
                .flat_map(|ch| match ch {
                    '\\' | '"' | '$' | '@' | '%' | '&' | '{' | '}' => vec!['\\', ch],
                    _ => vec![ch],
                })
                .collect();
            Some(format!("\"{escaped}\""))
        }
        _ => None,
    }
}

fn convert_entry(entry: &EnumerationElement) -> Value {
    match entry {
        EnumerationElement::Character(ch) => node(
            RakuAstClass::RegexCharClassEnumerationElementCharacter,
            vec![positional(Value::str(ch.to_string()))],
        ),
        EnumerationElement::Range(from, to) => node(
            RakuAstClass::RegexCharClassEnumerationElementRange,
            vec![
                named("from", Value::int(*from as i64)),
                named("to", Value::int(*to as i64)),
            ],
        ),
        EnumerationElement::Class(atom) => {
            Value::rakuast(Box::new(super::regex_char_class::convert(atom)))
        }
    }
}

fn field<'a>(node: &'a RakuAstNode, name: Option<&str>) -> Option<&'a Value> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) if f.name == name => Some(v),
        _ => None,
    })
}

fn negated(node: &RakuAstNode) -> bool {
    field(node, Some("negated")).is_some_and(Value::truthy)
}

fn codepoint(value: Option<&Value>) -> Option<char> {
    match value?.view() {
        ValueView::Int(n) => u32::try_from(n).ok().and_then(char::from_u32),
        _ => None,
    }
}

/// The tree assertion for an `Assertion::CharClass` node, or `None` when an
/// entry is malformed.
// Cost: O(n), n = total number of entries.
pub(super) fn lower(node: &RakuAstNode) -> Option<Vec<CharClassElement>> {
    let elements: Vec<CharClassElement> = node
        .fields
        .iter()
        .map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name.is_none() => match v.view() {
                ValueView::RakuAst(element) => lower_element(element),
                _ => None,
            },
            _ => None,
        })
        .collect::<Option<_>>()?;
    (!elements.is_empty()).then_some(elements)
}

fn lower_element(node: &RakuAstNode) -> Option<CharClassElement> {
    match node.class {
        RakuAstClass::RegexCharClassElementEnumeration => {
            let entries = node.fields.iter().find_map(|f| match &f.value {
                RakuAstFieldValue::List(items) if f.name == Some("elements") => Some(items),
                _ => None,
            });
            let elements = entries
                .map(|items| {
                    items
                        .iter()
                        .map(|item| match item.view() {
                            ValueView::RakuAst(entry) => lower_entry(entry),
                            _ => None,
                        })
                        .collect::<Option<Vec<_>>>()
                })
                .unwrap_or(Some(Vec::new()))?;
            Some(CharClassElement::Enumeration {
                negated: negated(node),
                elements,
            })
        }
        RakuAstClass::RegexCharClassElementRule => {
            let name = field(node, Some("name"))?.to_string_value();
            let identifier = crate::regex_tree::is_class_name(&name);
            identifier.then(|| CharClassElement::Rule {
                name,
                negated: negated(node),
            })
        }
        RakuAstClass::RegexCharClassElementProperty => {
            let name = field(node, Some("property"))?.to_string_value();
            let predicate = match field(node, Some("predicate")) {
                None => None,
                Some(value) => Some(lower_predicate(value)?),
            };
            let identifier = crate::regex_tree::is_class_name(&name);
            identifier.then(|| CharClassElement::Property {
                name,
                negated: negated(node),
                inverted: field(node, Some("inverted")).is_some_and(Value::truthy),
                predicate,
            })
        }
        _ => None,
    }
}

fn lower_entry(node: &RakuAstNode) -> Option<EnumerationElement> {
    match node.class {
        RakuAstClass::RegexCharClassEnumerationElementCharacter => {
            let text = field(node, None)?.to_string_value();
            let mut chars = text.chars();
            let ch = chars.next()?;
            chars
                .next()
                .is_none()
                .then_some(EnumerationElement::Character(ch))
        }
        RakuAstClass::RegexCharClassEnumerationElementRange => Some(EnumerationElement::Range(
            codepoint(field(node, Some("from")))?,
            codepoint(field(node, Some("to")))?,
        )),
        RakuAstClass::RegexCharClass(kind) => {
            super::regex_char_class::lower(kind, node).map(EnumerationElement::Class)
        }
        _ => None,
    }
}

fn pairs(args: &[Value]) -> (Vec<(String, Value)>, Vec<Value>) {
    let mut named = Vec::new();
    let mut positional = Vec::new();
    for arg in args {
        match arg.view() {
            ValueView::Pair(k, v) => named.push((k.as_str().to_string(), v.clone())),
            ValueView::ValuePair(k, v) => named.push((k.to_string_value(), v.clone())),
            _ => positional.push(arg.clone()),
        }
    }
    (named, positional)
}

fn is_class(value: &Value, accept: impl Fn(RakuAstClass) -> bool) -> bool {
    matches!(value.view(), ValueView::RakuAst(node) if accept(node.class))
}

/// The `.new` constructors of the char-class assertion classes, or `None`
/// when `class_name` is none of them.
// Cost: O(a), a = number of arguments.
pub(super) fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if method != "new" {
        return None;
    }
    let class = match class_name {
        "RakuAST::Regex::Assertion::CharClass" => RakuAstClass::RegexAssertionCharClass,
        "RakuAST::Regex::CharClassElement::Enumeration" => {
            RakuAstClass::RegexCharClassElementEnumeration
        }
        "RakuAST::Regex::CharClassElement::Rule" => RakuAstClass::RegexCharClassElementRule,
        "RakuAST::Regex::CharClassElement::Property" => RakuAstClass::RegexCharClassElementProperty,
        "RakuAST::Regex::CharClassEnumerationElement::Character" => {
            RakuAstClass::RegexCharClassEnumerationElementCharacter
        }
        "RakuAST::Regex::CharClassEnumerationElement::Range" => {
            RakuAstClass::RegexCharClassEnumerationElementRange
        }
        _ => return None,
    };
    let (named_args, positional_args) = pairs(args);
    let error = |what: &str| Some(Err(RuntimeError::new(format!("{class_name}.new {what}"))));
    let mut fields = Vec::new();
    match class {
        RakuAstClass::RegexAssertionCharClass => {
            if !named_args.is_empty() {
                return error("takes only positional elements");
            }
            for value in positional_args {
                if !is_class(&value, |c| {
                    matches!(
                        c,
                        RakuAstClass::RegexCharClassElementEnumeration
                            | RakuAstClass::RegexCharClassElementRule
                            | RakuAstClass::RegexCharClassElementProperty
                    )
                }) {
                    return error("takes CharClassElement nodes");
                }
                fields.push(positional(value));
            }
        }
        RakuAstClass::RegexCharClassEnumerationElementCharacter => {
            let [value] = positional_args.as_slice() else {
                return error("takes one character");
            };
            fields.push(positional(Value::str(value.to_string_value())));
        }
        _ => {
            if !positional_args.is_empty() {
                return error("takes only named arguments");
            }
            let order: &[&str] = match class {
                RakuAstClass::RegexCharClassElementEnumeration => &["negated", "elements"],
                RakuAstClass::RegexCharClassElementRule => &["negated", "name"],
                RakuAstClass::RegexCharClassElementProperty => {
                    &["negated", "inverted", "property", "predicate"]
                }
                _ => &["from", "to"],
            };
            if let Some((key, _)) = named_args
                .iter()
                .find(|(k, _)| !order.contains(&k.as_str()))
            {
                return error(&format!("does not accept `{key}`"));
            }
            for &name in order {
                let Some((_, value)) = named_args.iter().find(|(k, _)| k == name) else {
                    continue;
                };
                match name {
                    "negated" | "inverted" => {
                        if value.truthy() {
                            fields.push(named(name, Value::truth(true)));
                        }
                    }
                    "elements" => fields.push(RakuAstField {
                        name: Some("elements"),
                        value: RakuAstFieldValue::List(match value.view() {
                            ValueView::RakuAst(_) => vec![value.clone()],
                            _ => match value.as_list_items() {
                                Some(items) => items.to_vec(),
                                None => return error("expects `elements` to be a list"),
                            },
                        }),
                    }),
                    "name" | "property" => {
                        fields.push(named(name, Value::str(value.to_string_value())))
                    }
                    _ => fields.push(named(name, value.clone())),
                }
            }
        }
    }
    Some(Ok(node(class, fields)))
}

/// A property's `predicate` node as the text written after its name.
// Cost: O(|predicate|).
fn lower_predicate(value: &Value) -> Option<PropertyPredicate> {
    let ValueView::RakuAst(node) = value.view() else {
        return None;
    };
    match node.class {
        RakuAstClass::QuotedString => {
            let segments = node.fields.iter().find(|f| f.name == Some("segments"))?;
            let RakuAstFieldValue::List(items) = &segments.value else {
                return None;
            };
            let [only] = items.as_slice() else {
                return None;
            };
            let ValueView::RakuAst(segment) = only.view() else {
                return None;
            };
            Some(PropertyPredicate::Words(
                super::lower::positional_leaf(segment)
                    .ok()?
                    .to_string_value(),
            ))
        }
        RakuAstClass::CircumfixParentheses => {
            let ValueView::RakuAst(semilist) = field(node, None)?.view() else {
                return None;
            };
            let [statement] = semilist.fields.as_slice() else {
                return None;
            };
            let RakuAstFieldValue::Node(statement) = &statement.value else {
                return None;
            };
            let ValueView::RakuAst(statement) = statement.view() else {
                return None;
            };
            let expression = super::lower::named_child(statement, "expression").ok()?;
            Some(PropertyPredicate::Args(predicate_text(expression)?))
        }
        _ => None,
    }
}
