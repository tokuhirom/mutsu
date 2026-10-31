//! `enum` declarations: the `Type::Enum` node in both directions.
//!
//! Rakudo has no variant-list grammar for an enum: the body is one term that
//! is evaluated at compile time, and `RakuAST::Type::Enum.term` keeps it as
//! written (a word quote, or a parenthesized expression). A base type
//! (`our Str enum E ...`) is the `of` field. Measured on rakudo 2026.09:
//!
//! ```text
//! our Str enum E «:A<a> b»  ->  Type::Enum(name, of => Type::Simple(Str),
//!                                 term => QuotedString(processors => <quotewords val>,
//!                                   segments => (QuoteWordsAtom(ColonPair::Value), StrLiteral(" b"))))
//! enum E (:20A, slip <B>)   ->  Type::Enum(name, term => Circumfix::Parentheses(...))
//! ```
//!
//! The parser decomposes a static body into `(name, value)` variants and keeps
//! any other parenthesized body as one computed (`__DYNAMIC__`) expression.
//! Lowering a parenthesized term reuses the parser's own decomposition
//! ([`enum_variants_from_body`]), so a round-tripped enum takes the same path
//! the parsed one did.

use crate::scope::NameScope;
use super::convert::{
    convert_expr, leaf_field, node_field, plain_infix, quoted_string, statement_expression,
};
use super::lower::{list_field, lower_expr, named_child, positional_leaf, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{EnumVariantForm, Expr};
use crate::parser::enum_variants_from_body;
use crate::value::{RuntimeError, Value, ValueView};

type Variants = Vec<(String, Option<Expr>)>;

/// The name of the computed-body placeholder variant the parser emits.
const DYNAMIC: &str = "__DYNAMIC__";

/// The `term` of an enum declaration, from the parser's variants and the
/// spelling it recorded.
// Cost: O(v), v = variants.
pub(super) fn enum_term_node(names: &NameScope, 
    variants: &[(String, Option<Expr>)],
    form: &EnumVariantForm,
) -> Result<RakuAstNode, RuntimeError> {
    match form {
        EnumVariantForm::Words => word_term(names, variants, "words"),
        EnumVariantForm::EmptyWords(raw) => Ok(RakuAstNode {
            origin: Default::default(),
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
                    value: RakuAstFieldValue::List(vec![str_literal(raw.to_string())]),
                },
            ],
        }),
        EnumVariantForm::QuoteWords => word_term(names, variants, "quotewords"),
        EnumVariantForm::PairList => pair_list_term(names, variants),
        EnumVariantForm::Computed => match variants {
            [(name, Some(body))] if name == DYNAMIC => computed_term(names, body),
            _ => Err(super::convert::unsupported("enum with a computed body")),
        },
    }
}

/// A computed body is the parenthesized term itself.
fn computed_term(names: &NameScope, body: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let node = convert_expr(names, body)?;
    if node.class == RakuAstClass::CircumfixParentheses {
        return Ok(node);
    }
    Ok(parenthesized(node))
}

fn parenthesized(expr: RakuAstNode) -> RakuAstNode {
    let semilist = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(expr))],
    };
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    }
}

/// The unevaluated word-list term. `processors` is part of the RakuAST
/// contract: omitting it would turn `Red Green` into one enum member named
/// "Red Green" when the node is evaluated. A `«…»` body may hold colonpair
/// variants; each is a `QuoteWordsAtom` segment between the literal words.
fn word_term(names: &NameScope, 
    variants: &[(String, Option<Expr>)],
    processor: &'static str,
) -> Result<RakuAstNode, RuntimeError> {
    let mut segments = Vec::new();
    let mut text = String::new();
    for (i, (name, value)) in variants.iter().enumerate() {
        if i > 0 {
            text.push(' ');
        }
        let Some(value) = value else {
            text.push_str(name);
            continue;
        };
        if processor != "quotewords" {
            return Err(super::convert::unsupported(
                "word-quoted enum with explicit values",
            ));
        }
        if !text.is_empty() {
            segments.push(str_literal(std::mem::take(&mut text)));
        }
        let pair = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ColonPairValue,
            fields: vec![
                leaf_field(Some("key"), Value::str(name.clone())),
                node_field(Some("value"), convert_expr(names, value)?),
            ],
        };
        segments.push(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::QuoteWordsAtom,
            fields: vec![node_field(None, pair)],
        })));
    }
    if !text.is_empty() || segments.is_empty() {
        segments.push(str_literal(text));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(vec![
                    Value::str(processor.to_string()),
                    Value::str("val".to_string()),
                ]),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(segments),
            },
        ],
    })
}

fn str_literal(text: String) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StrLiteral,
        fields: vec![leaf_field(None, Value::str(text))],
    }))
}

/// The parenthesized pair-list term.
fn pair_list_term(names: &NameScope, variants: &[(String, Option<Expr>)]) -> Result<RakuAstNode, RuntimeError> {
    if variants.is_empty() {
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::CircumfixParentheses,
            fields: vec![node_field(
                None,
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::SemiList,
                    fields: Vec::new(),
                },
            )],
        });
    }
    let operands = variants
        .iter()
        .map(|(name, value)| {
            let value = match value {
                Some(value) => convert_expr(names, value)?,
                // A bare `'name'` element: the parser keeps no value for it.
                None => {
                    return Ok(Value::rakuast(Box::new(quoted_string(Value::str(
                        name.clone(),
                    )))));
                }
            };
            Ok(Value::rakuast(Box::new(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::FatArrow,
                fields: vec![
                    leaf_field(Some("key"), Value::str(name.clone())),
                    node_field(Some("value"), value),
                ],
            })))
        })
        .collect::<Result<Vec<_>, RuntimeError>>()?;
    let list = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), plain_infix(",")),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(operands),
            },
        ],
    };
    Ok(parenthesized(list))
}

/// The variants and spelling of an enum's `term` (the lowering direction).
// Cost: O(v), v = variants.
pub(super) fn lower_enum_term(names: &NameScope, 
    node: &RakuAstNode,
) -> Result<(Variants, EnumVariantForm), RuntimeError> {
    let term = named_child(node, "term")?;
    match term.class {
        RakuAstClass::QuotedString => lower_word_term(term),
        RakuAstClass::CircumfixParentheses => {
            let body = lower_expr(names, term)?;
            Ok(match enum_variants_from_body(&body) {
                Some(variants) => (variants, EnumVariantForm::PairList),
                None => (
                    vec![(DYNAMIC.to_string(), Some(body))],
                    EnumVariantForm::Computed,
                ),
            })
        }
        _ => Err(unsupported(node)),
    }
}

fn lower_word_term(term: &RakuAstNode) -> Result<(Variants, EnumVariantForm), RuntimeError> {
    let processors = list_field(term, "processors")?;
    let names: Vec<_> = processors.iter().filter_map(|p| p.as_str()).collect();
    let form = match names.as_slice() {
        ["words", "val"] => EnumVariantForm::Words,
        ["quotewords", "val"] => EnumVariantForm::QuoteWords,
        _ => return Err(unsupported(term)),
    };
    let mut variants = Vec::new();
    let mut empty_text = None;
    for segment in list_field(term, "segments")? {
        let ValueView::RakuAst(segment) = segment.view() else {
            return Err(unsupported(term));
        };
        match segment.class {
            RakuAstClass::StrLiteral => {
                let text = positional_leaf(segment)?;
                let ValueView::Str(text) = text.view() else {
                    return Err(unsupported(term));
                };
                if variants.is_empty() && text.split_whitespace().next().is_none() {
                    empty_text = Some(text.to_string());
                }
                variants.extend(text.split_whitespace().map(|name| (name.to_string(), None)));
            }
            RakuAstClass::QuoteWordsAtom if form == EnumVariantForm::QuoteWords => {
                variants.push(lower_atom_pair(term, segment)?);
            }
            _ => return Err(unsupported(term)),
        }
    }
    if variants.is_empty()
        && let (EnumVariantForm::Words, Some(text)) = (&form, empty_text)
    {
        return Ok((variants, EnumVariantForm::EmptyWords(text.into())));
    }
    Ok((variants, form))
}

/// A `:key(value)` / `:key` colonpair word of a `«…»` body.
fn lower_atom_pair(
    term: &RakuAstNode,
    atom: &RakuAstNode,
) -> Result<(String, Option<Expr>), RuntimeError> {
    let [field] = atom.fields.as_slice() else {
        return Err(unsupported(term));
    };
    let pair = super::lower::child_node(&field.value)?;
    match pair.class {
        RakuAstClass::ColonPairValue => {
            let key = super::lower::leaf_str(pair, "key")?;
            let value = lower_expr(named_child(pair, "value")?)?;
            Ok((key, Some(value)))
        }
        RakuAstClass::ColonPairTrue => match positional_leaf(pair)?.view() {
            ValueView::Str(key) => Ok((key.to_string(), None)),
            _ => Err(unsupported(term)),
        },
        _ => Err(unsupported(term)),
    }
}
