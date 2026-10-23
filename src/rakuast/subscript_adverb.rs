//! Subscript adverbs across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, an adverb on a subscript is a colonpair
//! of the postcircumfix itself, in source order:
//!
//! ```text
//! @a[0]:!exists     Postcircumfix::ArrayIndex(index => …,
//!                       colonpairs => (ColonPair::False("exists"),))
//! %h{"x"}:delete:v  Postcircumfix::HashIndex(index => …,
//!                       colonpairs => (ColonPair::True("delete"), ColonPair::True("v")))
//! @a[0]:exists(0)   … colonpairs => (ColonPair::Value(key => "exists",
//!                       value => Circumfix::Parentheses(…)))
//! ```
//!
//! The parser expands the adverbs into the shapes the compiler executes;
//! `ast::subscript_adverb` holds that expansion. The converter reads an
//! expansion back with `ast::subscript_adverb::adverbs`, and lowering rebuilds
//! it with `ast::subscript_adverb::expand`.

use super::convert::{
    angle_key_text, angle_subscript_node, colonpair_value_expr, convert_expr, leaf_field,
    splat_dimensions, subscript_dims_node, unsupported as convert_unsupported,
};
use super::lower::{list_field, lower_expr, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::subscript_adverb::{self, Adverb};
use crate::value::{RuntimeError, Value, ValueView};
use std::borrow::Cow;

mod dynamic_multidim;
mod hyperslice;

/// The postcircumfix node for `expr` when it is a subscript with adverbs;
/// `None` when it is not one.
// Cost: O(n), n = AST nodes under `expr`.
pub(super) fn convert(expr: &Expr) -> Option<Result<RakuAstNode, RuntimeError>> {
    if let Expr::Spelled(spelled) = expr
        && let crate::ast::spelled::Spelling::ConditionalMultidimDelete { source, condition } =
            &spelled.spelling
    {
        return Some(dynamic_multidim::convert_conditional(source, condition));
    }
    if let Some(node) = dynamic_multidim::convert_dynamic(expr) {
        return Some(node);
    }
    if let Expr::HyperSlice { target, adverb } = expr {
        let pair = colonpair(&(
            hyperslice::name(*adverb).to_string(),
            Expr::Literal(Value::TRUE),
        ));
        return Some(pair.and_then(|pair| {
            subscript_dims_node(target, &[Expr::HyperWhatever], false, None, vec![pair])
        }));
    }
    // The parser records an explicit `:exists(False)` value in `arg`. The
    // generic expansion inverse cannot infer that syntax from a false value:
    // it would rebuild `:!exists` and fail the structural identity check.
    if let Expr::Exists {
        target,
        negated: false,
        delete: false,
        arg: Some(arg),
        adverb: crate::ast::ExistsAdverb::None,
    } = expr
    {
        let pair = colonpair_value_expr(&Expr::Binary {
            left: Box::new(Expr::Literal(Value::str_from("exists"))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: arg.clone(),
            form: Default::default(),
        });
        return Some(pair.and_then(|pair| {
            let colonpairs = vec![Value::rakuast(Box::new(pair))];
            match target.as_ref() {
                Expr::Index {
                    target,
                    index,
                    is_positional: false,
                    spelling: crate::ast::IndexSpelling::Angle,
                } if angle_key_text(index).is_some() => {
                    angle_subscript_node(target, index, None, colonpairs)
                }
                Expr::Index {
                    target,
                    index,
                    is_positional,
                    ..
                } => subscript_dims_node(
                    target,
                    std::slice::from_ref(index.as_ref()),
                    *is_positional,
                    None,
                    colonpairs,
                ),
                _ => Err(convert_unsupported("explicit existence subscript")),
            }
        }));
    }
    if let Expr::Spelled(spelled) = expr
        && let crate::ast::spelled::Spelling::NamedSubscript { subscript, pairs }
        | crate::ast::spelled::Spelling::ConflictingSubscript { subscript, pairs } =
            &spelled.spelling
    {
        return Some(convert_named_subscript(subscript, pairs));
    }
    if let Expr::Call { name, args, .. } = expr
        && *name == subscript_adverb::SUBSCRIPT_NAMED_ADVERBS_FN
    {
        let [
            target,
            index,
            Expr::Literal(_source),
            Expr::Literal(shape),
            pairs @ ..,
        ] = args.as_slice()
        else {
            return Some(Err(convert_unsupported("named subscript adverb call")));
        };
        let ValueView::Str(shape) = shape.view() else {
            return Some(Err(convert_unsupported("named subscript shape")));
        };
        let shape = shape.as_str();
        let is_positional = shape.starts_with('[');
        let dimensions = match shape {
            "[; ]" | "{; }" => match index {
                Expr::ArrayLiteral(items) => items.as_slice(),
                _ => {
                    return Some(Err(convert_unsupported("named multidimensional subscript")));
                }
            },
            "[ ] zen" | "{ } zen" => &[],
            "[ ]" | "{ }" => std::slice::from_ref(index),
            _ => return Some(Err(convert_unsupported("named subscript shape"))),
        };
        return Some(
            pairs
                .iter()
                .map(|pair| convert_expr(pair).map(|node| Value::rakuast(Box::new(node))))
                .collect::<Result<Vec<_>, _>>()
                .and_then(|pairs| {
                    subscript_dims_node(target, dimensions, is_positional, None, pairs)
                }),
        );
    }
    if let Some((target, index, is_positional, mode, condition)) =
        subscript_adverb::explicit_value_condition(expr)
    {
        let pair = colonpair_value_expr(&Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(mode.to_string()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(condition.clone()),
            form: Default::default(),
        });
        return Some(pair.and_then(|pair| {
            let dims = if matches!(index, Expr::Literal(value) if matches!(value.view(), ValueView::Whatever)) {
                &[][..]
            } else {
                std::slice::from_ref(index)
            };
            subscript_dims_node(target, dims, is_positional, None, vec![Value::rakuast(Box::new(pair))])
        }));
    }
    if !matches!(
        expr,
        Expr::Exists { .. } | Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Ternary { .. }
    ) {
        return None;
    }
    let (subscript, adverbs) = subscript_adverb::adverbs(expr)?;
    let (target, dims, is_positional) = match &subscript {
        Expr::Index {
            target,
            index,
            is_positional: false,
            spelling: crate::ast::IndexSpelling::Angle,
        } if angle_key_text(index).is_some() => {
            return Some(
                adverbs
                    .iter()
                    .map(colonpair)
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|colonpairs| angle_subscript_node(target, index, None, colonpairs)),
            );
        }
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } if matches!(index.as_ref(), Expr::Literal(value) if matches!(value.view(), ValueView::Whatever)) =>
        {
            // The parser uses a literal Whatever for `[]:adverb`; a written
            // `[*]:adverb` uses Expr::Whatever. Rakudo keeps the former index
            // as an empty SemiList, even though both execute as a slice.
            (target, Cow::Borrowed(&[][..]), *is_positional)
        }
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => (
            target,
            Cow::Borrowed(std::slice::from_ref(&**index)),
            *is_positional,
        ),
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => (target, splat_dimensions(dimensions), *is_positional),
        // `@a[]:adverb` / `%h{}:adverb`: no dimension at all.
        Expr::ZenSlice(target) => (
            target,
            Cow::Borrowed(&[][..]),
            !matches!(&**target, Expr::HashVar(_)),
        ),
        _ => return None,
    };
    Some(
        adverbs
            .iter()
            .map(colonpair)
            .collect::<Result<Vec<_>, _>>()
            .and_then(|colonpairs| {
                subscript_dims_node(target, &dims, is_positional, None, colonpairs)
            }),
    )
}

// Cost: O(n), n = nodes under the subscript and its adverbs.
fn convert_named_subscript(subscript: &Expr, pairs: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let colonpairs = pairs
        .iter()
        .map(|pair| convert_expr(pair).map(|node| Value::rakuast(Box::new(node))))
        .collect::<Result<Vec<_>, _>>()?;
    match subscript {
        Expr::Index {
            target,
            index,
            is_positional: false,
            spelling: crate::ast::IndexSpelling::Angle,
        } if angle_key_text(index).is_some() => {
            angle_subscript_node(target, index, None, colonpairs)
        }
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => {
            let dims = if matches!(index.as_ref(), Expr::Literal(value) if matches!(value.view(), ValueView::Whatever))
            {
                &[][..]
            } else {
                std::slice::from_ref(index.as_ref())
            };
            subscript_dims_node(target, dims, *is_positional, None, colonpairs)
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => subscript_dims_node(
            target,
            &splat_dimensions(dimensions),
            *is_positional,
            None,
            colonpairs,
        ),
        _ => Err(convert_unsupported("named subscript source form")),
    }
}

/// `:key` / `:!key` / `:key(value)`.
fn colonpair((key, value): &Adverb) -> Result<Value, RuntimeError> {
    let flag = match value {
        Expr::Literal(v) => match v.view() {
            ValueView::Bool(true) => Some(RakuAstClass::ColonPairTrue),
            ValueView::Bool(false) => Some(RakuAstClass::ColonPairFalse),
            _ => None,
        },
        _ => None,
    };
    let node = match flag {
        Some(class) => RakuAstNode {
            origin: Default::default(),
            class,
            fields: vec![leaf_field(None, Value::str(key.clone()))],
        },
        None => colonpair_value_expr(&Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(key.clone()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(value.clone()),
            form: Default::default(),
        })?,
    };
    Ok(Value::rakuast(Box::new(node)))
}

/// `subscript` with the postcircumfix's `colonpairs` applied; `subscript`
/// itself when it has none.
// Cost: O(a), a = colonpairs (plus lowering their values).
pub(super) fn lower(subscript: Expr, postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let Ok(colonpairs) = list_field(postfix, "colonpairs") else {
        return Ok(subscript);
    };
    if colonpairs.is_empty() {
        return Ok(subscript);
    }
    let adverbs = colonpairs
        .iter()
        .map(|pair| {
            let ValueView::RakuAst(node) = pair.view() else {
                return Err(unsupported(postfix));
            };
            if !matches!(
                node.class,
                RakuAstClass::ColonPairTrue
                    | RakuAstClass::ColonPairFalse
                    | RakuAstClass::ColonPairValue
                    | RakuAstClass::ColonPairVariable
            ) {
                return Err(unsupported(node));
            }
            match lower_expr(node)? {
                Expr::Binary {
                    left,
                    op: crate::token_kind::TokenKind::FatArrow,
                    right,
                    ..
                } => match left.as_ref() {
                    Expr::Literal(key) => match key.view() {
                        ValueView::Str(key) => Ok((key.to_string(), *right)),
                        _ => Err(unsupported(node)),
                    },
                    _ => Err(unsupported(node)),
                },
                _ => Err(unsupported(node)),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    if let Expr::Index {
        target,
        index,
        is_positional: false,
        ..
    } = &subscript
        && matches!(index.as_ref(), Expr::HyperWhatever)
        && let [(key, Expr::Literal(value))] = adverbs.as_slice()
        && matches!(value.view(), ValueView::Bool(true))
        && let Some(adverb) = hyperslice::adverb(key)
    {
        return Ok(Expr::HyperSlice {
            target: target.clone(),
            adverb,
        });
    }
    if adverbs
        .iter()
        .any(|(key, _)| !subscript_adverb::is_builtin_name(key))
    {
        return subscript_adverb::named_adverb_call(
            &subscript,
            adverbs
                .iter()
                .map(|(key, value)| Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(key.clone()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(value.clone()),
                    form: Default::default(),
                })
                .collect(),
        )
        .ok_or_else(|| unsupported(postfix));
    }
    if let Some(expanded) = subscript_adverb::expand(
        subscript.clone(),
        &adverbs,
        crate::parser::current_language_version().starts_with("6.e"),
    ) {
        return Ok(expanded);
    }
    if adverbs
        .iter()
        .filter(|(key, _)| matches!(key.as_str(), "k" | "v" | "kv" | "p"))
        .count()
        >= 2
        && let Expr::Index { target, index, .. } = &subscript
    {
        let names = adverbs
            .iter()
            .map(|(key, value)| {
                if matches!(value, Expr::Literal(v) if matches!(v.view(), ValueView::Bool(false))) {
                    format!("!{key}")
                } else {
                    key.clone()
                }
            })
            .collect::<Vec<_>>();
        return Ok(subscript_adverb::build_adverb_error_call(
            subscript_adverb::subscript_what(target, index),
            &target.sigiled_var_name().unwrap_or_default(),
            Some(target),
            &names,
            &[],
        ));
    }
    Err(unsupported(postfix))
}

/// `RakuAST::Postcircumfix::ArrayIndex.new(index => …, colonpairs => (…),
/// assignee => …)` and its `HashIndex` / `LiteralHashIndex` siblings; `None`
/// for any other constructor.
// Cost: O(a), a = colonpairs.
pub(super) fn construct(class_name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = match class_name {
        "RakuAST::Postcircumfix::ArrayIndex" => RakuAstClass::PostcircumfixArrayIndex,
        "RakuAST::Postcircumfix::HashIndex" => RakuAstClass::PostcircumfixHashIndex,
        "RakuAST::Postcircumfix::LiteralHashIndex" => RakuAstClass::PostcircumfixLiteralHashIndex,
        _ => return None,
    };
    let constructor = format!("{class_name}.new");
    let build = || {
        let index = super::named_arg(args, "index")
            .ok_or_else(|| RuntimeError::new(format!("{constructor} requires `index`")))?;
        super::require_any_rakuast(&index, &constructor, "index")?;
        let mut fields = vec![RakuAstField {
            name: Some("index"),
            value: RakuAstFieldValue::Node(index),
        }];
        if let Some(colonpairs) = super::named_arg(args, "colonpairs") {
            let colonpairs = colonpairs
                .as_list_items()
                .map(<[Value]>::to_vec)
                .ok_or_else(|| {
                    RuntimeError::new(format!("{constructor} expects `colonpairs` to be a list"))
                })?;
            for pair in &colonpairs {
                let is_colonpair = matches!(pair.view(), ValueView::RakuAst(node) if matches!(
                    node.class,
                    RakuAstClass::ColonPairTrue
                        | RakuAstClass::ColonPairFalse
                        | RakuAstClass::ColonPairValue
                        | RakuAstClass::ColonPairVariable
                ));
                if !is_colonpair {
                    return Err(RuntimeError::new(format!(
                        "{constructor} expects `colonpairs` to hold RakuAST::ColonPair nodes"
                    )));
                }
            }
            if !colonpairs.is_empty() {
                fields.push(RakuAstField {
                    name: Some("colonpairs"),
                    value: RakuAstFieldValue::List(colonpairs),
                });
            }
        }
        if let Some(assignee) = super::named_arg(args, "assignee") {
            super::require_any_rakuast(&assignee, &constructor, "assignee")?;
            fields.push(RakuAstField {
                name: Some("assignee"),
                value: RakuAstFieldValue::Node(assignee),
            });
        }
        Ok(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class,
            fields,
        })))
    };
    Some(build())
}

/// Whether the postcircumfix's first colonpair is `:exists` / `:!exists`.
// Cost: O(1).
pub(super) fn leads_with_exists(postfix: &RakuAstNode) -> bool {
    let Ok(colonpairs) = list_field(postfix, "colonpairs") else {
        return false;
    };
    let Some(first) = colonpairs.first() else {
        return false;
    };
    let ValueView::RakuAst(node) = first.view() else {
        return false;
    };
    matches!(
        node.class,
        RakuAstClass::ColonPairTrue | RakuAstClass::ColonPairFalse
    ) && matches!(
        node.fields.first().map(|f| &f.value),
        Some(RakuAstFieldValue::Node(key)) if matches!(key.view(), ValueView::Str(k) if k.as_str() == "exists")
    )
}
