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
//! The parser keeps the colonpairs as written (`ParsedExpr::SubscriptAdverbs`);
//! [`postcircumfix_node`] is their view and [`lower`] the one lowering, which expands them
//! into the shapes the compiler executes (`ast::subscript_adverb::expand`).

use super::convert::{
    angle_key_text, angle_subscript_node, convert_expr, leaf_field, splat_dimensions,
    subscript_dims_node, unsupported as convert_unsupported,
};
use super::lower::{list_field, lower_expr, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::subscript_adverb;
use crate::value::{RuntimeError, Value, ValueView};

mod hyperslice;

/// `%h{**}:k`: the hash hyperslice the parser keeps as `Expr::HyperSlice`.
// Cost: O(n), n = nodes under the target.
pub(super) fn convert_hyper_slice(expr: &Expr) -> Option<Result<RakuAstNode, RuntimeError>> {
    let Expr::HyperSlice { target, adverb } = expr else {
        return None;
    };
    let pair = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairTrue,
        fields: vec![leaf_field(
            None,
            Value::str(hyperslice::name(*adverb).to_string()),
        )],
    };
    Some(subscript_dims_node(
        target,
        &[Expr::HyperWhatever],
        false,
        None,
        vec![Value::rakuast(Box::new(pair))],
    ))
}

/// The postcircumfix node of `subscript` carrying the colonpairs `pairs`, as
/// the parser kept them (`ParsedExpr::SubscriptAdverbs`).
// Cost: O(n), n = nodes under the subscript and its adverbs.
pub(super) fn postcircumfix_node(
    subscript: &Expr,
    pairs: &[Expr],
) -> Result<RakuAstNode, RuntimeError> {
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
            // The parser writes `[]:adverb` with a literal Whatever index;
            // a written `[*]:adverb` is `Expr::Whatever`. Rakudo keeps the
            // former as an empty SemiList, though both execute as a slice.
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
        _ => Err(convert_unsupported("subscript adverb source form")),
    }
}

/// A subscript (`Expr::Index` / `Expr::MultiDimIndex`) carrying the colonpairs
/// `pairs`, at least one of them not a built-in adverb: Raku passes them all as
/// named arguments to the subscript operator.
///
/// With a user `postcircumfix:<...>` candidate in scope the call goes to the
/// operator itself, so multi-dispatch picks the candidate exactly as an
/// explicit call would. Otherwise it goes to the CORE candidate model
/// (`subscript_adverb::named_adverb_call`), which raises what rakudo's
/// candidates raise for these arguments. `None` for any other expression.
// Cost: O(n), n = nodes under the subscript and its adverbs.
fn named_subscript(subscript: &Expr, pairs: Vec<Expr>) -> Option<Expr> {
    let (target, index, op_name) = match subscript {
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => {
            let op = if *is_positional {
                "postcircumfix:<[ ]>"
            } else {
                "postcircumfix:<{ }>"
            };
            (target.as_ref(), index.as_ref().clone(), op)
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => {
            let op = if *is_positional {
                "postcircumfix:<[; ]>"
            } else {
                "postcircumfix:<{; }>"
            };
            (target.as_ref(), Expr::ArrayLiteral(dimensions.clone()), op)
        }
        _ => return None,
    };
    if crate::parser::is_user_declared_sub(op_name) {
        let mut args = vec![target.clone(), index];
        args.extend(pairs);
        return Some(Expr::Call {
            name: crate::symbol::Symbol::intern(op_name),
            args,
            listop: false,
        });
    }
    subscript_adverb::named_adverb_call(subscript, pairs)
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
        return named_subscript(
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
        && let Some((target, what)) = match &subscript {
            Expr::Index { target, index, .. } => {
                Some((target, subscript_adverb::subscript_what(target, index)))
            }
            Expr::MultiDimIndex { target, .. } => Some((target, "slice")),
            _ => None,
        }
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
            what,
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
