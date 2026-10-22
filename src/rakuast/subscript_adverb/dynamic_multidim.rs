//! Recover the source postcircumfix from the parser's dynamic 6.e
//! multidimensional delete expansion. Execution keeps that expansion; only
//! the RakuAST read direction needs the colonpair spelling.

use super::{colonpair, subscript_dims_node};
use crate::ast::BinaryForm;
use crate::ast::Expr;
use crate::ast::subscript_adverb;
use crate::rakuast::RakuAstNode;
use crate::rakuast::convert::convert_expr;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};
use std::hash::{Hash, Hasher};

// Cost: O(n), n = nodes in the multidimensional subscript and its adverbs.
pub(super) fn convert_dynamic(expr: &Expr) -> Option<Result<RakuAstNode, RuntimeError>> {
    if let Some(node) = convert_subscript(expr) {
        return Some(node);
    }
    let Expr::Ternary {
        cond,
        then_expr,
        else_expr,
    } = expr
    else {
        return None;
    };
    let Expr::Var(delete_name) = cond.as_ref() else {
        return None;
    };
    let Expr::Call { name, args, .. } = then_expr.as_ref() else {
        return None;
    };
    if *name != Symbol::intern("__mutsu_multidim_exists_adverb_dyn") {
        return None;
    }
    let [
        Expr::Literal(var),
        Expr::Literal(negated),
        Expr::Var(flag),
        Expr::Literal(secondary),
        call_dims @ ..,
    ] = args.as_slice()
    else {
        return None;
    };
    let (ValueView::Str(var), ValueView::Bool(negated), ValueView::Str(secondary)) =
        (var.view(), negated.view(), secondary.view())
    else {
        return None;
    };
    let Expr::Exists {
        target: subscript,
        negated: read_negated,
        delete: false,
        arg: None,
        adverb,
    } = else_expr.as_ref()
    else {
        return None;
    };
    let Expr::MultiDimIndex {
        target,
        dimensions,
        is_positional,
    } = subscript.as_ref()
    else {
        return None;
    };
    if flag != delete_name
        || negated != *read_negated
        || var.as_str() != subscript_adverb::multidim_target_var_name(target)
        || secondary.as_str() != secondary_name(*adverb)
        || structural_hash(call_dims) != structural_hash(dimensions.as_slice())
    {
        return None;
    }
    let (_, adverbs) = subscript_adverb::adverbs(else_expr)?;
    Some((|| {
        let mut pairs = adverbs
            .iter()
            .map(colonpair)
            .collect::<Result<Vec<_>, _>>()?;
        let delete_pair = subscript_adverb::source_pair(
            "delete",
            Expr::Var(delete_name.clone()),
            BinaryForm::ColonPairVariable,
        );
        pairs.push(Value::rakuast(Box::new(convert_expr(&delete_pair)?)));
        subscript_dims_node(target, dimensions, *is_positional, None, pairs)
    })())
}

fn convert_subscript(expr: &Expr) -> Option<Result<RakuAstNode, RuntimeError>> {
    let Expr::Ternary {
        cond,
        then_expr,
        else_expr,
    } = expr
    else {
        return None;
    };
    let Expr::Var(delete_name) = cond.as_ref() else {
        return None;
    };
    let Expr::Call { name, args, .. } = then_expr.as_ref() else {
        return None;
    };
    let (var, mode, flag, call_dims) = if *name == Symbol::intern("__mutsu_multidim_delete") {
        let [Expr::Literal(var), dims @ ..] = args.as_slice() else {
            return None;
        };
        (var, None, None, dims)
    } else if *name == Symbol::intern("__mutsu_multidim_subscript_adverb_dyn") {
        let [Expr::Literal(var), mode, Expr::Var(flag), dims @ ..] = args.as_slice() else {
            return None;
        };
        (var, Some(mode), Some(flag), dims)
    } else {
        return None;
    };
    let ValueView::Str(var) = var.view() else {
        return None;
    };
    if flag.is_some_and(|flag| flag != delete_name) {
        return None;
    }
    let (subscript, adverbs) = if mode.is_some() {
        let Expr::Call { name, args, .. } = else_expr.as_ref() else {
            return None;
        };
        if *name != Symbol::intern("__mutsu_multidim_subscript_adverb")
            || args.get(1).map(structural_hash) != mode.map(structural_hash)
        {
            return None;
        }
        subscript_adverb::adverbs(else_expr)?
    } else {
        (else_expr.as_ref().clone(), Vec::new())
    };
    let Expr::MultiDimIndex {
        target,
        dimensions,
        is_positional,
    } = &subscript
    else {
        return None;
    };
    if var.as_str() != subscript_adverb::multidim_target_var_name(target)
        || structural_hash(call_dims) != structural_hash(dimensions.as_slice())
    {
        return None;
    }
    Some((|| {
        let mut pairs = adverbs
            .iter()
            .map(colonpair)
            .collect::<Result<Vec<_>, _>>()?;
        let delete_pair = subscript_adverb::source_pair(
            "delete",
            Expr::Var(delete_name.clone()),
            BinaryForm::ColonPairVariable,
        );
        pairs.push(Value::rakuast(Box::new(convert_expr(&delete_pair)?)));
        subscript_dims_node(target, dimensions, *is_positional, None, pairs)
    })())
}

fn structural_hash<T: Hash + ?Sized>(value: &T) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn secondary_name(adverb: crate::ast::ExistsAdverb) -> &'static str {
    use crate::ast::ExistsAdverb;
    match adverb {
        ExistsAdverb::None => "none",
        ExistsAdverb::Kv => "kv",
        ExistsAdverb::P => "p",
        ExistsAdverb::InvalidK => "k",
        ExistsAdverb::InvalidV => "v",
        _ => "invalid",
    }
}
