//! The expansion of a subscript's adverbs, `@a[0]:exists` / `%h<a>:delete` /
//! `@a[1]:kv` and their combinations.
//!
//! The compiler executes a subscript adverb as one of a few internal shapes:
//! an `Expr::Exists` node for `:exists`, a `DELETE-KEY` method call for
//! `:delete`, and a `__mutsu_subscript_adverb` builtin call for the value
//! adverbs `:k` / `:v` / `:kv` / `:p`. The builders of those shapes are here.
//!
//! The parser keeps the written colonpairs (`ParsedExpr::SubscriptAdverbs`),
//! and [`expand`] composes the builders for the whole adverb list, the way
//! RakuAST spells it (`Postcircumfix::*Index(colonpairs => …)`):
//! `rakuast::subscript_adverb::lower` calls it.
//!
//! An adverb is a `(key, value)` pair, `value` being `True` for `:key`,
//! `False` for `:!key` and the argument expression for `:key(…)`.

use super::{ExistsAdverb, Expr, SUBSCRIPT_ASSOCIATIVE_MARKER, SUBSCRIPT_POSITIONAL_MARKER};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The builtin a value adverb (`:k` / `:v` / `:kv` / `:p`) lowers to.
pub(crate) const SUBSCRIPT_ADVERB_FN: &str = "__mutsu_subscript_adverb";
pub(crate) const SUBSCRIPT_NAMED_ADVERBS_FN: &str = "__mutsu_subscript_named_adverbs";

/// The key of a colonpair expression (`:foo`, `:!foo`, `:foo(1)`, `:$foo`).
// Cost: O(1).
pub(crate) fn pair_key(pair: &Expr) -> Option<String> {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        ..
    } = pair
    else {
        return None;
    };
    match left.as_ref() {
        Expr::Literal(key) => match key.view() {
            ValueView::Str(key) => Some(key.to_string()),
            _ => None,
        },
        _ => None,
    }
}

// Cost: O(1).
pub(crate) fn is_builtin_name(name: &str) -> bool {
    matches!(name, "k" | "v" | "kv" | "p" | "exists" | "delete")
}

/// The descriptor an invalid subscript adverb reports for its index shape.
// Cost: O(1).
pub(crate) fn subscript_what(target: &Expr, index: &Expr) -> &'static str {
    if matches!(index, Expr::Literal(lit) if matches!(lit.view(), ValueView::Whatever)) {
        return if matches!(target, Expr::HashVar(_)) {
            "{} slice"
        } else {
            "zen slice"
        };
    }
    if matches!(index, Expr::Whatever) {
        return if matches!(target, Expr::HashVar(_)) {
            "{} slice"
        } else {
            "whatever slice"
        };
    }
    if matches!(target, Expr::HashVar(_)) {
        return "slice";
    }
    if let Expr::Binary { op, .. } = index
        && matches!(
            op,
            crate::token_kind::TokenKind::DotDot
                | crate::token_kind::TokenKind::DotDotCaret
                | crate::token_kind::TokenKind::CaretDotDot
                | crate::token_kind::TokenKind::CaretDotDotCaret
        )
    {
        return "slice";
    }
    match index {
        Expr::ArrayLiteral(items) if items.len() != 1 => "slice",
        Expr::ArrayVar(_) => "slice",
        _ => "element access",
    }
}

/// The CORE candidate call for a subscript carrying an unknown named adverb.
/// The parser and RakuAST lowering share this builder so both retain the same
/// zen-slice descriptor and source variable name.
// Cost: O(a + d), a = adverbs and d = dimensions.
pub(crate) fn named_adverb_call(subscript: &Expr, pairs: Vec<Expr>) -> Option<Expr> {
    let (target, index, shape) = match subscript {
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => {
            let zen = matches!(index.as_ref(), Expr::Literal(lit) if matches!(lit.view(), ValueView::Whatever))
                || (!is_positional && matches!(index.as_ref(), Expr::Whatever));
            let shape = match (is_positional, zen) {
                (true, false) => "[ ]",
                (true, true) => "[ ] zen",
                (false, false) => "{ }",
                (false, true) => "{ } zen",
            };
            (target.as_ref(), index.as_ref().clone(), shape)
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => {
            let shape = if *is_positional { "[; ]" } else { "{; }" };
            (
                target.as_ref(),
                Expr::ArrayLiteral(dimensions.clone()),
                shape,
            )
        }
        _ => return None,
    };
    let mut args = vec![
        target.clone(),
        index,
        Expr::Literal(Value::str(target.sigiled_var_name().unwrap_or_default())),
        Expr::Literal(Value::str(shape.to_string())),
    ];
    args.extend(pairs);
    Some(Expr::Call {
        name: Symbol::intern(SUBSCRIPT_NAMED_ADVERBS_FN),
        args,
        listop: false,
    })
}

/// The marker preceding a value adverb's runtime condition (`:k($ok)`) in a
/// [`SUBSCRIPT_ADVERB_FN`] call.
const ADVERB_COND_MARKER: &str = "__adverb_cond__";

/// The method a `:delete` adverb lowers to on a single-dimension subscript.
const DELETE_KEY: &str = "DELETE-KEY";

/// One adverb of a subscript: `:key` (`True`), `:!key` (`False`) or
/// `:key(value)`.
pub(crate) type Adverb = (String, Expr);

/// `TARGET:exists` / `TARGET:!exists` / `TARGET:exists(ARG)`, with the value
/// adverb that may follow it (`:exists:kv`).
pub(crate) fn exists_node(
    target: Expr,
    negated: bool,
    arg: Option<Box<Expr>>,
    adverb: ExistsAdverb,
) -> Expr {
    Expr::Exists {
        target: Box::new(target),
        negated,
        delete: false,
        arg,
        adverb,
    }
}

/// `TARGET:delete` on a single-dimension subscript, or on any other term.
pub(crate) fn delete_key(target: Expr) -> Expr {
    Expr::MethodCall {
        target: Box::new(target),
        name: Symbol::intern(DELETE_KEY),
        args: vec![],
        modifier: None,
        quoted: false,
        sugar: false,
    }
}

/// `:delete(COND)`: the deleting form when `COND` holds, the plain one
/// otherwise.
pub(crate) fn conditional_delete(cond: Expr, deleting: Expr, plain: Expr) -> Expr {
    Expr::Ternary {
        cond: Box::new(cond),
        then_expr: Box::new(deleting),
        else_expr: Box::new(plain),
    }
}

/// The `delete => True` flag a `:delete` adds to a value-adverb call
/// (`@a[0]:k:delete`).
pub(crate) fn delete_flag() -> Expr {
    Expr::Literal(Value::pair("delete".into(), Value::TRUE))
}

/// The deleting form of a single-dimension subscript's read, `read` being
/// the subscript itself, its `:exists` node or its value-adverb call.
pub(crate) fn deleting(read: &Expr) -> Expr {
    match read {
        Expr::Exists { .. } => apply_delete_to_exists(read.clone()),
        Expr::Call { name, args, .. } if *name == SUBSCRIPT_ADVERB_FN => {
            let mut args = args.clone();
            args.push(delete_flag());
            Expr::Call {
                name: *name,
                args,
                listop: false,
            }
        }
        // `@a[I;J]:k:delete`: the by-name `_dyn` builtin, which must mutate the
        // variable. Arguments go from [target, mode, dims...] to
        // [var, mode, True, dims...].
        Expr::Call { name, args, .. } if *name == Symbol::intern(MULTIDIM_ADVERB_FN) => {
            let [target, mode, dims @ ..] = args.as_slice() else {
                return delete_key(read.clone());
            };
            let mut dyn_args = vec![
                Expr::Literal(Value::str(multidim_target_var_name(target))),
                mode.clone(),
                Expr::Literal(Value::TRUE),
            ];
            dyn_args.extend(dims.iter().cloned());
            Expr::Call {
                name: Symbol::intern(MULTIDIM_ADVERB_DYN_FN),
                args: dyn_args,
                listop: false,
            }
        }
        _ => delete_key(read.clone()),
    }
}

/// Build a `__mutsu_subscript_adverb_error` call for X::Adverb. `target` is
/// the subscripted expression: its container descriptor names the report's
/// `.source` at run time (a `@a` parameter bound to `@n` reports `@n`), with
/// the spelled `source` as the fallback.
pub(crate) fn build_adverb_error_call(
    what: &str,
    source: &str,
    target: Option<&Expr>,
    nogo: &[String],
    unexpected: &[String],
) -> Expr {
    let mut args = vec![
        Expr::Literal(Value::str(what.to_string())),
        Expr::Literal(Value::str(source.to_string())),
        target.cloned().unwrap_or(Expr::Literal(Value::NIL)),
    ];
    for a in nogo {
        args.push(Expr::Literal(Value::str(format!("__nogo__{}", a))));
    }
    for u in unexpected {
        args.push(Expr::Literal(Value::str(format!("__unexpected__{}", u))));
    }
    Expr::Call {
        name: Symbol::intern("__mutsu_subscript_adverb_error"),
        args,
        listop: false,
    }
}

/// The variable a multi-dimension subscript's by-name builtins address.
pub(crate) fn multidim_target_var_name(target: &Expr) -> String {
    target.container_var_key().unwrap_or_default()
}

/// Apply a `:delete` adverb to an already-built `:exists` node, whichever
/// order the two were written in (`@a[0]:exists:delete` / `@a[0]:delete:exists`).
///
/// A single-dimension subscript just records the flag on the node; the compiler
/// emits the read and the delete together. A **multi**-dimensional subscript
/// (`@a[0;1;2]`) cannot: its `:exists` is lowered to a by-value builtin call,
/// and only the by-name `_dyn` builtin can mutate the variable — so lower to
/// that here, the same form the dynamic `:$delete` produces with a runtime-true
/// flag. `:!exists:delete` has no candidate at all there and is an X::Adverb.
pub(crate) fn apply_delete_to_exists(expr: Expr) -> Expr {
    let Expr::Exists {
        target,
        negated,
        arg,
        adverb,
        ..
    } = expr
    else {
        return expr;
    };
    let Expr::MultiDimIndex {
        target: mdt,
        dimensions,
        ..
    } = target.as_ref()
    else {
        return Expr::Exists {
            target,
            negated,
            delete: true,
            arg,
            adverb,
        };
    };
    let var_name = multidim_target_var_name(mdt);
    if negated {
        let error = build_adverb_error_call(
            "slice",
            &var_name,
            Some(mdt),
            &["!exists".to_string(), "delete".to_string()],
            &[],
        );
        return error;
    }
    let adverb_str = match adverb {
        ExistsAdverb::Kv => "kv",
        ExistsAdverb::P => "p",
        ExistsAdverb::InvalidK => "k",
        ExistsAdverb::InvalidV => "v",
        _ => "none",
    };
    let mut args = vec![
        Expr::Literal(Value::str(var_name)),
        Expr::Literal(Value::truth(negated)),
        Expr::Literal(Value::TRUE),
        Expr::Literal(Value::str(adverb_str.to_string())),
    ];
    args.extend(dimensions.iter().cloned());
    Expr::Call {
        name: Symbol::intern("__mutsu_multidim_exists_adverb_dyn"),
        args,
        listop: false,
    }
}

/// A value adverb (`:k` / `:v` / `:kv` / `:p`) on a subscript. `adverb` is the
/// mode the runtime reads: the name, `not-NAME` for `:!NAME`, `NAME0` for
/// `:NAME(0)`; `cond` is a runtime flag (`:k($ok)`).
pub(crate) fn subscript_adverb_expr_with_cond(
    expr: Expr,
    adverb: &'static str,
    cond: Option<Expr>,
) -> Expr {
    // `@a[...]:delete:k` (delete adverb BEFORE the value adverb): the leading
    // `:delete` already lowered the multislice to a `__mutsu_multidim_delete`
    // call `(var, dims...)`. Combine it with this value adverb (`:k`/`:kv`/`:p`/
    // `:v`) into the same delete+adverb `_dyn` form the reverse `:k:delete`
    // order produces, so both orders read the removed elements as
    // keys/kv-pairs/pairs/values. Args become [var, adverb, True(delete), dims...].
    if let Expr::Call { name, args, .. } = &expr
        && *name == Symbol::intern("__mutsu_multidim_delete")
        && !args.is_empty()
    {
        let mut new_args = vec![
            args[0].clone(),
            Expr::Literal(Value::str(adverb.to_string())),
            Expr::Literal(Value::TRUE),
        ];
        new_args.extend(args[1..].iter().cloned());
        if let Some(cond_expr) = cond {
            new_args.push(Expr::Literal(Value::str(ADVERB_COND_MARKER.to_string())));
            new_args.push(cond_expr);
        }
        return Expr::Call {
            name: Symbol::intern("__mutsu_multidim_subscript_adverb_dyn"),
            args: new_args,
            listop: false,
        };
    }
    // Handle MultiDimIndex: @a[0;0;0]:kv etc.
    if let Expr::MultiDimIndex {
        target, dimensions, ..
    } = expr
    {
        let mut args = vec![*target, Expr::Literal(Value::str(adverb.to_string()))];
        args.extend(dimensions);
        if let Some(cond_expr) = cond {
            args.push(Expr::Literal(Value::str(ADVERB_COND_MARKER.to_string())));
            args.push(cond_expr);
        }
        return Expr::Call {
            name: Symbol::intern("__mutsu_multidim_subscript_adverb"),
            args,
            listop: false,
        };
    }
    let Expr::Index {
        target,
        index,
        is_positional,
        ..
    } = expr
    else {
        return expr;
    };
    let var_name = match target.as_ref() {
        Expr::ArrayVar(name) => Expr::Literal(Value::str(format!("@{}", name))),
        Expr::HashVar(name) => Expr::Literal(Value::str(format!("%{}", name))),
        _ => Expr::Literal(Value::NIL),
    };
    // `%h{|@keys}:kv`: a slipped index is the flattened key list a plain
    // `%h{@keys}` slice already is. Left as `|EXPR` it would be spread across
    // the call's own arguments, shifting the adverb name out of place.
    let index = match *index {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Pipe,
            expr,
            ..
        } => *expr,
        other => other,
    };
    let mut args = vec![
        *target,
        index,
        Expr::Literal(Value::str(adverb.to_string())),
        var_name,
    ];
    // Record which bracket the subscript was written with, alongside the other
    // marker-tagged extras (`__adverb_cond__`, the `:delete` pair). The runtime
    // needs it to read `$c[0]:v` positionally — a value that is not Positional
    // is a one-element list holding itself — while leaving `$c<a>:v` a key
    // lookup.
    args.push(Expr::Literal(Value::str(
        if is_positional {
            SUBSCRIPT_POSITIONAL_MARKER
        } else {
            SUBSCRIPT_ASSOCIATIVE_MARKER
        }
        .to_string(),
    )));
    // When a dynamic condition is provided (e.g., `:k($ok)`), pass it as
    // a named Pair so the runtime can decide keep_missing at evaluation time.
    if let Some(cond_expr) = cond {
        args.push(Expr::Literal(Value::str(ADVERB_COND_MARKER.to_string())));
        args.push(cond_expr);
    }
    Expr::Call {
        name: Symbol::intern(SUBSCRIPT_ADVERB_FN),
        args,
        listop: false,
    }
}

/// The value adverbs a subscript accepts, as the `'static` mode names
/// [`subscript_adverb_expr_with_cond`] takes.
const VALUE_ADVERBS: [(&str, &str, &str); 4] = [
    ("k", "not-k", "k0"),
    ("v", "not-v", "v0"),
    ("kv", "not-kv", "kv0"),
    ("p", "not-p", "p0"),
];

fn is_bool_literal(expr: &Expr, want: bool) -> bool {
    matches!(expr, Expr::Literal(v) if matches!(v.view(), ValueView::Bool(b) if b == want))
}

fn is_int_literal(expr: &Expr, want: i64) -> bool {
    matches!(expr, Expr::Literal(v) if matches!(v.view(), ValueView::Int(i) if i == want))
}

/// A value adverb's runtime mode and condition, as the parser reads its
/// spelling: `:k(0)` is the `k0` mode, `:k(1)` plain `:k`.
fn value_adverb_mode(key: &str, value: &Expr) -> Option<(&'static str, Option<Expr>)> {
    let &(name, negated, zero) = VALUE_ADVERBS.iter().find(|(name, ..)| *name == key)?;
    Some(
        if is_bool_literal(value, true) || is_int_literal(value, 1) {
            (name, None)
        } else if is_bool_literal(value, false) {
            (negated, None)
        } else if is_int_literal(value, 0) {
            (zero, None)
        } else {
            (name, Some(value.clone()))
        },
    )
}

/// The `:exists:ADVERB` combination an `Expr::Exists` records.
fn exists_secondary(key: &str, value: &Expr) -> Option<ExistsAdverb> {
    let on = if is_bool_literal(value, true) {
        true
    } else if is_bool_literal(value, false) {
        false
    } else {
        return None;
    };
    Some(match (key, on) {
        ("kv", true) => ExistsAdverb::Kv,
        ("kv", false) => ExistsAdverb::NotKv,
        ("p", true) => ExistsAdverb::P,
        ("p", false) => ExistsAdverb::NotP,
        ("v", true) => ExistsAdverb::InvalidV,
        ("v", false) => ExistsAdverb::NotV,
        ("k", true) => ExistsAdverb::InvalidK,
        ("k", false) => ExistsAdverb::InvalidNotK,
        _ => return None,
    })
}

/// The by-name builtin a multi-dimensional `:delete` lowers to.
/// `six_e` is whether the source is `use v6.e` or later (see the parser's
/// `multidim_delete_fn`, which supplies the language version).
// Cost: O(1).
pub(crate) fn multidim_delete_fn(is_positional: bool, ndims: usize, six_e: bool) -> &'static str {
    if is_positional || ndims < 2 || six_e {
        MULTIDIM_DELETE
    } else {
        MULTIDIM_DELETE_ASSOC
    }
}

const MULTIDIM_ADVERB_FN: &str = "__mutsu_multidim_subscript_adverb";
const MULTIDIM_ADVERB_DYN_FN: &str = "__mutsu_multidim_subscript_adverb_dyn";
const MULTIDIM_DELETE: &str = "__mutsu_multidim_delete";
const MULTIDIM_DELETE_ASSOC: &str = "__mutsu_multidim_delete_assoc";

/// `@a[I;J]:delete` / `%h{I;J}:delete`: the by-name delete builtin over the
/// target's variable name and the dimensions.
// Cost: O(d), d = dimensions.
fn multidim_delete_call(read: &Expr, six_e: bool) -> Option<Expr> {
    let Expr::MultiDimIndex {
        target,
        dimensions,
        is_positional,
    } = read
    else {
        return None;
    };
    let mut args = vec![Expr::Literal(Value::str(multidim_target_var_name(target)))];
    args.extend(dimensions.iter().cloned());
    Some(Expr::Call {
        name: Symbol::intern(multidim_delete_fn(*is_positional, dimensions.len(), six_e)),
        args,
        listop: false,
    })
}

/// `SUBSCRIPT:ADVERB…`: the expression the parser builds for a subscript
/// carrying `adverbs`, or `None` for a combination this does not model (two
/// value adverbs, which is an X::Adverb, or a target that is not a
/// single-dimension subscript).
///
/// The adverbs are order-independent here, as they are to rakudo's
/// `postcircumfix` candidates: `:exists` takes at most one value adverb with
/// it, and `:delete` applies to whichever read the others build.
// Cost: O(n), n = AST nodes under `subscript` and the adverb values (cloned once).
pub(crate) fn expand(subscript: Expr, adverbs: &[Adverb], six_e: bool) -> Option<Expr> {
    match &subscript {
        Expr::Index { .. } => {}
        // A multi-dimensional subscript takes `:exists` and the value adverbs
        // here, and a bare `:delete`; `:delete` combined with another adverb
        // is a by-name builtin of its own the parser builds, which is not
        // modelled.
        Expr::MultiDimIndex { .. } => {}
        // `@a[]:exists...` keeps the zen slice itself (the parser only does so
        // when `:exists` / `:!exists` directly follows the empty subscript).
        Expr::ZenSlice(_) if adverbs.iter().any(|(key, _)| key == "exists") => {}
        _ => return None,
    }
    let mut exists = None;
    let mut delete = None;
    let mut values = Vec::new();
    for (key, value) in adverbs {
        let slot = match key.as_str() {
            "exists" => &mut exists,
            "delete" => &mut delete,
            _ => {
                values.push((key.as_str(), value));
                continue;
            }
        };
        if slot.replace(value).is_some() {
            return None;
        }
    }
    let read = if let Some(exists) = exists {
        let secondary = match values.as_slice() {
            [] => ExistsAdverb::None,
            [(key, value)] => exists_secondary(key, value)?,
            _ => return None,
        };
        let (negated, arg) = if is_bool_literal(exists, true) {
            (false, None)
        } else if is_bool_literal(exists, false) {
            (true, None)
        } else {
            (false, Some(Box::new(exists.clone())))
        };
        exists_node(subscript, negated, arg, secondary)
    } else {
        match values.as_slice() {
            [] => subscript,
            [(key, value)] => {
                let (mode, cond) = value_adverb_mode(key, value)?;
                subscript_adverb_expr_with_cond(subscript, mode, cond)
            }
            _ => return None,
        }
    };
    let Some(delete) = delete else {
        return Some(read);
    };
    if is_bool_literal(delete, false) {
        return Some(read);
    }
    if let Some(call) = multidim_delete_call(&read, six_e) {
        // A subscript form with no `:delete` candidate (`%h{1;2}:delete` before
        // 6.e) throws whatever the adverb's value is.
        let assoc = matches!(&call, Expr::Call { name, .. } if *name == MULTIDIM_DELETE_ASSOC);
        return Some(if assoc || is_bool_literal(delete, true) {
            call
        } else {
            conditional_delete(delete.clone(), call, read)
        });
    }
    let deleting = deleting(&read);
    Some(if is_bool_literal(delete, true) {
        deleting
    } else {
        conditional_delete(delete.clone(), deleting, read)
    })
}
