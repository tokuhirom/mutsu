//! The expansion of a subscript's adverbs, `@a[0]:exists` / `%h<a>:delete` /
//! `@a[1]:kv` and their combinations.
//!
//! The compiler executes a subscript adverb as one of a few internal shapes:
//! an `Expr::Exists` node for `:exists`, a `DELETE-KEY` method call for
//! `:delete`, and a `__mutsu_subscript_adverb` builtin call for the value
//! adverbs `:k` / `:v` / `:kv` / `:p`. The builders of those shapes are here,
//! and the parser calls them as it reads each adverb.
//!
//! [`expand`] composes the builders for a whole adverb list, the way RakuAST
//! spells it (`Postcircumfix::*Index(colonpairs => …)`): `rakuast::lower` calls
//! it (ADR-10723 Stage 1). [`adverbs`] goes the other way for the converter:
//! it reads an expression back as a subscript and its adverbs, and accepts it
//! only when [`expand`] rebuilds exactly that expression, so nothing is
//! reverse-engineered from a shape some other desugaring happens to share.
//!
//! An adverb is a `(key, value)` pair, `value` being `True` for `:key`,
//! `False` for `:!key` and the argument expression for `:key(…)`.

use std::hash::{Hash, Hasher};

use super::{ExistsAdverb, Expr, SUBSCRIPT_ASSOCIATIVE_MARKER, SUBSCRIPT_POSITIONAL_MARKER};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The builtin a value adverb (`:k` / `:v` / `:kv` / `:p`) lowers to.
pub(crate) const SUBSCRIPT_ADVERB_FN: &str = "__mutsu_subscript_adverb";

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
        Expr::Call { name, args } if *name == SUBSCRIPT_ADVERB_FN => {
            let mut args = args.clone();
            args.push(delete_flag());
            Expr::Call { name: *name, args }
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
        return build_adverb_error_call(
            "slice",
            &var_name,
            Some(mdt),
            &["!exists".to_string(), "delete".to_string()],
            &[],
        );
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
    if let Expr::Call { name, args } = &expr
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
        };
    }
    let Expr::Index {
        target,
        index,
        is_positional,
    } = expr
    else {
        return expr;
    };
    let var_name = match target.as_ref() {
        Expr::ArrayVar(name) => Expr::Literal(Value::str(format!("@{}", name))),
        Expr::HashVar(name) => Expr::Literal(Value::str(format!("%{}", name))),
        _ => Expr::Literal(Value::NIL),
    };
    let mut args = vec![
        *target,
        *index,
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

/// The adverb list of a `:exists:ADVERB` combination; the inverse of
/// [`exists_secondary`].
fn exists_secondary_adverb(adverb: ExistsAdverb) -> Option<Adverb> {
    let (key, on) = match adverb {
        ExistsAdverb::None => return None,
        ExistsAdverb::Kv => ("kv", true),
        ExistsAdverb::NotKv => ("kv", false),
        ExistsAdverb::P => ("p", true),
        ExistsAdverb::NotP => ("p", false),
        ExistsAdverb::InvalidV => ("v", true),
        ExistsAdverb::NotV => ("v", false),
        ExistsAdverb::InvalidK => ("k", true),
        ExistsAdverb::InvalidNotK => ("k", false),
    };
    Some((key.to_string(), Expr::Literal(Value::truth(on))))
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
pub(crate) fn expand(subscript: Expr, adverbs: &[Adverb]) -> Option<Expr> {
    match &subscript {
        Expr::Index { .. } => {}
        // A multi-dimensional subscript takes `:exists` and the value adverbs
        // here; its `:delete` is a by-name builtin of its own the parser
        // builds, which is not modelled.
        Expr::MultiDimIndex { .. } if !adverbs.iter().any(|(key, _)| key == "delete") => {}
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
    let deleting = deleting(&read);
    Some(if is_bool_literal(delete, true) {
        deleting
    } else {
        conditional_delete(delete.clone(), deleting, read)
    })
}

/// The subscript and adverbs `expr` is the [`expand`]ed form of, or `None`
/// when it is not one. Accepted only when [`expand`] rebuilds `expr` exactly.
// Cost: O(n), n = AST nodes under `expr` (the expansion is rebuilt and hashed).
pub(crate) fn adverbs(expr: &Expr) -> Option<(Expr, Vec<Adverb>)> {
    let (subscript, adverbs) = read_back(expr)?;
    let rebuilt = expand(subscript.clone(), &adverbs)?;
    (structural_hash(&rebuilt) == structural_hash(expr)).then_some((subscript, adverbs))
}

/// A candidate reading of `expr` for [`adverbs`] to verify. A `:delete(COND)`
/// is a ternary whose else-branch is the read the other adverbs build.
fn read_back(expr: &Expr) -> Option<(Expr, Vec<Adverb>)> {
    let Expr::Ternary {
        cond, else_expr, ..
    } = expr
    else {
        return read_back_read(expr);
    };
    let (subscript, mut adverbs) = match else_expr.as_ref() {
        index @ Expr::Index { .. } => (index.clone(), Vec::new()),
        read => read_back_read(read)?,
    };
    if adverbs.iter().any(|(key, _)| key == "delete") {
        return None;
    }
    adverbs.push(("delete".to_string(), cond.as_ref().clone()));
    Some((subscript, adverbs))
}

/// A candidate reading of a read that is not a `:delete(COND)` ternary.
fn read_back_read(expr: &Expr) -> Option<(Expr, Vec<Adverb>)> {
    let truth = |on: bool| Expr::Literal(Value::truth(on));
    match expr {
        Expr::Exists {
            target,
            negated,
            delete,
            arg,
            adverb,
        } => {
            let exists = match (negated, arg) {
                (false, None) => truth(true),
                (true, None) => truth(false),
                (false, Some(arg)) => arg.as_ref().clone(),
                (true, Some(_)) => return None,
            };
            let mut adverbs = vec![("exists".to_string(), exists)];
            adverbs.extend(exists_secondary_adverb(*adverb));
            if *delete {
                adverbs.push(("delete".to_string(), truth(true)));
            }
            Some((target.as_ref().clone(), adverbs))
        }
        Expr::Call { name, args } if *name == SUBSCRIPT_ADVERB_FN => {
            let [target, index, mode, _var_name, marker, extras @ ..] = args.as_slice() else {
                return None;
            };
            let is_positional = match marker {
                Expr::Literal(v) => match v.view() {
                    ValueView::Str(s) if s.as_str() == SUBSCRIPT_POSITIONAL_MARKER => true,
                    ValueView::Str(s) if s.as_str() == SUBSCRIPT_ASSOCIATIVE_MARKER => false,
                    _ => return None,
                },
                _ => return None,
            };
            let Expr::Literal(mode) = mode else {
                return None;
            };
            let ValueView::Str(mode) = mode.view() else {
                return None;
            };
            let (cond, extras) = match extras {
                [Expr::Literal(m), cond, rest @ ..] if matches!(m.view(), ValueView::Str(s) if s.as_str() == ADVERB_COND_MARKER) => {
                    (Some(cond.clone()), rest)
                }
                _ => (None, extras),
            };
            let delete = match extras {
                [] => false,
                [_] => true,
                _ => return None,
            };
            let (key, value) = VALUE_ADVERBS.iter().find_map(|&(name, negated, zero)| {
                let mode = mode.as_str();
                if mode == name {
                    Some((name, cond.clone().unwrap_or_else(|| truth(true))))
                } else if mode == negated {
                    Some((name, truth(false)))
                } else if mode == zero {
                    Some((name, Expr::Literal(Value::int(0))))
                } else {
                    None
                }
            })?;
            let mut adverbs = vec![(key.to_string(), value)];
            if delete {
                adverbs.push(("delete".to_string(), truth(true)));
            }
            let subscript = Expr::Index {
                target: Box::new(target.clone()),
                index: Box::new(index.clone()),
                is_positional,
            };
            Some((subscript, adverbs))
        }
        Expr::MethodCall {
            target, name, args, ..
        } if *name == DELETE_KEY && args.is_empty() && matches!(**target, Expr::Index { .. }) => {
            Some((
                target.as_ref().clone(),
                vec![("delete".to_string(), truth(true))],
            ))
        }
        _ => None,
    }
}

/// A structural identity of `expr` through the derived `Hash` impls.
fn structural_hash(expr: &Expr) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    expr.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn index(positional: bool) -> Expr {
        Expr::Index {
            target: Box::new(if positional {
                Expr::ArrayVar("a".to_string())
            } else {
                Expr::HashVar("h".to_string())
            }),
            index: Box::new(Expr::Literal(Value::int(0))),
            is_positional: positional,
        }
    }

    fn adverb(key: &str, value: Expr) -> Adverb {
        (key.to_string(), value)
    }

    fn on(b: bool) -> Expr {
        Expr::Literal(Value::truth(b))
    }

    #[test]
    fn every_expansion_reads_back_as_its_adverbs() {
        let cond = Expr::Var("c".to_string());
        let lists = vec![
            vec![adverb("exists", on(true))],
            vec![adverb("exists", on(false))],
            vec![adverb("exists", cond.clone())],
            vec![adverb("exists", on(true)), adverb("kv", on(true))],
            vec![adverb("exists", on(true)), adverb("delete", on(true))],
            vec![adverb("exists", on(true)), adverb("delete", cond.clone())],
            vec![adverb("delete", on(true))],
            vec![adverb("delete", cond.clone())],
            vec![adverb("k", on(true))],
            vec![adverb("kv", on(false))],
            vec![adverb("p", Expr::Literal(Value::int(0)))],
            vec![adverb("v", cond.clone())],
            vec![adverb("v", on(true)), adverb("delete", on(true))],
            vec![adverb("p", on(true)), adverb("delete", cond.clone())],
        ];
        for positional in [true, false] {
            for list in &lists {
                let expanded = expand(index(positional), list).expect("expands");
                let (subscript, read) = adverbs(&expanded).expect("reads back");
                assert_eq!(
                    structural_hash(&subscript),
                    structural_hash(&index(positional))
                );
                assert_eq!(read.len(), list.len(), "{list:?}");
            }
        }
    }

    #[test]
    fn a_conflict_or_a_foreign_shape_is_not_an_expansion() {
        let two_values = [adverb("k", on(true)), adverb("v", on(true))];
        assert!(expand(index(true), &two_values).is_none());
        assert!(expand(Expr::ArrayVar("a".to_string()), &[adverb("k", on(true))]).is_none());
        // A ternary over a subscript that is not a conditional delete.
        let foreign = Expr::Ternary {
            cond: Box::new(Expr::Var("c".to_string())),
            then_expr: Box::new(Expr::Literal(Value::int(1))),
            else_expr: Box::new(index(true)),
        };
        assert!(adverbs(&foreign).is_none());
    }
}
