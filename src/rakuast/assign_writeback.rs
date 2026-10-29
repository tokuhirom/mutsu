//! The writeback constructors of an assignment (ADR-15334 slice 1): the
//! rw-accessor writeback for a method-call target, the named-routine and
//! callable writebacks, and the subscript-adverb target. The spelling the
//! compiler dispatches on (`__mutsu_assign_*_lvalue`) exists here only.

use super::assign_target::assign_to_target_expr;
use crate::ast::Expr;
use crate::parser::wrap_finished_expr;
use crate::symbol::Symbol;
use crate::value::Value;

pub(crate) fn method_lvalue_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    method_lvalue_assign_expr_with_intent(
        target,
        target_var_name,
        method_name,
        method_args,
        value,
        false,
    )
}

pub(crate) fn method_lvalue_roundtrip_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    method_lvalue_assign_expr_with_intent(
        target,
        target_var_name,
        method_name,
        method_args,
        value,
        true,
    )
}

fn method_lvalue_assign_expr_with_intent(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
    preserve_hash_entries: bool,
) -> Expr {
    // Assignment RHS parsing normally plants a WhateverCurry marker, but the
    // method-lvalue lowering is reached after the lvalue parser has consumed
    // the accessor call. Preserve that language-level value here so
    // `$obj.rw-accessor = * + 1` stores a WhateverCode rather than evaluating
    // `Whatever + 1` immediately.
    let value = force_positional_pairs(wrap_finished_expr(value));
    // `$(EXPR) = value` lowers `$(EXPR)` to `EXPR.item`, but the item
    // contextualizer is transparent as an lvalue: it names the same container as
    // `EXPR`. Assign straight through to `EXPR` (`$(@a[0]) = ...` writes `@a[0]`)
    // instead of dispatching a `.item` method-lvalue, which has no writable slot
    // and threw X::Assignment::RO.
    if method_name == "item" && method_args.is_empty() {
        return assign_to_target_expr(target, value);
    }
    // A Slip RHS is the value assigned to the accessor, not an interpolation
    // into this generated helper call. Group it so the call compiler preserves
    // the Slip itself; putting it in an ArrayLiteral would flatten it into a
    // List before the target's `$`/`@`/`%` assignment semantics can apply.
    let value = match value {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Pipe,
            ..
        } => Expr::Grouped(Box::new(value)),
        other => other,
    };
    method_lvalue_call(
        target,
        target_var_name,
        method_name,
        method_args,
        value,
        preserve_hash_entries,
    )
}

/// The writeback call itself, for a caller that has already prepared `value`
/// (`$x .= foo` over a set operator builds its combined value first).
// Cost: O(n), n = size of the target, arguments and value.
pub(crate) fn method_lvalue_call(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
    preserve_hash_entries: bool,
) -> Expr {
    let mut args = vec![
        target,
        Expr::Literal(Value::str(method_name)),
        Expr::ArrayLiteral(method_args),
        value,
    ];
    args.push(match target_var_name {
        Some(name) => Expr::Literal(Value::str(name)),
        None => Expr::Literal(Value::NIL),
    });
    if preserve_hash_entries {
        args.push(Expr::Literal(Value::TRUE));
    }
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_method_lvalue"),
        args,
        listop: false,
    }
}

/// The pieces of a method-lvalue writeback call [`method_lvalue_call`] built,
/// or `None` for any other expression. `temp $obj.method = value` saves and
/// restores through the same accessor, so it reads the call back apart.
// Cost: O(1).
pub(crate) fn method_lvalue_call_parts(expr: &Expr) -> Option<MethodLvalueParts<'_>> {
    let Expr::Call { name, args, .. } = expr else {
        return None;
    };
    if name != "__mutsu_assign_method_lvalue" || args.len() < 4 {
        return None;
    }
    let Expr::Literal(method_name) = &args[1] else {
        return None;
    };
    let Expr::ArrayLiteral(method_args) = &args[2] else {
        return None;
    };
    Some(MethodLvalueParts {
        invocant: &args[0],
        method_name: method_name.as_str()?,
        method_args,
        value: &args[3],
    })
}

/// See [`method_lvalue_call_parts`].
pub(crate) struct MethodLvalueParts<'a> {
    pub(crate) invocant: &'a Expr,
    pub(crate) method_name: &'a str,
    pub(crate) method_args: &'a [Expr],
    pub(crate) value: &'a Expr,
}

/// A method-lvalue writeback is an internal positional call, not a user call
/// whose RHS pairs are named arguments. Preserve hash-assignment pairs as data
/// while the generated `__mutsu_assign_method_lvalue` call is compiled.
fn force_positional_pairs(expr: Expr) -> Expr {
    match expr {
        Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
            ..
        } => Expr::PositionalPair(Box::new(Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
            form: Default::default(),
        })),
        Expr::ArrayLiteral(items) => {
            Expr::ArrayLiteral(items.into_iter().map(force_positional_pairs).collect())
        }
        Expr::BracketArray(items, is_flat) => Expr::BracketArray(
            items.into_iter().map(force_positional_pairs).collect(),
            is_flat,
        ),
        other => other,
    }
}

/// Lower an assignment whose lvalue is an indirect method call
/// (`$o."$name"() = value`) to the same rw-accessor writeback as a literal
/// method-call lvalue, but with the method name computed at runtime. The name
/// expression is passed as `__mutsu_assign_method_lvalue`'s method-name argument
/// (evaluated eagerly to a string), so `$o."x"() = v` writes through the `x`
/// accessor exactly like `$o.x = v`. A private indirect call (`$o!"$name"()`,
/// `modifier: Some('!')`) prefixes `!` so it dispatches to the private accessor.
pub(crate) fn dynamic_method_lvalue_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    name_expr: Expr,
    modifier: Option<char>,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    let value = match value {
        Expr::Unary {
            op: crate::token_kind::TokenKind::Pipe,
            ..
        } => Expr::Grouped(Box::new(value)),
        other => other,
    };
    let name_expr = if modifier == Some('!') {
        Expr::Binary {
            left: Box::new(Expr::Literal(Value::str_from("!"))),
            op: crate::token_kind::TokenKind::Tilde,
            right: Box::new(name_expr),
            form: Default::default(),
        }
    } else {
        name_expr
    };
    let args = vec![
        target,
        name_expr,
        Expr::ArrayLiteral(method_args),
        value,
        match target_var_name {
            Some(name) => Expr::Literal(Value::str(name)),
            None => Expr::Literal(Value::NIL),
        },
    ];
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_method_lvalue"),
        args,
        listop: false,
    }
}

pub(crate) fn named_sub_lvalue_assign_expr(
    name: String,
    call_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    // An internal `__mutsu_*` call (`@a[1]:v` lowers to
    // `__mutsu_subscript_adverb(...)`) is a builtin, never a user routine, so
    // resolving it by name as an rw sub can only fail with "Unknown call".
    // Evaluate it and assign to the value it yields instead, as for any other
    // non-routine expression: a container is written through, anything else
    // dies with X::Assignment::RO naming the value (#9811).
    //
    // An `nqp::` op is no routine either: `nqp::atposref_i(@a, 0) = 5` runs the
    // op and writes through the container it returns (#11451).
    if name.starts_with("__mutsu_") || name.starts_with("nqp::") {
        return callable_lvalue_assign_expr(
            Expr::Call {
                name: Symbol::intern(&name),
                args: call_args,
                listop: false,
            },
            Vec::new(),
            value,
        );
    }
    // The CORE subscript routines called by name (`postcircumfix:<{; }>(%h,
    // @keys) = v`) assign exactly like the syntax they back. The `{; }` /
    // `[; ]` forms take their dimensions as one list, which is the
    // single-dimension `||` shape of `MultiDimIndexAssign`.
    if call_args.len() == 2 {
        let subscript = match name.as_str() {
            "postcircumfix:<[ ]>" => Some((false, true)),
            "postcircumfix:<{ }>" => Some((false, false)),
            "postcircumfix:<[; ]>" => Some((true, true)),
            "postcircumfix:<{; }>" => Some((true, false)),
            _ => None,
        };
        if let Some((multidim, is_positional)) = subscript {
            let mut it = call_args.into_iter();
            let target = Box::new(it.next().expect("two args"));
            let index = it.next().expect("two args");
            return if multidim {
                Expr::MultiDimIndexAssign {
                    target,
                    dimensions: vec![index],
                    value: Box::new(value),
                    is_positional,
                }
            } else {
                Expr::IndexAssign {
                    target,
                    index: Box::new(index),
                    value: Box::new(value),
                    is_positional,
                    spelling: Default::default(),
                }
            };
        }
    }
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
        args: vec![
            Expr::Literal(Value::str(name)),
            Expr::ArrayLiteral(call_args),
            value,
        ],
        listop: false,
    }
}

pub(crate) fn callable_lvalue_assign_expr(target: Expr, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
        args: vec![target, Expr::ArrayLiteral(call_args), value],
        listop: false,
    }
}

pub(crate) fn subscript_adverb_lvalue_assign_expr(lhs: Expr, rhs: Expr) -> Option<Expr> {
    fn subscript_parts(expr: &Expr) -> Option<(Expr, Expr, String)> {
        let Expr::Call { name, args, .. } = expr else {
            return None;
        };
        if name != "__mutsu_subscript_adverb" || args.len() < 3 {
            return None;
        }
        let Expr::Literal(lit) = &args[2] else {
            return None;
        };
        let mode = lit.as_str()?;
        Some((args[0].clone(), args[1].clone(), mode.to_string()))
    }

    match lhs {
        Expr::MethodCall {
            target, name, args, ..
        } if name == "value" && args.is_empty() => {
            if let Some((base_target, base_index, mode)) = subscript_parts(target.as_ref())
                && (mode == "p" || mode == "not-p")
            {
                return Some(Expr::IndexAssign {
                    target: Box::new(base_target),
                    index: Box::new(base_index),
                    value: Box::new(rhs),
                    is_positional: true,
                    spelling: Default::default(),
                });
            }
            None
        }
        // ADR-0036 slice 2: the `:kv` rewrite that used to live here
        // (`(@a[0]:kv)[1] = rhs` -> a plain `@a[0] = rhs` element assign) is
        // deleted — `:kv`'s list value now carries a live element container,
        // so the generic index-assign path's `ContainerRef` write-through
        // reaches the same backing element without a special case.
        _ => None,
    }
}
