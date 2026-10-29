//! List-infix normalization of a parenthesized list or a listop argument list:
//! the columnar lift of `Z` / `X` meta-ops and `minmax` across the comma, the
//! Whatever-curry of a standalone `X` / `Z`, chained zip metas and sequence
//! waypoints. One implementation for the parser and for `rakuast::lower`.

use crate::ast::Expr;
use crate::parser::{contains_whatever, contains_xx_with_bare_whatever};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Finalize a flat (comma-separated) parenthesized list.
/// Lifts embedded meta-ops (X/Z), minmax, and handles Whatever-curry.
pub(crate) fn finalize_paren_list(items: Vec<Expr>) -> Expr {
    let lifted = lift_meta_ops_in_paren_list(items);
    // A feed is looser than the comma: `(1, 2 ==> f())` feeds `(1, 2)`.
    let lifted = crate::parser::lift_feed_in_list(lifted);
    if lifted.len() == 1 && matches!(&lifted[0], Expr::Feed { .. }) {
        return lifted.into_iter().next().unwrap();
    }
    if let Some(expr) = lift_minmax_in_paren_list(&lifted) {
        return expr;
    }
    // If lifting produced a single MetaOp, return it unwrapped
    // (the Z/X meta-op already produces a list result)
    if lifted.len() == 1
        && (matches!(&lifted[0], Expr::MetaOp { .. })
            || matches!(&lifted[0], Expr::Call { name, .. } if *name == Symbol::intern("zip")))
    {
        return maybe_curry_xz_metaop(lifted.into_iter().next().unwrap());
    }
    // A standalone X/Z meta-op element with Whatever operands curries into a
    // WhateverCode (e.g. `(* X+ *, 5)`); list-operand metaops were already folded
    // by the lift above and left as plain (extending) MetaOps.
    let lifted = lifted.into_iter().map(maybe_curry_xz_metaop).collect();
    Expr::ArrayLiteral(lifted)
}

/// Lift a top-level list-infix meta-op (`Z`/`X`) or `minmax` across an
/// unparenthesized listop argument list. Raku's list-infix operators are LOOSER
/// than the comma separating listop arguments, so `say 100, 200 Z+ 42, 23` is
/// `say((100, 200) Z+ (42, 23))` — the whole comma level is one operand — not
/// `say(100, (200 Z+ (42, 23)))`. The per-argument parse leaves the meta-op with
/// only its immediately-preceding element as its left operand (and the comma
/// tail absorbed on the right); this reuses the same columnar lift the
/// parenthesized-list finalizer applies (`finalize_paren_list`) to pull the
/// leading arguments into the left operand.
///
/// Analogous to `try_parse_sequence_arg_list` (parser), which does the same absorption
/// for the sequence operator (`...`). Returns an ordinary comma list untouched
/// (no top-level meta-op/minmax), so the caller keeps its per-argument handling.
pub(crate) fn lift_list_infix_in_arg_list(items: Vec<Expr>) -> Vec<Expr> {
    let has_metaop = items.iter().any(|e| matches!(e, Expr::MetaOp { .. }));
    if !has_metaop && lift_minmax_in_paren_list(&items).is_none() {
        return items;
    }
    let lifted = lift_meta_ops_in_paren_list(items);
    if let Some(expr) = lift_minmax_in_paren_list(&lifted) {
        return vec![expr];
    }
    // NOTE: unlike `finalize_paren_list`, this does NOT Whatever-curry the lifted
    // meta-op. The argument-list callers run each element through the full
    // `expression` parser, whose `should_wrap_whatevercode` already curries a
    // genuine standalone `*` (`say * X+ *`) while correctly leaving a `*` that is
    // a list *extender* inside an operand (`<a b c> Z (1 xx *)`) untouched.
    // Currying here would wrap the latter into a WhateverCode by mistake.
    lifted
}

/// Whatever-curry an X/Z meta-op when (and only when) a *standalone* `*` operand
/// makes it a WhateverCode. A `*` that is merely the trailing element of a
/// comma-list operand is a list *extender* (zip/cross repeats the last element),
/// not a curry placeholder, so an `ArrayLiteral` operand never triggers currying.
pub(crate) fn maybe_curry_xz_metaop(expr: Expr) -> Expr {
    if matches!(&expr, Expr::MetaOp { meta, .. } if meta == "X" || meta == "Z")
        && xz_metaop_curries(&expr)
    {
        Expr::WhateverCurry(Box::new(expr))
    } else {
        expr
    }
}

/// True when an X/Z meta-op (possibly a left-nested chain of them) has a
/// standalone Whatever operand that should curry.
fn xz_metaop_curries(expr: &Expr) -> bool {
    if let Expr::MetaOp {
        meta, left, right, ..
    } = expr
        && (meta == "X" || meta == "Z")
    {
        operand_curries(left) || operand_curries(right)
    } else {
        false
    }
}

/// An operand curries when it carries a standalone Whatever placeholder. A
/// comma-list operand (`ArrayLiteral`) is an extender, never a curry. Nested
/// X/Z meta-ops recurse (so `1 X+ * X+ 3` curries through the inner op even
/// though the global `contains_whatever` deliberately ignores X/Z).
fn operand_curries(e: &Expr) -> bool {
    match e {
        Expr::ArrayLiteral(_) => false,
        Expr::MetaOp { meta, .. } if meta == "X" || meta == "Z" => xz_metaop_curries(e),
        // `(1 xx *) Z ...`: a bare `*` under `xx` is the infinite repeat, not a
        // curry point.
        _ if contains_xx_with_bare_whatever(e) => false,
        _ => contains_whatever(e) || matches!(e, Expr::WhateverCurry(_)),
    }
}

/// Whether an infixed-function node (`Expr::InfixFunc`) is a list-infix
/// operator -- looser than the comma, so the comma list around it belongs to
/// its operands: the built-in `minmax`, or a user infix declared at that level
/// (`is equiv<Z>`, `is equiv(&infix:<...>)`, or looser; #9405).
fn is_list_infix_func(name: &str) -> bool {
    name == "minmax"
        || crate::parser::lookup_custom_infix_precedence(name)
            .is_some_and(|level| level <= crate::parser::PREC_SEQUENCE)
}

/// Lift a list-infix infixed function (see [`is_list_infix_func`]) across the
/// comma list it sits in: the elements before it join its left operand and
/// the ones after it its right operand.
fn lift_minmax_in_paren_list(items: &[Expr]) -> Option<Expr> {
    // One neighbour is enough: `5 minmax 3, 2` is `5 minmax (3, 2)`.
    if items.len() < 2 {
        return None;
    }
    // A Whatever-curried list infix leading the list (`* op a, b`, curried
    // when its element was parsed) takes the rest of the list into its right
    // operand too, and stays curried: `@x.map: * op a, b` is
    // `@x.map: { $_ op (a, b) }`, as `(1,2).map(* Z 7, 8)` is in rakudo.
    if let Expr::WhateverCurry(inner) = &items[0]
        && let Expr::InfixFunc {
            name,
            left,
            right,
            modifier: None,
        } = inner.as_ref()
        && right.len() == 1
        && is_list_infix_func(name)
    {
        let mut rhs_items: Vec<Expr> = vec![right[0].clone()];
        rhs_items.extend_from_slice(&items[1..]);
        return Some(Expr::WhateverCurry(Box::new(Expr::InfixFunc {
            name: name.clone(),
            left: left.clone(),
            right: vec![Expr::ArrayLiteral(rhs_items)],
            modifier: None,
        })));
    }
    let idx = items.iter().position(|expr| {
        matches!(
            expr,
            Expr::InfixFunc {
                name,
                modifier: None,
                right,
                ..
            } if right.len() == 1 && is_list_infix_func(name)
        )
    })?;
    let Expr::InfixFunc {
        name,
        left,
        right,
        modifier,
    } = &items[idx]
    else {
        return None;
    };

    let mut lhs_items: Vec<Expr> = items[..idx].to_vec();
    lhs_items.push((**left).clone());

    let mut rhs_items: Vec<Expr> = vec![right[0].clone()];
    rhs_items.extend_from_slice(&items[idx + 1..]);

    let lhs = if lhs_items.len() == 1 {
        lhs_items.remove(0)
    } else {
        Expr::ArrayLiteral(lhs_items)
    };
    let rhs = if rhs_items.len() == 1 {
        rhs_items.remove(0)
    } else {
        Expr::ArrayLiteral(rhs_items)
    };

    Some(Expr::InfixFunc {
        name: name.clone(),
        left: Box::new(lhs),
        right: vec![rhs],
        modifier: modifier.clone(),
    })
}

pub(crate) fn normalize_sequence_waypoints(expr: Expr) -> Expr {
    match expr {
        Expr::Binary {
            left, op, right, ..
        } if matches!(
            op,
            crate::token_kind::TokenKind::DotDotDot | crate::token_kind::TokenKind::DotDotDotCaret
        ) =>
        {
            let right = match *right {
                Expr::ArrayLiteral(items) => Expr::ArrayLiteral(
                    items
                        .into_iter()
                        .map(|item| match item {
                            Expr::Binary {
                                left,
                                op:
                                    crate::token_kind::TokenKind::DotDotDot
                                    | crate::token_kind::TokenKind::DotDotDotCaret,
                                right,
                                ..
                            } => Expr::ArrayLiteral(vec![*left, *right]),
                            other => other,
                        })
                        .collect(),
                ),
                other => other,
            };
            Expr::Binary {
                left,
                op,
                right: Box::new(right),
                form: Default::default(),
            }
        }
        other => other,
    }
}

pub(crate) fn normalize_chained_zip_meta(expr: Expr) -> Expr {
    match expr {
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } if meta == "Z" => {
            // Case 2 (checked first, on the operands as written): left is
            // itself a Z MetaOp with the same op, e.g. (A Z B Z C Z D) where
            // left = Z(Z(A, B), C). Collect the whole chain into one multi-way
            // zip call. Normalizing `left` first would already have turned its
            // own chain into a `zip(A, B, C)` call, and the outer level then
            // nested it: `(1 Z 2 Z 3 Z 4)` came out `((1, 2, 3), 4)`.
            if let Expr::MetaOp {
                meta: ref inner_meta,
                op: ref inner_op,
                ..
            } = *left
                && inner_meta == "Z"
                && *inner_op == op
            {
                let mut args: Vec<Expr> = left
                    .flatten_meta_chain("Z", &op)
                    .into_iter()
                    .cloned()
                    .map(normalize_chained_zip_meta)
                    .collect();
                args.push(normalize_chained_zip_meta(*right));
                if !op.is_empty() {
                    args.push(Expr::Binary {
                        left: Box::new(Expr::Literal(Value::str_from("with"))),
                        op: TokenKind::FatArrow,
                        right: Box::new(Expr::CodeVar(format!("infix:<{}>", op))),
                        form: Default::default(),
                    });
                }
                return Expr::Call {
                    name: Symbol::intern("zip"),
                    args,
                    listop: false,
                };
            }
            let left = normalize_chained_zip_meta(*left);
            let right = normalize_chained_zip_meta(*right);

            // Case 1: left is an ArrayLiteral whose last element is a nested Z MetaOp.
            // e.g. (a, b Z+ c, d) Z+ (e, f)
            if let Expr::ArrayLiteral(mut left_items) = left.clone()
                && let Expr::ArrayLiteral(right_items) = right.clone()
                && let Some(Expr::MetaOp {
                    meta: inner_meta,
                    op: inner_op,
                    left: inner_left,
                    right: inner_right,
                }) = left_items.pop()
                && inner_meta == "Z"
                && inner_op == op
                && let Expr::ArrayLiteral(inner_right_items) = *inner_right
            {
                let mut first_col = left_items;
                first_col.push(*inner_left);
                let to_expr = |col: Vec<Expr>| {
                    if col.len() == 1 {
                        col.into_iter().next().unwrap()
                    } else {
                        Expr::ArrayLiteral(col)
                    }
                };
                let mut args = vec![
                    to_expr(first_col),
                    to_expr(inner_right_items),
                    to_expr(right_items),
                ];
                // A BARE `Z` has no inner operator, so it must not carry a
                // `with` adverb -- `infix:<>` names nothing. (Case 2 below has
                // always had this guard; this arm and the paren-list lift did
                // not, which is what made `(1, 2 Z <a b> Z <c d>)` fail.)
                if !op.is_empty() {
                    args.push(Expr::Binary {
                        left: Box::new(Expr::Literal(Value::str_from("with"))),
                        op: TokenKind::FatArrow,
                        right: Box::new(Expr::CodeVar(format!("infix:<{}>", op))),
                        form: Default::default(),
                    });
                }
                return Expr::Call {
                    name: Symbol::intern("zip"),
                    args,
                    listop: false,
                };
            }

            Expr::MetaOp {
                meta,
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        }
        other => other,
    }
}

fn lift_meta_ops_in_paren_list(items: Vec<Expr>) -> Vec<Expr> {
    let meta_idx = items.iter().position(|e| matches!(e, Expr::MetaOp { .. }));
    if let Some(idx) = meta_idx
        && idx > 0
        && let Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } = &items[idx]
    {
        // Flatten left-nested same-op chains into column lists.
        // Example source shape:
        //   (a, (b Zop c,d) Zop e,f)
        // becomes columns:
        //   (a,b), (c,d), (e,f)
        let as_column = |expr: &Expr| match expr {
            Expr::ArrayLiteral(items) => items.clone(),
            other => vec![other.clone()],
        };
        let mut columns_rev: Vec<Vec<Expr>> = vec![as_column(right)];
        let mut chain_left = left.as_ref().clone();
        while let Expr::MetaOp {
            meta: inner_meta,
            op: inner_op,
            left: inner_left,
            right: inner_right,
        } = &chain_left
        {
            if *inner_meta != *meta || *inner_op != *op {
                break;
            }
            columns_rev.push(as_column(inner_right));
            chain_left = *inner_left.clone();
        }
        let mut first_col: Vec<Expr> = items[..idx].to_vec();
        first_col.push(chain_left);
        columns_rev.push(first_col);
        columns_rev.reverse();

        let col_to_expr = |col: Vec<Expr>| {
            if col.len() == 1 {
                col.into_iter().next().unwrap()
            } else {
                Expr::ArrayLiteral(col)
            }
        };
        // A standalone `*` column (`(1,2 Z~ * Z~ 3,4)`) makes the whole chain a
        // WhateverCode. We must keep it as a nested meta-op so the curry logic in
        // `maybe_curry_xz_metaop` can wrap it; the lazy multi-arg `zip` call form
        // (used for plain `Z` chains to preserve laziness) is not curryable.
        let has_standalone_whatever = columns_rev
            .iter()
            .any(|col| col.len() == 1 && matches!(col[0], Expr::Whatever));
        let lifted_expr = if *meta == "Z" && columns_rev.len() > 2 && !has_standalone_whatever {
            let mut args: Vec<Expr> = columns_rev.into_iter().map(col_to_expr).collect();
            // A BARE `Z` chain zips into tuples with no combining operator, so
            // it takes no `with` adverb: emitting one built `infix:<>`, which
            // names nothing, and `(1, 2 Z <a b> Z <c d>)` died reporting the
            // failed operator lookup as "Two terms in a row". Only `Zop`
            // (`Z+`, `Z~`, ...) carries the adverb -- which is why the `Z+`
            // spelling of the very same shape always worked.
            if !op.is_empty() {
                args.push(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str_from("with"))),
                    op: TokenKind::FatArrow,
                    right: Box::new(Expr::CodeVar(format!("infix:<{}>", op))),
                    form: Default::default(),
                });
            }
            Expr::Call {
                name: Symbol::intern("zip"),
                args,
                listop: false,
            }
        } else {
            // Left-fold the columns into a nested meta-op chain. This handles X
            // chains of any arity (`a X~ b X~ c` => `(a X~ b) X~ c`, previously
            // the trailing columns were dropped) and curryable Z chains alike.
            let mut iter = columns_rev.into_iter().map(col_to_expr);
            let mut acc = iter.next().unwrap_or(Expr::ArrayLiteral(Vec::new()));
            for col in iter {
                acc = Expr::MetaOp {
                    meta: meta.clone(),
                    op: op.clone(),
                    left: Box::new(acc),
                    right: Box::new(col),
                };
            }
            acc
        };

        let mut result = vec![lifted_expr];
        result.extend(items[idx + 1..].to_vec());
        return result;
    }
    items
}
