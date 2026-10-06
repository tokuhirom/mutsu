//! Subscript adverbs whose name is not one of the built-in subscript adverbs.
//!
//! Raku parses ANY colonpair after a postcircumfix subscript as an adverb and
//! passes it as a named argument to `postcircumfix:<[ ]>` / `<{ }>` (or the
//! multi-dimensional `<[; ]>` / `<{; }>`). Only the CORE candidates decide what
//! an unknown one means -- an `X::Multi::NoMatch` for a single positional
//! element, an `X::Adverb` from the slice candidates -- and a user candidate
//! (`multi postcircumfix:<[ ]>(\SELF, \pos, :$eject!)`) may accept it.
//!
//! The dedicated parsers in `adverb.rs` only know the built-in names (and only
//! some of their spellings), so a chain containing any other name is read here
//! as a whole, as plain colonpairs, and lowered to a call carrying every adverb
//! of the chain as a named argument.

use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::{Value, ValueView};

/// The built-in subscript adverb names (after l10n aliasing).
fn is_builtin_subscript_adverb(name: &str) -> bool {
    matches!(name, "k" | "v" | "kv" | "p" | "exists" | "delete")
}

/// The name of a colonpair expression (`:foo`, `:!foo`, `:foo(1)`, `:$foo`).
fn colonpair_name(expr: &Expr) -> Option<String> {
    let Expr::Binary {
        left,
        op: TokenKind::FatArrow,
        ..
    } = expr
    else {
        return None;
    };
    let Expr::Literal(key) = left.as_ref() else {
        return None;
    };
    match key.view() {
        ValueView::Str(s) => Some(s.to_string()),
        _ => None,
    }
}

/// Read the chain of colonpairs starting at `input` (whitespace may precede
/// each one, as raku allows). Returns the chain only when it contains at least
/// one adverb that is not a built-in subscript adverb: a chain of built-in
/// adverbs alone is left to the dedicated parsers.
///
/// Built-in names inside the returned chain are canonicalized (l10n aliases
/// resolved), so the runtime sees `:k` whatever spelling the source used.
pub(crate) fn scan_subscript_named_adverbs(input: &str) -> Option<(&str, Vec<Expr>)> {
    let mut rest = input;
    let mut pairs = Vec::new();
    let mut has_unknown = false;
    loop {
        let r = ws(rest).map_or(rest, |(r, _)| r);
        if !super::helpers::colonpair_adverb_follows(r) {
            break;
        }
        let Ok((after, pair)) = crate::parser::primary::colonpair_expr(r) else {
            break;
        };
        let Some(name) = colonpair_name(&pair) else {
            break;
        };
        let canonical = crate::parser::stmt::simple::l10n_adverb_alias("adverb-pc", &name)
            .unwrap_or_else(|| name.clone());
        let pair = if canonical != name
            && let Expr::Binary { op, right, .. } = pair
        {
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(canonical.clone()))),
                op,
                right,
            }
        } else {
            pair
        };
        has_unknown |= !is_builtin_subscript_adverb(&canonical);
        pairs.push(pair);
        rest = after;
    }
    has_unknown.then_some((rest, pairs))
}

/// Does the adverb chain at `input` carry a non-built-in adverb? The dedicated
/// built-in adverb parsers decline such a chain, so that it reaches
/// [`lower_subscript_named_adverbs`] whole instead of being split in two.
pub(crate) fn has_subscript_named_adverb(input: &str) -> bool {
    scan_subscript_named_adverbs(input).is_some()
}

/// The variable name an `X::Adverb` reports as its `.source`, when the target
/// is a plain variable; empty otherwise (the runtime then names the type).
fn subscript_source_name(target: &Expr) -> String {
    target.sigiled_var_name().unwrap_or_default()
}

/// Lower a subscript (`Expr::Index` / `Expr::MultiDimIndex`) carrying the
/// colonpair chain `pairs` (at least one of them not a built-in adverb).
///
/// With a user `postcircumfix:<...>` candidate in scope the call goes to the
/// operator itself, so multi-dispatch picks the candidate exactly as an
/// explicit call would. Otherwise it goes to the CORE candidate model
/// `__mutsu_subscript_named_adverbs(target, index, source, shape, |pairs)`,
/// which raises what rakudo's candidates raise for these arguments. Returns
/// `None` for any other expression.
pub(crate) fn lower_subscript_named_adverbs(subscript: &Expr, pairs: Vec<Expr>) -> Option<Expr> {
    let (target, index, op_name, shape) = match subscript {
        Expr::Index {
            target,
            index,
            is_positional,
        } => {
            // A zen slice (`@a[]:foo` / `%h{}:foo`) is modelled as a Whatever
            // index by the empty-subscript paths; only its error descriptor
            // tells it apart from the whatever slice.
            let zen = matches!(index.as_ref(), Expr::Literal(lit) if matches!(lit.view(), ValueView::Whatever))
                || (!is_positional && matches!(index.as_ref(), Expr::Whatever));
            let (op, shape) = match (is_positional, zen) {
                (true, false) => ("postcircumfix:<[ ]>", "[ ]"),
                (true, true) => ("postcircumfix:<[ ]>", "[ ] zen"),
                (false, false) => ("postcircumfix:<{ }>", "{ }"),
                (false, true) => ("postcircumfix:<{ }>", "{ } zen"),
            };
            (target.as_ref(), index.as_ref().clone(), op, shape)
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => {
            let (op, shape) = if *is_positional {
                ("postcircumfix:<[; ]>", "[; ]")
            } else {
                ("postcircumfix:<{; }>", "{; }")
            };
            (
                target.as_ref(),
                Expr::ArrayLiteral(dimensions.clone()),
                op,
                shape,
            )
        }
        _ => return None,
    };
    if crate::parser::stmt::simple::is_user_declared_sub(op_name) {
        let mut args = vec![target.clone(), index];
        args.extend(pairs);
        return Some(Expr::Call {
            name: Symbol::intern(op_name),
            args,
            listop: false,
        });
    }
    let mut args = vec![
        target.clone(),
        index,
        Expr::Literal(Value::str(subscript_source_name(target))),
        Expr::Literal(Value::str(shape.to_string())),
    ];
    args.extend(pairs);
    Some(Expr::Call {
        name: Symbol::intern("__mutsu_subscript_named_adverbs"),
        args,
        listop: false,
    })
}
