//! The adverbs written after a subscript (`@a[0]:exists`, `%h<a>:delete:k`,
//! `@a[1] :foo`).
//!
//! Raku parses ANY colonpair after a postcircumfix subscript as an adverb and
//! passes it as a named argument to `postcircumfix:<[ ]>` / `<{ }>` (or the
//! multi-dimensional `<[; ]>` / `<{; }>`). Which ones mean something is decided
//! when the node is lowered (`rakuast::subscript_adverb::lower`): the CORE
//! candidates answer the built-in names, a user candidate
//! (`multi postcircumfix:<[ ]>(\SELF, \pos, :$eject!)`) may accept others.
//! The parser therefore keeps the chain as written, as the typed
//! `ParsedExpr::SubscriptAdverbs` node.

use super::call_method::has_ternary_else_after;
use crate::ast::parsed::{ParsedExpr, SubscriptAdverbs};
use crate::ast::subscript_adverb::pair_key;
use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::value::Value;

/// Read the chain of colonpairs starting at `input` (whitespace may precede
/// each one, as raku allows). `None` when no colonpair follows.
///
/// Built-in names inside the returned chain are canonicalized (l10n aliases
/// resolved), so the runtime sees `:k` whatever spelling the source used. A
/// value adverb (`:k` / `:v` / `:kv` / `:p`) in front of the `!!` of a
/// conditional ends the chain: it is not an adverb of this subscript.
// Cost: O(n), n = length of the chain.
pub(crate) fn scan_subscript_adverbs(input: &str) -> Option<(&str, Vec<Expr>)> {
    let mut rest = input;
    let mut pairs = Vec::new();
    loop {
        let r = ws(rest).map_or(rest, |(r, _)| r);
        if !super::helpers::colonpair_adverb_follows(r) {
            break;
        }
        let Ok((after, pair)) = crate::parser::primary::colonpair_expr(r) else {
            break;
        };
        let Some(name) = pair_key(&pair) else {
            break;
        };
        let canonical = crate::parser::stmt::simple::l10n_adverb_alias("adverb-pc", &name)
            .unwrap_or_else(|| name.clone());
        if matches!(canonical.as_str(), "k" | "v" | "kv" | "p") && has_ternary_else_after(after) {
            break;
        }
        let pair = if canonical != name
            && let Expr::Binary { op, right, form, .. } = pair
        {
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(canonical))),
                op,
                right,
                form,
            }
        } else {
            pair
        };
        pairs.push(pair);
        rest = after;
    }
    (!pairs.is_empty()).then_some((rest, pairs))
}

/// The node for `subscript` carrying the colonpairs `pairs`. An empty
/// subscript (`@a[]`, `%h{}`) is the Whatever slice here, as it is for the
/// adverbs that read every element.
// Cost: O(1).
pub(crate) fn subscript_adverbs_expr(subscript: Expr, pairs: Vec<Expr>) -> Expr {
    let subscript = match subscript {
        Expr::ZenSlice(target) => Expr::Index {
            is_positional: !matches!(*target, Expr::HashVar(_)),
            target,
            index: Box::new(Expr::Literal(Value::WHATEVER)),
            spelling: Default::default(),
        },
        other => other,
    };
    Expr::Ast(Box::new(ParsedExpr::SubscriptAdverbs(SubscriptAdverbs {
        subscript,
        pairs,
    })))
}

pub(crate) fn supports_postfix_call_adverbs(expr: &Expr) -> bool {
    let mut expr = expr;
    while let Expr::Spelled(spelled) = expr {
        expr = &spelled.expr;
    }
    matches!(
        expr,
        Expr::Call { .. }
            | Expr::MethodCall { .. }
            | Expr::CallOn { .. }
            | Expr::HyperMethodCall { .. }
            | Expr::HyperMethodCallDynamic { .. }
    )
}

/// Whether `expr` is a subscript carrying an `:exists` adverb.
// Cost: O(a), a = adverbs.
pub(crate) fn has_exists_adverb(expr: &Expr) -> bool {
    matches!(expr, Expr::Ast(node)
        if matches!(node.as_ref(), ParsedExpr::SubscriptAdverbs(s) if s.has_exists()))
}
