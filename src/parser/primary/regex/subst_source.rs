//! A substitution's source tree, and the way back from it.
//!
//! `s:g:i/a/b/` executes from a normalized form: the pattern text behind a
//! prefix of inline modifiers (`:i a`) and a handful of flags (`global`,
//! `nth`, `samecase`, ...). The RakuAST boundary shows what was *written*: the
//! pattern's tree and the adverbs in their spelling and order (`g`, `i`).
//! [`subst_expr`] builds both, and [`subst_pattern_source`] derives the
//! normalized form from the written one again by running the adverbs through
//! the parser's own adverb routine, so a substitution lowered from a tree
//! cannot read an adverb differently from a parsed one.

use super::adverbs::{
    MatchAdverbs, adverbs_need_value, apply_inline_match_adverbs, build_regex_with_adverbs,
    parse_match_adverbs,
};
use crate::ast::Expr;
use crate::regex_tree::{RegexAdverb, RegexTree};
use crate::value::Value;

/// The flags a substitution's adverbs set, as `Expr::Subst` carries them.
pub(crate) struct SubstFlags {
    pub(crate) samecase: bool,
    pub(crate) sigspace: bool,
    pub(crate) samemark: bool,
    pub(crate) samespace: bool,
    pub(crate) global: bool,
    pub(crate) nth: Option<Box<str>>,
    pub(crate) x: Option<Box<str>>,
}

/// The source tree of a substitution whose pattern was written `raw`
/// (without the inline adverb prefix), with `adverbs` as written. `None` when
/// the pattern has no source tree.
// Cost: O(|raw|).
fn subst_tree(raw: &str, adverbs: &MatchAdverbs) -> Option<Box<RegexTree>> {
    let mut tree = RegexTree::parse_static_at(
        raw,
        false,
        crate::parser::primary::fragment_attempts::unit_offset(raw),
    )?;
    tree.adverbs = adverbs
        .source
        .iter()
        .map(|(name, argument)| RegexAdverb {
            name: name.clone(),
            argument: argument.clone(),
        })
        .collect();
    Some(Box::new(tree))
}

/// `s/.../.../` (or `S`, with `destructive` false) as the parser carries it.
/// `raw` is the pattern as written and `pattern` its normalized form; exactly
/// one of `replacement` (a `qq` source) and `thunk` (the right-hand side of
/// `s[...] = EXPR`) is meaningful.
// Cost: O(|raw|).
pub(super) fn subst_expr(
    destructive: bool,
    raw: &str,
    pattern: String,
    replacement: String,
    adverbs: &MatchAdverbs,
    thunk: Option<Box<Expr>>,
) -> Expr {
    let tree = subst_tree(raw, adverbs);
    let (samecase, sigspace, samemark, samespace) = (
        adverbs.samecase,
        adverbs.sigspace,
        adverbs.samemark,
        adverbs.samespace,
    );
    let (global, nth, x) = (
        adverbs.global,
        adverbs.nth.as_deref().map(Box::from),
        adverbs.repeat.as_deref().map(Box::from),
    );
    if destructive {
        Expr::Subst {
            pattern,
            replacement,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth,
            x,
            replacement_thunk: thunk,
            tree,
        }
    } else {
        Expr::NonDestructiveSubst {
            pattern,
            replacement,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth,
            x,
            replacement_thunk: thunk,
            tree,
        }
    }
}

/// `adverbs` as they are written between the construct and its delimiter.
// Cost: O(a), a = size of the adverbs.
fn written_adverbs(adverbs: &[RegexAdverb]) -> String {
    let mut written = String::new();
    for adverb in adverbs {
        written.push(':');
        written.push_str(&adverb.name);
        if let Some(argument) = &adverb.argument {
            written.push('(');
            written.push_str(argument);
            written.push(')');
        }
    }
    written
}

/// The normalized pattern text and flags of a substitution written with
/// `adverbs` over `body` (the pattern's source). `ss` is set for the `ss///`
/// construct, which is `:ss` without writing it. `None` for an adverb a
/// substitution does not take (`:ov`, `:ex`, ...) or one the parser does not
/// know.
// Cost: O(|body| + a), a = size of the adverbs.
pub(crate) fn subst_pattern_source(
    body: String,
    adverbs: &[RegexAdverb],
    ss: bool,
) -> Option<(String, SubstFlags)> {
    let written = written_adverbs(adverbs);
    let (rest, mut parsed) = parse_match_adverbs(&written, "s").ok()?;
    if !rest.is_empty() || parsed.overlap || parsed.exhaustive {
        return None;
    }
    if ss {
        parsed.samespace = true;
        parsed.sigspace = true;
    }
    let flags = SubstFlags {
        samecase: parsed.samecase,
        sigspace: parsed.sigspace,
        samemark: parsed.samemark,
        samespace: parsed.samespace,
        global: parsed.global,
        nth: parsed.nth.as_deref().map(Box::from),
        x: parsed.repeat.as_deref().map(Box::from),
    };
    Some((apply_inline_match_adverbs(body, &parsed), flags))
}

/// An adverb's argument (`:nth(2)`, `:x(1..3)`) as an expression, or `None`
/// when it does not parse as one.
// Cost: O(|source|).
pub(crate) fn parse_adverb_argument(source: &str) -> Option<Expr> {
    match crate::parser::stmt::assign::parse_comma_or_expr(source.trim()) {
        Ok((rest, expr)) if rest.trim().is_empty() => Some(expr),
        _ => None,
    }
}

/// The execution value of a regex literal whose source is `body`, written with
/// `adverbs`: the one the parser builds for `rx:i/.../` or `m:g/.../`. `None`
/// for an adverb the parser does not take.
// Cost: O(|body| + a), a = size of the adverbs.
pub(crate) fn regex_execution_value(body: String, adverbs: &[RegexAdverb]) -> Option<Value> {
    if adverbs.is_empty() {
        return Some(Value::regex(body));
    }
    let written = written_adverbs(adverbs);
    let (rest, parsed) = parse_match_adverbs(&written, "m").ok()?;
    if !rest.is_empty() {
        return None;
    }
    let pattern = apply_inline_match_adverbs(body, &parsed);
    Some(if adverbs_need_value(&parsed) {
        build_regex_with_adverbs(pattern, &parsed)
    } else {
        Value::regex(pattern)
    })
}
