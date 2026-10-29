use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{MISSING_BLOCK, PError, PResult, opt_char, parse_char};

use crate::ast::{Expr, ParamDef, Stmt, collect_placeholders_shallow};
use crate::symbol::Symbol;
use crate::value::Value;

use super::decl::parse_array_shape_suffix;
use super::sub;
use super::{block, block_inner, ident, keyword, parse_comma_or_expr, statement, var_name};
/// Parse a control statement's `{ ... }` body with the pointy block's own
/// parameters in scope.
///
/// A sigilless parameter (`-> \index { ... }`) declares a TERM: inside the body
/// a bare `index` IS that binding, so the parse scope has to carry it — exactly
/// as `for`'s own header already does. Without it the body is parsed against
/// whatever the name means OUTSIDE the block, and a name that also spells a
/// listop (`index`, `join`, ...) reads as a call and swallows the rest of the
/// statement, so it is the ENCLOSING construct that fails to parse — which is
/// why `Hash::Ordered` (and `Archive::Ar` / `BSON::Simple` / `Terminal::Tests`
/// through it) reported its error at a `method ... (` several lines away
/// (#7954).
///
/// A `&name` parameter is registered for the same reason `for` registers it:
/// `-> &m { m() }` must read `m()` as a call to the bound routine, not as an
/// `m//` match.
pub(crate) fn block_with_pointy_params<'a>(
    input: &'a str,
    params: &[ParamDef],
) -> PResult<'a, Vec<Stmt>> {
    let mut code_names: Vec<&str> = Vec::new();
    let mut term_names: Vec<&str> = Vec::new();
    for pd in params {
        let name = pd.name.trim_start_matches('\\');
        if name.is_empty() {
            continue;
        }
        if pd.sigilless {
            term_names.push(name);
        } else if let Some(bare) = name.strip_prefix('&') {
            code_names.push(bare);
        }
    }
    if code_names.is_empty() && term_names.is_empty() {
        return block(input);
    }
    let (rest, _) =
        parse_char(input, '{').map_err(|_| PError::expected_at(MISSING_BLOCK, input))?;
    // The parse memo is keyed by the input slice alone, so an entry made while
    // the header was being parsed — before these names existed — would be
    // handed straight back here, and the body would be re-used at its OLD
    // meaning. That is not hypothetical: `if`'s header runs the full parameter
    // list parser, which speculates over the `{ ... }` that follows, so
    // `if 1 -> \index { say index .. 2 }` memoized the body's failure (`index`
    // read as the listop) and then replayed it under the correct scope.
    // A fresh generation makes every key in this body distinct from the
    // enclosing parse's, which is exactly the scope's effect on the grammar.
    let _generation = crate::parser::memo::begin_parse_generation();
    super::simple::push_scope();
    for name in code_names {
        super::simple::register_user_sub(name);
    }
    for name in term_names {
        super::simple::register_user_term_symbol(name);
    }
    let mut result = block_inner(rest);
    super::simple::finish_block_anon_states(&mut result);
    super::simple::pop_scope();
    result
}

fn condition_has_assignment_tail(rest: &str) -> bool {
    if rest.starts_with(":=") || rest.starts_with("::=") || rest.starts_with("⚛=") {
        return true;
    }
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        return true;
    }
    super::assign::parse_compound_assign_op(rest).is_some()
        || super::assign::parse_custom_compound_assign_op(rest).is_some()
}

fn condition_expr(input: &str) -> PResult<'_, Expr> {
    match expression(input) {
        Ok((rest, cond)) => {
            let (tail, _) = ws(rest)?;
            if condition_has_assignment_tail(tail)
                && let Ok((assign_rest, assign_cond)) = super::assign::try_parse_assign_expr(input)
            {
                return Ok((assign_rest, assign_cond));
            }
            // The condition is a full EXPR, comma included: `with 1, 2, 3 { }`
            // topicalizes the list, and `if 1, 0 { }` tests it (a non-empty
            // list is true).
            if tail.starts_with(',') && !tail.starts_with(",,") {
                return super::assign::parse_comma_or_expr(input);
            }
            Ok((rest, cond))
        }
        Err(_) => super::assign::try_parse_assign_expr(input),
    }
}

/// Detect the Perl 5 `for my $x (LIST) { }` foreach pattern: a `my`/`our`/
/// `state` declaration of a single variable immediately followed by a
/// parenthesized list. In Raku the topic variable goes in a pointy block
/// (`for LIST -> $x { }`), so this is X::Syntax::P5.
fn looks_like_p5_foreach(input: &str) -> bool {
    let rest = keyword("my", input)
        .or_else(|| keyword("our", input))
        .or_else(|| keyword("state", input));
    let Some(rest) = rest else { return false };
    let Ok((rest, _)) = ws1(rest) else {
        return false;
    };
    // A single sigil-variable: `$x`, `@x`, `%x`.
    let Some(after_sigil) = rest
        .strip_prefix('$')
        .or_else(|| rest.strip_prefix('@'))
        .or_else(|| rest.strip_prefix('%'))
    else {
        return false;
    };
    let Ok((rest, _name)) = ident(after_sigil) else {
        return false;
    };
    let (rest, _) = ws(rest).unwrap_or((rest, ()));
    rest.starts_with('(')
}

fn p5_foreach_error() -> PError {
    let msg = "This appears to be Perl code".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
    let ex =
        crate::value::Value::make_instance(crate::symbol::Symbol::intern("X::Syntax::P5"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

mod conditional_binding;
mod conditionals;
mod for_loops;
mod for_params;
pub(crate) use for_params::{FOR_UNPACK, FOR_UNPACK_ARRAY, indexed_unpack_name};
mod given_when;
mod labeled_loop;
mod loop_repeat;
pub(crate) mod parsed_block;
pub(crate) use parsed_block::{finish, parsed_block};
mod pointy_param;
mod react;
pub(in crate::parser::stmt) mod source_body;
mod while_until;
pub(crate) use source_body::check_signature;
mod with_stmt;

// Re-exports preserving each function's original visibility (all `pub(super)`).
pub(super) use conditionals::{if_stmt, unless_stmt};
pub(crate) use for_loops::placeholder_loop_params;
pub(super) use for_loops::{
    for_stmt, foreach_stmt, hyper_for_body, hyper_for_stmt, lazy_for_body, race_for_body,
    race_for_stmt,
};
pub(super) use for_params::parse_for_params;
pub(super) use given_when::{default_stmt, given_stmt, when_stmt};
pub(super) use labeled_loop::labeled_loop_stmt;
pub(super) use loop_repeat::{loop_stmt, repeat_stmt};
pub(super) use pointy_param::parse_pointy_param;
pub(crate) use react::react_stmt;
pub(crate) use react::whenever_stmt;
pub(super) use while_until::{until_stmt, while_stmt};
pub(super) use with_stmt::with_stmt;

// Cross-submodule helpers: originally private `fn`s in the if/unless block that
// are also called from `with_stmt` (for `orwith` elsif-chain lowering). Bumped
// to `pub(crate)` in `conditionals` and re-exported here so `with_stmt` reaches
// them via `use super::*`.
pub(in crate::parser) use conditionals::parse_elsif_chain;
