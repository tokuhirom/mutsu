use std::sync::atomic::{AtomicUsize, Ordering};

use super::super::expr::{
    QuotedMethodName, expression, expression_no_sequence, expression_no_word_logical,
    parse_quoted_method_name,
};
use super::super::helpers::ws;
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, take_while1,
};
use super::super::primary::parse_call_arg_list;

use crate::ast::{AssignOp, Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::{MetaAssignIdentity, TokenKind};
use crate::value::Value;

use super::{ident, parse_statement_modifier};

static TMP_INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Strip a leading atomic compound-assignment operator, returning the rest of
/// the input and whether the delta must be negated.
///
/// `⚛+=` and `⚛-=` are the same read-modify-write on the same target; only the
/// sign of the delta differs, so the compiler lowers both onto
/// `__mutsu_atomic_add_var` and negates the right-hand side of the subtract
/// form. Doing it there rather than with a second builtin keeps the atomicity
/// identical — the negation is applied to the delta expression, before the
/// atomic update ever runs.
///
/// rakudo declares the minus form under two spellings (see
/// `runtime::core_infix_names`): ASCII HYPHEN-MINUS and U+2212 MINUS SIGN.
/// Both were already listed there as operators mutsu claims to know, while the
/// parser only ever recognised `⚛+=` — so `$i ⚛-= 2` (Async::Workers,
/// FFmpegProgressBar, Russian) failed to parse at all.
pub(crate) fn strip_atomic_compound_assign(rest: &str) -> Option<(&str, bool)> {
    if let Some(stripped) = rest.strip_prefix("⚛+=") {
        return Some((stripped, false));
    }
    for minus in ["⚛-=", "⚛\u{2212}="] {
        if let Some(stripped) = rest.strip_prefix(minus) {
            return Some((stripped, true));
        }
    }
    None
}

/// The atomic store `@a[0] ⚛= rhs` / `%h<k> ⚛= rhs` is: the call
/// `atomic-assign(@a[0], rhs)`, which the compiler lowers onto the element's own
/// atomic cell (the one `cas(@a[0], ...)` swaps), so a refused element --
/// one of a narrow native-int array (#12008) -- is refused like any other
/// atomic. `None` for a target that is not an `@`/`%` element.
pub(crate) fn atomic_elem_store_call(target: &Expr, rhs: Expr) -> Option<Expr> {
    // TODO: share this predicate with `expr::postfix::is_atomic_elem_target`
    // (not reachable from here while it is private to the postfix module).
    let is_elem = matches!(target, Expr::Index { target: base, .. }
        if base.container_var_key().is_some_and(|k| k.starts_with(['@', '%'])));
    is_elem.then(|| Expr::Call {
        name: Symbol::intern("atomic-assign"),
        args: vec![target.clone(), rhs],
    })
}

/// Strip a leading atomic STORE operator, returning the rest of the input.
///
/// Both spellings rakudo accepts are consumed: `⚛=` itself, and `⚛==`, which is
/// `infix:<⚛=>` under the assignment metaoperator (`&infix:«⚛==».name` answers
/// `infix:<⚛=> + {assigning}`). The metaoperator form assigns the base
/// operator's result back to the target, and `⚛=`'s result *is* the value it
/// just stored, so the two spellings are the same store and lower to the same
/// `__mutsu_atomic_store_var` call.
///
/// Selkie writes the metaoperator form (`$!mouse-capture-stale ⚛== 1`), which
/// no site recognised at all.
pub(crate) fn strip_atomic_store_assign(rest: &str) -> Option<&str> {
    let after = rest.strip_prefix("⚛=")?;
    // The metaoperator's own `=`. `⚛=>` is not one of ours, and a `⚛=` followed
    // by a further `=` after that (`⚛===`) is the error rakudo reports too.
    Some(after.strip_prefix('=').unwrap_or(after))
}

/// The `⚛+=` / `⚛-=` update of the variable `name` by `rhs`: a call of
/// Rakudo's own `infix:<⚛+=>` / `infix:<⚛-=>`, so a refused target is reported
/// under the operator the program wrote and the AST round-trips through
/// RakuAST (#11834). The compiler lowers both onto the one atomic add; the
/// subtract form negates its operand there.
pub(crate) fn atomic_compound_call(name: String, rhs: Expr, negate: bool) -> Expr {
    let operator = if negate {
        "infix:<⚛-=>"
    } else {
        "infix:<⚛+=>"
    };
    Expr::Call {
        name: Symbol::intern(operator),
        args: vec![Expr::Var(name), rhs],
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CompoundAssignOp {
    Comma,
    DefinedOr,
    LogicalOr,
    LogicalAnd,
    Add,
    Sub,
    Concat,
    Mul,
    Div,
    Mod,
    Power,
    Repeat,
    ListRepeat,
    BitOr,
    BitAnd,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    Min,
    Max,
    KeywordOr,
    KeywordAnd,
    KeywordXor,
    Orelse,
    Andthen,
    Notandthen,
    IntDiv,
    Lcm,
    Gcd,
    StrBitAnd,
    StrBitOr,
    StrBitXor,
    StrShiftLeft,
    StrShiftRight,
    BoolBitAnd,
    BoolBitOr,
    BoolBitXor,
    XorXor,
    JuncAny,
    JuncAll,
    JuncOne,
}

mod assign_stmt;
mod bracket;
mod comma;
mod compound_expr;
mod lvalue;
mod op;
mod paren;
mod sink;
mod try_assign;

// ---- Re-exports preserving each public function's original visibility ----
pub(in crate::parser) use assign_stmt::assign_stmt;
pub(in crate::parser) use comma::{
    comma_list_ends_here, normalize_comma_list_items, parse_comma_or_expr,
    parse_comma_or_expr_item_no_word_logical, parse_comma_or_expr_no_word_logical,
};
pub(in crate::parser) use try_assign::{paren_assign_rhs_is_complete, try_parse_assign_expr};

pub(crate) use bracket::parse_bracket_meta_assign_op;
pub(crate) use compound_expr::{
    DOTTY_ASSIGN_OP, build_compound_assign_expr, build_custom_compound_assign_expr,
    build_meta_assign_expr, compound_assign_marker, dotty_assign_marker, is_dotty_assign,
    preserve_compound_assign,
};
pub(crate) use lvalue::{
    callable_lvalue_assign_expr, dynamic_method_lvalue_assign_expr, list_lvalue_assign_expr,
    method_lvalue_assign_expr, method_lvalue_roundtrip_assign_expr, named_sub_lvalue_assign_expr,
    subscript_adverb_lvalue_assign_expr,
};
pub(crate) use op::{ShortCircuitKeep, short_circuit_keep, short_circuit_test};
pub(crate) use op::{
    autoviv_set_compound_lhs, compound_assign_op_from_name, compound_assigned_value_expr,
    parse_compound_assign_op, parse_custom_compound_assign_op, parse_meta_compound_assign_op,
    parse_set_compound_assign_op, short_circuit_compound_assign_expr,
};
pub(crate) use paren::{
    looks_like_parenthesized_assignment, paren_list_assign_expr, parenthesized_assign_expr,
};
pub(crate) use sink::{
    parse_assign_expr_or_comma, parse_assign_expr_or_comma_no_word_logical,
    rewrite_scalar_assignment_rhs_as_sink, rewrite_scalar_assignment_stmt_as_sink,
};
pub(crate) use try_assign::{parse_colon_args, parse_colon_args_with};
