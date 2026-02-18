mod container;
mod ident;
mod misc;
mod number;
mod regex;
mod string;
mod var;

use super::memo::{MemoEntry, MemoStats, ParseMemo};
use super::parse_result::{PError, PResult, update_best_error};
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::Expr;

thread_local! {
    static PRIMARY_MEMO_TLS: RefCell<HashMap<(usize, usize), MemoEntry<Expr>>> = RefCell::new(HashMap::new());
    static PRIMARY_MEMO_STATS_TLS: RefCell<MemoStats> = RefCell::new(MemoStats::default());
    /// Original source pointer and length, set at parse_program start for $?LINE computation.
    static ORIGINAL_SOURCE: RefCell<(usize, usize)> = const { RefCell::new((0, 0)) };
}

static PRIMARY_MEMO: ParseMemo<Expr> = ParseMemo::new(&PRIMARY_MEMO_TLS, &PRIMARY_MEMO_STATS_TLS);

/// Set the original source for $?LINE computation.
pub(super) fn set_original_source(source: &str) {
    ORIGINAL_SOURCE.with(|s| {
        *s.borrow_mut() = (source.as_ptr() as usize, source.len());
    });
}

/// Compute the 1-based line number of `input` within the original source.
fn current_line_number(input: &str) -> i64 {
    ORIGINAL_SOURCE.with(|s| {
        let (src_ptr, src_len) = *s.borrow();
        if src_ptr == 0 {
            return 1;
        }
        let input_ptr = input.as_ptr() as usize;
        if input_ptr < src_ptr || input_ptr > src_ptr + src_len {
            return 1;
        }
        let offset = input_ptr - src_ptr;
        // SAFETY: offset is within the original source bounds
        let src_slice = unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(src_ptr as *const u8, offset))
        };
        (src_slice.matches('\n').count() + 1) as i64
    })
}

pub(super) fn reset_primary_memo() {
    PRIMARY_MEMO.reset();
}

pub(super) fn primary_memo_stats() -> (usize, usize, usize) {
    PRIMARY_MEMO.stats()
}

// Re-exports used by other modules
pub(in crate::parser) use regex::parse_call_arg_list;

pub(super) fn primary(input: &str) -> PResult<'_, Expr> {
    if let Some(cached) = PRIMARY_MEMO.get(input) {
        return cached;
    }
    let result = (|| {
        let input_len = input.len();
        let mut best_error: Option<(usize, PError)> = None;
        macro_rules! try_primary {
            ($expr:expr) => {
                match $expr {
                    Ok(r) => return Ok(r),
                    Err(err) => update_best_error(&mut best_error, err, input_len),
                }
            };
        }

        try_primary!(number::decimal(input));
        try_primary!(number::integer(input));
        try_primary!(string::single_quoted_string(input));
        try_primary!(string::double_quoted_string(input));
        try_primary!(string::q_string(input));
        try_primary!(string::corner_bracket_string(input));
        try_primary!(regex::regex_lit(input));
        try_primary!(regex::version_lit(input));
        try_primary!(ident::keyword_literal(input));
        try_primary!(regex::topic_method_call(input));
        try_primary!(var::scalar_var(input));
        try_primary!(var::array_var(input));
        try_primary!(var::hash_var(input));
        try_primary!(var::code_var(input));
        try_primary!(container::paren_expr(input));
        try_primary!(misc::reduction_op(input));
        try_primary!(container::array_literal(input));
        try_primary!(container::angle_list(input));
        try_primary!(ident::whatever(input));
        try_primary!(misc::capture_literal(input));
        try_primary!(misc::arrow_lambda(input));
        try_primary!(misc::block_or_hash_expr(input));
        // ::Foo class literal (type object reference)
        try_primary!(ident::class_literal(input));
        try_primary!(misc::colonpair_expr(input));

        match ident::identifier_or_call(input) {
            Ok(r) => Ok(r),
            Err(err) => {
                update_best_error(&mut best_error, err, input_len);
                Err(best_error
                    .map(|(_, err)| err)
                    .unwrap_or_else(|| PError::expected_at("primary expression", input)))
            }
        }
    })();
    PRIMARY_MEMO.store(input, &result);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn parse_integer() {
        let (rest, expr) = primary("42").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(42))));
    }

    #[test]
    fn parse_hex() {
        let (rest, expr) = primary("0xFF").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(255))));
    }

    #[test]
    fn parse_scalar() {
        let (rest, expr) = primary("$x").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Var(ref n) if n == "x"));
    }

    #[test]
    fn parse_twigil_var() {
        let (rest, expr) = primary("$*OUT").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Var(ref n) if n == "*OUT"));
    }

    #[test]
    fn parse_angle_single() {
        let (rest, expr) = primary("<hello>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn parse_angle_list() {
        let (rest, expr) = primary("<a b c>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::ArrayLiteral(ref items) if items.len() == 3));
    }

    #[test]
    fn parse_dq_interpolation() {
        let (rest, expr) = primary("\"hello $x world\"").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::StringInterpolation(_)));
    }

    #[test]
    fn primary_memo_reuses_result() {
        reset_primary_memo();
        let (rest, expr) = primary("42").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(42))));
        let (rest2, expr2) = primary("42").unwrap();
        assert_eq!(rest2, "");
        assert!(matches!(expr2, Expr::Literal(Value::Int(42))));
    }

    #[test]
    fn primary_bare_at_sigil_parses_as_anon_array() {
        let (rest, expr) = primary("@").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::ArrayVar(ref n) if n == "__ANON_ARRAY__"));
    }

    #[test]
    fn primary_reports_invalid_qualified_identifier_tail() {
        let err = primary("Foo::").unwrap_err();
        assert!(err.message().contains("identifier after '::'"));
    }

    #[test]
    fn primary_reports_missing_listop_argument() {
        let err = primary("shift :").unwrap_err();
        assert!(err.message().contains("listop argument expression"));
    }

    #[test]
    fn primary_reports_invalid_reduction_list_item() {
        let err = primary("[+] 1, :").unwrap_err();
        assert!(err.message().contains("reduction list"));
    }

    #[test]
    fn primary_reports_invalid_anon_sub_params() {
        let err = primary("sub ($x,)").unwrap_err();
        assert!(err.message().contains("anonymous sub parameter list/body"));
    }
}
