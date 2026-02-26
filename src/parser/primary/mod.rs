mod container;
mod ident;
pub(in crate::parser) mod misc;
mod number;
pub(crate) mod regex;
pub(in crate::parser) mod string;
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
pub(in crate::parser) fn current_line_number(input: &str) -> i64 {
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
pub(in crate::parser) use misc::{colonpair_expr, parse_block_body};
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

        try_primary!(number::dot_decimal(input));
        try_primary!(number::decimal(input));
        try_primary!(number::integer(input));
        try_primary!(number::unicode_numeric_literal(input));
        try_primary!(string::single_quoted_string(input));
        try_primary!(string::smart_single_quoted_string(input));
        try_primary!(string::double_quoted_string(input));
        try_primary!(string::smart_double_quoted_string(input));
        try_primary!(string::big_q_string(input));
        try_primary!(string::qx_string(input));
        try_primary!(string::q_string(input));
        try_primary!(string::corner_bracket_string(input));
        try_primary!(regex::regex_lit(input));
        try_primary!(regex::version_lit(input));
        try_primary!(ident::keyword_literal(input));
        try_primary!(regex::topic_method_call(input));
        try_primary!(container::itemized_paren_expr(input));
        try_primary!(container::itemized_bracket_expr(input));
        try_primary!(container::itemized_brace_expr(input));
        try_primary!(var::scalar_var(input));
        try_primary!(var::array_var(input));
        try_primary!(container::percent_hash_literal(input));
        try_primary!(var::hash_var(input));
        try_primary!(var::code_var(input));
        try_primary!(container::paren_expr(input));
        try_primary!(misc::reduction_op(input));
        try_primary!(container::array_literal(input));
        try_primary!(container::double_angle_list(input));
        try_primary!(container::angle_list(input));
        try_primary!(container::french_quote_list(input));
        try_primary!(ident::whatever(input));
        try_primary!(misc::capture_literal(input));
        try_primary!(misc::arrow_lambda(input));
        try_primary!(misc::block_or_hash_expr(input));
        // ::Foo class literal (type object reference)
        try_primary!(ident::class_literal(input));
        try_primary!(misc::colonpair_expr(input));
        // anonymous role: role { ... }
        try_primary!(misc::anon_role_expr(input));
        // anonymous class: class { ... }
        try_primary!(misc::anon_class_expr(input));
        // anonymous grammar: grammar { ... }
        try_primary!(misc::anon_grammar_expr(input));
        try_primary!(ident::declared_circumfix_op(input));
        try_primary!(ident::declared_term_symbol(input));

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
    fn parse_percent_hash_literal_colonpair() {
        let (rest, expr) = primary("%(:a)").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Hash(pairs) => {
                assert_eq!(pairs.len(), 1);
                assert_eq!(pairs[0].0, "a");
                assert!(matches!(pairs[0].1, Some(Expr::Literal(Value::Bool(true)))));
            }
            _ => panic!("expected hash literal"),
        }
    }

    #[test]
    fn parse_brace_hash_literal_mixed_pair_and_positional_items() {
        let (rest, expr) = primary("{ a => 1, \"b\", 2 }").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Hash(pairs) => {
                assert_eq!(pairs.len(), 2);
                assert_eq!(pairs[0].0, "a");
                assert!(matches!(pairs[0].1, Some(Expr::Literal(Value::Int(1)))));
                assert_eq!(pairs[1].0, "b");
                assert!(matches!(pairs[1].1, Some(Expr::Literal(Value::Int(2)))));
            }
            _ => panic!("expected hash literal"),
        }
    }

    #[test]
    fn parse_brace_hash_literal_pair_with_expression_value() {
        let (rest, expr) = primary("{ status => * != 0 }").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Hash(pairs) => {
                assert_eq!(pairs.len(), 1);
                assert_eq!(pairs[0].0, "status");
                assert!(pairs[0].1.is_some());
            }
            _ => panic!("expected hash literal"),
        }
    }

    #[test]
    fn parse_itemized_paren_expr() {
        let (rest, expr) = primary("$(1,2)").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::CaptureLiteral(ref items) if items.len() == 1));
    }

    #[test]
    fn parse_itemized_bracket_expr() {
        let (rest, expr) = primary("$[1,2]").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::MethodCall {
                target, name, args, ..
            } => {
                assert_eq!(name, "item");
                assert!(args.is_empty());
                assert!(matches!(*target, Expr::BracketArray(ref elems) if elems.len() == 2));
            }
            _ => panic!("expected itemized bracket method call"),
        }
    }

    #[test]
    fn parse_dq_interpolation() {
        let (rest, expr) = primary("\"hello $x world\"").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::StringInterpolation(_)));
    }

    #[test]
    fn parse_sq_with_escaped_qq_interpolation() {
        let (rest, expr) = primary("'a\\qq[$x]b'").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::StringInterpolation(_)));
    }

    #[test]
    fn parse_big_q_to_heredoc() {
        let src = "Q:to/END/\nhello\nEND\n";
        let (rest, expr) = primary(src).unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello\n"));
    }

    #[test]
    fn parse_big_q_to_angle_heredoc() {
        let src = "Q:to<--END-->;\nhello\n--END--\n";
        let (rest, expr) = primary(src).unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello\n"));
    }

    #[test]
    fn parse_big_q_to_heredoc_with_indented_terminator() {
        let src = "Q:to/END/\nhello\n    END\n";
        let (rest, expr) = primary(src).unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello\n"));
    }

    #[test]
    fn parse_match_regex_with_repetition_modifiers() {
        let (rest1, expr1) = primary("m:2x/ab/").unwrap();
        assert_eq!(rest1, "");
        assert!(matches!(
            expr1,
            Expr::MatchRegex(Value::RegexWithAdverbs {
                pattern: ref s,
                exhaustive: false,
                repeat: Some(2),
                perl5: false,
                ..
            }) if s == "ab"
        ));

        let (rest2, expr2) = primary("m:x(2)/ab/").unwrap();
        assert_eq!(rest2, "");
        assert!(matches!(
            expr2,
            Expr::MatchRegex(Value::RegexWithAdverbs {
                pattern: ref s,
                exhaustive: false,
                repeat: Some(2),
                perl5: false,
                ..
            }) if s == "ab"
        ));
    }

    #[test]
    fn parse_p5_regex_with_adverb() {
        let (rest1, expr1) = primary("m:P5/(?<name>.+)/").unwrap();
        assert_eq!(rest1, "");
        assert!(matches!(
            expr1,
            Expr::MatchRegex(Value::RegexWithAdverbs {
                pattern: ref s,
                perl5: true,
                ..
            }) if s == "(?<name>.+)"
        ));

        let (rest2, expr2) = primary("rx:P5/a/").unwrap();
        assert_eq!(rest2, "");
        assert!(matches!(
            expr2,
            Expr::Literal(Value::RegexWithAdverbs {
                pattern: ref s,
                perl5: true,
                ..
            }) if s == "a"
        ));
    }

    #[test]
    fn parse_match_regex_with_compact_adverbs() {
        let (rest, expr) = primary("ms/ab cd/").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::MatchRegex(Value::Regex(ref s)) if s == ":s ab cd"));
    }

    #[test]
    fn parse_hash_literal_with_semicolon_separator() {
        let (rest, expr) = primary("{ out => \"x\"; }").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Hash(ref pairs) if pairs.len() == 1));
    }

    #[test]
    fn parse_token_term_literal() {
        let (rest, expr) = primary("token { <foo> }").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Regex(ref s)) if s == "<foo>"));
    }

    #[test]
    fn parse_package_stash_lookup_term() {
        let (rest, expr) = primary("A::").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::PseudoStash(ref s) if s == "A::"));
    }

    #[test]
    fn parse_anonymous_role_expr() {
        let (rest, expr) = primary("role { method foo { 1 } }").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::DoStmt(_)));
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
    fn primary_parses_array_match_var() {
        let (rest, expr) = primary("@$/").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::ArrayVar(ref n) if n == "/"));
    }

    #[test]
    fn primary_parses_hash_match_var() {
        let (rest, expr) = primary("%$/").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::HashVar(ref n) if n == "/"));
    }

    #[test]
    fn primary_reports_invalid_qualified_identifier_tail() {
        let (rest, expr) = primary("Foo::").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::PseudoStash(ref s) if s == "Foo::"));
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
    fn primary_reduction_call_style_with_parenthesized_argument() {
        let (rest, expr) = primary("is([+](^20 .grep: *.is-prime), 77)").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args, .. } => {
                assert_eq!(name, "is");
                match &args[0] {
                    Expr::Reduction { expr, .. } => {
                        assert!(matches!(&**expr, Expr::MethodCall { .. }));
                    }
                    _ => panic!("expected reduction expression as first argument"),
                }
            }
            _ => panic!("expected call expression"),
        }
    }

    #[test]
    fn primary_reduction_call_style_with_parenthesized_list_argument() {
        let (rest, expr) = primary("is([===]($foo, $_foo2, $_foo1), 77)").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args, .. } => {
                assert_eq!(name, "is");
                match &args[0] {
                    Expr::Reduction { expr, .. } => {
                        assert!(matches!(&**expr, Expr::ArrayLiteral(_)));
                    }
                    _ => panic!("expected reduction expression as first argument"),
                }
            }
            _ => panic!("expected call expression"),
        }
    }

    #[test]
    fn primary_accepts_cross_reduction_operator() {
        let (rest, expr) = primary("[X] Mu.new X Mu xx 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Reduction { ref op, .. } if op == "X"));
    }

    #[test]
    fn primary_reports_invalid_anon_sub_params() {
        let err = primary("sub ($x,)").unwrap_err();
        assert!(err.message().contains("anonymous sub parameter list/body"));
    }

    #[test]
    fn primary_big_q_bang_delimiter() {
        reset_primary_memo();
        let (rest, expr) = primary("Q!hello!").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn primary_big_q_bracket_delimiter() {
        reset_primary_memo();
        let (rest, expr) = primary("Q{hello world}").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello world"));
    }

    #[test]
    fn primary_big_q_bang_with_newline() {
        reset_primary_memo();
        let (rest, expr) = primary("Q!hello!\n").unwrap();
        assert_eq!(rest, "\n");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn primary_qx_backtick_form() {
        reset_primary_memo();
        let (rest, expr) = primary("qx`pwd`").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Call { ref name, ref args } if name == "QX" && args.len() == 1
        ));
    }

    #[test]
    fn primary_qx_interpolates_command() {
        reset_primary_memo();
        let (rest, expr) = primary("qx{echo $x}").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "QX");
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::StringInterpolation(_)));
            }
            _ => panic!("expected qx call expression"),
        }
    }
}
