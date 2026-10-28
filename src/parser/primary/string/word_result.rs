//! Word-quote result construction shared with the RakuAST lowerer.
use super::helpers::literal_str;
use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::Value;

/// The `List` a word-quoting construct (`qw<a b c>`, `<a b c>`, `qqw//`, `q:w//`)
/// evaluates to.
///
/// Spelled with a RESERVED name rather than `list`, because a word list is a
/// LITERAL, not a call the program can intercept: `sub list` is an ordinary
/// routine name a Raku program may declare (`Crane::List` declares a whole
/// `multi sub list` set), and emitting a plain `list(...)` call made every word
/// list in that compilation unit resolve to the user's routine —
/// `sub list($a, $b) { }; my $x = qw<a b c>` died with
/// "Too many positionals passed; expected 2 arguments but got 3". Every other
/// lowering the parser synthesizes already uses a `__mutsu_`-prefixed name for
/// exactly this reason (`__mutsu_word_list`).
pub(crate) fn make_list_expr(items: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_word_list"),
        args: items,
        listop: false,
    }
}

pub(crate) fn make_word_result_expr(items: Vec<Expr>) -> Expr {
    match <[Expr; 1]>::try_from(items) {
        Ok([item]) => item,
        Err(items) => make_list_expr(items),
    }
}

/// `:val` may produce one allomorph; plain `qw` always produces a List.
// Cost: O(1), the elements are moved into their result expression.
pub(crate) fn make_processed_word_result(items: Vec<Expr>, val: bool) -> Expr {
    if val {
        make_word_result_expr(items)
    } else {
        make_list_expr(items)
    }
}

/// Whether the text of a word quote is only plain words: nothing that
/// interpolates, quotes, nests or escapes, so its RakuAST segment is the one
/// literal text.
// Cost: O(n), n = length of `text`.
pub(crate) fn word_quote_text_is_plain(text: &str) -> bool {
    word_quote_text_is_plain_mode(text, true)
}

// Cost: O(n), n = quoted text length.
pub(crate) fn word_quote_text_is_plain_mode(text: &str, interpolates: bool) -> bool {
    !text.chars().any(|c| {
        if matches!(c, '$' | '@' | '%' | '&') {
            return interpolates;
        }
        matches!(
            c,
            '{' | '}'
                | '\\'
                | '"'
                | '\''
                | '#'
                | ':'
                | '<'
                | '>'
                | '\u{ab}'
                | '\u{bb}'
                | '\u{201c}'
                | '\u{201d}'
                | '\u{201e}'
                | '\u{2018}'
                | '\u{2019}'
                | '\u{201a}'
                | '\u{ff62}'
        )
    })
}

/// The expression a word quote with the given processors and plain `text`
/// evaluates to; the one place the RakuAST lowering and the quote parsers
/// agree on what `words`, `quotewords` and `val` mean.
// Cost: O(n), n = length of `text`.
pub(crate) fn word_quote_expr(quotewords: bool, val: bool, text: &str) -> Expr {
    use crate::parser::primary::container::{angle_word_value, angle_words_subscript_index_expr};
    match (quotewords, val) {
        (true, true) => angle_words_subscript_index_expr(text),
        (true, false) => {
            let flags = crate::parser::primary::quote_adverbs::QuoteFlags {
                quotewords: true,
                ..crate::parser::primary::quote_adverbs::QuoteFlags::q_single()
            };
            make_processed_word_result(
                super::quotewords::parse_quotewords_items(text, &flags).unwrap_or_default(),
                false,
            )
        }
        (false, _) => make_processed_word_result(
            text.split_whitespace()
                .map(|w| {
                    if val {
                        Expr::Literal(angle_word_value(w))
                    } else {
                        Expr::Literal(literal_str(w))
                    }
                })
                .collect(),
            val,
        ),
    }
}

/// The `quotewords` list of `words`, each `(was quoted, its expression)`: a
/// quoted word is the string itself, an unquoted one an allomorph candidate.
/// The one lowering the quote parsers and the RakuAST segments share.
// Cost: O(w), w = words.
pub(crate) fn quotewords_from_words(val: bool, words: Vec<(bool, Expr)>) -> Expr {
    make_processed_word_result(
        words
            .into_iter()
            .map(|(quoted, expr)| {
                if quoted {
                    expr
                } else {
                    quotewords_atom_expr_allomorphic(expr, val)
                }
            })
            .collect(),
        val,
    )
}

pub(crate) fn quotewords_literal_marker(s: String) -> Expr {
    Expr::Literal(Value::scalar(Value::pair(
        "__mutsu_qw_literal".to_string(),
        Value::str(s),
    )))
}

pub(crate) fn quotewords_atom_expr(atom_expr: Expr) -> Expr {
    quotewords_atom_expr_allomorphic(atom_expr, true)
}

pub(crate) fn quotewords_atom_expr_allomorphic(atom_expr: Expr, allomorphic: bool) -> Expr {
    let args = match atom_expr {
        Expr::StringInterpolation(parts) => parts
            .into_iter()
            .map(|part| {
                if let Expr::Literal(lit) = &part
                    && let Some(s) = lit.as_str()
                {
                    quotewords_literal_marker(s.to_string())
                } else {
                    part
                }
            })
            .collect(),
        other => vec![other],
    };
    let func_name = if allomorphic {
        "__mutsu_quotewords_atom"
    } else {
        "__mutsu_words_atom"
    };
    Expr::Call {
        name: Symbol::intern(func_name),
        args,
        listop: false,
    }
}
