//! Parse-side helpers of the word quotes; the node they build and its one
//! lowering live in `rakuast::word_list`.
use super::helpers::literal_str;
use crate::ast::Expr;
use crate::ast::parsed::WordItem;

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

/// The words a word quote with the given processors spells over plain `text`;
/// `None` when a `quotewords` + `val` quote cannot be split (it then stands for
/// its text).
// Cost: O(n), n = length of `text`.
pub(crate) fn word_quote_items(quotewords: bool, val: bool, text: &str) -> Option<Vec<WordItem>> {
    use crate::parser::primary::container::{angle_word_value, angle_words_subscript_items};
    match (quotewords, val) {
        (true, true) => angle_words_subscript_items(text).ok(),
        (true, false) => {
            let flags = crate::parser::primary::quote_adverbs::QuoteFlags {
                quotewords: true,
                ..crate::parser::primary::quote_adverbs::QuoteFlags::q_single()
            };
            Some(super::quotewords::parse_quotewords_items(text, &flags).unwrap_or_default())
        }
        (false, _) => Some(
            text.split_whitespace()
                .map(|w| {
                    WordItem::Expr(Expr::Literal(if val {
                        angle_word_value(w)
                    } else {
                        literal_str(w)
                    }))
                })
                .collect(),
        ),
    }
}
