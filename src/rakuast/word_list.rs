//! Word quotes: `qw/a b/`, `qqww/a $b/`, `«a b»`, `<<a b>>`, `%h<<$k>>`.
//!
//! The parser builds one typed node per quote ([`ParsedExpr::WordList`], and
//! [`ParsedExpr::WordsAtom`] for an interpolating `words` string split at run
//! time). The reserved calls the runtime executes (`__mutsu_word_list`,
//! `__mutsu_words_atom`, `__mutsu_quotewords_atom`) and the marker that glues
//! literal text to the words around it are spelled only here, in the one
//! lowering every path shares: the executing parse (a kept node), `EVAL` of a
//! string and `EVAL` of the quote's `.AST`.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::parsed::{ParsedExpr, WordItem, WordList, WordSource, WordsAtom};
use crate::scope::NameScope;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// The `List` a word quote evaluates to.
///
/// A RESERVED name rather than `list`, because a word list is a LITERAL, not a
/// call the program can intercept: `sub list` is an ordinary routine name a
/// Raku program may declare, and a plain `list(...)` call made every word list
/// in that compilation unit resolve to the user's routine.
const WORD_LIST: &str = "__mutsu_word_list";
/// An interpolating `words` string, split into plain-string words at run time.
const WORDS_ATOM: &str = "__mutsu_words_atom";
/// An interpolating `quotewords` word or `words` string, split into allomorphs.
const QUOTEWORDS_ATOM: &str = "__mutsu_quotewords_atom";
/// The pair key that marks literal text of a `quotewords` word: it is glued to
/// the values next to it instead of being split at whitespace.
const QW_LITERAL: &str = "__mutsu_qw_literal";

/// The node the parser builds for a word quote.
// Cost: O(1).
pub(crate) fn word_list_expr(items: Vec<WordItem>, val: bool, source: WordSource) -> Expr {
    Expr::Ast(Box::new(ParsedExpr::WordList(WordList {
        items,
        val,
        source,
    })))
}

/// The node the parser builds for an interpolating `words` string.
// Cost: O(1).
pub(crate) fn words_atom_expr(string: Expr, allomorphic: bool) -> Expr {
    Expr::Ast(Box::new(ParsedExpr::WordsAtom(WordsAtom {
        string,
        allomorphic,
    })))
}

/// The expression a word quote with the given `words` evaluates to: a list,
/// except that a `val` quote of one word is the word itself (it may be one
/// allomorph).
// Cost: O(w), w = words.
pub(crate) fn lower_word_list(items: Vec<WordItem>, val: bool) -> Expr {
    let mut items: Vec<Expr> = items.into_iter().map(lower_item).collect();
    if val && items.len() == 1 {
        return items.remove(0);
    }
    Expr::Call {
        name: Symbol::intern(WORD_LIST),
        args: items,
        listop: false,
    }
}

// Cost: O(n), n = parts of the word.
fn lower_item(item: WordItem) -> Expr {
    match item {
        WordItem::Expr(expr) => expr,
        WordItem::Atom {
            string,
            allomorphic,
        } => lower_glued_atom(string, allomorphic),
    }
}

/// An unquoted word of an interpolating `quotewords` quote: the literal runs of
/// its string are marked so they glue to the values beside them.
// Cost: O(p), p = parts of the string.
fn lower_glued_atom(string: Expr, allomorphic: bool) -> Expr {
    let args = match string {
        Expr::StringInterpolation(parts) => parts
            .into_iter()
            .map(|part| {
                if let Expr::Literal(lit) = &part
                    && let Some(text) = lit.as_str()
                {
                    Expr::Literal(Value::scalar(Value::pair(
                        QW_LITERAL.to_string(),
                        Value::str(text.to_string()),
                    )))
                } else {
                    part
                }
            })
            .collect(),
        other => vec![other],
    };
    atom_call(allomorphic, args)
}

/// The call for an interpolating `words` quote: the string is split into words
/// when it runs, into allomorphs when `allomorphic`.
// Cost: O(1).
pub(super) fn lower_words_atom(string: Expr, allomorphic: bool) -> Expr {
    atom_call(allomorphic, vec![string])
}

fn atom_call(allomorphic: bool, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern(if allomorphic {
            QUOTEWORDS_ATOM
        } else {
            WORDS_ATOM
        }),
        args,
        listop: false,
    }
}

/// The `quotewords` list of `words`, each `(was quoted, its expression)`: a
/// quoted word is the string itself, an unquoted one an allomorph candidate.
// Cost: O(w), w = words.
pub(super) fn lower_quotewords(val: bool, words: Vec<(bool, Expr)>) -> Expr {
    lower_word_list(
        words
            .into_iter()
            .map(|(quoted, expr)| {
                if quoted {
                    WordItem::Expr(expr)
                } else {
                    WordItem::Atom {
                        string: expr,
                        allomorphic: val,
                    }
                }
            })
            .collect(),
        val,
    )
}

/// The expression a word quote with the given processors evaluates to, over the
/// text a `QuotedString` of one plain literal segment holds.
// Cost: O(n), n = length of `text`.
pub(super) fn lower_word_quote(quotewords: bool, val: bool, text: &str) -> Expr {
    match crate::parser::word_quote_items(quotewords, val, text) {
        Some(items) => lower_word_list(items, val),
        None => Expr::Literal(Value::str(text.to_string())),
    }
}

/// The `QuotedString` of a word quote as written.
// Cost: O(n), n = length of the quote's text.
pub(super) fn list_node(names: &NameScope, list: &WordList) -> Result<RakuAstNode, RuntimeError> {
    match &list.source {
        WordSource::Plain { quotewords, text } => {
            Ok(super::convert::word_quote_with(*quotewords, list.val, text))
        }
        WordSource::Interpolating {
            text,
            delimiter,
            interpolates,
        } => super::word_quote::convert(names, list.val, text, *delimiter, *interpolates),
    }
}

/// The `QuotedString` of an interpolating `words` quote
/// (`processors => ("words",)`, or `<words val>`) over its interpolation
/// segments.
// Cost: O(n), n = size of the string.
pub(super) fn atom_node(names: &NameScope, atom: &WordsAtom) -> Result<RakuAstNode, RuntimeError> {
    let items = match &atom.string {
        Expr::StringInterpolation(items) => items.as_slice(),
        other => std::slice::from_ref(other),
    };
    let segments = items
        .iter()
        .map(|item| {
            Ok(Value::rakuast(Box::new(super::convert::interp_segment(
                names, item,
            )?)))
        })
        .collect::<Result<Vec<_>, RuntimeError>>()?;
    let mut processors = vec![Value::str("words".to_string())];
    if atom.allomorphic {
        processors.push(Value::str("val".to_string()));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(processors),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(segments),
            },
        ],
    })
}
