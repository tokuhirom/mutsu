//! A term's source spelling, kept for RakuAST (ADR-12199).
//!
//! The parser normalizes some terms to an expression the compiler treats like
//! any other: a word list `<a b  c>` is an `ArrayLiteral` of three `Literal`s,
//! indistinguishable from `'a', 'b', 'c'`, and its raw text is gone. RakuAST
//! keeps that text (`QuotedString(processors => <words val>, segments =>
//! ("a b  c",))`), so a parse that was asked to keep spellings wraps the term
//! in [`Expr::Spelled`].
//!
//! Only the `.AST` entry points ask. Every other parse builds the plain
//! expression, so the compiler, the precompilation cache and every analysis
//! see exactly the tree they always saw, and no pass has to take a wrapper off
//! again. The wrapper holds only what RakuAST needs and the `Expr` does not
//! have; the value stays in `expr`, so the two cannot disagree. A parse-time
//! consumer that matches a shape looks through it with [`Expr::peel_parens`].

use super::Expr;
use std::cell::Cell;

/// How a term was written, when that is not recoverable from its value.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum Spelling {
    /// `<a b  c>`: the raw text between the brackets, whitespace included.
    Words(Box<str>),
    /// `q:to/END/`: the terminator line as written, indentation and newline
    /// included (`    END\n`).
    Heredoc { stop: Box<str> },
    /// `qw/a b/`, `qqww/a b/`, `«a b»`: the raw text and the processors the
    /// quote applies to it (`words` or `quotewords`, then optionally `val`).
    WordQuote {
        /// `quotewords` (honours inner quotes) rather than `words`.
        quotewords: bool,
        /// Words become allomorphs (`val`).
        val: bool,
        text: Box<str>,
    },
    /// `«a $b "c d"»`, `qqww/a $b/`: a `quotewords` quote whose text
    /// interpolates or quotes a word. RakuAST keeps its segments, which the
    /// conversion re-derives from the raw text.
    InterpolatingWords {
        /// Words become allomorphs (`val`).
        val: bool,
        text: Box<str>,
    },
    /// `gather say 1`, `try say 1`, `start say 1`, `once say 1`, `BEGIN say 1`:
    /// a statement prefix written over a bare statement. The wrapped
    /// expression is the same prefix over the one-statement block
    /// `gather { say 1 }` makes; RakuAST keeps the statement itself.
    BareStatement,
    /// A subscript whose colonpairs include an unknown adverb. The runtime
    /// uses a CORE-candidate call, which loses the index's source spelling.
    NamedSubscript {
        subscript: Box<Expr>,
        pairs: Vec<Expr>,
    },
}

/// A term and the way it was spelled; see the module documentation.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Spelled {
    pub(crate) expr: Expr,
    pub(crate) spelling: Spelling,
}

thread_local! {
    /// Whether the parse running on this thread keeps spellings.
    static KEEPING: Cell<bool> = const { Cell::new(false) };
}

/// Restores the previous keep-spellings setting when dropped.
#[must_use = "the setting reverts when the guard is dropped"]
pub(crate) struct KeepGuard(bool);

impl Drop for KeepGuard {
    fn drop(&mut self) {
        KEEPING.with(|keeping| keeping.set(self.0));
    }
}

/// Make the parse that runs while the guard lives keep spellings (or not). A
/// parse nested inside it (a module load) sets its own and puts this one back.
// Cost: O(1).
pub(crate) fn keep_spelling(keep: bool) -> KeepGuard {
    KeepGuard(KEEPING.with(|keeping| keeping.replace(keep)))
}

/// Whether the running parse keeps spellings.
// Cost: O(1).
pub(crate) fn keeping() -> bool {
    KEEPING.with(Cell::get)
}

impl Expr {
    /// `expr`, wrapped to remember its `spelling` when the running parse keeps
    /// spellings; `expr` itself otherwise. `spelling` is only built then.
    // Cost: O(1), plus the cost of `spelling` when it is built.
    pub(crate) fn spelled(expr: Expr, spelling: impl FnOnce() -> Spelling) -> Expr {
        if KEEPING.with(Cell::get) {
            Expr::Spelled(Box::new(Spelled {
                expr,
                spelling: spelling(),
            }))
        } else {
            expr
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    fn words() -> Expr {
        Expr::ArrayLiteral(vec![
            Expr::Literal(Value::str_from("a")),
            Expr::Literal(Value::str_from("b")),
        ])
    }

    #[test]
    fn a_plain_parse_builds_no_wrapper() {
        let expr = Expr::spelled(words(), || panic!("the spelling must not be built"));
        assert!(matches!(expr, Expr::ArrayLiteral(_)));
    }

    #[test]
    fn a_keeping_parse_wraps_and_peels_back() {
        let _guard = keep_spelling(true);
        let expr = Expr::spelled(words(), || Spelling::Words("a  b".into()));
        assert!(matches!(expr, Expr::Spelled(_)));
        assert!(matches!(expr.peel_parens(), Expr::ArrayLiteral(items) if items.len() == 2));
    }

    #[test]
    fn the_guard_restores_the_outer_setting() {
        let outer = keep_spelling(true);
        {
            let _nested = keep_spelling(false);
            assert!(matches!(
                Expr::spelled(words(), || unreachable!()),
                Expr::ArrayLiteral(_)
            ));
        }
        assert!(matches!(
            Expr::spelled(words(), || Spelling::Words("a b".into())),
            Expr::Spelled(_)
        ));
        drop(outer);
        assert!(matches!(
            Expr::spelled(words(), || unreachable!()),
            Expr::ArrayLiteral(_)
        ));
    }
}
