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
    /// An identifier written in source whose spelling also resembles an
    /// internal parser name. Only this leaf/call is source-written; its
    /// children retain their own provenance.
    SourceIdentifier,
    /// A labelled bare block, whose execution uses a labelled do-block.
    LabelledBlock,
    /// A source label reference whose executable term is a Label value.
    LabelTerm(Box<str>),
    /// A value lookup parsed in a unit with a runtime export hook.
    ImportedTerm,
    /// Semicolon-separated array composer sections before execution lowering.
    ArraySections(Vec<Vec<Expr>>),
    /// Semicolon-separated sections of a parenthesized list, `(1; 2)`: rakudo's
    /// `SemiList` holds one statement per section, where `(1, 2)` holds one.
    ParenSections(Vec<Vec<Expr>>),
    /// `<a b  c>`: the raw text between the brackets, whitespace included.
    Words(Box<str>),
    /// A parenthesized package declarator adverb, including when its expression
    /// normalizes to the same value as an angle adverb.
    ParenthesizedDeclaratorAdverb,
    /// An attribute trait's direct `[ ... ]` argument, not `([ ... ])`.
    BracketTraitArgument,
    /// `q:to/END/`: the terminator line as written, indentation and newline
    /// included (`    END\n`), and where the body was written (ADR-10723
    /// Stage 2: the origin of its text).
    Heredoc {
        stop: Box<str>,
        #[serde(default)]
        body: crate::ast::SrcSpan,
    },
    /// `qw/a b/`, `qqww/a b/`, `«a b»`: the raw text and the processors the
    /// quote applies to it (`words` or `quotewords`, then optionally `val`).
    WordQuote {
        /// `quotewords` (honours inner quotes) rather than `words`.
        quotewords: bool,
        /// Words become allomorphs (`val`).
        val: bool,
        text: Box<str>,
    },
    /// `«a $b "c d"»`, `qqww/a $b/`, `Qww/"c d"/`: a `quotewords` quote
    /// whose text is nonplain. RakuAST keeps its segments, which conversion
    /// re-derives from the raw text.
    InterpolatingWords {
        /// Words become allomorphs (`val`).
        val: bool,
        text: Box<str>,
        /// The source delimiter for nested `<< >>` or `« »` word quotes.
        delimiter: Option<char>,
        /// Whether unquoted text interpolates (`qqww`/angle quotes, unlike `Qww`).
        interpolates: bool,
    },
    /// `not(EXPR)` / `so(EXPR)`: the tight call form, which the parser folds
    /// into the prefix `!` / `?` but RakuAST keeps as a `Call::Name` of the sub.
    PrefixCall,
    /// `a !< b` / `a !eq b`: the parser evaluates this as `!(a < b)` but
    /// RakuAST keeps the `MetaInfix::Negate` at the infix application.
    NegatedInfix,
    /// `($x) = 1`: an assignment whose variable target the source wrote in
    /// parentheses; rakudo keeps the `Circumfix::Parentheses` (and makes the
    /// assignment a list assignment).
    GroupedLvalue,
    /// `$x [[+]]= 6`: bracket depth of an assignment whose executable form
    /// folds the bracketed infix into an ordinary compound assignment.
    BracketedAssignment { depth: usize },
    /// A C-style loop's comma-separated increment expressions, executed as
    /// a do-block but represented as one list infix in RakuAST.
    LoopStepComma,
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
    /// `MY::(EXPR)`, `GLOBAL::('Int')`: a symbolic lookup qualified by a
    /// written package. The parser concatenates `"PKG::" ~ EXPR`; RakuAST
    /// keeps the package parts and the key expression apart.
    PackageIndirect { package: Box<str>, key: Box<Expr> },
    /// Conflicting built-in subscript adverbs lower to an error call, which
    /// otherwise loses both the index and the colonpair source forms.
    ConflictingSubscript {
        subscript: Box<Expr>,
        pairs: Vec<Expr>,
    },
    /// `:delete(EXPR)` on a multidimensional subscript shares an execution
    /// expansion with `:$delete`, but its RakuAST colonpair has a value.
    ConditionalMultidimDelete {
        source: Box<Expr>,
        condition: Box<Expr>,
    },
    /// A bare Whatever operand of `o` is expanded to a closure for execution;
    /// the public RakuAST keeps the written composition and Whatever argument.
    CompositionWhatever(Box<Expr>),
    /// `TERM .meth`: a method call after whitespace, which rakudo parses as
    /// the dotty infix `.` (`ApplyDottyInfix(TERM, DottyInfix::Call, ...)`);
    /// the wrapped expression is the ordinary method call.
    DottyInfixCall,
    /// `$?LINE`, `$?FILE`, `$?TABSTOP`: a compiler variable the parser folded
    /// into its literal; the written name.
    CompilerVar(Box<str>),
    /// Where the term was written (ADR-10723 Stage 2, expression families):
    /// the byte range its RakuAST node's `origin` answers.
    Origin(crate::ast::SrcSpan),
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
    /// Preserve a source identifier that otherwise resembles a generated name.
    /// The input check excludes anonymous variables minted while parsing a
    /// bare sigil and helpers synthesized by a keyword parser.
    // Cost: O(k), k = identifier length.
    pub(crate) fn source_identifier(expr: Expr, written: &str) -> Expr {
        if !KEEPING.with(Cell::get) {
            return expr;
        }
        let source_name = match &expr {
            Expr::Var(name) => written.strip_prefix('$').map(|rest| (name.as_str(), rest)),
            Expr::ArrayVar(name) => written.strip_prefix('@').map(|rest| (name.as_str(), rest)),
            Expr::HashVar(name) => written.strip_prefix('%').map(|rest| (name.as_str(), rest)),
            Expr::CodeVar(name) => written.strip_prefix('&').map(|rest| (name.as_str(), rest)),
            Expr::Call { name, .. } | Expr::UserRoutineCall { name, .. } => {
                Some((name.as_str(), written))
            }
            _ => None,
        };
        if source_name.is_some_and(|(name, text)| name.starts_with("__") && text.starts_with(name))
        {
            Self::spelled(expr, || Spelling::SourceIdentifier)
        } else {
            expr
        }
    }

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

impl Expr {
    /// The term itself, without the origin a `.AST` parse attaches around
    /// it once the unit is parsed (ADR-10723 Stage 2, `parser::term_origins`).
    // Cost: O(d), d = nested origin wrappers.
    pub(crate) fn unpositioned_mut(&mut self) -> &mut Expr {
        if !matches!(self, Expr::Spelled(spelled) if matches!(spelled.spelling, Spelling::Origin(_)))
        {
            return self;
        }
        match self {
            Expr::Spelled(spelled) => spelled.expr.unpositioned_mut(),
            _ => unreachable!("checked above"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn source_identifiers_only_wrap_when_requested_and_written() {
        let plain = Expr::source_identifier(Expr::Var("__x".into()), "$__x");
        assert!(matches!(plain, Expr::Var(_)));
        let _guard = keep_spelling(true);
        let written = Expr::source_identifier(Expr::Var("__x".into()), "$__x");
        assert!(matches!(written, Expr::Spelled(_)));
        let anonymous = Expr::source_identifier(Expr::Var("__ANON_STATE_1__".into()), "$");
        assert!(matches!(anonymous, Expr::Var(_)));
        let synthesized = Expr::source_identifier(Expr::Var("__with_tmp_0".into()), "with");
        assert!(matches!(synthesized, Expr::Var(_)));
    }

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
