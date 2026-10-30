//! A term's source spelling, kept for RakuAST (ADR-12199).
//!
//! The parser normalizes some terms to an expression the compiler treats like
//! any other: a word list `<a b  c>` is an `ArrayLiteral` of three `Literal`s,
//! indistinguishable from `'a', 'b', 'c'`, and its raw text is gone. RakuAST
//! keeps that text (`QuotedString(processors => <words val>, segments =>
//! ("a b  c",))`), so the parser wraps the term in [`Expr::Spelled`].
//!
//! Every parse does: the parser's tree is only ever consumed by the RakuAST
//! conversion, and the lowered tree the compiler sees carries no wrapper. The
//! wrapper holds only what RakuAST needs and the `Expr` does not have; the
//! value stays in `expr`, so the two cannot disagree. A parse-time consumer
//! that matches a shape looks through it with [`Expr::peel_parens`].

use super::Expr;

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
    /// `not(EXPR)` / `so(EXPR)`: the tight call form, which the parser folds
    /// into the prefix `!` / `?` but RakuAST keeps as a `Call::Name` of the sub.
    PrefixCall,
    /// `a !< b` / `a !eq b`: the parser evaluates this as `!(a < b)` but
    /// RakuAST keeps the `MetaInfix::Negate` at the infix application.
    NegatedInfix,
    /// `a !(elem) b`, `a !∈ b`: the `!` meta-negation of a set relation, with the
    /// relation as written after the `!`. The parser evaluates it as `!(a ∈ b)`;
    /// RakuAST keeps `MetaInfix::Negate(Infix("(elem)"))`.
    NegatedSetInfix(Box<str>),
    /// `a ∉ b`, `a ⊄ b`: a precomposed negated set glyph, which rakudo declares
    /// as an infix of its own (`Infix("∉")`). The parser evaluates it as the
    /// junction-threading negation of the positive relation.
    NegatedSetGlyph(Box<str>),
    /// A `;` that terminates no statement (`1;; 2`): rakudo's
    /// `Statement::Empty`, which the statement list otherwise drops.
    EmptyStatement,
    /// `a ^... b`, `a ^…^ b`: a left-exclusive sequence, which rakudo keeps as
    /// one list infix with the written spelling. The parser evaluates it as
    /// the plain sequence followed by `.skip(1)`.
    LeftExclusiveSequence(Box<str>),
    /// A C-style loop's comma-separated increment expressions, executed as
    /// a do-block but represented as one list infix in RakuAST.
    LoopStepComma,
    /// `gather say 1`, `try say 1`, `start say 1`, `once say 1`, `BEGIN say 1`:
    /// a statement prefix written over a bare statement. The wrapped
    /// expression is the same prefix over the one-statement block
    /// `gather { say 1 }` makes; RakuAST keeps the statement itself.
    BareStatement,
    /// `MY::(EXPR)`, `GLOBAL::('Int')`: a symbolic lookup qualified by a
    /// written package. The parser concatenates `"PKG::" ~ EXPR`; RakuAST
    /// keeps the package parts and the key expression apart.
    PackageIndirect { package: Box<str>, key: Box<Expr> },
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

impl Expr {
    /// Preserve a source identifier that otherwise resembles a generated name.
    /// The input check excludes anonymous variables minted while parsing a
    /// bare sigil and helpers synthesized by a keyword parser.
    // Cost: O(k), k = identifier length.
    pub(crate) fn source_identifier(expr: Expr, written: &str) -> Expr {
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

    /// `expr`, wrapped to remember its `spelling`.
    // Cost: O(1), plus the cost of `spelling`.
    pub(crate) fn spelled(expr: Expr, spelling: impl FnOnce() -> Spelling) -> Expr {
        Expr::Spelled(Box::new(Spelled {
            expr,
            spelling: spelling(),
        }))
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
    fn source_identifiers_only_wrap_when_written() {
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
    fn a_spelled_term_wraps_and_peels_back() {
        let expr = Expr::spelled(words(), || Spelling::Words("a  b".into()));
        assert!(matches!(expr, Expr::Spelled(_)));
        assert!(matches!(expr.peel_parens(), Expr::ArrayLiteral(items) if items.len() == 2));
    }
}
