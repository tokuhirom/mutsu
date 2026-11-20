//! A term's source spelling, kept for RakuAST (ADR-12199).
//!
//! The parser normalizes some terms to an expression the compiler treats like
//! any other: a word list `<a b  c>` is an `ArrayLiteral` of three `Literal`s,
//! indistinguishable from `'a', 'b', 'c'`, and its raw text is gone. RakuAST
//! keeps that text (`QuotedString(processors => <words val>, segments =>
//! ("a b  c",))`), so the parser wraps the term in [`Expr::Spelled`].
//!
//! Only a parse whose tree is read as RakuAST spells (`Str.AST`, a slang
//! production, the analysis views): [`KeepSpellings`] is set around it. An
//! executing parse builds the expression alone (ADR-15760 §2.4), so the tree the
//! finalization lowers and the compiler sees carries no wrapper. The wrapper
//! holds only what RakuAST needs and the `Expr` does not have; the value stays
//! in `expr`, so the two cannot disagree. A parse-time consumer that matches a
//! shape looks through it with [`Expr::peel_parens`].

use std::cell::Cell;

use super::Expr;

thread_local! {
    /// Whether the code running builds spellings: only a parse whose tree is
    /// read as RakuAST does. A nested compilation unit (a module loaded while
    /// a `.AST` parse runs) starts with it off (`parser::unit_state`).
    pub(crate) static KEEP: Cell<bool> = const { Cell::new(false) };
}

/// Whether [`Expr::spelled`] wraps: the parse in progress is read as RakuAST.
// Cost: O(1).
pub(crate) fn kept() -> bool {
    KEEP.with(Cell::get)
}

/// While alive, spellings are built (`keep`) or not; restores the enclosing
/// setting on drop.
pub(crate) struct KeepSpellings(bool);

impl KeepSpellings {
    // Cost: O(1).
    pub(crate) fn enter(keep: bool) -> Self {
        KeepSpellings(KEEP.with(|k| k.replace(keep)))
    }
}

impl Drop for KeepSpellings {
    fn drop(&mut self) {
        KEEP.with(|k| k.set(self.0));
    }
}

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
    /// `!$x` with a user `multi prefix:<!>` in scope: the parser calls the
    /// operator routine (`prefix:<!>($x)`), but RakuAST keeps the operator
    /// application `ApplyPrefix(Prefix("!"))` of a core prefix symbol.
    UserPrefixOperator,
    /// `5!`, or `$x++` with a user `multi postfix:<++>` in scope: the parser
    /// calls the operator routine (`postfix:<!>(5)`), but RakuAST keeps the
    /// operator application `ApplyPostfix(Postfix("!"))`.
    UserPostfixOperator,
    /// `$<x>:exists` / `$0:delete`: subscript adverbs on capture-variable
    /// sugar. The parser evaluates the subscript of `$/` it stands for; RakuAST
    /// keeps the `Var::NamedCapture` / `Var::PositionalCapture` with its
    /// `colonpairs`.
    CaptureAdverbs,
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
    /// The value of a colonpair written inside a hash literal (`{:2b, :!c}`),
    /// whose `(key, value)` entry keeps no pair node: the colonpair family
    /// (`ColonPair::Number`, `::False`, ...) the entry was written as.
    ColonPairValue(crate::ast::BinaryForm),
    /// `0.0i`, `3e2i`: an imaginary literal the parser folds to its Complex
    /// value; the number as written (underscores removed), which RakuAST
    /// keeps as the literal operand of a `Postfix("i")`.
    Imaginary(Box<str>),
    /// `$.meth(args)` / `$.meth: args` / `$.^meth`: the `self` the parser
    /// calls the method on (sigil `$`), or, for `@.meth(args)` /
    /// `%.meth(args)`, the contextualized call itself (sigil `@`/`%`). RakuAST
    /// keeps a public attribute access with arguments
    /// (`Var::Attribute::Public(name => "$.meth", args => ...)`), and
    /// `$.^meth` with its item contextualizer.
    AttributeCall(char),
    /// `.++` / `.--`: a postfix increment or decrement of the topic, which the
    /// parser runs as `$_++`. RakuAST keeps it as a `Term::TopicCall` of the
    /// `Postfix`.
    TopicPostfix,
    /// `@a».^name` / `@a>>.^name`: a metamethod call is not distributed, so the
    /// parser keeps the plain call, while RakuAST wraps its postfix in
    /// `MetaPostfix::Hyper`.
    HyperMetaMethod,
    /// The `$!` a bare `die` / `fail` throws again: the parser supplies it as
    /// the argument, while RakuAST keeps the call without arguments.
    ImplicitError,
    /// `:5($n)`: a radix conversion the parser runs as `UNBASE(5, $n)`, which
    /// RakuAST keeps as a `Term::RadixNumber`.
    RadixNumber,
    /// An entry of a hash literal whose key was not written as a bare
    /// identifier (`"a" => 1`, `1 => 2`): the key as written and the pair's
    /// form. The entry itself keeps only the key's string, while RakuAST keeps
    /// the `ApplyInfix` over the written key.
    HashPairKey {
        key: Box<Expr>,
        form: crate::ast::BinaryForm,
    },
    /// Where the term was written (ADR-10723 Stage 2, expression families):
    /// `::?CLASS` / `::?ROLE` / `::?PACKAGE` / `::?MODULE`: the pseudo-package
    /// as written (the parser keeps `Var("?CLASS")`, the same as `$?CLASS`).
    PseudoPackage(Box<str>),
    /// A zen slice written with the brackets `<>` or `{}` (`@a<>`, `*{}`): the
    /// node is the same decontainerizing zen slice whatever the brackets, but
    /// RakuAST keeps a different subscript class for each.
    ZenBrackets(char),
    /// `x²`: a superscript power, which is the `**` the parser builds but a
    /// `Postfix::Power` in RakuAST.
    SuperscriptPower,
    /// `return Nil`: a written `Nil` argument, which a bare `return` (also a
    /// `Nil` literal in the parser's tree) does not have.
    WrittenNil,
    /// `|<< EXPR` / `|» EXPR`: the hyper slip, which the parser runs as the
    /// plain `|EXPR`; RakuAST keeps `MetaPrefix::Hyper(Prefix("|"))`.
    HyperSlip,
    /// `constant X := EXPR`: an initializer written with `:=`, which a
    /// constant runs the same as `=`; RakuAST keeps `Initializer::Bind`.
    ConstantBind,
    /// `A Z B Z C`, `A Z+ B Z+ C`: a chain of three or more zip operands, which
    /// the parser runs as one `zip(A, B, C, :with(&infix:<+>))` call. RakuAST
    /// keeps the list infix over every operand.
    ZipChain,
    /// `@<a>` / `%<a>`: a named capture read through the written sigil, which
    /// the parser runs as `$<a>.list` / `$<a>.hash`; RakuAST keeps the
    /// `Var::NamedCapture` with that `sigil`.
    CaptureSigil(char),
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

    /// `expr`, wrapped to remember its `spelling` when the parse in progress
    /// keeps spellings ([`kept`]); `expr` alone otherwise.
    // Cost: O(1), plus the cost of `spelling`.
    pub(crate) fn spelled(expr: Expr, spelling: impl FnOnce() -> Spelling) -> Expr {
        if !kept() {
            return expr;
        }
        Expr::Spelled(Box::new(Spelled {
            expr,
            spelling: spelling(),
        }))
    }
}

impl Expr {
    /// `@<name>` (`sigil` `@`) or `%<name>` (`%`): the `.list` / `.hash` of
    /// `$<name>` the parser runs, remembering the written sigil.
    // Cost: O(|name|).
    pub(crate) fn named_capture_sigiled(name: &str, sigil: char) -> Expr {
        Self::spelled(Self::named_capture_call(name, sigil), || {
            Spelling::CaptureSigil(sigil)
        })
    }

    /// The call [`Expr::named_capture_sigiled`] wraps: `$<name>.list` for
    /// `@`, `$<name>.hash` otherwise.
    // Cost: O(|name|).
    pub(crate) fn named_capture_call(name: &str, sigil: char) -> Expr {
        let (method, sugar) = if sigil == '@' {
            ("list", true)
        } else {
            ("hash", false)
        };
        Expr::MethodCall {
            target: Box::new(Expr::CaptureVar(name.to_string())),
            name: crate::symbol::Symbol::intern(method),
            args: Vec::new(),
            modifier: None,
            quoted: false,
            sugar,
        }
    }
}

impl Expr {
    /// The expression with only its [`Expr::Spelled`] wrappers taken off; unlike
    /// [`Expr::peel_parens`] it keeps a written `(...)`.
    // Cost: O(w), w = nested wrappers.
    pub(crate) fn peel_spelled(&self) -> &Expr {
        let mut expr = self;
        while let Expr::Spelled(spelled) = expr {
            expr = &spelled.expr;
        }
        expr
    }
}

impl Expr {
    /// The argument of a `die` as written: the parser groups a lone `<x y>`
    /// to tell it from several arguments, but the source has no parentheses.
    // Cost: O(1).
    pub(crate) fn written_list_arg(&self) -> &Expr {
        match self {
            Expr::Grouped(inner) if matches!(&**inner, Expr::Spelled(s) if matches!(s.spelling, Spelling::Words(_))) => {
                inner
            }
            other => other,
        }
    }

    /// The arguments of a `die` as written: several of them are one
    /// `ArrayLiteral` in the parser, while a lone list (`die <x y>`,
    /// `die (1,2)`) is grouped and stays one argument.
    // Cost: O(1).
    pub(crate) fn written_die_args(&self) -> &[Expr] {
        match self {
            Expr::ArrayLiteral(items) => items,
            Expr::Spelled(spelled) if matches!(spelled.spelling, Spelling::ImplicitError) => &[],
            other => std::slice::from_ref(other.written_list_arg()),
        }
    }

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

/// The `$!` argument of a bare `die` / `fail` (`Spelling::ImplicitError`).
// Cost: O(1).
pub(crate) fn implicit_error() -> Expr {
    Expr::spelled(Expr::Var("!".to_string()), || Spelling::ImplicitError)
}

/// The arguments of a `fail` as written: none for the `$!` a bare one
/// supplies.
// Cost: O(1).
pub(crate) fn written_control_args(arg: &Expr) -> &[Expr] {
    match arg {
        Expr::Spelled(spelled) if matches!(spelled.spelling, Spelling::ImplicitError) => &[],
        other => std::slice::from_ref(other),
    }
}

/// Take every parse-time mark off `expr` and everything inside it: a spelling
/// (ADR-12199) and an imported-term node, which becomes the word it stands for
/// (what `rakuast::bareword::imported_term_expr` lowers a plain one to).
///
/// A tree the compiler builds from source text at compile time (the thunk of
/// a regex literal's `"..."` atom, a signature literal's default) comes straight
/// from a parser entry point and is not finalized like a compilation unit.
// Cost: O(n), n = size of the tree.
pub(crate) fn strip_parse_marks(expr: &mut Expr) {
    use crate::ast_visit::{VisitMut, walk_expr_mut};
    struct Strip;
    impl VisitMut for Strip {
        fn visit_expr_mut(&mut self, expr: &mut Expr) {
            loop {
                match std::mem::replace(expr, Expr::Whatever) {
                    Expr::Spelled(spelled) => *expr = spelled.expr,
                    Expr::Ast(node) => match *node {
                        super::parsed::ParsedExpr::ImportedTerm(word) => {
                            *expr = Expr::BareWord(word);
                        }
                        other => {
                            *expr = Expr::Ast(Box::new(other));
                            break;
                        }
                    },
                    other => {
                        *expr = other;
                        break;
                    }
                }
            }
            walk_expr_mut(self, expr);
        }
    }
    Strip.visit_expr_mut(expr);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn source_identifiers_only_wrap_when_written() {
        // Spellings are built only where a parse is read as RakuAST.
        let _keep = crate::ast::spelled::KeepSpellings::enter(true);
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
        // Spellings are built only where a parse is read as RakuAST.
        let _keep = crate::ast::spelled::KeepSpellings::enter(true);
        let expr = Expr::spelled(words(), || Spelling::Words("a  b".into()));
        assert!(matches!(expr, Expr::Spelled(_)));
        assert!(matches!(expr.peel_parens(), Expr::ArrayLiteral(items) if items.len() == 2));
    }
}
