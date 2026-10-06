//! Constructs of the source-level regex tree that no execution plan reads.
//!
//! A pattern runs from its *text* (the matcher parses it again), so the tree
//! exists for the RakuAST boundary and for rebuilding that text. These
//! constructs are only that: the tree models them, [`RegexNode::to_source`]
//! spells them again, and the execution lowering always declines a pattern
//! holding one, so the matcher keeps the text path it always had. Keeping them
//! in one enum means a new construct of this kind is a variant here and its
//! RakuAST conversion, not an arm in every traversal of the tree.
//!
//! Measured on rakudo 2026.09:
//!
//! ```text
//! <?alpha>   Assertion::Lookahead(assertion => Assertion::Named(alpha, capturing => True))
//! <!ww>      the same with negated => True
//! <?[x]>     Assertion::Lookahead(assertion => Assertion::CharClass(...))
//! < a b >    Regex::Quote(QuotedString(processors => ("words",), segments => (" a b ",)))
//! A ~ B C    Sequence(A, Nested(B, C))
//! :my $x = 1;  Regex::Statement(Statement::Expression(VarDeclaration::Simple(...)))
//! $<name>    Regex::BackReference::Named("name")
//! $0         Regex::BackReference::Positional(0)
//! <~~>       Assertion::Recurse
//! a:!        BacktrackModifiedAtom(atom => ..., backtrack => Backtrack::Greedy)
//! "x $y z"   Regex::Quote(QuotedString(segments => (StrLiteral, Var::Lexical, StrLiteral)))
//! a & b      Regex::Conjunction(a, b)           a && b   Regex::SequentialConjunction(a, b)
//! $(1+1)     Regex::Interpolation(sequential => False,
//!                                 var => Contextualizer::Item(StatementSequence(...)))
//! <rx=$r>    Assertion::Alias(name => "rx", assertion => Assertion::InterpolatedVar(...))
//! ```

use super::{RegexBacktrack, RegexNode};
use crate::ast::{Expr, Stmt};

/// See the module documentation.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RegexExtension {
    /// `<?name>`, `<!name>`, `<?.name>`, `<?name(args)>`, `<?[x]>`: a
    /// lookahead over a named assertion or a character class.
    Lookahead {
        negated: bool,
        assertion: Box<RegexNode>,
    },
    /// `< a b >`: a word list, as written between the brackets.
    Words(String),
    /// `A ~ goal expr`: `expr` must match, and `goal` must follow it. `A`
    /// stays the preceding term of the sequence.
    Tilde {
        goal: Box<RegexNode>,
        expr: Box<RegexNode>,
    },
    /// `:my $x = 1;`, `:temp @*x;`: a statement run during the match, as
    /// written (without the leading `:` and the closing `;`).
    Statement { code: String, body: Vec<Stmt> },
    /// `$<name>`: match what the capture `name` matched.
    BackReferenceNamed(String),
    /// `$0`: match what the numbered capture matched.
    BackReferencePositional(u32),
    /// `<~~>`: match the enclosing regex again.
    Recurse,
    /// `a:`, `a:!`, `a:?`: an atom with its own backtracking control.
    BacktrackModified {
        atom: Box<RegexNode>,
        backtrack: RegexBacktrack,
    },
    /// `"x $y z"`: a double-quoted term that interpolates. `source` is the body
    /// as written; `expr` is what the `qq` parser makes of it.
    InterpolatedQuote { source: String, expr: Box<Expr> },
    /// `a & b`: every operand must match the same text.
    Conjunction(Vec<RegexNode>),
    /// `a && b`: the operands are tried in order.
    SequentialConjunction(Vec<RegexNode>),
    /// `$(EXPR)`, `@(EXPR)`, `%(EXPR)`: an interpolated expression, as written
    /// (without the sigil and the parentheses).
    ContextualizedInterpolation {
        sigil: char,
        code: String,
        body: Vec<Stmt>,
        sequential: bool,
    },
    /// `<rx=$r>`, `<foo=[bao]>`: an alias over an assertion that is not a
    /// subrule call.
    Alias {
        alias: String,
        assertion: Box<RegexNode>,
    },
}

impl RegexExtension {
    /// The regex nodes nested in this construct.
    // Cost: O(k), k = number of nested nodes.
    pub(crate) fn children(&self) -> Vec<&RegexNode> {
        match self {
            Self::Lookahead { assertion, .. } | Self::Alias { assertion, .. } => vec![assertion],
            Self::BacktrackModified { atom, .. } => vec![atom],
            Self::Tilde { goal, expr } => vec![goal, expr],
            Self::Conjunction(operands) | Self::SequentialConjunction(operands) => {
                operands.iter().collect()
            }
            Self::Words(_)
            | Self::Statement { .. }
            | Self::BackReferenceNamed(_)
            | Self::BackReferencePositional(_)
            | Self::Recurse
            | Self::InterpolatedQuote { .. }
            | Self::ContextualizedInterpolation { .. } => Vec::new(),
        }
    }

    /// [`Self::children`], mutably.
    // Cost: O(k), k = number of nested nodes.
    pub(crate) fn children_mut(&mut self) -> Vec<&mut RegexNode> {
        match self {
            Self::Lookahead { assertion, .. } | Self::Alias { assertion, .. } => vec![assertion],
            Self::BacktrackModified { atom, .. } => vec![atom],
            Self::Tilde { goal, expr } => vec![goal, expr],
            Self::Conjunction(operands) | Self::SequentialConjunction(operands) => {
                operands.iter_mut().collect()
            }
            Self::Words(_)
            | Self::Statement { .. }
            | Self::BackReferenceNamed(_)
            | Self::BackReferencePositional(_)
            | Self::Recurse
            | Self::InterpolatedQuote { .. }
            | Self::ContextualizedInterpolation { .. } => Vec::new(),
        }
    }

    /// The expression the construct interpolates.
    // Cost: O(1).
    pub(crate) fn interpolation(&self) -> Option<(&str, &Expr)> {
        match self {
            Self::InterpolatedQuote { source, expr } => Some((source, expr)),
            _ => None,
        }
    }

    /// [`Self::interpolation`], with the expression mutable.
    // Cost: O(1).
    pub(crate) fn interpolation_mut(&mut self) -> Option<&mut Expr> {
        match self {
            Self::InterpolatedQuote { expr, .. } => Some(expr),
            _ => None,
        }
    }

    /// The code the construct runs, and its source.
    // Cost: O(1).
    pub(crate) fn code(&self) -> Option<(&str, &[Stmt])> {
        match self {
            Self::Statement { code, body }
            | Self::ContextualizedInterpolation { code, body, .. } => Some((code, body)),
            _ => None,
        }
    }

    /// [`Self::code`], with its statements mutable.
    // Cost: O(1).
    pub(crate) fn code_mut(&mut self) -> Option<(&str, &mut Vec<Stmt>)> {
        match self {
            Self::Statement { code, body }
            | Self::ContextualizedInterpolation { code, body, .. } => Some((code, body)),
            _ => None,
        }
    }

    /// The regex source of the construct.
    // Cost: O(n), n = size of the construct.
    pub(super) fn to_source(&self) -> String {
        match self {
            Self::Lookahead { negated, assertion } => {
                let marker = if *negated { '!' } else { '?' };
                let inner = assertion.to_source();
                format!("<{marker}{}", inner.strip_prefix('<').unwrap_or(&inner))
            }
            Self::Words(text) => format!("<{text}>"),
            // The space after `expr` is written here, not left to the sequence:
            // it separates `expr` from `goal`, so a rule must match whitespace
            // there.
            Self::Tilde { goal, expr } => {
                let space = |node: &RegexNode| {
                    if matches!(node, RegexNode::WithWhitespace(_)) {
                        " "
                    } else {
                        ""
                    }
                };
                format!(
                    "~ {}{}{}{}",
                    goal.to_source(),
                    space(goal),
                    expr.to_source(),
                    space(expr)
                )
            }
            Self::Statement { code, .. } => format!(":{code};"),
            Self::BackReferenceNamed(name) => format!("$<{name}>"),
            Self::BackReferencePositional(index) => format!("${index}"),
            Self::Recurse => "<~~>".to_string(),
            Self::BacktrackModified { atom, backtrack } => {
                format!("{}:{}", atom.to_source(), backtrack.modifier_suffix())
            }
            Self::InterpolatedQuote { source, .. } => format!("\"{source}\""),
            Self::Conjunction(operands) => join_operands(operands, "&"),
            Self::SequentialConjunction(operands) => join_operands(operands, "&&"),
            Self::ContextualizedInterpolation { sigil, code, .. } => format!("{sigil}({code})"),
            Self::Alias { alias, assertion } => {
                let inner = assertion.to_source();
                format!("<{alias}={}", inner.strip_prefix('<').unwrap_or(&inner))
            }
        }
    }
}

/// `operands` joined by `operator`, with the space a `WithWhitespace` operand
/// leaves before it (the same layout the tree gives `|`).
// Cost: O(n), n = size of the operands.
fn join_operands(operands: &[RegexNode], operator: &str) -> String {
    let mut source = String::new();
    for (index, operand) in operands.iter().enumerate() {
        if index > 0 {
            if matches!(operands[index - 1], RegexNode::WithWhitespace(_)) {
                source.push(' ');
            }
            source.push_str(operator);
        }
        source.push_str(&operand.to_source());
    }
    source
}
