//! Quantifiers of the source-level regex tree, modelled after rakudo's
//! `RakuAST::Regex::Quantifier::*` and `RakuAST::Regex::QuantifiedAtom`:
//! `*`, `+`, `?`, the `**` ranges, a backtracking modifier (`+?`, `*!`,
//! `?:`) and a `%` / `%%` separator.
//!
//! Measured on rakudo 2026.09:
//!
//! ```text
//! $ raku -e 'say Q{/a**0^..^5 b+? c+%","/}.AST'
//! ... Quantifier::Range.new(min => 0, excludes-min => True, max => 5, excludes-max => True)
//! ... Quantifier::OneOrMore.new(backtrack => RakuAST::Regex::Backtrack::Frugal)
//! ... QuantifiedAtom.new(atom => ..., quantifier => ..., separator => Regex::Quote...)
//! ```

use super::{Parser, RegexNode};

/// How many times a quantified atom matches.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum QuantifierKind {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    /// `** min..max`; an absent bound is open (`**2..*` has no `max`).
    Range {
        min: Option<u64>,
        max: Option<u64>,
        excludes_min: bool,
        excludes_max: bool,
    },
    /// `** { EXPR }`: the count is the value of a block, as written (boxed, so
    /// a quantifier stays as small as a range).
    Block(Box<super::RegexCode>),
}

/// A backtracking modifier written after a quantifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RegexBacktrack {
    /// `?`
    Frugal,
    /// `!`
    Greedy,
    /// `:`
    Ratchet,
}

impl RegexBacktrack {
    /// What follows the `:` of an atom's own modifier (`a:`, `a:!`, `a:?`).
    // Cost: O(1).
    pub(crate) fn modifier_suffix(self) -> &'static str {
        match self {
            Self::Frugal => "?",
            Self::Greedy => "!",
            Self::Ratchet => "",
        }
    }

    // Cost: O(1).
    pub(crate) fn symbol(self) -> char {
        match self {
            Self::Frugal => '?',
            Self::Greedy => '!',
            Self::Ratchet => ':',
        }
    }
}

/// The `%` (or, `trailing`, `%%`) separator of a quantified atom.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct RegexSeparator {
    pub(crate) node: RegexNode,
    pub(crate) trailing: bool,
}

/// A quantifier with its modifiers.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct RegexQuantifier {
    pub(crate) kind: QuantifierKind,
    #[serde(default)]
    pub(crate) backtrack: Option<RegexBacktrack>,
    #[serde(default)]
    pub(crate) separator: Option<Box<RegexSeparator>>,
}

impl RegexQuantifier {
    /// Regex source for the quantifier written after an atom.
    // Cost: O(s), s = size of the separator.
    pub(crate) fn to_source(&self) -> String {
        let mut source = match &self.kind {
            QuantifierKind::ZeroOrMore => "*".to_string(),
            QuantifierKind::OneOrMore => "+".to_string(),
            QuantifierKind::ZeroOrOne => "?".to_string(),
            QuantifierKind::Block(code) => format!("**{{{}}}", code.code),
            QuantifierKind::Range {
                min,
                max,
                excludes_min,
                excludes_max,
            } => {
                let min = min.unwrap_or(0).to_string();
                let max = max.map_or_else(|| "*".to_string(), |max| max.to_string());
                format!(
                    "**{min}{}..{}{max}",
                    if *excludes_min { "^" } else { "" },
                    if *excludes_max { "^" } else { "" }
                )
            }
        };
        if let Some(backtrack) = self.backtrack {
            // A range takes its modifier right after the `**`.
            let at = if matches!(
                self.kind,
                QuantifierKind::Range { .. } | QuantifierKind::Block(_)
            ) {
                2
            } else {
                source.len()
            };
            source.insert(at, backtrack.symbol());
        }
        if let Some(separator) = &self.separator {
            source.push_str(if separator.trailing { " %% " } else { " % " });
            source.push_str(&separator.node.to_source());
        }
        source
    }
}

impl Parser {
    /// Parse a quantifier and its backtracking modifier after an atom. The
    /// separator is the caller's ([`Self::parse_separator`]).
    // Cost: O(k), k = length of the quantifier.
    pub(super) fn parse_quantifier(&mut self) -> Option<RegexQuantifier> {
        let start = self.pos;
        let mut range_backtrack = None;
        let kind = match self.chars.get(self.pos).copied()? {
            // A range takes its modifier before the range (`**?2..5`).
            '*' if self.chars.get(self.pos + 1) == Some(&'*') => {
                self.pos += 2;
                range_backtrack = self.parse_backtrack();
                self.skip_whitespace();
                match self.parse_range() {
                    Some(kind) => kind,
                    None => {
                        self.pos = start;
                        return None;
                    }
                }
            }
            '*' => {
                self.pos += 1;
                QuantifierKind::ZeroOrMore
            }
            '+' => {
                self.pos += 1;
                QuantifierKind::OneOrMore
            }
            '?' => {
                self.pos += 1;
                QuantifierKind::ZeroOrOne
            }
            _ => return None,
        };
        let backtrack = if matches!(
            kind,
            QuantifierKind::Range { .. } | QuantifierKind::Block(_)
        ) {
            range_backtrack
        } else {
            self.parse_backtrack()
        };
        // A second quantifier character (`+*`, `?+`), or a modifier written
        // twice (`+:?`), is a spelling the tree has no node for. Leave it
        // unconsumed so the whole tree declines: taking only the first `*`
        // let an aliased atom's `$<a>=x**2` become `(x*)*` then a literal `2`
        // (#9198).
        if self
            .chars
            .get(self.pos)
            .is_some_and(|next| matches!(next, '*' | '+' | '?' | '!' | ':'))
        {
            self.pos = start;
            return None;
        }
        Some(RegexQuantifier {
            kind,
            backtrack,
            separator: None,
        })
    }

    // Cost: O(1).
    fn parse_backtrack(&mut self) -> Option<RegexBacktrack> {
        let backtrack = match self.chars.get(self.pos).copied() {
            Some('?') => RegexBacktrack::Frugal,
            Some('!') => RegexBacktrack::Greedy,
            Some(':') => RegexBacktrack::Ratchet,
            _ => return None,
        };
        self.pos += 1;
        Some(backtrack)
    }

    /// The range after `**`: `3`, `2..5`, `2..*`, `^3`, `0^..^5`, or a block
    /// (`**{...}`).
    // Cost: O(k), k = length of the range.
    fn parse_range(&mut self) -> Option<QuantifierKind> {
        if self.chars.get(self.pos) == Some(&'{') {
            let (code, body) = self.parse_code_body()?;
            return Some(QuantifierKind::Block(Box::new(super::RegexCode {
                code,
                body,
            })));
        }
        if self.consume_if('^') {
            let max = self.parse_count()?;
            return Some(QuantifierKind::Range {
                min: None,
                max: Some(max),
                excludes_min: false,
                excludes_max: true,
            });
        }
        let min = self.parse_count()?;
        let excludes_min = self.consume_if('^');
        if !(self.consume_if('.') && self.consume_if('.')) {
            return (!excludes_min).then_some(QuantifierKind::Range {
                min: Some(min),
                max: Some(min),
                excludes_min: false,
                excludes_max: false,
            });
        }
        let excludes_max = self.consume_if('^');
        let max = if self.consume_if('*') {
            None
        } else {
            Some(self.parse_count()?)
        };
        Some(QuantifierKind::Range {
            min: Some(min),
            max,
            excludes_min,
            excludes_max,
        })
    }

    // Cost: O(k), k = number of digits.
    fn parse_count(&mut self) -> Option<u64> {
        let start = self.pos;
        while self.chars.get(self.pos).is_some_and(char::is_ascii_digit) {
            self.pos += 1;
        }
        let digits: String = self.chars[start..self.pos].iter().collect();
        digits.parse().ok()
    }

    /// Parse a `%` / `%%` separator after a quantifier, with the whitespace
    /// written around it. Returns the separator and whether whitespace was
    /// written before the `%`: rakudo then wraps the whole quantified atom in
    /// `WithWhitespace`, while whitespace after the separator wraps the
    /// separator.
    // Cost: O(s), s = length of the separator.
    pub(super) fn parse_separator(&mut self) -> Option<(RegexSeparator, bool)> {
        let start = self.pos;
        self.skip_whitespace();
        let whitespace_before = self.pos != start;
        if !self.consume_if('%') {
            self.pos = start;
            return None;
        }
        let trailing = self.consume_if('%');
        self.skip_whitespace();
        let Some(mut node) = self.parse_atom(&[], false, false) else {
            self.pos = start;
            return None;
        };
        // The separator may itself be quantified (`<w>+ % \s+`), though not
        // separated again.
        if let Some(quantifier) = self.parse_quantifier() {
            node = RegexNode::Quantified {
                atom: Box::new(node),
                quantifier,
            };
        }
        let before = self.pos;
        self.skip_whitespace();
        if self.pos != before {
            node = RegexNode::WithWhitespace(Box::new(node));
        }
        Some((RegexSeparator { node, trailing }, whitespace_before))
    }
}
