//! The source-level regex tree shared by the parser, RakuAST, and execution
//! lowering (ADR-0088).
//!
//! This is deliberately smaller than the runtime matcher plan.  It records
//! source constructs that must remain visible to RakuAST, while unsupported
//! dynamic constructs stay on the existing string-based execution path until
//! their own tree nodes are implemented.

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct RegexTree {
    pub(crate) body: RegexNode,
    #[serde(default)]
    pub(crate) match_immediately: bool,
    #[serde(default)]
    pub(crate) adverbs: Vec<RegexAdverb>,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, Default, serde::Serialize, serde::Deserialize,
)]
pub(crate) enum RegexDeclKind {
    #[default]
    Token,
    Rule,
    Regex,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct RegexAdverb {
    pub(crate) name: String,
    pub(crate) argument: Option<String>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RegexNode {
    Literal(String),
    Quote(String),
    Sequence(Vec<RegexNode>),
    Alternation(Vec<RegexNode>),
    Group(Box<RegexNode>),
    Quantified {
        atom: Box<RegexNode>,
        quantifier: RegexQuantifier,
    },
    CharClassDigit,
    WithWhitespace(Box<RegexNode>),
}

#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RegexQuantifier {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

impl RegexTree {
    /// Parse the static subset whose source shape is currently needed by the
    /// RakuAST boundary. Returning `None` is intentional: execution still
    /// receives the original pattern, while the converter reports an honest
    /// unsupported boundary for a construct without a source tree.
    pub(crate) fn parse_static(source: &str, declaration: bool) -> Option<Self> {
        let mut parser = Parser {
            chars: source.chars().collect(),
            pos: 0,
            declaration,
        };
        let body = parser.parse_alternation(&[])?;
        parser.skip_whitespace();
        (parser.pos == parser.chars.len()).then_some(Self {
            body: if !declaration
                && let RegexNode::Literal(text) = &body
                && text.chars().count() > 1
            {
                RegexNode::Sequence(vec![body])
            } else {
                body
            },
            match_immediately: false,
            adverbs: Vec::new(),
        })
    }

    pub(crate) fn to_source(&self) -> String {
        self.body.to_source()
    }
}

impl RegexNode {
    fn to_source(&self) -> String {
        match self {
            Self::Literal(text) => text
                .chars()
                .flat_map(|ch| {
                    if matches!(ch, '\\' | '|' | '+' | '*' | '?' | '(' | ')' | '[' | ']') {
                        vec!['\\', ch]
                    } else {
                        vec![ch]
                    }
                })
                .collect(),
            Self::Quote(text) => {
                let escaped = text.replace('\\', "\\\\").replace('"', "\\\"");
                format!("\"{escaped}\"")
            }
            Self::Sequence(nodes) => nodes
                .iter()
                .map(Self::to_source)
                .collect::<Vec<_>>()
                .join(" "),
            Self::Alternation(nodes) => nodes
                .iter()
                .map(Self::to_source)
                .collect::<Vec<_>>()
                .join(" | "),
            Self::Group(child) => format!("[{}]", child.to_source()),
            Self::Quantified { atom, quantifier } => {
                let suffix = match quantifier {
                    RegexQuantifier::ZeroOrMore => '*',
                    RegexQuantifier::OneOrMore => '+',
                    RegexQuantifier::ZeroOrOne => '?',
                };
                format!("{}{}", atom.to_source(), suffix)
            }
            Self::CharClassDigit => "\\d".to_string(),
            Self::WithWhitespace(child) => child.to_source(),
        }
    }
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
    declaration: bool,
}

impl Parser {
    fn parse_alternation(&mut self, stops: &[char]) -> Option<RegexNode> {
        let mut branches = vec![self.parse_sequence(stops)?];
        while self.consume_if('|') {
            branches.push(self.parse_sequence(stops)?);
        }
        if branches.len() == 1 {
            Some(branches.pop().unwrap())
        } else {
            Some(RegexNode::Alternation(branches))
        }
    }

    fn parse_sequence(&mut self, stops: &[char]) -> Option<RegexNode> {
        let mut nodes = Vec::new();
        loop {
            let before = self.pos;
            self.skip_whitespace();
            let saw_whitespace = self.pos != before;
            let Some(&ch) = self.chars.get(self.pos) else {
                break;
            };
            if stops.contains(&ch) || ch == '|' {
                break;
            }
            let mut atom = self.parse_atom(stops)?;
            if let Some(quantifier) = self.parse_quantifier() {
                atom = RegexNode::Quantified {
                    atom: Box::new(atom),
                    quantifier,
                };
            }

            if self.declaration || saw_whitespace && !nodes.is_empty() {
                // In a declaration, sigspace applies to every top-level atom.
                // In an ordinary regex, whitespace belongs to the atom before
                // the next source atom (`/a b/` -> WithWhitespace(a), b).
                if self.declaration {
                    atom = RegexNode::WithWhitespace(Box::new(atom));
                } else if let Some(previous) = nodes.pop() {
                    nodes.push(RegexNode::WithWhitespace(Box::new(previous)));
                }
            }
            nodes.push(atom);
        }

        if nodes.is_empty() {
            return None;
        }
        if nodes.len() == 1 {
            Some(nodes.pop().unwrap())
        } else {
            Some(RegexNode::Sequence(nodes))
        }
    }

    fn parse_atom(&mut self, stops: &[char]) -> Option<RegexNode> {
        let ch = *self.chars.get(self.pos)?;
        match ch {
            '"' | '\'' => self.parse_quote(ch),
            '\\' => self.parse_escape(),
            '[' => {
                self.pos += 1;
                let inner = self.parse_alternation(&[']'])?;
                if !self.consume_if(']') {
                    return None;
                }
                Some(RegexNode::Group(Box::new(inner)))
            }
            '(' => {
                self.pos += 1;
                let inner = self.parse_alternation(&[')'])?;
                if !self.consume_if(')') {
                    return None;
                }
                Some(RegexNode::Group(Box::new(inner)))
            }
            ')' | ']' if stops.contains(&ch) => None,
            '|' | '+' | '*' | '?' | '.' | '^' | '$' | '<' | '>' => None,
            _ => self.parse_literal(),
        }
    }

    fn parse_quote(&mut self, quote: char) -> Option<RegexNode> {
        self.pos += 1;
        let mut text = String::new();
        while let Some(ch) = self.chars.get(self.pos).copied() {
            self.pos += 1;
            match ch {
                c if c == quote => return Some(RegexNode::Quote(text)),
                '\\' => {
                    let escaped = self.chars.get(self.pos).copied()?;
                    self.pos += 1;
                    text.push(escaped);
                }
                _ => text.push(ch),
            }
        }
        None
    }

    fn parse_escape(&mut self) -> Option<RegexNode> {
        self.pos += 1;
        let escaped = self.chars.get(self.pos).copied()?;
        self.pos += 1;
        match escaped {
            'd' => Some(RegexNode::CharClassDigit),
            // A quoted escaped character is still a static literal. The
            // runtime parser remains authoritative for all other escapes.
            c if c.is_ascii_punctuation() => Some(RegexNode::Literal(c.to_string())),
            _ => None,
        }
    }

    fn parse_literal(&mut self) -> Option<RegexNode> {
        let start = self.pos;
        while let Some(ch) = self.chars.get(self.pos).copied() {
            if ch.is_whitespace() || matches!(ch, '|' | '+' | '*' | '?' | '(' | ')' | '[' | ']') {
                break;
            }
            self.pos += 1;
        }
        (self.pos > start).then(|| RegexNode::Literal(self.chars[start..self.pos].iter().collect()))
    }

    fn parse_quantifier(&mut self) -> Option<RegexQuantifier> {
        let quantifier = match self.chars.get(self.pos).copied()? {
            '*' => RegexQuantifier::ZeroOrMore,
            '+' => RegexQuantifier::OneOrMore,
            '?' => RegexQuantifier::ZeroOrOne,
            _ => return None,
        };
        self.pos += 1;
        Some(quantifier)
    }

    fn skip_whitespace(&mut self) {
        while self
            .chars
            .get(self.pos)
            .is_some_and(|ch| ch.is_whitespace())
        {
            self.pos += 1;
        }
    }

    fn consume_if(&mut self, expected: char) -> bool {
        if self.chars.get(self.pos).copied() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
}
