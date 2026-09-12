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
    /// The declaration policy that produced this tree, when it came from a
    /// `regex`, `token`, or `rule` declaration. Execution-only prefixes such
    /// as `:ratchet` remain outside the source tree and are applied by the
    /// execution lowerer.
    #[serde(default)]
    pub(crate) declaration_kind: Option<RegexDeclKind>,
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
        let body = parser.parse_alternation(&[], declaration)?;
        parser.skip_whitespace();
        (parser.pos == parser.chars.len()).then_some(Self {
            body: sequence_for_multichar_literal(body),
            match_immediately: false,
            adverbs: Vec::new(),
            declaration_kind: None,
        })
    }

    pub(crate) fn to_source(&self) -> String {
        self.body.to_source()
    }

    /// Lower the static source tree into the execution matcher plan.
    ///
    /// The runtime parser still owns every construct that needs package state,
    /// interpolation, or code evaluation.  This deliberately small bridge is
    /// for the source forms that `parse_static` can prove are structural only;
    /// returning `None` keeps those forms on the established parser path.
    pub(crate) fn lower_execution(
        &self,
        ratchet: bool,
        ignore_case: bool,
        ignore_mark: bool,
    ) -> Option<crate::runtime::RegexPattern> {
        fn token(
            atom: crate::runtime::RegexAtom,
            quant: crate::runtime::RegexQuant,
            ratchet: bool,
        ) -> crate::runtime::RegexToken {
            crate::runtime::RegexToken {
                atom,
                quant,
                named_capture: None,
                secondary_named_capture: None,
                hash_capture: None,
                force_list_capture: false,
                ratchet,
                frugal: false,
                separator: None,
                from_runtime_interpolation: false,
            }
        }

        fn pattern(
            tokens: Vec<crate::runtime::RegexToken>,
            ignore_case: bool,
            ignore_mark: bool,
        ) -> crate::runtime::RegexPattern {
            crate::runtime::RegexPattern {
                tokens,
                anchor_start: false,
                anchor_end: false,
                ignore_case,
                ignore_mark,
            }
        }

        fn lower_node(
            node: &RegexNode,
            ratchet: bool,
            ignore_case: bool,
            ignore_mark: bool,
            rule_sigspace: bool,
        ) -> Option<Vec<crate::runtime::RegexToken>> {
            match node {
                RegexNode::Literal(text) => {
                    // These characters are syntax in an unquoted runtime
                    // pattern. An escaped spelling is intentionally left to
                    // the established parser because the source tree does
                    // not yet retain whether a literal character was escaped.
                    if text.chars().any(|ch| {
                        matches!(
                            ch,
                            '\u{1}'
                                | '\\'
                                | '#'
                                | '&'
                                | ':'
                                | ';'
                                | '='
                                | '%'
                                | '~'
                                | '{'
                                | '}'
                                | '.'
                                | '^'
                                | '$'
                                | '<'
                                | '>'
                                | '!'
                                | '\''
                                | '"'
                                | '\u{2018}'
                                | '\u{2019}'
                                | '\u{201a}'
                                | '\u{201c}'
                                | '\u{201d}'
                                | '\u{201e}'
                                | '\u{ff62}'
                                | '\u{ff63}'
                                | '\u{00ab}'
                                | '\u{00bb}'
                        )
                    }) {
                        return None;
                    }
                    Some(
                        text.chars()
                            .map(|ch| {
                                token(
                                    crate::runtime::RegexAtom::Literal(ch),
                                    crate::runtime::RegexQuant::One,
                                    ratchet,
                                )
                            })
                            .collect(),
                    )
                }
                RegexNode::Quote(text) => Some(
                    text.chars()
                        .map(|ch| {
                            token(
                                crate::runtime::RegexAtom::Literal(ch),
                                crate::runtime::RegexQuant::One,
                                ratchet,
                            )
                        })
                        .collect(),
                ),
                RegexNode::CharClassDigit => Some(vec![token(
                    crate::runtime::RegexAtom::CharClass(crate::runtime::CharClass {
                        negated: false,
                        items: vec![crate::runtime::ClassItem::Digit],
                    }),
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::Sequence(nodes) => {
                    let mut tokens = Vec::new();
                    for (index, child) in nodes.iter().enumerate() {
                        tokens.extend(lower_node(
                            child,
                            ratchet,
                            ignore_case,
                            ignore_mark,
                            rule_sigspace,
                        )?);
                        // WithWhitespace marks whitespace after its child.
                        // A final wrapper is the RakuAST model's implicit
                        // declaration boundary, not trailing input to consume.
                        if rule_sigspace
                            && index + 1 < nodes.len()
                            && matches!(child, RegexNode::WithWhitespace(_))
                        {
                            tokens.push(token(
                                crate::runtime::RegexAtom::WsRule,
                                crate::runtime::RegexQuant::One,
                                ratchet,
                            ));
                        }
                    }
                    Some(tokens)
                }
                RegexNode::Alternation(branches) => {
                    let alternatives = branches
                        .iter()
                        .map(|branch| {
                            lower_node(branch, ratchet, ignore_case, ignore_mark, rule_sigspace)
                                .map(|tokens| pattern(tokens, ignore_case, ignore_mark))
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(vec![token(
                        crate::runtime::RegexAtom::Alternation(alternatives),
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    )])
                }
                RegexNode::Group(child) => {
                    let tokens =
                        lower_node(child, ratchet, ignore_case, ignore_mark, rule_sigspace)?;
                    Some(vec![token(
                        crate::runtime::RegexAtom::Group(pattern(tokens, ignore_case, ignore_mark)),
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    )])
                }
                RegexNode::Quantified { atom, quantifier } => {
                    let quant = match quantifier {
                        RegexQuantifier::ZeroOrMore => crate::runtime::RegexQuant::ZeroOrMore,
                        RegexQuantifier::OneOrMore => crate::runtime::RegexQuant::OneOrMore,
                        RegexQuantifier::ZeroOrOne => crate::runtime::RegexQuant::ZeroOrOne,
                    };
                    let mut tokens =
                        lower_node(atom, ratchet, ignore_case, ignore_mark, rule_sigspace)?;
                    if tokens.len() == 1 {
                        tokens[0].quant = quant;
                        return Some(tokens);
                    }
                    Some(vec![token(
                        crate::runtime::RegexAtom::Group(pattern(tokens, ignore_case, ignore_mark)),
                        quant,
                        ratchet,
                    )])
                }
                // `WithWhitespace` is a source/model wrapper for ordinary,
                // token, and regex trees. Rule declaration policy consumes it
                // as `WsRule` between terms in the enclosing sequence.
                RegexNode::WithWhitespace(child) => {
                    lower_node(child, ratchet, ignore_case, ignore_mark, rule_sigspace)
                }
            }
        }

        Some(pattern(
            lower_node(
                &self.body,
                ratchet,
                ignore_case,
                ignore_mark,
                self.declaration_kind == Some(RegexDeclKind::Rule),
            )?,
            ignore_case,
            ignore_mark,
        ))
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
            Self::Sequence(nodes) => {
                nodes
                    .iter()
                    .enumerate()
                    .fold(String::new(), |mut source, (index, node)| {
                        if index > 0 && has_whitespace_after(&nodes[index - 1]) {
                            source.push(' ');
                        }
                        source.push_str(&node.to_source());
                        source
                    })
            }
            Self::Alternation(nodes) => {
                nodes
                    .iter()
                    .enumerate()
                    .fold(String::new(), |mut source, (index, node)| {
                        if index > 0 {
                            if has_whitespace_after(&nodes[index - 1]) {
                                source.push(' ');
                            }
                            source.push('|');
                        }
                        source.push_str(&node.to_source());
                        source
                    })
            }
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

fn sequence_for_multichar_literal(node: RegexNode) -> RegexNode {
    match node {
        RegexNode::Literal(text) if text.chars().count() > 1 => {
            RegexNode::Sequence(vec![RegexNode::Literal(text)])
        }
        RegexNode::WithWhitespace(inner) if matches!(inner.as_ref(), RegexNode::Literal(text) if text.chars().count() > 1) => {
            RegexNode::Sequence(vec![RegexNode::WithWhitespace(inner)])
        }
        node => node,
    }
}

fn has_whitespace_after(node: &RegexNode) -> bool {
    matches!(node, RegexNode::WithWhitespace(_))
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
    declaration: bool,
}

impl Parser {
    fn parse_alternation(&mut self, stops: &[char], top_level: bool) -> Option<RegexNode> {
        let mut branches = vec![self.parse_sequence(stops)?];
        while self.consume_if('|') {
            branches.push(self.parse_sequence(stops)?);
        }
        if top_level && let Some(last) = branches.last_mut() {
            wrap_last_node(last);
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
                if saw_whitespace {
                    wrap_last_with_whitespace(&mut nodes);
                }
                break;
            };
            if stops.contains(&ch) || ch == '|' {
                if saw_whitespace {
                    wrap_last_with_whitespace(&mut nodes);
                }
                break;
            }
            let mut atom = self.parse_atom(stops)?;
            if let Some(quantifier) = self.parse_quantifier() {
                // A quantifier binds to the final atom, not to a run of
                // adjacent literal characters (`ab+` means `a` then `b+`).
                // Keep the measured RakuAST shape and let execution lowering
                // preserve the same boundary.
                if let RegexNode::Literal(text) = &mut atom
                    && text.chars().count() > 1
                {
                    let last = text.pop().expect("literal has more than one character");
                    let prefix = std::mem::take(text);
                    atom = RegexNode::Sequence(vec![
                        RegexNode::Literal(prefix),
                        RegexNode::Quantified {
                            atom: Box::new(RegexNode::Literal(last.to_string())),
                            quantifier,
                        },
                    ]);
                    // The quantifier has already been attached to the final
                    // literal in the sequence.
                } else {
                    atom = RegexNode::Quantified {
                        atom: Box::new(atom),
                        quantifier,
                    };
                }
            }

            if self.declaration {
                // WithWhitespace belongs to the term before a written space.
                // The root declaration gets one implicit final wrapper, while
                // nested groups only retain wrappers caused by their own
                // written whitespace. This is the distinction Rakudo exposes
                // for `rule x { a[bc]d }`.
                if saw_whitespace {
                    wrap_last_with_whitespace(&mut nodes);
                }
            } else if saw_whitespace
                && !nodes.is_empty()
                && let Some(previous) = nodes.pop()
            {
                nodes.push(RegexNode::WithWhitespace(Box::new(previous)));
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
                let inner = self.parse_alternation(&[']'], false)?;
                if !self.consume_if(']') {
                    return None;
                }
                Some(RegexNode::Group(Box::new(sequence_for_multichar_literal(
                    inner,
                ))))
            }
            // Parentheses are capture groups in regex slang.  Captures need
            // runtime slot metadata, which this source tree does not retain;
            // leave them on the established execution parser instead of
            // silently turning them into a non-capturing `RegexGroup`.
            '(' => None,
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
                // Quoted escapes can decode codepoints (`\\x20`) or alter
                // quoting (`\\"`).  The current Quote node stores only the
                // decoded-looking text, so it cannot preserve that source
                // distinction for execution lowering.
                '\\' => return None,
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

fn wrap_last_with_whitespace(nodes: &mut [RegexNode]) {
    if let Some(last) = nodes.last_mut()
        && !matches!(last, RegexNode::WithWhitespace(_))
    {
        let node = std::mem::replace(last, RegexNode::Sequence(Vec::new()));
        *last = RegexNode::WithWhitespace(Box::new(node));
    }
}

fn wrap_last_node(node: &mut RegexNode) {
    match node {
        RegexNode::Sequence(nodes) => wrap_last_with_whitespace(nodes),
        other => wrap_node_with_whitespace(other),
    }
}

fn wrap_node_with_whitespace(node: &mut RegexNode) {
    if !matches!(node, RegexNode::WithWhitespace(_)) {
        let old = std::mem::replace(node, RegexNode::Sequence(Vec::new()));
        *node = RegexNode::WithWhitespace(Box::new(old));
    }
}
