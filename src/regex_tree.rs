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

fn default_regex_value_sigil() -> char {
    '$'
}

fn default_subrule_alias_capturing() -> bool {
    true
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct SubruleArgs {
    pub(crate) args: Vec<crate::ast::Expr>,
    #[serde(default)]
    pub(crate) source: Option<String>,
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RegexNode {
    Literal(String),
    Quote(String),
    Sequence(Vec<RegexNode>),
    Alternation(Vec<RegexNode>),
    SequentialAlternation(Vec<RegexNode>),
    Group(Box<RegexNode>),
    CapturingGroup(Box<RegexNode>),
    NamedCapture {
        name: String,
        #[serde(default)]
        array: bool,
        regex: Box<RegexNode>,
    },
    Subrule {
        name: String,
        capturing: bool,
        /// `None` is an argument-less subrule; `Some` retains an explicit
        /// argument list, including an empty `()`/`:` list. The runtime
        /// matcher still receives the source spelling and evaluates these
        /// expressions at match time. Box the metadata so the structural
        /// regex tree remains small enough for deeply recursive source
        /// parsing.
        #[serde(default)]
        args: Option<Box<SubruleArgs>>,
    },
    SubruleAlias {
        alias: String,
        name: String,
        /// Whether the aliased subrule keeps its own named capture. The
        /// default preserves the historical `<alias=subrule>` shape; a
        /// dot-prefixed target (`<alias=.subrule>`) suppresses it.
        #[serde(default = "default_subrule_alias_capturing")]
        capturing: bool,
        /// `None` is an argument-less alias; `Some` retains an explicit call
        /// boundary, including an empty argument list.
        #[serde(default)]
        args: Option<Box<SubruleArgs>>,
    },
    Lookaround {
        assertion: Box<RegexNode>,
        negated: bool,
        is_behind: bool,
    },
    NamedLookaround {
        assertion: Box<RegexNode>,
        is_behind: bool,
        capturing: bool,
    },
    Interpolation {
        name: String,
        sequential: bool,
    },
    /// `<$name>`, `<@name>`, or `<%name>` — interpolate the current lexical
    /// value as an indirect subrule. The runtime parser resolves the value
    /// when the regex is matched, so this node is retained for RakuAST but
    /// deliberately stays off the static execution-plan path.
    RegexValueInterpolation {
        name: String,
        sequential: bool,
        /// The lexical sigil is observable in RakuAST. These forms share one
        /// model class, but the runtime parser gives each one type-specific
        /// subrule semantics.
        #[serde(default = "default_regex_value_sigil")]
        sigil: char,
    },
    /// `@name` inside a named lookaround's regex argument. It has the same
    /// RakuAST class as scalar interpolation, but array-valued interpolation
    /// must retain the `@` sigil and stay on the runtime parser path.
    ArrayInterpolation {
        name: String,
        sequential: bool,
    },
    /// `<?@name>` / `<!@name>` — a zero-width assertion over the current
    /// elements of an array interpolation. The runtime parser resolves the
    /// array at match time; the source form is retained for RakuAST.
    ArrayLookaround {
        name: String,
        negated: bool,
    },
    /// `<&name>` / `<&name(...)>` — call a lexical routine and interpolate its
    /// return value as a regex. Execution remains on the legacy parser path
    /// while the argument tree and source are retained for RakuAST.
    Callable {
        name: String,
        args: Vec<crate::ast::Expr>,
        /// Preserve the written argument source for the existing runtime
        /// parser. The internal expression tree is not a general-purpose
        /// source printer, so constructed RakuAST nodes fill this from the
        /// representable argument subset.
        #[serde(default)]
        arg_source: Option<String>,
    },
    /// `<?{ ... }>` / `<!{ ... }>` — a zero-width predicate whose body runs
    /// inline in the real interpreter. Keep both the source spelling and the
    /// parsed statements so RakuAST conversion and execution use one tree.
    CodeAssertion {
        code: String,
        negated: bool,
        body: Vec<crate::ast::Stmt>,
    },
    /// `{ ... }` — an inline code block that participates in matching rather
    /// than asserting zero width. Keep the source spelling and parsed body for
    /// the same shared RakuAST/execution boundary as predicate assertions.
    CodeBlock {
        code: String,
        body: Vec<crate::ast::Stmt>,
    },
    /// `<{ ... }>` — evaluate an inline block and match its result as a
    /// regex. This is distinct from a predicate assertion: the block's value
    /// supplies the pattern to match rather than deciding a zero-width test.
    InterpolatedBlock {
        code: String,
        body: Vec<crate::ast::Stmt>,
        sequential: bool,
    },
    Quantified {
        atom: Box<RegexNode>,
        quantifier: RegexQuantifier,
    },
    AnchorBeginningOfString,
    AnchorBeginningOfLine,
    AnchorEndOfString,
    AnchorEndOfLine,
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
    /// Parse the supported source subset whose shape is currently needed by
    /// the RakuAST boundary. Returning `None` is intentional: execution
    /// still receives the original pattern, while the converter reports an
    /// honest unsupported boundary for a construct without a source tree.
    pub(crate) fn parse_static(source: &str, declaration: bool) -> Option<Self> {
        // Array interpolation keeps the existing match-time runtime path, but
        // its source form is still part of the RakuAST regex tree. The
        // execution lowerer deliberately declines these nodes and the value
        // parser reparses them uncached, so retaining them here does not turn
        // dynamic array contents into a static plan.
        Self::parse_static_with_options(source, declaration, true)
    }

    fn parse_static_with_options(
        source: &str,
        declaration: bool,
        allow_array_interpolation: bool,
    ) -> Option<Self> {
        let mut parser = Parser {
            chars: source.chars().collect(),
            pos: 0,
            declaration,
            allow_array_interpolation,
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

    fn parse_lookaround_body(source: &str) -> Option<Self> {
        Self::parse_static_with_options(source, false, true)
    }

    pub(crate) fn to_source(&self) -> String {
        self.body.to_source()
    }

    /// Return the scalar variables whose values are read by interpolation
    /// nodes. The execution boundary uses this to preserve Regex-valued
    /// interpolation, whose value is itself a regex rather than literal text.
    pub(crate) fn interpolation_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        self.body.collect_interpolation_names(&mut names);
        names
    }

    /// Array-valued interpolation is source-representable, but its execution
    /// semantics are owned by the runtime parser, which resolves the current
    /// array elements for each match. Such trees must not enter the static
    /// plan cache, whose key does not include array contents.
    pub(crate) fn contains_array_interpolation(&self) -> bool {
        self.body.contains_array_interpolation()
    }

    /// Angle value interpolation reads the referenced value while the runtime
    /// parser builds its execution plan. Such a plan must not enter the
    /// source-tree cache, whose key does not include the current value of the
    /// lexical.
    pub(crate) fn contains_regex_value_interpolation(&self) -> bool {
        self.body.contains_regex_value_interpolation()
    }

    /// Anchored patterns with outer lexical interpolation retain the legacy
    /// parser path until the execution plan can resolve the lexical cell
    /// rather than the environment snapshot. This keeps repeated assignments
    /// (for example, interpolation inside a loop) dynamic.
    pub(crate) fn contains_anchor(&self) -> bool {
        self.body.contains_anchor()
    }

    /// Lower the supported source tree into the execution matcher plan.
    ///
    /// The runtime parser still owns every construct that needs package state,
    /// type-sensitive interpolation, or code evaluation. This deliberately
    /// small bridge is for the source forms that `parse_static` can prove are
    /// structurally representable; returning `None` keeps other forms on the
    /// established parser path.
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
                stripped_pattern: std::sync::Arc::new(std::sync::OnceLock::new()),
            }
        }

        /// A combining mark (or a `\r`, which joins a following `\n`) means the
        /// literal spans a grapheme cluster, and a cluster can straddle two
        /// source-tree nodes (`/क्ष+/` is `Literal("क्")` followed by a
        /// quantified `Literal("ष")`, yet the whole cluster is the atom the
        /// `+` applies to). Only the runtime parser re-joins tokens across
        /// that boundary, so hand such a pattern back to it rather than
        /// lowering it codepoint by codepoint here.
        fn spans_a_grapheme_cluster(text: &str) -> bool {
            text.chars()
                .any(|c| c == '\r' || unicode_normalization::char::is_combining_mark(c))
        }

        fn lower_node(
            node: &RegexNode,
            ratchet: bool,
            ignore_case: bool,
            ignore_mark: bool,
            rule_sigspace: bool,
            root: bool,
            anchor_start: &mut bool,
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
                    if spans_a_grapheme_cluster(text) {
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
                RegexNode::Quote(text) => {
                    if spans_a_grapheme_cluster(text) {
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
                RegexNode::CharClassDigit => Some(vec![token(
                    crate::runtime::RegexAtom::CharClass(crate::runtime::CharClass {
                        negated: false,
                        items: vec![crate::runtime::ClassItem::Digit],
                    }),
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::AnchorBeginningOfString => {
                    if root {
                        *anchor_start = true;
                        Some(Vec::new())
                    } else {
                        None
                    }
                }
                RegexNode::AnchorBeginningOfLine => Some(vec![token(
                    crate::runtime::RegexAtom::StartOfLine,
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::AnchorEndOfString => Some(vec![token(
                    crate::runtime::RegexAtom::EndOfString,
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::AnchorEndOfLine => Some(vec![token(
                    crate::runtime::RegexAtom::EndOfLine,
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
                            root && index == 0,
                            anchor_start,
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
                            lower_node(
                                branch,
                                ratchet,
                                ignore_case,
                                ignore_mark,
                                rule_sigspace,
                                false,
                                anchor_start,
                            )
                            .map(|tokens| pattern(tokens, ignore_case, ignore_mark))
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(vec![token(
                        crate::runtime::RegexAtom::Alternation(alternatives),
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    )])
                }
                RegexNode::SequentialAlternation(branches) => {
                    let alternatives = branches
                        .iter()
                        .map(|branch| {
                            lower_node(
                                branch,
                                ratchet,
                                ignore_case,
                                ignore_mark,
                                rule_sigspace,
                                false,
                                anchor_start,
                            )
                            .map(|tokens| pattern(tokens, ignore_case, ignore_mark))
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(vec![token(
                        crate::runtime::RegexAtom::SequentialAlternation(alternatives),
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    )])
                }
                RegexNode::Group(child) => {
                    let tokens = lower_node(
                        child,
                        ratchet,
                        ignore_case,
                        ignore_mark,
                        rule_sigspace,
                        false,
                        anchor_start,
                    )?;
                    Some(vec![token(
                        crate::runtime::RegexAtom::Group(pattern(tokens, ignore_case, ignore_mark)),
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    )])
                }
                RegexNode::CapturingGroup(child) => {
                    let tokens = lower_node(
                        child,
                        ratchet,
                        ignore_case,
                        ignore_mark,
                        rule_sigspace,
                        false,
                        anchor_start,
                    )?;
                    Some(vec![token(
                        crate::runtime::RegexAtom::CaptureGroup(pattern(
                            tokens,
                            ignore_case,
                            ignore_mark,
                        )),
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    )])
                }
                RegexNode::NamedCapture { name, array, regex } => {
                    // A scalar alias around a non-capturing quantified atom
                    // captures the whole run as one Match. This mirrors the
                    // legacy parser's user-alias wrapper; an aliased
                    // CapturingGroup intentionally stays per-iteration.
                    if let RegexNode::Quantified { atom, .. } = regex.as_ref()
                        && !matches!(atom.as_ref(), RegexNode::CapturingGroup(_))
                    {
                        let inner = lower_node(
                            regex,
                            ratchet,
                            ignore_case,
                            ignore_mark,
                            rule_sigspace,
                            false,
                            anchor_start,
                        )?;
                        let mut outer = token(
                            crate::runtime::RegexAtom::Group(pattern(
                                inner,
                                ignore_case,
                                ignore_mark,
                            )),
                            crate::runtime::RegexQuant::One,
                            ratchet,
                        );
                        outer.named_capture = Some(name.clone());
                        outer.force_list_capture =
                            *array && matches!(atom.as_ref(), RegexNode::CapturingGroup(_));
                        return Some(vec![outer]);
                    }
                    let mut tokens = lower_node(
                        regex,
                        ratchet,
                        ignore_case,
                        ignore_mark,
                        rule_sigspace,
                        false,
                        anchor_start,
                    )?;
                    let first = tokens.first_mut()?;
                    first.named_capture = Some(name.clone());
                    first.force_list_capture = *array
                        && match regex.as_ref() {
                            RegexNode::CapturingGroup(_) => true,
                            RegexNode::Quantified { atom, .. } => {
                                matches!(atom.as_ref(), RegexNode::CapturingGroup(_))
                            }
                            _ => false,
                        };
                    Some(tokens)
                }
                RegexNode::Subrule { .. } => {
                    // Bare and dot-suppressed subrules share spelling with
                    // builtin assertions and grammar-local names. Let the
                    // runtime parser resolve that context-sensitive spelling
                    // instead of reducing it to a generic Named atom here.
                    None
                }
                RegexNode::SubruleAlias {
                    alias,
                    name,
                    capturing,
                    args,
                } => Some(vec![token(
                    crate::runtime::RegexAtom::Named(
                        subrule_alias_inner_source(alias, name, *capturing, args).into(),
                    ),
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::Lookaround {
                    assertion,
                    negated,
                    is_behind,
                } => {
                    let mut inner_anchor_start = false;
                    let inner_tokens = lower_node(
                        assertion,
                        ratchet,
                        ignore_case,
                        ignore_mark,
                        rule_sigspace,
                        true,
                        &mut inner_anchor_start,
                    )?;
                    let mut inner = pattern(inner_tokens, ignore_case, ignore_mark);
                    inner.anchor_start = inner_anchor_start;
                    Some(vec![token(
                        crate::runtime::RegexAtom::Lookaround {
                            pattern: inner,
                            negated: *negated,
                            is_behind: *is_behind,
                        },
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    )])
                }
                RegexNode::NamedLookaround {
                    assertion,
                    is_behind,
                    capturing,
                } => {
                    let mut inner_anchor_start = false;
                    let inner_tokens = lower_node(
                        assertion,
                        ratchet,
                        ignore_case,
                        ignore_mark,
                        rule_sigspace,
                        true,
                        &mut inner_anchor_start,
                    )?;
                    let mut inner = pattern(inner_tokens, ignore_case, ignore_mark);
                    inner.anchor_start = inner_anchor_start;
                    let mut lookaround = token(
                        crate::runtime::RegexAtom::Lookaround {
                            pattern: inner,
                            negated: false,
                            is_behind: *is_behind,
                        },
                        crate::runtime::RegexQuant::One,
                        ratchet,
                    );
                    if *capturing {
                        lookaround.named_capture = Some(if *is_behind {
                            "after".to_string()
                        } else {
                            "before".to_string()
                        });
                    }
                    Some(vec![lookaround])
                }
                RegexNode::Interpolation { name, .. } => Some(vec![token(
                    crate::runtime::RegexAtom::VarInterp(name.clone()),
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::ArrayInterpolation { .. }
                | RegexNode::ArrayLookaround { .. }
                | RegexNode::RegexValueInterpolation { .. } => None,
                RegexNode::Callable { .. } => None,
                RegexNode::CodeAssertion {
                    code,
                    negated,
                    body,
                } => Some(vec![token(
                    crate::runtime::RegexAtom::CodeAssertion {
                        code: code.clone(),
                        negated: *negated,
                        is_assertion: true,
                        body: Some(std::sync::Arc::new(body.clone())),
                        code_cache_id: crate::value::next_instance_id(),
                    },
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::CodeBlock { code, body } => Some(vec![token(
                    crate::runtime::RegexAtom::CodeAssertion {
                        code: code.clone(),
                        negated: false,
                        is_assertion: false,
                        body: Some(std::sync::Arc::new(body.clone())),
                        code_cache_id: crate::value::next_instance_id(),
                    },
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::InterpolatedBlock { code, body, .. } => Some(vec![token(
                    crate::runtime::RegexAtom::ClosureInterpolation {
                        code: code.clone(),
                        body: Some(std::sync::Arc::new(body.clone())),
                    },
                    crate::runtime::RegexQuant::One,
                    ratchet,
                )]),
                RegexNode::Quantified { atom, quantifier } => {
                    let quant = match quantifier {
                        RegexQuantifier::ZeroOrMore => crate::runtime::RegexQuant::ZeroOrMore,
                        RegexQuantifier::OneOrMore => crate::runtime::RegexQuant::OneOrMore,
                        RegexQuantifier::ZeroOrOne => crate::runtime::RegexQuant::ZeroOrOne,
                    };
                    let mut tokens = lower_node(
                        atom,
                        ratchet,
                        ignore_case,
                        ignore_mark,
                        rule_sigspace,
                        false,
                        anchor_start,
                    )?;
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
                RegexNode::WithWhitespace(child) => lower_node(
                    child,
                    ratchet,
                    ignore_case,
                    ignore_mark,
                    rule_sigspace,
                    false,
                    anchor_start,
                ),
            }
        }

        let mut anchor_start = false;
        let tokens = lower_node(
            &self.body,
            ratchet,
            ignore_case,
            ignore_mark,
            self.declaration_kind == Some(RegexDeclKind::Rule),
            true,
            &mut anchor_start,
        )?;
        let mut result = pattern(tokens, ignore_case, ignore_mark);
        result.anchor_start = anchor_start;
        Some(result)
    }
}

impl RegexNode {
    fn collect_interpolation_names(&self, names: &mut Vec<String>) {
        match self {
            Self::Interpolation { name, .. } => names.push(name.clone()),
            Self::Sequence(nodes)
            | Self::Alternation(nodes)
            | Self::SequentialAlternation(nodes) => {
                for node in nodes {
                    node.collect_interpolation_names(names);
                }
            }
            Self::Group(child)
            | Self::CapturingGroup(child)
            | Self::Quantified { atom: child, .. }
            | Self::WithWhitespace(child) => child.collect_interpolation_names(names),
            Self::NamedCapture { regex, .. } => regex.collect_interpolation_names(names),
            Self::Lookaround { assertion, .. } => assertion.collect_interpolation_names(names),
            Self::NamedLookaround { assertion, .. } => assertion.collect_interpolation_names(names),
            Self::ArrayInterpolation { .. }
            | Self::ArrayLookaround { .. }
            | Self::RegexValueInterpolation { .. }
            | Self::Callable { .. }
            | Self::CodeAssertion { .. }
            | Self::CodeBlock { .. }
            | Self::InterpolatedBlock { .. } => {}
            Self::Literal(_)
            | Self::Quote(_)
            | Self::Subrule { .. }
            | Self::SubruleAlias { .. }
            | Self::AnchorBeginningOfString
            | Self::AnchorBeginningOfLine
            | Self::AnchorEndOfString
            | Self::AnchorEndOfLine
            | Self::CharClassDigit => {}
        }
    }

    fn contains_array_interpolation(&self) -> bool {
        match self {
            Self::Interpolation { .. } => false,
            Self::ArrayInterpolation { .. } | Self::ArrayLookaround { .. } => true,
            Self::RegexValueInterpolation { .. } => false,
            Self::Callable { .. } => false,
            Self::CodeAssertion { .. }
            | Self::CodeBlock { .. }
            | Self::InterpolatedBlock { .. } => false,
            Self::Sequence(nodes)
            | Self::Alternation(nodes)
            | Self::SequentialAlternation(nodes) => {
                nodes.iter().any(Self::contains_array_interpolation)
            }
            Self::Group(child)
            | Self::CapturingGroup(child)
            | Self::Quantified { atom: child, .. }
            | Self::WithWhitespace(child) => child.contains_array_interpolation(),
            Self::NamedCapture { regex, .. } => regex.contains_array_interpolation(),
            Self::Lookaround { assertion, .. } | Self::NamedLookaround { assertion, .. } => {
                assertion.contains_array_interpolation()
            }
            Self::Literal(_)
            | Self::Quote(_)
            | Self::Subrule { .. }
            | Self::SubruleAlias { .. }
            | Self::AnchorBeginningOfString
            | Self::AnchorBeginningOfLine
            | Self::AnchorEndOfString
            | Self::AnchorEndOfLine
            | Self::CharClassDigit => false,
        }
    }

    fn contains_regex_value_interpolation(&self) -> bool {
        match self {
            Self::RegexValueInterpolation { .. } => true,
            Self::Sequence(nodes)
            | Self::Alternation(nodes)
            | Self::SequentialAlternation(nodes) => {
                nodes.iter().any(Self::contains_regex_value_interpolation)
            }
            Self::Group(child)
            | Self::CapturingGroup(child)
            | Self::Quantified { atom: child, .. }
            | Self::WithWhitespace(child) => child.contains_regex_value_interpolation(),
            Self::NamedCapture { regex, .. } => regex.contains_regex_value_interpolation(),
            Self::Lookaround { assertion, .. } | Self::NamedLookaround { assertion, .. } => {
                assertion.contains_regex_value_interpolation()
            }
            Self::Literal(_)
            | Self::Quote(_)
            | Self::Subrule { .. }
            | Self::SubruleAlias { .. }
            | Self::Interpolation { .. }
            | Self::ArrayInterpolation { .. }
            | Self::ArrayLookaround { .. }
            | Self::Callable { .. }
            | Self::CodeAssertion { .. }
            | Self::CodeBlock { .. }
            | Self::InterpolatedBlock { .. }
            | Self::AnchorBeginningOfString
            | Self::AnchorBeginningOfLine
            | Self::AnchorEndOfString
            | Self::AnchorEndOfLine
            | Self::CharClassDigit => false,
        }
    }

    fn contains_anchor(&self) -> bool {
        match self {
            Self::AnchorBeginningOfString
            | Self::AnchorBeginningOfLine
            | Self::AnchorEndOfString
            | Self::AnchorEndOfLine => true,
            Self::Sequence(nodes)
            | Self::Alternation(nodes)
            | Self::SequentialAlternation(nodes) => nodes.iter().any(Self::contains_anchor),
            Self::Group(child)
            | Self::CapturingGroup(child)
            | Self::Quantified { atom: child, .. }
            | Self::WithWhitespace(child) => child.contains_anchor(),
            Self::NamedCapture { regex, .. } => regex.contains_anchor(),
            Self::Lookaround { assertion, .. } => assertion.contains_anchor(),
            Self::NamedLookaround { assertion, .. } => assertion.contains_anchor(),
            Self::Literal(_)
            | Self::Quote(_)
            | Self::Subrule { .. }
            | Self::SubruleAlias { .. }
            | Self::Interpolation { .. }
            | Self::RegexValueInterpolation { .. }
            | Self::ArrayInterpolation { .. }
            | Self::ArrayLookaround { .. }
            | Self::Callable { .. }
            | Self::CodeAssertion { .. }
            | Self::CodeBlock { .. }
            | Self::InterpolatedBlock { .. }
            | Self::CharClassDigit => false,
        }
    }

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
            Self::SequentialAlternation(nodes) => {
                nodes
                    .iter()
                    .enumerate()
                    .fold(String::new(), |mut source, (index, node)| {
                        if index > 0 {
                            source.push_str("||");
                        }
                        source.push_str(&node.to_source());
                        source
                    })
            }
            Self::Group(child) => format!("[{}]", child.to_source()),
            Self::CapturingGroup(child) => format!("({})", child.to_source()),
            Self::NamedCapture { name, array, regex } => {
                let sigil = if *array { '@' } else { '$' };
                format!("{sigil}<{name}> = {}", regex.to_source())
            }
            Self::Subrule {
                name,
                capturing,
                args,
            } => {
                let prefix = if *capturing { "" } else { "." };
                let Some(args) = args.as_deref() else {
                    return format!("<{prefix}{name}>");
                };
                let rendered = args.source.clone().or_else(|| {
                    args.args
                        .iter()
                        .map(expression_source)
                        .collect::<Option<Vec<_>>>()
                        .map(|parts| parts.join(", "))
                });
                format!("<{prefix}{name}({})>", rendered.unwrap_or_default())
            }
            Self::SubruleAlias {
                alias,
                name,
                capturing,
                args,
            } => format!(
                "<{}>",
                subrule_alias_inner_source(alias, name, *capturing, args)
            ),
            Self::Lookaround {
                assertion,
                negated,
                is_behind,
            } => {
                let polarity = if *negated { '!' } else { '?' };
                let keyword = if *is_behind { "after" } else { "before" };
                format!("<{polarity}{keyword} {}>", assertion.to_source())
            }
            Self::NamedLookaround {
                assertion,
                is_behind,
                capturing,
            } => {
                let prefix = if *capturing { "" } else { "." };
                let keyword = if *is_behind { "after" } else { "before" };
                format!("<{prefix}{keyword} {}>", assertion.to_source())
            }
            Self::Interpolation { name, .. } => format!("${name}"),
            Self::RegexValueInterpolation { name, sigil, .. } => {
                format!("<{sigil}{name}>")
            }
            Self::ArrayInterpolation { name, .. } => format!("@{name}"),
            Self::ArrayLookaround { name, negated } => {
                let marker = if *negated { '!' } else { '?' };
                format!("<{marker}@{name}>")
            }
            Self::Callable {
                name,
                args,
                arg_source,
            } => {
                if args.is_empty() {
                    format!("<&{name}>")
                } else {
                    let rendered = arg_source.clone().or_else(|| {
                        args.iter()
                            .map(expression_source)
                            .collect::<Option<Vec<_>>>()
                            .map(|parts| parts.join(", "))
                    });
                    format!("<&{name}({})>", rendered.unwrap_or_default())
                }
            }
            Self::CodeAssertion { code, negated, .. } => {
                let marker = if *negated { '!' } else { '?' };
                format!("<{marker}{{{code}}}>")
            }
            Self::CodeBlock { code, .. } => format!("{{{code}}}"),
            Self::InterpolatedBlock { code, .. } => format!("<{{{code}}}>"),
            Self::Quantified { atom, quantifier } => {
                let suffix = match quantifier {
                    RegexQuantifier::ZeroOrMore => '*',
                    RegexQuantifier::OneOrMore => '+',
                    RegexQuantifier::ZeroOrOne => '?',
                };
                format!("{}{}", atom.to_source(), suffix)
            }
            Self::AnchorBeginningOfString => "^".to_string(),
            Self::AnchorBeginningOfLine => "^^".to_string(),
            Self::AnchorEndOfString => "$".to_string(),
            Self::AnchorEndOfLine => "$$".to_string(),
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

fn collapse_alternation(nodes: Vec<RegexNode>) -> RegexNode {
    let mut nodes = nodes.into_iter();
    let Some(first) = nodes.next() else {
        return RegexNode::Alternation(Vec::new());
    };
    let Some(second) = nodes.next() else {
        return first;
    };
    let mut alternatives = vec![first, second];
    alternatives.extend(nodes);
    RegexNode::Alternation(alternatives)
}

fn has_whitespace_after(node: &RegexNode) -> bool {
    matches!(node, RegexNode::WithWhitespace(_))
}

/// Render the small expression subset needed when a hand-built RakuAST
/// callable is lowered back to the existing string-based regex parser.
/// Parser-created trees retain their exact argument source in
/// `RegexNode::Callable::arg_source`; this is only the construction fallback.
pub(crate) fn expression_source(expr: &crate::ast::Expr) -> Option<String> {
    fn quote_string(value: &str) -> String {
        let mut out = String::with_capacity(value.len() + 2);
        out.push('\'');
        for ch in value.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '\'' => out.push_str("\\'"),
                _ => out.push(ch),
            }
        }
        out.push('\'');
        out
    }

    fn join_args(args: &[crate::ast::Expr]) -> Option<String> {
        args.iter()
            .map(expression_source)
            .collect::<Option<Vec<_>>>()
            .map(|parts| parts.join(", "))
    }

    match expr {
        crate::ast::Expr::Literal(value) => match value.view() {
            crate::value::ValueView::Str(value) => Some(quote_string(&value)),
            crate::value::ValueView::Int(_)
            | crate::value::ValueView::BigInt(_)
            | crate::value::ValueView::Num(_)
            | crate::value::ValueView::Bool(_)
            | crate::value::ValueView::Rat(..)
            | crate::value::ValueView::FatRat(..)
            | crate::value::ValueView::BigRat(..)
            | crate::value::ValueView::Complex(..)
            | crate::value::ValueView::Nil => Some(value.to_string_value()),
            _ => None,
        },
        crate::ast::Expr::LiteralSrc(_, source) => Some(source.to_string()),
        crate::ast::Expr::Grouped(inner) => Some(format!("({})", expression_source(inner)?)),
        crate::ast::Expr::Var(name) => Some(format!("${name}")),
        crate::ast::Expr::CaptureVar(name) => Some(format!("${name}")),
        crate::ast::Expr::ArrayVar(name) => Some(format!("@{name}")),
        crate::ast::Expr::HashVar(name) => Some(format!("%{name}")),
        crate::ast::Expr::CodeVar(name) => Some(format!("&{name}")),
        crate::ast::Expr::BareWord(name) => Some(name.clone()),
        crate::ast::Expr::Unary { op, expr } => Some(format!(
            "{}{}",
            crate::compiler::helpers_ops::token_kind_to_op_name(op),
            expression_source(expr)?
        )),
        crate::ast::Expr::Binary { left, op, right } => Some(format!(
            "{} {} {}",
            expression_source(left)?,
            crate::compiler::helpers_ops::token_kind_to_op_name(op),
            expression_source(right)?
        )),
        // Keep a dynamic selector grouped when a constructed RakuAST regex is
        // lowered through the established string parser.  Its branches remain
        // expressions for the existing match-time subrule-argument evaluator.
        crate::ast::Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Some(format!(
            "({} ?? {} !! {})",
            expression_source(cond)?,
            expression_source(then_expr)?,
            expression_source(else_expr)?
        )),
        crate::ast::Expr::Call { name, args }
        | crate::ast::Expr::UserRoutineCall { name, args } => {
            Some(format!("{}({})", name.resolve(), join_args(args)?))
        }
        crate::ast::Expr::MethodCall {
            target,
            name,
            args,
            modifier: None,
            quoted: false,
        } => Some(format!(
            "{}.{}({})",
            expression_source(target)?,
            name.resolve(),
            join_args(args)?
        )),
        // Preserve the ordinary dispatch modifiers of a method call when a
        // constructed RakuAST regex returns through the established string
        // parser.  The matcher continues to evaluate the call at match time;
        // rendering must not turn `.?`, `.+`, or `.*` into an unsupported
        // argument expression.
        crate::ast::Expr::MethodCall {
            target,
            name,
            args,
            modifier: Some(modifier @ ('?' | '+' | '*')),
            quoted: false,
        } => Some(format!(
            "{}.{}{}({})",
            expression_source(target)?,
            modifier,
            name.resolve(),
            join_args(args)?
        )),
        // Argumented subrules retain their expressions structurally.  When a
        // constructed RakuAST tree returns through the existing regex parser,
        // preserve an ordinary subscript rather than rejecting the whole
        // assertion at the source-rendering boundary.
        crate::ast::Expr::Index {
            target,
            index,
            is_positional,
        } => Some(format!(
            "{}{}{}{}",
            expression_source(target)?,
            if *is_positional { '[' } else { '{' },
            expression_source(index)?,
            if *is_positional { ']' } else { '}' },
        )),
        crate::ast::Expr::ArrayLiteral(items) => Some(format!("[{}]", join_args(items)?)),
        crate::ast::Expr::BracketArray(items, _) => Some(format!("[{}]", join_args(items)?)),
        crate::ast::Expr::PositionalPair(inner) => Some(format!("({})", expression_source(inner)?)),
        _ => None,
    }
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
    declaration: bool,
    allow_array_interpolation: bool,
}

impl Parser {
    fn parse_alternation(&mut self, stops: &[char], top_level: bool) -> Option<RegexNode> {
        let mut branches = vec![self.parse_sequence(stops, false)?];
        let mut sequential_operators = Vec::new();
        while self.consume_if('|') {
            let sequential = self.consume_if('|');
            sequential_operators.push(sequential);
            branches.push(self.parse_sequence(stops, sequential)?);
        }
        if top_level && let Some(last) = branches.last_mut() {
            wrap_last_node(last);
        }
        // RakuAST represents a multi-character literal branch as a
        // `Sequence`, even when that branch contains only the literal. Apply
        // the same normalization here so `bar||$x` retains the branch shape
        // independently of whether the separator is `|` or `||`.
        branches = branches
            .into_iter()
            .map(sequence_for_multichar_literal)
            .collect();
        if branches.len() == 1 {
            Some(branches.pop().unwrap())
        } else {
            // `||` binds less tightly than `|`: `a | b || c` is a
            // sequential alternation whose first branch is the ordinary
            // alternation `a | b`, while `a || b | c` keeps `b | c` as its
            // second branch. This is also the shape Rakudo exposes through
            // RakuAST::Regex::SequentialAlternation.
            let mut groups = Vec::new();
            let mut branch_iter = branches.into_iter();
            let first = branch_iter.next()?;
            let mut current = vec![first];
            for (sequential, branch) in sequential_operators.into_iter().zip(branch_iter) {
                if sequential {
                    groups.push(collapse_alternation(std::mem::take(&mut current)));
                }
                current.push(branch);
            }
            groups.push(collapse_alternation(current));
            if groups.len() == 1 {
                Some(collapse_alternation(groups))
            } else {
                Some(RegexNode::SequentialAlternation(groups))
            }
        }
    }

    fn parse_sequence(
        &mut self,
        stops: &[char],
        sequential_interpolation: bool,
    ) -> Option<RegexNode> {
        let mut nodes = Vec::new();
        let mut sequential_interpolation = sequential_interpolation;
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
            let mut atom = self.parse_atom(stops, nodes.is_empty(), sequential_interpolation)?;
            // Rakudo marks only the first interpolation after `||`; nested
            // groups and later atoms retain their ordinary non-sequential
            // interpolation shape.
            sequential_interpolation = false;
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

    fn parse_atom(
        &mut self,
        stops: &[char],
        at_sequence_start: bool,
        sequential_interpolation: bool,
    ) -> Option<RegexNode> {
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
            '(' => {
                self.pos += 1;
                let inner = self.parse_alternation(&[')'], false)?;
                if !self.consume_if(')') {
                    return None;
                }
                Some(RegexNode::CapturingGroup(Box::new(
                    sequence_for_multichar_literal(inner),
                )))
            }
            '$' if self.chars.get(self.pos + 1) == Some(&'<') => self.parse_named_capture(false),
            '@' if self.chars.get(self.pos + 1) == Some(&'<') => self.parse_named_capture(true),
            '^' if self.chars.get(self.pos + 1) == Some(&'^') => {
                self.pos += 2;
                Some(RegexNode::AnchorBeginningOfLine)
            }
            '^' if at_sequence_start => {
                self.pos += 1;
                Some(RegexNode::AnchorBeginningOfString)
            }
            '$' if self.chars.get(self.pos + 1) == Some(&'$') => {
                self.pos += 2;
                Some(RegexNode::AnchorEndOfLine)
            }
            '$' if self.chars.get(self.pos + 1).is_none_or(|next| {
                next.is_whitespace() || stops.contains(next) || *next == '|'
            }) =>
            {
                self.pos += 1;
                Some(RegexNode::AnchorEndOfString)
            }
            '$' => self.parse_interpolation(sequential_interpolation),
            '@' if self.allow_array_interpolation => {
                self.parse_array_interpolation(sequential_interpolation)
            }
            '@' | '%' => None,
            ')' | ']' if stops.contains(&ch) => None,
            '|' | '+' | '*' | '?' | '.' | '^' | '>' => None,
            '<' => self
                .parse_lookaround(sequential_interpolation)
                .or_else(|| self.parse_subrule()),
            '{' => self.parse_code_block(),
            _ => self.parse_literal(),
        }
    }

    /// Parse lookaround forms whose source and execution shapes are currently
    /// shared: the explicit `<?before body>`, `<!before body>`, `<?after
    /// body>`, and `<!after body>` forms, plus the unprefixed and dot-prefixed
    /// named forms. The latter retain their capture policy in a separate tree
    /// node because their RakuAST shape omits the Lookahead wrapper. Escaped
    /// characters and ordinary scalar interpolations are accepted when the
    /// nested tree already has a source and execution representation.
    fn parse_lookaround(&mut self, sequential_interpolation: bool) -> Option<RegexNode> {
        let start = self.pos;
        self.pos += 1; // '<'
        let (negated, explicit, capturing) = match self.chars.get(self.pos).copied() {
            Some('?') => {
                self.pos += 1;
                (false, true, true)
            }
            Some('!') => {
                self.pos += 1;
                (true, true, true)
            }
            Some('.') => {
                self.pos += 1;
                (false, false, false)
            }
            _ => (false, false, true),
        };

        // Predicate blocks are source-representable in RakuAST and the
        // existing matcher already evaluates them inline. Interpolated blocks
        // (`<{ ... }>`), and code interpolation remain outside this boundary.
        if explicit && self.chars.get(self.pos) == Some(&'{') {
            return self.parse_code_assertion(negated);
        }

        // `<{ ... }>` evaluates its block and interpolates the resulting value
        // as a regex. `<!{ ... }>` above is the separate predicate assertion
        // form and must remain zero-width.
        if !explicit && self.chars.get(self.pos) == Some(&'{') {
            return self.parse_code_interpolation(sequential_interpolation);
        }

        // `<&name>` and `<&name()>` are callable regex interpolations. Keep
        // only the argument-less form in this slice; a non-empty argument
        // list must continue through the legacy parser until its RakuAST
        // argument tree is represented here.
        if !explicit && self.chars.get(self.pos) == Some(&'&') {
            return self.parse_callable(start);
        }

        // `<$name>`, `<@name>`, and `<%name>` interpolate the current lexical
        // value as an indirect subrule. They share one RakuAST node but the
        // sigil remains observable and gives the runtime parser its
        // type-specific semantics. Keep all three value-sensitive forms on
        // the established parser path.
        if !explicit
            && self
                .chars
                .get(self.pos)
                .is_some_and(|ch| matches!(ch, '$' | '@' | '%'))
        {
            let sigil = self.chars[self.pos];
            self.pos += 1;
            let Some(name) = self.parse_variable_name() else {
                self.pos = start;
                return None;
            };
            if !self.consume_if('>') {
                self.pos = start;
                return None;
            }
            return Some(RegexNode::RegexValueInterpolation {
                name,
                sequential: sequential_interpolation,
                sigil,
            });
        }

        // `<?@name>` and `<!@name>` are the direct array-interpolation
        // assertion forms. They have a distinct RakuAST node from the
        // `<?before @name>` form, even though both are zero-width assertions
        // over the current array value.
        if explicit && self.chars.get(self.pos) == Some(&'@') {
            self.pos += 1;
            let name = self.parse_variable_name()?;
            if !self.consume_if('>') {
                self.pos = start;
                return None;
            }
            return Some(RegexNode::ArrayLookaround { name, negated });
        }

        let is_behind = if self.chars[self.pos..].starts_with(&['a', 'f', 't', 'e', 'r']) {
            self.pos += "after".chars().count();
            true
        } else if self.chars[self.pos..].starts_with(&['b', 'e', 'f', 'o', 'r', 'e']) {
            self.pos += "before".chars().count();
            false
        } else {
            self.pos = start;
            return None;
        };

        if !self
            .chars
            .get(self.pos)
            .is_some_and(|ch| ch.is_whitespace())
        {
            self.pos = start;
            return None;
        }
        self.skip_whitespace();
        let body_start = self.pos;
        let mut quote = None;
        let mut nested_assertions = 0usize;
        while let Some(ch) = self.chars.get(self.pos).copied() {
            if let Some(closer) = quote {
                self.pos += 1;
                if ch == closer {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\\' => {
                    // Skip the escaped character while looking for the
                    // closing angle bracket. The nested parser will decide
                    // whether the escape has a shared-tree representation;
                    // this also prevents an escaped `>` from ending the
                    // assertion prematurely.
                    self.pos += 1;
                    if self.pos < self.chars.len() {
                        self.pos += 1;
                    }
                }
                '"' | '\'' => {
                    quote = Some(ch);
                    self.pos += 1;
                }
                '>' if nested_assertions > 0 => {
                    nested_assertions -= 1;
                    self.pos += 1;
                }
                '>' => break,
                '<' => {
                    // A nested lookaround can contain another assertion in a
                    // static group, for example
                    // `<?before [<?before bar>]>`. Keep scanning through the
                    // inner terminator so the body parser receives the whole
                    // source-level tree. Unsupported angle-bracket forms are
                    // still rejected by `parse_static` below rather than
                    // being mistaken for literals.
                    nested_assertions += 1;
                    self.pos += 1;
                }
                _ => self.pos += 1,
            }
        }
        if self.chars.get(self.pos) != Some(&'>') || quote.is_some() || nested_assertions != 0 {
            self.pos = start;
            return None;
        }
        let body_source: String = self.chars[body_start..self.pos].iter().collect();
        let assertion = RegexTree::parse_lookaround_body(&body_source)?.body;
        if !is_supported_lookaround_body(&assertion) {
            self.pos = start;
            return None;
        }
        self.pos += 1; // '>'
        Some(if explicit {
            RegexNode::Lookaround {
                assertion: Box::new(assertion),
                negated,
                is_behind,
            }
        } else {
            RegexNode::NamedLookaround {
                assertion: Box::new(assertion),
                is_behind,
                capturing,
            }
        })
    }

    fn parse_code_assertion(&mut self, negated: bool) -> Option<RegexNode> {
        let start = self.pos;
        let (code, body) = self.parse_code_body()?;
        if self.chars.get(self.pos) != Some(&'>') {
            self.pos = start;
            return None;
        }
        self.pos += 1; // '>'
        Some(RegexNode::CodeAssertion {
            code,
            negated,
            body,
        })
    }

    fn parse_code_interpolation(&mut self, sequential: bool) -> Option<RegexNode> {
        let start = self.pos;
        let (code, body) = self.parse_code_body()?;
        if self.chars.get(self.pos) != Some(&'>') {
            self.pos = start;
            return None;
        }
        self.pos += 1; // '>'
        Some(RegexNode::InterpolatedBlock {
            code,
            body,
            sequential,
        })
    }

    fn parse_callable(&mut self, start: usize) -> Option<RegexNode> {
        self.pos += 1; // '&'
        let Some(name) = self.parse_variable_name() else {
            self.pos = start;
            return None;
        };
        self.skip_whitespace();
        let mut args = Vec::new();
        let mut arg_source = None;
        if self.consume_if('(') {
            let args_start = self.pos;
            self.skip_whitespace();
            if self.chars.get(self.pos) != Some(&')') {
                let remaining: String = self.chars[self.pos..].iter().collect();
                let Some((rest, parsed_args)) =
                    crate::parser::parse_regex_call_arg_list(&remaining)
                else {
                    self.pos = start;
                    return None;
                };
                let consumed = remaining.chars().count() - rest.chars().count();
                self.pos += consumed;
                args = parsed_args;
                if !args.is_empty() {
                    arg_source = Some(self.chars[args_start..self.pos].iter().collect());
                }
            }
            if !self.consume_if(')') {
                self.pos = start;
                return None;
            }
        } else if self.consume_if(':') {
            let args_start = self.pos;
            self.skip_whitespace();
            let mut paren_depth = 0usize;
            let mut bracket_depth = 0usize;
            let mut brace_depth = 0usize;
            let mut angle_depth = 0usize;
            let mut quote = None;
            let mut escaped = false;
            let mut args_end = None;
            for index in self.pos..self.chars.len() {
                let ch = self.chars[index];
                if escaped {
                    escaped = false;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    continue;
                }
                if let Some(closer) = quote {
                    if ch == closer {
                        quote = None;
                    }
                    continue;
                }
                match ch {
                    '\'' | '"' => quote = Some(ch),
                    '(' => paren_depth += 1,
                    ')' if paren_depth > 0 => paren_depth -= 1,
                    '[' => bracket_depth += 1,
                    ']' if bracket_depth > 0 => bracket_depth -= 1,
                    '{' => brace_depth += 1,
                    '}' if brace_depth > 0 => brace_depth -= 1,
                    '<' => angle_depth += 1,
                    '>' if angle_depth > 0 => angle_depth -= 1,
                    '>' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                        args_end = Some(index);
                        break;
                    }
                    _ => {}
                }
            }
            let Some(args_end) = args_end else {
                self.pos = start;
                return None;
            };
            let remaining: String = self.chars[self.pos..args_end].iter().collect();
            if !remaining.is_empty() {
                let Some((rest, parsed_args)) =
                    crate::parser::parse_regex_call_arg_list(&remaining)
                else {
                    self.pos = start;
                    return None;
                };
                if !rest.is_empty() {
                    self.pos = start;
                    return None;
                }
                args = parsed_args;
                if !args.is_empty() {
                    arg_source = Some(self.chars[args_start..args_end].iter().collect());
                }
            }
            self.pos = args_end;
        }
        if !self.consume_if('>') {
            self.pos = start;
            return None;
        }
        Some(RegexNode::Callable {
            name,
            args,
            arg_source,
        })
    }

    fn parse_code_block(&mut self) -> Option<RegexNode> {
        let start = self.pos;
        let (code, body) = self.parse_code_body()?;
        // `{2,3}` and `{2,}` are legacy P5-style quantifiers. Leave those on
        // the existing parser path rather than interpreting them as code.
        let trimmed = code.trim();
        if let Some((lower, upper)) = trimmed.split_once(',')
            && !lower.trim().is_empty()
            && lower.trim().chars().all(|ch| ch.is_ascii_digit())
            && (upper.trim().is_empty() || upper.trim().chars().all(|ch| ch.is_ascii_digit()))
        {
            self.pos = start;
            return None;
        }
        Some(RegexNode::CodeBlock { code, body })
    }

    fn parse_code_body(&mut self) -> Option<(String, Vec<crate::ast::Stmt>)> {
        let start = self.pos;
        self.pos += 1; // '{'
        let body_start = self.pos;
        let mut depth = 1usize;
        let mut quote = None;
        while let Some(ch) = self.chars.get(self.pos).copied() {
            if let Some(closer) = quote {
                self.pos += 1;
                if ch == '\\' {
                    self.pos += usize::from(self.pos < self.chars.len());
                } else if ch == closer {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\\' => {
                    self.pos += 1;
                    self.pos += usize::from(self.pos < self.chars.len());
                }
                '\'' | '"' => {
                    quote = Some(ch);
                    self.pos += 1;
                }
                '{' => {
                    depth += 1;
                    self.pos += 1;
                }
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    self.pos += 1;
                }
                _ => self.pos += 1,
            }
        }
        if depth != 0 || quote.is_some() || self.chars.get(self.pos) != Some(&'}') {
            self.pos = start;
            return None;
        }
        let code: String = self.chars[body_start..self.pos].iter().collect();
        self.pos += 1; // '}'
        let Ok((body, _)) = crate::parser::parse_fragment(&code) else {
            self.pos = start;
            return None;
        };
        Some((code, body))
    }

    fn parse_quote(&mut self, quote: char) -> Option<RegexNode> {
        self.pos += 1;
        let mut text = String::new();
        while let Some(ch) = self.chars.get(self.pos).copied() {
            self.pos += 1;
            match ch {
                c if c == quote => return Some(RegexNode::Quote(text)),
                // A double-quoted regex term has its own qq interpolation
                // segments. Keep that form on the explicit follow-up path
                // until the shared tree can retain those segments instead of
                // pretending that `$name` was part of one quoted literal.
                '$' | '@' | '%' if quote == '"' => return None,
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
        // Regex declarators contain Main-slang code rather than regex source
        // terms.  Keeping a partial tree for one would route a declaration
        // through the value-aware matcher and change its writeback semantics
        // (notably for :temp and :constant).  The legacy parser remains the
        // execution and RakuAST boundary for these forms.
        if self.declaration && self.starts_embedded_declaration() {
            return None;
        }
        let start = self.pos;
        while let Some(ch) = self.chars.get(self.pos).copied() {
            if ch.is_whitespace()
                || matches!(
                    ch,
                    '|' | '+' | '*' | '?' | '(' | ')' | '[' | ']' | '$' | '@' | '%'
                )
            {
                break;
            }
            self.pos += 1;
        }
        (self.pos > start).then(|| RegexNode::Literal(self.chars[start..self.pos].iter().collect()))
    }

    fn starts_embedded_declaration(&self) -> bool {
        if self.chars.get(self.pos) != Some(&':') {
            return false;
        }
        let rest: String = self.chars[self.pos + 1..].iter().collect();
        ["my ", "our ", "state ", "constant ", "temp ", "let "]
            .iter()
            .any(|keyword| rest.starts_with(keyword))
    }

    fn parse_interpolation(&mut self, sequential: bool) -> Option<RegexNode> {
        self.pos += 1; // '$'
        let name = if self.consume_if('{') {
            let name = self.parse_variable_name()?;
            if !self.consume_if('}') {
                return None;
            }
            name
        } else {
            self.parse_variable_name()?
        };
        Some(RegexNode::Interpolation { name, sequential })
    }

    fn parse_array_interpolation(&mut self, sequential: bool) -> Option<RegexNode> {
        self.pos += 1; // '@'
        let name = self.parse_variable_name()?;
        Some(RegexNode::ArrayInterpolation { name, sequential })
    }

    fn parse_named_capture(&mut self, array: bool) -> Option<RegexNode> {
        self.pos += 2; // '$<' or '@<'
        let start = self.pos;
        while self.chars.get(self.pos).is_some_and(|ch| *ch != '>') {
            self.pos += 1;
        }
        if self.pos == start || !self.consume_if('>') {
            return None;
        }
        let name: String = self.chars[start..self.pos - 1].iter().collect();
        self.skip_whitespace();
        if !self.consume_if('=') {
            return None;
        }
        self.skip_whitespace();
        let mut regex = self.parse_atom(&[], true, false)?;
        let quantifier = self.parse_quantifier();
        // Aggregate aliases have their own list-context semantics in the
        // legacy matcher. Keep subrule-containing forms there until the
        // shared tree has a representation for that combination.
        if array && contains_subrule(&regex) {
            return None;
        }
        if let RegexNode::Literal(text) = &mut regex
            && text.chars().count() > 1
        {
            // The alias binds to the first atom. A trailing quantifier still
            // belongs to the final character (`$<x>=ab+` is `$<x>=a` then
            // `b+`), matching the ordinary regex parser's literal splitting.
            let mut chars = text.chars();
            let first = chars.next()?;
            let rest: String = chars.collect();
            let mut nodes = vec![RegexNode::NamedCapture {
                name,
                array,
                regex: Box::new(RegexNode::Literal(first.to_string())),
            }];
            if let Some(quantifier) = quantifier {
                let mut rest_chars = rest.chars();
                let last = rest_chars.next_back()?;
                let prefix: String = rest_chars.collect();
                if !prefix.is_empty() {
                    nodes.push(RegexNode::Literal(prefix));
                }
                nodes.push(RegexNode::Quantified {
                    atom: Box::new(RegexNode::Literal(last.to_string())),
                    quantifier,
                });
            } else {
                nodes.push(RegexNode::Literal(rest));
            }
            return Some(RegexNode::Sequence(nodes));
        }
        if let Some(quantifier) = quantifier {
            regex = RegexNode::Quantified {
                atom: Box::new(regex),
                quantifier,
            };
        }
        Some(RegexNode::NamedCapture {
            name,
            array,
            regex: Box::new(regex),
        })
    }

    fn parse_subrule(&mut self) -> Option<RegexNode> {
        self.pos += 1; // '<'
        let start = self.pos;
        while self.chars.get(self.pos).is_some_and(|ch| *ch != '>') {
            self.pos += 1;
        }
        if self.pos == start || !self.consume_if('>') {
            return None;
        }
        let contents: String = self.chars[start..self.pos - 1].iter().collect();
        if let Some((alias, target)) = contents.split_once('=') {
            let alias = alias.trim();
            let target = target.trim();
            let (capturing, target) = if let Some(target) = target.strip_prefix('.') {
                (false, target.trim())
            } else {
                (true, target)
            };
            if is_simple_subrule_name(alias)
                && let Some((name, args)) = parse_subrule_target(target)
                && is_subrule_name(&name)
            {
                return Some(RegexNode::SubruleAlias {
                    alias: alias.to_string(),
                    name,
                    capturing,
                    args,
                });
            }
            return None;
        }
        let (capturing, name) = if let Some(name) = contents.strip_prefix('.') {
            (false, name)
        } else {
            (true, contents.as_str())
        };
        let (name, args) = parse_subrule_target(name)?;
        is_subrule_name(&name).then(|| RegexNode::Subrule {
            name,
            capturing,
            args,
        })
    }

    fn parse_variable_name(&mut self) -> Option<String> {
        let start = self.pos;
        let first = self.chars.get(self.pos).copied()?;
        if !first.is_alphabetic() && first != '_' {
            return None;
        }
        self.pos += 1;
        while let Some(ch) = self.chars.get(self.pos).copied() {
            let hyphen = ch == '-'
                && self
                    .chars
                    .get(self.pos + 1)
                    .is_some_and(|next| next.is_alphabetic() || *next == '_');
            if ch.is_alphanumeric() || ch == '_' || hyphen {
                self.pos += 1;
            } else {
                break;
            }
        }
        Some(self.chars[start..self.pos].iter().collect())
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

fn is_simple_subrule_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first.is_alphabetic() || first == '_')
        && chars.all(|ch| ch.is_alphanumeric() || ch == '_' || ch == '-')
}

/// A qualified subrule name is a sequence of ordinary identifier segments.
/// Keep the alias side restricted to `is_simple_subrule_name`: Rakudo rejects
/// a long name on the alias side, while the called rule may be qualified.
fn is_subrule_name(name: &str) -> bool {
    !name.is_empty() && name.split("::").all(is_simple_subrule_name)
}

fn subrule_alias_inner_source(
    alias: &str,
    name: &str,
    capturing: bool,
    args: &Option<Box<SubruleArgs>>,
) -> String {
    let target = if let Some(args) = args.as_deref() {
        let rendered = args.source.clone().or_else(|| {
            args.args
                .iter()
                .map(expression_source)
                .collect::<Option<Vec<_>>>()
                .map(|parts| parts.join(", "))
        });
        format!(
            "{}{}({})",
            if capturing { "" } else { "." },
            name,
            rendered.unwrap_or_default()
        )
    } else {
        format!("{}{}", if capturing { "" } else { "." }, name)
    };
    format!("{alias}={target}")
}

/// Parse the source-level target of a named subrule assertion. The ordinary
/// runtime parser already supports both `<name(args)>` and `<name: args>`;
/// retain the same expression list here so RakuAST can expose the argument
/// tree without changing that execution path.
fn parse_subrule_target(source: &str) -> Option<(String, Option<Box<SubruleArgs>>)> {
    let source = source.trim();
    let colon = source.char_indices().find_map(|(index, ch)| {
        if ch != ':' || index > 0 && source[..index].ends_with(':') {
            return None;
        }
        if source[index + ch.len_utf8()..].starts_with(':') {
            return None;
        }
        Some(index)
    });
    if let Some(open) = source.find('(')
        && source.ends_with(')')
        && colon.is_none_or(|colon| open < colon)
    {
        let name = source[..open].trim();
        if !is_subrule_name(name) {
            return None;
        }
        let args_source = source[open + 1..source.len() - 1].trim();
        let args = if args_source.is_empty() {
            Vec::new()
        } else {
            let (rest, args) = crate::parser::parse_regex_call_arg_list(args_source)?;
            if !rest.trim().is_empty() {
                return None;
            }
            args
        };
        return Some((
            name.to_string(),
            Some(Box::new(SubruleArgs {
                args,
                source: Some(args_source.to_string()),
            })),
        ));
    }

    let Some(colon) = colon else {
        return Some((source.to_string(), None));
    };
    let name = source[..colon].trim();
    if !is_subrule_name(name) {
        return None;
    }
    let args_source = source[colon + 1..].trim();
    let args = if args_source.is_empty() {
        Vec::new()
    } else {
        let (rest, args) = crate::parser::parse_regex_call_arg_list(args_source)?;
        if !rest.trim().is_empty() {
            return None;
        }
        args
    };
    Some((
        name.to_string(),
        Some(Box::new(SubruleArgs {
            args,
            source: Some(args_source.to_string()),
        })),
    ))
}

fn contains_subrule(node: &RegexNode) -> bool {
    match node {
        RegexNode::Subrule { .. } | RegexNode::SubruleAlias { .. } => true,
        RegexNode::Sequence(nodes)
        | RegexNode::Alternation(nodes)
        | RegexNode::SequentialAlternation(nodes) => nodes.iter().any(contains_subrule),
        RegexNode::Group(child)
        | RegexNode::CapturingGroup(child)
        | RegexNode::Quantified { atom: child, .. }
        | RegexNode::WithWhitespace(child) => contains_subrule(child),
        RegexNode::NamedCapture { regex, .. } => contains_subrule(regex),
        RegexNode::Lookaround { assertion, .. } => contains_subrule(assertion),
        RegexNode::NamedLookaround { assertion, .. } => contains_subrule(assertion),
        RegexNode::Literal(_)
        | RegexNode::Quote(_)
        | RegexNode::Interpolation { .. }
        | RegexNode::RegexValueInterpolation { .. }
        | RegexNode::ArrayInterpolation { .. }
        | RegexNode::ArrayLookaround { .. }
        | RegexNode::Callable { .. }
        | RegexNode::CodeAssertion { .. }
        | RegexNode::CodeBlock { .. }
        | RegexNode::InterpolatedBlock { .. }
        | RegexNode::AnchorBeginningOfString
        | RegexNode::AnchorBeginningOfLine
        | RegexNode::AnchorEndOfString
        | RegexNode::AnchorEndOfLine
        | RegexNode::CharClassDigit => false,
    }
}

fn is_supported_lookaround_body(node: &RegexNode) -> bool {
    match node {
        RegexNode::Literal(_) | RegexNode::Quote(_) | RegexNode::CharClassDigit => true,
        RegexNode::Sequence(nodes)
        | RegexNode::Alternation(nodes)
        | RegexNode::SequentialAlternation(nodes) => nodes.iter().all(is_supported_lookaround_body),
        RegexNode::Group(child)
        | RegexNode::Quantified { atom: child, .. }
        | RegexNode::WithWhitespace(child) => is_supported_lookaround_body(child),
        RegexNode::Interpolation { .. }
        | RegexNode::RegexValueInterpolation { .. }
        | RegexNode::ArrayInterpolation { .. }
        | RegexNode::Callable { .. }
        | RegexNode::CodeAssertion { .. }
        | RegexNode::CodeBlock { .. }
        | RegexNode::InterpolatedBlock { .. } => true,
        RegexNode::CapturingGroup(_)
        | RegexNode::NamedCapture { .. }
        | RegexNode::Subrule { .. }
        | RegexNode::SubruleAlias { .. }
        | RegexNode::ArrayLookaround { .. }
        | RegexNode::AnchorBeginningOfString
        | RegexNode::AnchorBeginningOfLine
        | RegexNode::AnchorEndOfString
        | RegexNode::AnchorEndOfLine => false,
        RegexNode::Lookaround { assertion, .. } | RegexNode::NamedLookaround { assertion, .. } => {
            is_supported_lookaround_body(assertion)
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
