use super::*;

impl Parser {
    // Cost: O(n), n = assertion source; at most 128 prefix wrappers are retained.
    pub(super) fn parse_lookaround(&mut self, sequential_interpolation: bool) -> Option<RegexNode> {
        let start = self.pos;
        let mut prefixes = Vec::new();
        let mut at = start + 1;
        while let Some(&marker @ ('?' | '!')) = self.chars.get(at) {
            if prefixes.len() == 128 {
                return None;
            }
            prefixes.push(marker == '!');
            at += 1;
        }
        if prefixes.len() < 2 {
            return self.parse_single_lookaround(sequential_interpolation);
        }
        // The single-assertion parser skips its initial '<'. For the inner
        // assertion that position is the preceding prefix character instead.
        self.pos = at - 2;
        let Some(mut assertion) = self.parse_single_lookaround(sequential_interpolation) else {
            self.pos = start;
            return None;
        };
        prefixes.pop();
        for negated in prefixes.into_iter().rev() {
            assertion = RegexNode::Extension(RegexExtension::Lookahead {
                negated,
                assertion: Box::new(assertion),
            });
        }
        Some(assertion)
    }

    /// Parse lookaround forms whose source and execution shapes are currently
    /// shared: the explicit `<?before body>`, `<!before body>`, `<?after
    /// body>`, and `<!after body>` forms, plus the unprefixed and dot-prefixed
    /// named forms. The latter retain their capture policy in a separate tree
    /// node because their RakuAST shape omits the Lookahead wrapper. Escaped
    /// characters and ordinary scalar interpolations are accepted when the
    /// nested tree already has a source and execution representation.
    /// `$(EXPR)>` / `@(EXPR)>` / `%(EXPR)>` after the `<`; `None` (position
    /// unchanged) for any other shape.
    // Cost: O(n), n = length of the expression source.
    fn parse_assertion_contextualizer(&mut self, sequential: bool) -> Option<RegexNode> {
        let start = self.pos;
        if !matches!(self.chars.get(start), Some('$' | '@' | '%'))
            || self.chars.get(start + 1) != Some(&'(')
        {
            return None;
        }
        let node = self.parse_contextualized_interpolation(sequential);
        match node {
            Some(RegexNode::Extension(RegexExtension::ContextualizedInterpolation {
                sigil,
                sequential,
                code,
                ..
            })) if self.consume_if('>') => Some(RegexNode::Extension(
                RegexExtension::ContextualizedInterpolation {
                    sigil,
                    sequential,
                    assertion: true,
                    code,
                },
            )),
            _ => {
                self.pos = start;
                None
            }
        }
    }

    fn parse_single_lookaround(&mut self, sequential_interpolation: bool) -> Option<RegexNode> {
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

        // `<|w>` is rakudo's spelling of the word boundary `<.wb>`, and the
        // same node in RakuAST.
        // `<|b>` / `<|c>` / `<|g>` (grapheme and codepoint boundaries) hold
        // at every position of an NFG string: rakudo models them as
        // `Assertion::Pass`. `<?|x>` / `<!|x>` wrap either in a lookahead.
        if (explicit || self.pos == start + 1) && self.chars.get(self.pos) == Some(&'|') {
            let boundary = match self.chars.get(self.pos + 1) {
                Some('w') => RegexTree::parse_lookaround_body("<.wb>", self.in_unit_parse)?.body,
                Some('b' | 'c' | 'g') => RegexNode::AssertionPass,
                _ => {
                    self.pos = start;
                    return None;
                }
            };
            if self.chars.get(self.pos + 2) != Some(&'>') {
                self.pos = start;
                return None;
            }
            self.pos += 3;
            return Some(if explicit {
                RegexNode::Extension(RegexExtension::Lookahead {
                    negated,
                    assertion: Box::new(boundary),
                })
            } else {
                boundary
            });
        }

        // `<?>` / `<!>`: the bare assertions that always pass / fail.
        if explicit && self.chars.get(self.pos) == Some(&'>') {
            self.pos += 1;
            return Some(if negated {
                RegexNode::AssertionFail
            } else {
                RegexNode::AssertionPass
            });
        }

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

        // `<$(EXPR)>`, `<@(EXPR)>` and `<%(EXPR)>`: the same interpolation over a
        // contextualized expression.
        if !explicit && let Some(node) = self.parse_assertion_contextualizer(sequential_interpolation)
        {
            return Some(node);
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
            let Some(mut name) = self.parse_twigil_variable_name() else {
                self.pos = start;
                return None;
            };
            // `<@Pkg::name>`: a package-qualified variable.
            while self.chars[self.pos..].starts_with(&[':', ':']) {
                self.pos += 2;
                let Some(part) = self.parse_variable_name() else {
                    self.pos = start;
                    return None;
                };
                name = crate::qualified::qualified_text(name.as_str(), part.as_str()).to_string();
            }
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

        // `<?alpha>` / `<!ww>` / `<?.name>` / `<?name(args)>` / `<?[x]>`: a
        // lookahead over a named assertion or a character class. Only the
        // keywords `before` and `after` (followed by a space) take a regex.
        let keyword = |word: &str| {
            self.chars[self.pos..].starts_with(&word.chars().collect::<Vec<_>>())
                && self
                    .chars
                    .get(self.pos + word.len())
                    .is_some_and(|ch| ch.is_whitespace())
        };
        if explicit && !keyword("before") && !keyword("after") {
            // The marker stands in for the `<` the assertion parsers skip.
            self.pos = start + 1;
            let assertion = if self
                .chars
                .get(self.pos + 1)
                .is_some_and(|ch| matches!(ch, '[' | '-' | '+' | ':'))
            {
                self.parse_char_class_assertion()
            } else {
                self.parse_subrule()
            };
            return match assertion {
                Some(assertion) => Some(RegexNode::Extension(RegexExtension::Lookahead {
                    negated,
                    assertion: Box::new(assertion),
                })),
                None => {
                    self.pos = start;
                    None
                }
            };
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
        let mut nested_assertions = 0usize;
        while let Some(ch) = self.chars.get(self.pos).copied() {
            if crate::regex_qq_atoms::quote_closer(ch).is_some() {
                self.pos = crate::regex_qq_atoms::quote_end(&self.chars, self.pos)? + 1;
                continue;
            }
            match ch {
                // A quote character inside an enumeration (`<-["]>`) is a
                // member of the class, not the start of a quote.
                '[' if nested_assertions > 0
                    && matches!(self.chars.get(self.pos - 1), Some('<' | '-' | '+')) =>
                {
                    self.pos += 1;
                    while let Some(&inner) = self.chars.get(self.pos) {
                        self.pos += if inner == '\\' { 2 } else { 1 };
                        if inner == ']' {
                            break;
                        }
                    }
                }
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
        if self.chars.get(self.pos) != Some(&'>') || nested_assertions != 0 {
            self.pos = start;
            return None;
        }
        let body_source: String = self.chars[body_start..self.pos].iter().collect();
        let assertion = RegexTree::parse_lookaround_body(&body_source, self.in_unit_parse)?.body;
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
}
