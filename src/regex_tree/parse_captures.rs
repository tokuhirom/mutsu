use super::*;

const MAX_STATIC_HASH_ALIAS_NAME_CHARS: usize = 1024;

impl Parser {
    // Cost: O(n), n = source consumed by the alias and its atom.
    pub(super) fn parse_numbered_capture(&mut self) -> Option<RegexNode> {
        self.pos += 1; // '$'
        let start = self.pos;
        while self.chars.get(self.pos).is_some_and(char::is_ascii_digit) {
            self.pos += 1;
        }
        let name: String = self.chars[start..self.pos].iter().collect();
        name.parse::<u32>().ok()?;
        self.parse_capture_body(name, false, false)
    }

    pub(super) fn parse_named_capture(&mut self, array: bool) -> Option<RegexNode> {
        self.pos += 2; // '$<' or '@<'
        let start = self.pos;
        while self.chars.get(self.pos).is_some_and(|ch| *ch != '>') {
            self.pos += 1;
        }
        if self.pos == start || !self.consume_if('>') {
            return None;
        }
        let name: String = self.chars[start..self.pos - 1].iter().collect();
        self.parse_capture_body(name, array, false)
    }

    // Cost: O(n), n = alias name length.
    pub(super) fn parse_hash_capture(&mut self) -> Option<RegexNode> {
        self.pos += 1; // '%'
        let bracketed = self.consume_if('<');
        let start = self.pos;
        if bracketed {
            while self.chars.get(self.pos).is_some_and(|ch| *ch != '>') {
                self.pos += 1;
            }
            if self.pos == start
                || self.pos - start > MAX_STATIC_HASH_ALIAS_NAME_CHARS
                || !self.consume_if('>')
            {
                return None;
            }
            let name: String = self.chars[start..self.pos - 1].iter().collect();
            self.parse_capture_body(name, false, true)
        } else {
            while self
                .chars
                .get(self.pos)
                .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
            {
                self.pos += 1;
            }
            if self.pos == start || self.pos - start > MAX_STATIC_HASH_ALIAS_NAME_CHARS {
                return None;
            }
            let name: String = self.chars[start..self.pos].iter().collect();
            self.parse_capture_body(name, false, true)
        }
    }

    fn parse_capture_body(&mut self, name: String, array: bool, hash: bool) -> Option<RegexNode> {
        self.skip_whitespace();
        if !self.consume_if('=') {
            return None;
        }
        self.skip_whitespace();
        self.parse_alias_target(|regex| RegexNode::NamedCapture {
            name,
            array,
            hash,
            regex,
        })
    }

    /// `$name=ATOM` right after the interpolated `$name`: an alias to a
    /// variable rather than to a capture. Only the unspaced spelling is
    /// modelled, so the rebuilt text is the text that was written (whitespace
    /// around `=` is significant under `:sigspace`); any other spelling keeps
    /// the plain interpolation and leaves the `=` to the caller.
    // Cost: O(n), n = source consumed by the alias and its atom.
    pub(super) fn parse_variable_alias(&mut self, name: String, sequential: bool) -> Option<RegexNode> {
        let start = self.pos;
        if self.chars.get(self.pos) != Some(&'=')
            || self
                .chars
                .get(self.pos + 1)
                .is_none_or(|next| next.is_whitespace() || *next == '=')
        {
            return Some(RegexNode::Interpolation { name, sequential });
        }
        self.pos += 1;
        let alias = self.parse_alias_target(|regex| {
            RegexNode::Extension(RegexExtension::VariableAlias {
                name: name.clone(),
                sequential,
                regex,
            })
        });
        if alias.is_none() {
            self.pos = start;
        }
        alias
    }

    /// The aliased atom after `=` (with its quantifier), wrapped by `wrap`.
    // Cost: O(n), n = source consumed by the atom.
    fn parse_alias_target(
        &mut self,
        wrap: impl FnOnce(Box<RegexNode>) -> RegexNode,
    ) -> Option<RegexNode> {
        let mut regex = self.parse_atom(&[], false)?;
        // An alias on a `<?before …>` / `<?after …>` assertion drops its zero
        // width marker: Rakudo's `subrule_alias` resets the aliased subrule's
        // subtype to `capture`, so `$<a>=<?before x>` is `$<a>=<before x>` and
        // also publishes the `before` key. The negated spelling can never
        // match once aliased; the runtime parser models that case.
        match regex {
            RegexNode::Lookaround { negated: true, .. } => {}
            RegexNode::Lookaround {
                assertion,
                negated: false,
                is_behind,
            } => {
                regex = RegexNode::NamedLookaround {
                    assertion,
                    is_behind,
                    capturing: true,
                };
            }
            _ => {}
        }
        let before_quantifier = self.pos;
        self.skip_whitespace();
        let spaced_quantifier = self.pos != before_quantifier
            && matches!(self.chars.get(self.pos), Some('*' | '+' | '?'));
        if !spaced_quantifier {
            self.pos = before_quantifier;
        }
        let mut quantifier = self.parse_quantifier();
        let mut spaced_separator = false;
        if let Some(quantifier) = &mut quantifier
            && let Some((separator, whitespace_before)) = self.parse_separator()
        {
            quantifier.separator = Some(Box::new(separator));
            spaced_separator = whitespace_before;
        }
        // Subrule aliases retain their source tree for either sigil. Their
        // execution lowerer already declines Subrule nodes, so the established
        // matcher continues to own per-iteration capture/list semantics.
        if let RegexNode::Literal(text) = &mut regex
            && text.chars().count() > 1
        {
            // The alias binds to the first atom. A trailing quantifier still
            // belongs to the final character (`$<x>=ab+` is `$<x>=a` then
            // `b+`), matching the ordinary regex parser's literal splitting.
            let mut chars = text.chars();
            let first = chars.next()?;
            let rest: String = chars.collect();
            let mut nodes = vec![wrap(Box::new(RegexNode::Literal(first.to_string())))];
            if let Some(quantifier) = quantifier {
                let mut rest_chars = rest.chars();
                let last = rest_chars.next_back()?;
                let prefix: String = rest_chars.collect();
                if !prefix.is_empty() {
                    nodes.push(RegexNode::Literal(prefix));
                }
                nodes.push(spaced(
                    quantified(
                        RegexNode::Literal(last.to_string()),
                        quantifier,
                        spaced_quantifier,
                    ),
                    spaced_separator,
                ));
            } else {
                nodes.push(RegexNode::Literal(rest));
            }
            return Some(RegexNode::Sequence(nodes));
        }
        if let Some(quantifier) = quantifier {
            // Rakudo puts pre-quantifier space on the atom, but does not
            // wrap the alias's quantified body for space before its separator.
            regex = quantified(regex, quantifier, spaced_quantifier);
        }
        Some(wrap(Box::new(regex)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn static_hash_alias_names_are_bounded() {
        let long = "x".repeat(MAX_STATIC_HASH_ALIAS_NAME_CHARS + 1);
        assert!(RegexTree::parse_static(&format!("%<{long}>=a"), false).is_none());
        assert!(RegexTree::parse_static(&format!("%{long}=a"), false).is_none());
    }
}
