use super::*;

impl Parser {
    // Cost: O(n), n = source consumed by the atom; recursive nesting is bounded.
    pub(super) fn parse_atom(
        &mut self,
        stops: &[char],
        sequential_interpolation: bool,
    ) -> Option<RegexNode> {
        if self.atom_depth >= 128 {
            return None;
        }
        self.atom_depth += 1;
        let result = self.parse_atom_inner(stops, sequential_interpolation);
        self.atom_depth -= 1;
        result
    }

    fn parse_atom_inner(
        &mut self,
        stops: &[char],
        sequential_interpolation: bool,
    ) -> Option<RegexNode> {
        let ch = *self.chars.get(self.pos)?;
        match ch {
            ch if crate::regex_qq_atoms::quote_closer(ch).is_some() => self.parse_quote(ch),
            '\\' => self.parse_escape(),
            '[' => {
                self.pos += 1;
                let inner = self.parse_alternation(&[']'])?;
                if !self.consume_if(']') {
                    return None;
                }
                Some(RegexNode::Group(Box::new(sequence_for_multichar_literal(
                    inner,
                ))))
            }
            '(' => {
                self.pos += 1;
                let inner = self.parse_alternation(&[')'])?;
                if !self.consume_if(')') {
                    return None;
                }
                Some(RegexNode::CapturingGroup(Box::new(
                    sequence_for_multichar_literal(inner),
                )))
            }
            '$' if self.chars.get(self.pos + 1) == Some(&'<') => {
                let start = self.pos;
                self.parse_named_capture(false).or_else(|| {
                    self.pos = start;
                    self.parse_named_back_reference()
                })
            }
            '$' if self
                .chars
                .get(self.pos + 1)
                .is_some_and(char::is_ascii_digit) =>
            {
                let start = self.pos;
                self.parse_numbered_capture().or_else(|| {
                    self.pos = start;
                    self.parse_positional_back_reference()
                })
            }
            '@' if self.chars.get(self.pos + 1) == Some(&'<') => self.parse_named_capture(true),
            '%' if self.chars.get(self.pos + 1) == Some(&'<')
                || self
                    .chars
                    .get(self.pos + 1)
                    .is_some_and(|ch| ch.is_alphabetic() || *ch == '_') =>
            {
                self.parse_hash_capture()
            }
            '^' if self.chars.get(self.pos + 1) == Some(&'^') => {
                self.pos += 2;
                Some(RegexNode::AnchorBeginningOfLine)
            }
            '^' => {
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
            '$' | '@' | '%' if self.chars.get(self.pos + 1) == Some(&'(') => {
                self.parse_contextualized_interpolation(sequential_interpolation)
            }
            '$' => self.parse_interpolation(sequential_interpolation),
            '@' if self.allow_array_interpolation => {
                self.parse_array_interpolation(sequential_interpolation)
            }
            '@' | '%' => None,
            ')' if self.chars.get(self.pos + 1) == Some(&'>') => {
                self.pos += 2;
                Some(RegexNode::MatchTo)
            }
            ')' | ']' if stops.contains(&ch) => None,
            '.' => {
                self.pos += 1;
                Some(RegexNode::CharClass(CharClassAtom::Any))
            }
            '«' => {
                self.pos += 1;
                Some(RegexNode::AnchorLeftWordBoundary)
            }
            '»' => {
                self.pos += 1;
                Some(RegexNode::AnchorRightWordBoundary)
            }
            '<' if self.chars.get(self.pos + 1) == Some(&'(') => {
                self.pos += 2;
                Some(RegexNode::MatchFrom)
            }
            '<' if self.chars[self.pos..].starts_with(&['<', '~', '~', '>']) => {
                self.pos += 4;
                Some(RegexNode::Extension(RegexExtension::Recurse))
            }
            '<' if self
                .chars
                .get(self.pos + 1)
                .is_some_and(|c| c.is_whitespace()) =>
            {
                self.parse_word_list()
            }
            '<' if self.chars.get(self.pos + 1) == Some(&'<') => {
                self.pos += 2;
                Some(RegexNode::AnchorLeftWordBoundary)
            }
            '>' if self.chars.get(self.pos + 1) == Some(&'>') => {
                self.pos += 2;
                Some(RegexNode::AnchorRightWordBoundary)
            }
            '|' | '+' | '*' | '?' | '>' => None,
            // `<::(EXPR)>` is a symbolic subrule call, not a `<:prop>` class.
            '<' if self.chars[self.pos + 1..].starts_with(&[':', ':', '('])
                || self.chars[self.pos + 1..].starts_with(&['.', ':', ':', '(']) =>
            {
                self.parse_subrule()
            }
            '<' if self
                .chars
                .get(self.pos + 1)
                .is_some_and(|next| matches!(next, '[' | '-' | '+' | ':'))
                || self.chars.get(self.pos + 1) == Some(&'.')
                    && self
                        .chars
                        .get(self.pos + 2)
                        .is_some_and(|next| matches!(next, '-' | '+')) =>
            {
                self.parse_char_class_assertion()
            }
            '<' => self
                .parse_lookaround(sequential_interpolation)
                .or_else(|| self.parse_subrule()),
            '{' => self.parse_code_block(),
            // Another adverb (`:s`, `:r`) keeps the literal path it took
            // before modifiers were modelled.
            ':' => self
                .parse_internal_modifier()
                .or_else(|| self.parse_statement())
                .or_else(|| self.parse_literal()),
            _ => self.parse_literal(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_atoms_bound_nested_aliases_and_assertion_prefixes() {
        let nested = format!("{}a", "$0=".repeat(129));
        assert!(RegexTree::parse_static(&nested, false).is_none());
        let prefixes = format!("<{}alpha>", "?!".repeat(65));
        assert!(RegexTree::parse_static(&prefixes, false).is_none());
    }

    #[test]
    fn numeric_alias_source_preserves_static_numbering() {
        let tree = RegexTree::parse_static("$2=(a) (b)", false).unwrap();
        assert!(tree.to_source().contains("$2 ="));
        assert!(!tree.to_source().contains("$<2>"));
        assert!(RegexTree::parse_static(&tree.to_source(), false).is_some());
    }
}
