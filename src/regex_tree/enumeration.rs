//! Character-class assertions of the source-level regex tree: `<[a..z]>`,
//! `<-[aeiou]>`, `<+alpha -[x]>`, modelled after rakudo's
//! `RakuAST::Regex::Assertion::CharClass` and its `CharClassElement::*`.
//!
//! Measured on rakudo 2026.09:
//!
//! ```text
//! $ raku -e 'say Q{/<[a..z\w]-[q]>/}.AST'
//! ... Assertion::CharClass.new(
//!       CharClassElement::Enumeration.new(elements => (
//!         CharClassEnumerationElement::Range.new(from => 97, to => 122),
//!         CharClass::Word.new)),
//!       CharClassElement::Enumeration.new(negated => True, elements => (
//!         CharClassEnumerationElement::Character.new("q"))))
//! ```

use super::{CharClassAtom, Parser, RegexNode};

/// One `+`/`-` term of a character-class assertion.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum CharClassElement {
    /// `[...]`
    Enumeration {
        negated: bool,
        elements: Vec<EnumerationElement>,
    },
    /// A named rule (`alpha` in `<+alpha>`).
    Rule { name: String, negated: bool },
    /// A Unicode property (`:Lu` in `<-:Lu>`); `inverted` is its `!`
    /// (`<:!Lu>`), `negated` the term's `-`.
    Property {
        name: String,
        negated: bool,
        inverted: bool,
        /// The value it must have (`<:Script<Latin>>`, `<:Nv(1)>`).
        #[serde(default)]
        predicate: Option<PropertyPredicate>,
    },
}

/// The value a property must have, as written after its name.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum PropertyPredicate {
    /// `<Latin>`: the text between the angle brackets.
    Words(String),
    /// `(1)`: the text between the parentheses.
    Args(String),
}

/// One entry of an enumerated `[...]` class.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum EnumerationElement {
    Character(char),
    Range(char, char),
    /// A backslash class or a codepoint escape (`\w`, `\x41`).
    Class(CharClassAtom),
}

/// A character as enumeration source: a word character as itself, anything
/// else as a codepoint escape (a space would be insignificant, a `]` would
/// close the class).
// Cost: O(1).
fn enumeration_char(ch: char) -> String {
    if ch.is_alphanumeric() || ch == '_' {
        ch.to_string()
    } else {
        format!("\\x[{:X}]", ch as u32)
    }
}

impl CharClassElement {
    // Cost: O(n), n = number of enumerated entries.
    fn to_source(&self, first: bool) -> String {
        let (negated, body) = match self {
            Self::Enumeration { negated, elements } => {
                let body: String = elements
                    .iter()
                    .map(|element| match element {
                        EnumerationElement::Character(ch) => enumeration_char(*ch),
                        EnumerationElement::Range(from, to) => {
                            format!("{}..{}", enumeration_char(*from), enumeration_char(*to))
                        }
                        EnumerationElement::Class(atom) => atom.to_source(),
                    })
                    .collect::<Vec<_>>()
                    .join(" ");
                (*negated, format!("[{body}]"))
            }
            Self::Rule { name, negated } => (*negated, name.clone()),
            Self::Property {
                name,
                negated,
                inverted,
                predicate,
            } => (
                *negated,
                format!(
                    ":{}{name}{}",
                    if *inverted { "!" } else { "" },
                    match predicate {
                        None => String::new(),
                        Some(PropertyPredicate::Words(text)) => format!("<{text}>"),
                        Some(PropertyPredicate::Args(text)) => format!("({text})"),
                    }
                ),
            ),
        };
        // A leading positive rule needs its `+`: `<alpha>` is a subrule.
        let sign = match (negated, first, self) {
            (true, _, _) => "-",
            (false, true, Self::Enumeration { .. } | Self::Property { .. }) => "",
            (false, _, _) => "+",
        };
        format!("{sign}{body}")
    }
}

/// Regex source for a character-class assertion.
// Cost: O(n), n = total number of entries.
pub(crate) fn assertion_source(elements: &[CharClassElement]) -> String {
    let body: Vec<String> = elements
        .iter()
        .enumerate()
        .map(|(index, element)| element.to_source(index == 0))
        .collect();
    format!("<{}>", body.join(" "))
}

impl Parser {
    /// Parse a character-class assertion at `<`. Returns `None`, with the
    /// position restored, for any other angle form and for an unmodelled
    /// entry (a property with an argument `<:Nv(1)>`, an unescaped `a-z`
    /// range).
    // Cost: O(k), k = length of the assertion.
    pub(super) fn parse_char_class_assertion(&mut self) -> Option<RegexNode> {
        let start = self.pos;
        let parsed = self.parse_char_class_elements();
        if parsed.is_none() {
            self.pos = start;
        }
        parsed.map(RegexNode::CharClassAssertion)
    }

    fn parse_char_class_elements(&mut self) -> Option<Vec<CharClassElement>> {
        self.pos += 1; // '<'
        let mut elements = Vec::new();
        loop {
            self.skip_whitespace();
            if self.consume_if('>') {
                break;
            }
            let negated = match self.chars.get(self.pos).copied()? {
                '-' => {
                    self.pos += 1;
                    true
                }
                '+' => {
                    self.pos += 1;
                    false
                }
                // Only the first term may omit its sign, and only a `[` or
                // a property (`<alpha>` is a subrule).
                '[' | ':' if elements.is_empty() => false,
                _ => return None,
            };
            self.skip_whitespace();
            if self.consume_if('[') {
                elements.push(CharClassElement::Enumeration {
                    negated,
                    elements: self.parse_enumeration()?,
                });
            } else if self.consume_if(':') {
                let inverted = self.consume_if('!');
                let name = self.parse_class_name()?;
                let predicate = self.parse_property_predicate()?;
                elements.push(CharClassElement::Property {
                    name,
                    negated,
                    inverted,
                    predicate,
                });
            } else {
                let name = self.parse_class_name()?;
                elements.push(CharClassElement::Rule { name, negated });
            }
        }
        (!elements.is_empty()).then_some(elements)
    }

    /// The `<Latin>` or `(1)` after a property's name, if any. `None` (not an
    /// absent predicate) for a form the tree does not model.
    // Cost: O(k), k = length of the predicate.
    fn parse_property_predicate(&mut self) -> Option<Option<PropertyPredicate>> {
        let (open, close) = match self.chars.get(self.pos) {
            Some('<') => ('<', '>'),
            Some('(') => ('(', ')'),
            Some('[') => return None,
            _ => return Some(None),
        };
        let start = self.pos + 1;
        let mut depth = 1usize;
        let mut end = start;
        while let Some(&c) = self.chars.get(end) {
            if c == open {
                depth += 1;
            } else if c == close {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            end += 1;
        }
        if depth != 0 {
            return None;
        }
        let text: String = self.chars[start..end].iter().collect();
        self.pos = end + 1;
        Some(Some(if open == '<' {
            PropertyPredicate::Words(text)
        } else {
            PropertyPredicate::Args(text)
        }))
    }

    /// The identifier naming a rule or property term. A hyphen followed by a
    /// letter continues the name (`+name-sep`); any other `-` is the operator.
    // Cost: O(k), k = length of the name.
    fn parse_class_name(&mut self) -> Option<String> {
        let name_start = self.pos;
        while let Some(&c) = self.chars.get(self.pos) {
            let hyphen = c == '-'
                && self.pos != name_start
                && self
                    .chars
                    .get(self.pos + 1)
                    .is_some_and(|next| next.is_alphabetic() || *next == '_');
            if !(c.is_alphanumeric() || c == '_' || hyphen) {
                break;
            }
            self.pos += 1;
        }
        (self.pos != name_start).then(|| self.chars[name_start..self.pos].iter().collect())
    }

    /// The entries of a `[...]` class, after its `[`, through its `]`.
    // Cost: O(k), k = length of the class.
    fn parse_enumeration(&mut self) -> Option<Vec<EnumerationElement>> {
        let mut elements = Vec::new();
        loop {
            self.skip_whitespace();
            let ch = self.chars.get(self.pos).copied()?;
            if ch == ']' {
                self.pos += 1;
                return Some(elements);
            }
            let element = if ch == '\\'
                && let Some(space) = self
                    .chars
                    .get(self.pos + 1)
                    .copied()
                    .filter(|c| c.is_whitespace())
            {
                // `[\ ]`: an escaped whitespace character is that character
                // here, though outside a class it is an "unspace" error.
                self.pos += 2;
                EnumerationElement::Character(space)
            } else if ch == '\\' {
                self.parse_enumeration_escape()?
            } else {
                // `-` is literal only at an edge: `a-z` is rakudo's
                // "Unsupported use of - as character range".
                if ch == '-'
                    && !elements.is_empty()
                    && self
                        .chars
                        .get(self.pos + 1)
                        .is_some_and(|next| *next != ']')
                {
                    return None;
                }
                self.pos += 1;
                EnumerationElement::Character(ch)
            };
            // `a..z`
            let before_range = self.pos;
            self.skip_whitespace();
            if self.chars.get(self.pos) == Some(&'.') && self.chars.get(self.pos + 1) == Some(&'.')
            {
                self.pos += 2;
                self.skip_whitespace();
                let from = single_char(&element)?;
                let to = match self.chars.get(self.pos).copied()? {
                    '\\' => single_char(&self.parse_enumeration_escape()?)?,
                    ']' => return None,
                    ch => {
                        self.pos += 1;
                        ch
                    }
                };
                elements.push(EnumerationElement::Range(from, to));
            } else {
                self.pos = before_range;
                elements.push(element);
            }
        }
    }

    /// An escape inside `[...]`: a class (`\w`, `\x41`) or an escaped
    /// character (`\]`).
    // Cost: O(k), k = length of the escape.
    fn parse_enumeration_escape(&mut self) -> Option<EnumerationElement> {
        match self.parse_escape()? {
            RegexNode::CharClass(atom) => Some(EnumerationElement::Class(atom)),
            RegexNode::Literal(text) => {
                let mut chars = text.chars();
                let ch = chars.next()?;
                chars
                    .next()
                    .is_none()
                    .then_some(EnumerationElement::Character(ch))
            }
            _ => None,
        }
    }
}

/// The one character an entry denotes, for a range endpoint.
// Cost: O(1).
fn single_char(element: &EnumerationElement) -> Option<char> {
    match element {
        EnumerationElement::Character(ch) => Some(*ch),
        EnumerationElement::Class(CharClassAtom::Specified {
            characters,
            negated: false,
        }) => {
            let mut chars = characters.chars();
            let ch = chars.next()?;
            chars.next().is_none().then_some(ch)
        }
        _ => None,
    }
}

/// Whether `name` is the identifier of a rule or property term: word
/// characters, with a hyphen allowed before a letter (`name-sep`).
// Cost: O(|name|).
pub(crate) fn is_class_name(name: &str) -> bool {
    let chars: Vec<char> = name.chars().collect();
    !chars.is_empty()
        && chars.iter().enumerate().all(|(at, &c)| {
            c.is_alphanumeric()
                || c == '_'
                || (c == '-'
                    && at > 0
                    && chars
                        .get(at + 1)
                        .is_some_and(|next| next.is_alphabetic() || *next == '_'))
        })
}
