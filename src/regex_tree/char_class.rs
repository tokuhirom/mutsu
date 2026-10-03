//! Character-class atoms of the source-level regex tree, modelled after
//! rakudo's `RakuAST::Regex::CharClass::*` nodes:
//!
//! - the backslash classes (`\d`, `\w`, `\N`, `\h`, `\0`, ...), each with its
//!   upper-case negation;
//! - the `.` wildcard (`CharClass::Any`);
//! - the codepoint escapes `\x41`, `\o[101]`, `\c[SPACE]` and their negated
//!   upper-case forms, which rakudo keeps only as the characters they denote
//!   (`CharClass::Specified`).
//!
//! Measured on rakudo 2026.09:
//!
//! ```text
//! $ raku -e 'say Q{/\W\x[41,42]/}.AST'
//! ... RakuAST::Regex::CharClass::Word.new(negated => True),
//!     RakuAST::Regex::CharClass::Specified.new(characters => "AB") ...
//! ```

use super::{Parser, RegexNode};

/// A backslash character class (`\d`, `\w`, ...). `Nul` (`\0`) has no
/// negated spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum BackslashClass {
    Digit,
    Word,
    Space,
    Newline,
    HorizontalSpace,
    VerticalSpace,
    Tab,
    Escape,
    FormFeed,
    CarriageReturn,
    Nul,
}

impl BackslashClass {
    const ALL: [(Self, char); 11] = [
        (Self::Digit, 'd'),
        (Self::Word, 'w'),
        (Self::Space, 's'),
        (Self::Newline, 'n'),
        (Self::HorizontalSpace, 'h'),
        (Self::VerticalSpace, 'v'),
        (Self::Tab, 't'),
        (Self::Escape, 'e'),
        (Self::FormFeed, 'f'),
        (Self::CarriageReturn, 'r'),
        (Self::Nul, '0'),
    ];

    /// The class a backslash letter names, and whether the letter is the
    /// negated (upper-case) spelling.
    // Cost: O(1).
    pub(crate) fn from_letter(letter: char) -> Option<(Self, bool)> {
        Self::ALL.iter().find_map(|&(class, lower)| {
            if letter == lower {
                Some((class, false))
            } else if lower != '0' && letter == lower.to_ascii_uppercase() {
                Some((class, true))
            } else {
                None
            }
        })
    }

    /// The backslash letter that spells the class.
    // Cost: O(1).
    pub(crate) fn letter(self, negated: bool) -> char {
        let lower = Self::ALL
            .iter()
            .find_map(|&(class, letter)| (class == self).then_some(letter))
            .expect("every class has a letter");
        if negated {
            lower.to_ascii_uppercase()
        } else {
            lower
        }
    }
}

/// A character-class atom outside an enumerated `<[...]>` class.
#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum CharClassAtom {
    Backslash {
        class: BackslashClass,
        negated: bool,
    },
    /// `.`
    Any,
    /// `\x41`, `\o101`, `\c[NAME]` (negated: `\X41`, ...). Only the denoted
    /// characters are kept, as rakudo does.
    Specified { characters: String, negated: bool },
}

impl CharClassAtom {
    /// Regex source that denotes the atom.
    // Cost: O(n), n = number of specified characters.
    pub(crate) fn to_source(&self) -> String {
        match self {
            Self::Backslash { class, negated } => format!("\\{}", class.letter(*negated)),
            Self::Any => ".".to_string(),
            Self::Specified {
                characters,
                negated,
            } => {
                let letter = if *negated { 'X' } else { 'x' };
                characters
                    .chars()
                    .map(|ch| format!("\\{letter}[{:X}]", ch as u32))
                    .collect()
            }
        }
    }
}

impl Parser {
    /// Parse a backslash escape. A backslash class or a codepoint escape is a
    /// [`CharClassAtom`]; an escaped non-word character is that literal
    /// character. Any other escape (`\b`, `\q`, ...) stays on the runtime
    /// parser, which owns its diagnostics.
    // Cost: O(k), k = length of the escape.
    pub(super) fn parse_escape(&mut self) -> Option<RegexNode> {
        let start = self.pos;
        self.pos += 1; // '\'
        let escaped = self.chars.get(self.pos).copied()?;
        self.pos += 1;
        let node = match escaped {
            'x' | 'X' | 'o' | 'O' | 'c' | 'C' => {
                let negated = escaped.is_ascii_uppercase();
                let characters = self.parse_codepoints(escaped.to_ascii_lowercase());
                match characters {
                    // A negated escape denotes one character.
                    Some(characters) if !negated || characters.chars().count() == 1 => {
                        Some(RegexNode::CharClass(CharClassAtom::Specified {
                            characters,
                            negated,
                        }))
                    }
                    _ => None,
                }
            }
            // `\0` followed by a digit is not the NUL class.
            '0' if self.chars.get(self.pos).is_some_and(char::is_ascii_digit) => None,
            c => match BackslashClass::from_letter(c) {
                Some((class, negated)) => Some(RegexNode::CharClass(CharClassAtom::Backslash {
                    class,
                    negated,
                })),
                None if !c.is_alphanumeric() && c != '_' && !c.is_whitespace() => {
                    Some(RegexNode::Literal(c.to_string()))
                }
                None => None,
            },
        };
        if node.is_none() {
            self.pos = start;
        }
        node
    }

    /// The characters a `\x` / `\o` / `\c` escape denotes, after its letter:
    /// a bracketed, comma-separated list or a bare run of digits.
    // Cost: O(k), k = length of the escape body.
    fn parse_codepoints(&mut self, kind: char) -> Option<String> {
        let radix = match kind {
            'x' => 16,
            'o' => 8,
            _ => 10,
        };
        if self.chars.get(self.pos) == Some(&'[') {
            let body_start = self.pos + 1;
            let body_end = body_start + self.chars[body_start..].iter().position(|c| *c == ']')?;
            let body: String = self.chars[body_start..body_end].iter().collect();
            self.pos = body_end + 1;
            let mut characters = String::new();
            for part in body.split(',').map(str::trim) {
                if kind == 'c' && !part.chars().all(|c| c.is_ascii_digit()) {
                    characters.push_str(&crate::token_kind::lookup_unicode_name_string(part)?);
                } else {
                    characters.push(codepoint(part, radix)?);
                }
            }
            return (!characters.is_empty()).then_some(characters);
        }
        let digits_start = self.pos;
        while self.chars.get(self.pos).is_some_and(|c| c.is_digit(radix)) {
            self.pos += 1;
        }
        let digits: String = self.chars[digits_start..self.pos].iter().collect();
        codepoint(&digits, radix).map(String::from)
    }
}

// Cost: O(k), k = number of digits.
fn codepoint(digits: &str, radix: u32) -> Option<char> {
    if digits.is_empty() {
        return None;
    }
    u32::from_str_radix(digits, radix)
        .ok()
        .and_then(char::from_u32)
}
