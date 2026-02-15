use super::*;

pub(crate) struct Lexer {
    pub(super) src: Vec<char>,
    pub(super) pos: usize,
    pub(super) line: usize,
    pub(super) finish_content: Option<String>,
    /// True when the last emitted token was a "term" (value/closing delimiter),
    /// meaning the next `/` should be treated as a division operator.
    /// False when the last token was an operator or opening delimiter,
    /// meaning the next `/` should be treated as a regex delimiter.
    pub(super) last_was_term: bool,
}

impl Lexer {
    pub(crate) fn new(input: &str) -> Self {
        Self {
            src: input.chars().collect(),
            pos: 0,
            line: 1,
            finish_content: None,
            last_was_term: false,
        }
    }

    /// Returns the content captured after `=finish`, if any.
    pub(crate) fn finish_content(&self) -> Option<&str> {
        self.finish_content.as_deref()
    }

    // Parse \x hex escape: \xNN or \x[NNNN]
    pub(super) fn parse_hex_escape(&mut self) -> Option<char> {
        if self.peek() == Some('[') {
            self.pos += 1; // skip '['
            let mut hex = String::new();
            while let Some(c) = self.peek() {
                if c == ']' {
                    self.pos += 1;
                    break;
                }
                hex.push(c);
                self.pos += 1;
            }
            u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
        } else {
            let mut hex = String::new();
            while let Some(c) = self.peek() {
                if c.is_ascii_hexdigit() {
                    hex.push(c);
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if hex.is_empty() {
                None
            } else {
                u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
            }
        }
    }

    // Parse \o octal escape: \oNN or \o[NNN]
    pub(super) fn parse_octal_escape(&mut self) -> Option<char> {
        if self.peek() == Some('[') {
            self.pos += 1; // skip '['
            let mut oct = String::new();
            while let Some(c) = self.peek() {
                if c == ']' {
                    self.pos += 1;
                    break;
                }
                oct.push(c);
                self.pos += 1;
            }
            u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
        } else {
            let mut oct = String::new();
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() && c != '8' && c != '9' {
                    oct.push(c);
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if oct.is_empty() {
                None
            } else {
                u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
            }
        }
    }

    // Parse \c[NAME] named Unicode character escape
    pub(super) fn parse_named_char_escape(&mut self) -> Option<char> {
        if self.peek() == Some('[') {
            self.pos += 1; // skip '['
            let mut name = String::new();
            while let Some(c) = self.peek() {
                if c == ']' {
                    self.pos += 1;
                    break;
                }
                name.push(c);
                self.pos += 1;
            }
            lookup_unicode_char_by_name(&name)
        } else {
            None
        }
    }

    pub(super) fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        if matches!(self.peek(), Some('*') | Some('~') | Some('?') | Some('^'))
            && let Some(c) = self.peek()
        {
            ident.push(c);
            self.pos += 1;
        }
        self.read_ident_tail(&mut ident);
        ident
    }

    pub(super) fn read_version_literal(&mut self) -> TokenKind {
        // Already consumed 'v', peek is a digit
        let mut parts = Vec::new();
        // Read first part (must be a number since we checked peek is digit)
        let mut num = 0i64;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                num = num * 10 + (c as i64 - '0' as i64);
                self.pos += 1;
            } else {
                break;
            }
        }
        parts.push(VersionPart::Num(num));
        // Read subsequent .part segments
        while self.peek() == Some('.') {
            // Check what follows the dot
            let after_dot = self.src.get(self.pos + 1).copied();
            match after_dot {
                Some(c) if c.is_ascii_digit() => {
                    self.pos += 1; // consume '.'
                    let mut n = 0i64;
                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() {
                            n = n * 10 + (c as i64 - '0' as i64);
                            self.pos += 1;
                        } else {
                            break;
                        }
                    }
                    parts.push(VersionPart::Num(n));
                }
                Some('*') => {
                    self.pos += 1; // consume '.'
                    self.pos += 1; // consume '*'
                    parts.push(VersionPart::Whatever);
                }
                _ => break,
            }
        }
        // Check for trailing + or -
        let mut plus = false;
        let mut minus = false;
        if self.peek() == Some('+') {
            plus = true;
            self.pos += 1;
        } else if self.peek() == Some('-') {
            minus = true;
            self.pos += 1;
        }
        TokenKind::VersionLiteral { parts, plus, minus }
    }

    pub(super) fn read_ident_start(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);
        self.read_ident_tail(&mut ident);
        ident
    }

    pub(super) fn read_ident_tail(&mut self, ident: &mut String) {
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c.is_alphabetic() || c == '_' || self.is_ident_hyphen(c)
            {
                ident.push(c);
                self.pos += 1;
                continue;
            }
            if c == ':' && self.peek_next() == Some(':') {
                ident.push(':');
                ident.push(':');
                self.pos += 2;
                continue;
            }
            break;
        }
    }

    pub(super) fn skip_ws_and_comments(&mut self) {
        loop {
            while let Some(c) = self.peek() {
                if c == '\n' {
                    self.line += 1;
                    self.pos += 1;
                } else if c == '\u{feff}' || c.is_whitespace() {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if self.peek() == Some('=')
                && (self.pos == 0 || self.src.get(self.pos - 1) == Some(&'\n'))
            {
                let mut i = self.pos;
                let mut word = String::new();
                while let Some(c) = self.src.get(i) {
                    if c.is_whitespace() {
                        break;
                    }
                    word.push(*c);
                    i += 1;
                }
                if word == "=begin" {
                    self.pos = i;
                    while self.pos < self.src.len() {
                        if self.src.get(self.pos) == Some(&'\n') {
                            let mut j = self.pos + 1;
                            let mut marker = String::new();
                            while let Some(c) = self.src.get(j) {
                                if c.is_whitespace() {
                                    break;
                                }
                                marker.push(*c);
                                j += 1;
                            }
                            if marker == "=end" {
                                // Skip the rest of the =end line
                                while j < self.src.len() && self.src[j] != '\n' {
                                    j += 1;
                                }
                                if j < self.src.len() {
                                    j += 1; // skip the newline
                                    self.line += 1;
                                }
                                self.pos = j;
                                break;
                            }
                        }
                        self.pos += 1;
                    }
                    continue;
                }
                if word == "=finish" {
                    // Skip to end of the =finish line
                    while i < self.src.len() && self.src[i] != '\n' {
                        i += 1;
                    }
                    if i < self.src.len() {
                        i += 1; // skip the newline after =finish
                    }
                    // Capture everything after =finish as the finish content
                    let content: String = self.src[i..].iter().collect();
                    self.finish_content = Some(content);
                    // Move position to end so lexer emits EOF
                    self.pos = self.src.len();
                    break;
                }
            }
            if self.peek() == Some('#') {
                self.pos += 1;
                // Check for embedded comment #`(...)
                if self.peek() == Some('`') {
                    self.pos += 1;
                    if let Some(open) = self.peek()
                        && let Some(close) = Self::matching_bracket(open)
                    {
                        self.pos += 1;
                        self.skip_bracketed_comment(open, close);
                        continue;
                    }
                    // Not a valid embedded comment, treat as regular comment
                    self.pos -= 1; // back to after #
                }
                // Regular single-line comment
                while let Some(c) = self.peek() {
                    self.pos += 1;
                    if c == '\n' {
                        self.line += 1;
                        break;
                    }
                }
                continue;
            }
            break;
        }
    }

    pub(super) fn matching_bracket(open: char) -> Option<char> {
        match open {
            '(' => Some(')'),
            '{' => Some('}'),
            '[' => Some(']'),
            '<' => Some('>'),
            '\u{ff08}' => Some('\u{ff09}'), // fullwidth parenthesis
            '\u{ff3b}' => Some('\u{ff3d}'), // fullwidth square brackets
            '\u{ff5b}' => Some('\u{ff5d}'), // fullwidth curly brackets
            '\u{ff62}' => Some('\u{ff63}'), // halfwidth corner brackets
            '\u{3008}' => Some('\u{3009}'), // CJK angle brackets
            '\u{300a}' => Some('\u{300b}'), // CJK double angle brackets
            '\u{300c}' => Some('\u{300d}'), // CJK corner brackets
            '\u{300e}' => Some('\u{300f}'), // CJK white corner brackets
            '\u{3010}' => Some('\u{3011}'), // CJK black lenticular brackets
            '\u{3014}' => Some('\u{3015}'), // CJK tortoise shell brackets
            '\u{3016}' => Some('\u{3017}'), // CJK white lenticular brackets
            '\u{3018}' => Some('\u{3019}'), // CJK white tortoise shell brackets
            '\u{301a}' => Some('\u{301b}'), // CJK white square brackets
            '\u{00ab}' => Some('\u{00bb}'), // guillemets
            '\u{2018}' => Some('\u{2019}'), // single quotes
            '\u{201c}' => Some('\u{201d}'), // double quotes
            '\u{169b}' => Some('\u{169c}'), // ogham feather mark
            '\u{2045}' => Some('\u{2046}'), // square bracket with quill
            '\u{207d}' => Some('\u{207e}'), // superscript parenthesis
            '\u{208d}' => Some('\u{208e}'), // subscript parenthesis
            '\u{2768}' => Some('\u{2769}'), // medium parenthesis ornament
            '\u{276a}' => Some('\u{276b}'), // medium flattened parenthesis ornament
            '\u{276c}' => Some('\u{276d}'), // medium pointing angle bracket ornament
            '\u{276e}' => Some('\u{276f}'), // heavy pointing angle quotation mark ornament
            '\u{2770}' => Some('\u{2771}'), // heavy pointing angle bracket ornament
            '\u{2772}' => Some('\u{2773}'), // light tortoise shell bracket ornament
            '\u{2774}' => Some('\u{2775}'), // medium curly bracket ornament
            '\u{27e6}' => Some('\u{27e7}'), // mathematical white square bracket
            '\u{27e8}' => Some('\u{27e9}'), // mathematical angle bracket
            '\u{27ea}' => Some('\u{27eb}'), // mathematical double angle bracket
            '\u{2983}' => Some('\u{2984}'), // white curly bracket
            '\u{2985}' => Some('\u{2986}'), // white parenthesis
            '\u{2987}' => Some('\u{2988}'), // z notation image bracket
            '\u{2989}' => Some('\u{298a}'), // z notation binding bracket
            '\u{2991}' => Some('\u{2992}'), // left/right angle bracket with dot
            '\u{2993}' => Some('\u{2994}'), // left/right arc less/greater-than bracket
            '\u{2995}' => Some('\u{2996}'), // double left/right arc greater/less-than bracket
            '\u{2997}' => Some('\u{2998}'), // left/right black tortoise shell bracket
            '\u{29fc}' => Some('\u{29fd}'), // left/right-pointing curved angle bracket
            _ => None,
        }
    }

    pub(super) fn skip_bracketed_comment(&mut self, open: char, close: char) {
        let mut depth = 1usize;
        while self.pos < self.src.len() && depth > 0 {
            let c = self.src[self.pos];
            if c == '\n' {
                self.line += 1;
            }
            if c == open {
                depth += 1;
            } else if c == close {
                depth -= 1;
            }
            self.pos += 1;
        }
    }

    pub(super) fn bump(&mut self) -> char {
        let c = self.src[self.pos];
        self.pos += 1;
        c
    }

    pub(super) fn peek(&self) -> Option<char> {
        self.src.get(self.pos).copied()
    }

    pub(super) fn peek_next(&self) -> Option<char> {
        self.src.get(self.pos + 1).copied()
    }

    pub(super) fn is_ident_hyphen(&self, c: char) -> bool {
        c == '-'
            && self
                .peek_next()
                .map(|n| n.is_ascii_alphabetic() || n.is_alphabetic())
                .unwrap_or(false)
    }

    pub(super) fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    pub(super) fn try_match_str(&mut self, expected: &str) -> bool {
        let chars: Vec<char> = expected.chars().collect();
        if self.pos + chars.len() > self.src.len() {
            return false;
        }
        for (i, c) in chars.iter().enumerate() {
            if self.src[self.pos + i] != *c {
                return false;
            }
        }
        self.pos += chars.len();
        true
    }
}
