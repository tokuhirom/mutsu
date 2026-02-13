use crate::value::VersionPart;
use num_bigint::BigInt as NumBigInt;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum DStrPart {
    Lit(String),
    Var(String),
    Block(String),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenKind {
    Number(i64),
    BigNumber(NumBigInt),
    Float(f64),
    Imaginary(f64),
    Str(String),
    DStr(Vec<DStrPart>),
    Regex(String),
    Subst {
        pattern: String,
        replacement: String,
    },
    Ident(String),
    Var(String),
    CaptureVar(String),
    HashVar(String),
    RoutineMagic,
    BlockMagic,
    ArrayVar(String),
    CodeVar(String),
    True,
    False,
    Nil,
    Plus,
    Minus,
    Star,
    StarStar,
    Percent,
    PercentPercent,
    Slash,
    Tilde,
    Eq,
    EqEq,
    FatArrow,
    MatchAssign,
    Dot,
    DotDot,
    DotDotDot,
    DotDotDotCaret,
    DotDotCaret,
    CaretDotDot,
    CaretDotDotCaret,
    EqEqEq,
    Arrow,
    SmartMatch,
    BangEq,
    BangTilde,
    BangBang,
    Lt,
    Lte,
    Gt,
    Gte,
    AndAnd,
    OrOr,
    OrWord,
    OrElse,
    AndThen,
    NotAndThen,
    Bang,
    QuestionQuestion,
    Ampersand,
    Pipe,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Colon,
    PlusPlus,
    MinusMinus,
    PlusEq,
    MinusEq,
    TildeEq,
    StarEq,
    SlashSlash,
    DotEq,
    Bind,
    Question,
    Caret,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    LtEqGt,
    SetUnion,          // (|) ∪
    SetIntersect,      // (&) ∩
    SetDiff,           // (-)
    SetSymDiff,        // (^)
    SetElem,           // (elem) ∈
    SetCont,           // (cont)
    SetSubset,         // (<=) ⊆
    SetSuperset,       // (>=) ⊇
    SetStrictSubset,   // (<) ⊂
    SetStrictSuperset, // (>) ⊃
    HyperLeft,         // << or «
    HyperRight,        // >> or »
    VersionLiteral {
        parts: Vec<VersionPart>,
        plus: bool,
        minus: bool,
    },
    Semicolon,
    Eof,
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) line: usize,
}

/// Look up a Unicode character by name, with fallback for control characters and aliases.
pub(crate) fn lookup_unicode_char_by_name(name: &str) -> Option<char> {
    // Try the unicode_names2 crate first (covers most named characters)
    if let Some(c) = unicode_names2::character(name) {
        return Some(c);
    }
    // Fallback: control characters and common aliases (case-insensitive)
    let upper = name.to_uppercase();
    match upper.as_str() {
        "NULL" | "NUL" => Some('\u{0000}'),
        "START OF HEADING" | "SOH" => Some('\u{0001}'),
        "START OF TEXT" | "STX" => Some('\u{0002}'),
        "END OF TEXT" | "ETX" => Some('\u{0003}'),
        "END OF TRANSMISSION" | "EOT" => Some('\u{0004}'),
        "ENQUIRY" | "ENQ" => Some('\u{0005}'),
        "ACKNOWLEDGE" | "ACK" => Some('\u{0006}'),
        "BELL" | "BEL" | "ALERT" => Some('\u{0007}'),
        "BACKSPACE" | "BS" => Some('\u{0008}'),
        "CHARACTER TABULATION" | "HORIZONTAL TABULATION" | "HT" | "TAB" => Some('\u{0009}'),
        "LINE FEED" | "LINE FEED (LF)" | "NEW LINE" | "END OF LINE" | "LF" | "NL" | "EOL" => {
            Some('\u{000A}')
        }
        "LINE TABULATION" | "VERTICAL TABULATION" | "VT" => Some('\u{000B}'),
        "FORM FEED" | "FORM FEED (FF)" | "FF" => Some('\u{000C}'),
        "CARRIAGE RETURN" | "CARRIAGE RETURN (CR)" | "CR" => Some('\u{000D}'),
        "SHIFT OUT" | "LOCKING-SHIFT ONE" | "SO" => Some('\u{000E}'),
        "SHIFT IN" | "LOCKING-SHIFT ZERO" | "SI" => Some('\u{000F}'),
        "DATA LINK ESCAPE" | "DLE" => Some('\u{0010}'),
        "DEVICE CONTROL ONE" | "DC1" => Some('\u{0011}'),
        "DEVICE CONTROL TWO" | "DC2" => Some('\u{0012}'),
        "DEVICE CONTROL THREE" | "DC3" => Some('\u{0013}'),
        "DEVICE CONTROL FOUR" | "DC4" => Some('\u{0014}'),
        "NEGATIVE ACKNOWLEDGE" | "NAK" => Some('\u{0015}'),
        "SYNCHRONOUS IDLE" | "SYN" => Some('\u{0016}'),
        "END OF TRANSMISSION BLOCK" | "ETB" => Some('\u{0017}'),
        "CANCEL" | "CAN" => Some('\u{0018}'),
        "END OF MEDIUM" | "EM" => Some('\u{0019}'),
        "SUBSTITUTE" | "SUB" => Some('\u{001A}'),
        "ESCAPE" | "ESC" => Some('\u{001B}'),
        "INFORMATION SEPARATOR FOUR" | "FILE SEPARATOR" | "FS" => Some('\u{001C}'),
        "INFORMATION SEPARATOR THREE" | "GROUP SEPARATOR" | "GS" => Some('\u{001D}'),
        "INFORMATION SEPARATOR TWO" | "RECORD SEPARATOR" | "RS" => Some('\u{001E}'),
        "INFORMATION SEPARATOR ONE" | "UNIT SEPARATOR" | "US" => Some('\u{001F}'),
        "SPACE" | "SP" => Some('\u{0020}'),
        "DELETE" | "DEL" => Some('\u{007F}'),
        "NEXT LINE" | "NEXT LINE (NEL)" | "NEL" => Some('\u{0085}'),
        "NO-BREAK SPACE" | "NBSP" => Some('\u{00A0}'),
        _ => None,
    }
}

pub(crate) struct Lexer {
    src: Vec<char>,
    pos: usize,
    line: usize,
    finish_content: Option<String>,
    /// True when the last emitted token was a "term" (value/closing delimiter),
    /// meaning the next `/` should be treated as a division operator.
    /// False when the last token was an operator or opening delimiter,
    /// meaning the next `/` should be treated as a regex delimiter.
    last_was_term: bool,
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
    fn parse_hex_escape(&mut self) -> Option<char> {
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
    fn parse_octal_escape(&mut self) -> Option<char> {
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
    fn parse_named_char_escape(&mut self) -> Option<char> {
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

    pub(crate) fn next_token(&mut self) -> Token {
        loop {
            self.skip_ws_and_comments();
            let token_line = self.line;
            if self.pos >= self.src.len() {
                return Token {
                    kind: TokenKind::Eof,
                    line: token_line,
                };
            }
            let ch = self.bump();
            let kind = match ch {
                '.' => {
                    if self.peek() == Some('.') {
                        self.pos += 1;
                        if self.peek() == Some('^') {
                            self.pos += 1;
                            TokenKind::DotDotCaret
                        } else if self.peek() == Some('.') {
                            self.pos += 1;
                            if self.peek() == Some('^') {
                                self.pos += 1;
                                TokenKind::DotDotDotCaret
                            } else {
                                TokenKind::DotDotDot
                            }
                        } else {
                            TokenKind::DotDot
                        }
                    } else if self.peek() == Some('=') {
                        self.pos += 1;
                        TokenKind::DotEq
                    } else {
                        TokenKind::Dot
                    }
                }
                'q' => {
                    if self.peek() == Some('<') {
                        self.pos += 1;
                        let mut s = String::new();
                        while let Some(c) = self.peek() {
                            self.pos += 1;
                            if c == '>' {
                                break;
                            }
                            s.push(c);
                        }
                        TokenKind::Str(s)
                    } else if self.peek() == Some('[') {
                        self.pos += 1;
                        let mut s = String::new();
                        while let Some(c) = self.peek() {
                            self.pos += 1;
                            if c == ']' {
                                break;
                            }
                            s.push(c);
                        }
                        TokenKind::Str(s)
                    } else if self.peek() == Some('/') {
                        // q/string/
                        self.pos += 1;
                        let s = self.read_delimited_string('/');
                        TokenKind::Str(s)
                    } else if self.peek() == Some('(') {
                        // q(string)
                        self.pos += 1;
                        let s = self.read_bracketed_string('(', ')');
                        TokenKind::Str(s)
                    } else if self.peek() == Some('{') {
                        // q{string}
                        self.pos += 1;
                        let s = self.read_bracketed_string('{', '}');
                        TokenKind::Str(s)
                    } else {
                        // Try qq direct-delimiter, q/qq adverb forms, or identifier
                        let save_pos = self.pos;
                        let save_line = self.line;
                        let mut is_qq = false;

                        // Check for qq prefix
                        if self.peek() == Some('q') {
                            if let Some(next) = self.peek_next()
                                && matches!(next, '/' | '(' | '[' | '{' | '<')
                            {
                                // qq direct-delimiter form (qq/, qq(, etc.)
                                self.pos += 1; // skip second 'q'
                                self.pos += 1; // skip delimiter
                                let result = if matches!(next, '(' | '[' | '{' | '<') {
                                    let close = match next {
                                        '(' => ')',
                                        '[' => ']',
                                        '{' => '}',
                                        '<' => '>',
                                        _ => '/',
                                    };
                                    self.read_interpolated_string(close, Some(next))
                                } else {
                                    self.read_interpolated_string(next, None)
                                };
                                self.last_was_term = true;
                                return Token {
                                    kind: result,
                                    line: token_line,
                                };
                            }
                            is_qq = true;
                            self.pos += 1; // skip second 'q'
                        }

                        // Skip optional whitespace before ':'
                        while self.peek() == Some(' ') {
                            self.pos += 1;
                        }

                        if self.peek() == Some(':') {
                            self.pos += 1;
                            // Read adverb name
                            let mut adverb = String::new();
                            while let Some(c) = self.peek() {
                                if c.is_ascii_alphabetic() {
                                    adverb.push(c);
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            if adverb == "to" || adverb == "heredoc" {
                                // Skip optional whitespace before delimiter
                                while self.peek() == Some(' ') {
                                    self.pos += 1;
                                }
                                self.last_was_term = true;
                                return Token {
                                    kind: self.read_heredoc(is_qq, false),
                                    line: token_line,
                                };
                            }
                        }

                        // Not a special form, restore and read as identifier
                        self.pos = save_pos;
                        self.line = save_line;
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            "Any" => TokenKind::Ident("Any".to_string()),
                            "or" => TokenKind::OrWord,
                            "orelse" => TokenKind::OrElse,
                            "andthen" => TokenKind::AndThen,
                            "notandthen" => TokenKind::NotAndThen,
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
                'm' => {
                    if self.peek() == Some('/') {
                        self.pos += 1;
                        let regex = self.read_regex_literal();
                        TokenKind::Regex(regex)
                    } else if self.peek() == Some('{') {
                        self.pos += 1;
                        let regex = self.read_brace_regex_literal();
                        TokenKind::Regex(regex)
                    } else {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            "Any" => TokenKind::Ident("Any".to_string()),
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
                'r' => {
                    if self.peek() == Some('x') && self.peek_next() == Some('/') {
                        self.pos += 1;
                        self.pos += 1;
                        let regex = self.read_regex_literal();
                        TokenKind::Regex(regex)
                    } else if self.peek() == Some('/') {
                        self.pos += 1;
                        let regex = self.read_regex_literal();
                        TokenKind::Regex(regex)
                    } else {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            "Any" => TokenKind::Ident("Any".to_string()),
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
                's' => {
                    if self.peek() == Some('/') {
                        self.pos += 1;
                        let pattern = self.read_regex_literal();
                        let replacement = self.read_delimited_string('/');
                        TokenKind::Subst {
                            pattern,
                            replacement,
                        }
                    } else {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            "Any" => TokenKind::Ident("Any".to_string()),
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
                '0'..='9' => {
                    // Radix literals: 0x, 0o, 0b
                    if ch == '0' {
                        if self.peek() == Some('x') || self.peek() == Some('X') {
                            self.pos += 1;
                            let mut hex = String::new();
                            while let Some(c) = self.peek() {
                                if c.is_ascii_hexdigit() || c == '_' {
                                    if c != '_' {
                                        hex.push(c);
                                    }
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            let value = i64::from_str_radix(&hex, 16).unwrap_or(0);
                            self.last_was_term = true;
                            return Token {
                                kind: TokenKind::Number(value),
                                line: token_line,
                            };
                        }
                        if self.peek() == Some('o') || self.peek() == Some('O') {
                            self.pos += 1;
                            let mut oct = String::new();
                            while let Some(c) = self.peek() {
                                if c.is_ascii_digit() && c < '8' || c == '_' {
                                    if c != '_' {
                                        oct.push(c);
                                    }
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            let value = i64::from_str_radix(&oct, 8).unwrap_or(0);
                            self.last_was_term = true;
                            return Token {
                                kind: TokenKind::Number(value),
                                line: token_line,
                            };
                        }
                        if self.peek() == Some('b') || self.peek() == Some('B') {
                            self.pos += 1;
                            let mut bin = String::new();
                            while let Some(c) = self.peek() {
                                if c == '0' || c == '1' || c == '_' {
                                    if c != '_' {
                                        bin.push(c);
                                    }
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            let value = i64::from_str_radix(&bin, 2).unwrap_or(0);
                            self.last_was_term = true;
                            return Token {
                                kind: TokenKind::Number(value),
                                line: token_line,
                            };
                        }
                    }
                    let mut num = ch.to_string();
                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() || c == '_' {
                            if c != '_' {
                                num.push(c);
                            }
                            self.pos += 1;
                        } else {
                            break;
                        }
                    }
                    // Check for decimal point
                    if self.peek() == Some('.')
                        && self.peek_next().is_some_and(|c| c.is_ascii_digit())
                    {
                        num.push('.');
                        self.pos += 1;
                        while let Some(c) = self.peek() {
                            if c.is_ascii_digit() || c == '_' {
                                if c != '_' {
                                    num.push(c);
                                }
                                self.pos += 1;
                            } else {
                                break;
                            }
                        }
                        // Check for exponent
                        if matches!(self.peek(), Some('e') | Some('E')) {
                            num.push('e');
                            self.pos += 1;
                            if matches!(self.peek(), Some('+') | Some('-')) {
                                num.push(self.bump());
                            }
                            while let Some(c) = self.peek() {
                                if c.is_ascii_digit() {
                                    num.push(c);
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                        }
                        let value = num.parse::<f64>().unwrap_or(0.0);
                        if self.peek() == Some('i') {
                            self.pos += 1;
                            TokenKind::Imaginary(value)
                        } else {
                            TokenKind::Float(value)
                        }
                    } else {
                        // Check for exponent on integer (e.g. 1e10)
                        if matches!(self.peek(), Some('e') | Some('E')) {
                            num.push('e');
                            self.pos += 1;
                            if matches!(self.peek(), Some('+') | Some('-')) {
                                num.push(self.bump());
                            }
                            while let Some(c) = self.peek() {
                                if c.is_ascii_digit() {
                                    num.push(c);
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            let value = num.parse::<f64>().unwrap_or(0.0);
                            if self.peek() == Some('i') {
                                self.pos += 1;
                                TokenKind::Imaginary(value)
                            } else {
                                TokenKind::Float(value)
                            }
                        } else if let Ok(value) = num.parse::<i64>() {
                            if self.peek() == Some('i') {
                                self.pos += 1;
                                TokenKind::Imaginary(value as f64)
                            } else {
                                TokenKind::Number(value)
                            }
                        } else if let Ok(value) = num.parse::<NumBigInt>() {
                            TokenKind::BigNumber(value)
                        } else {
                            TokenKind::Number(i64::MAX)
                        }
                    }
                }
                '"' => {
                    let mut parts: Vec<DStrPart> = Vec::new();
                    let mut current = String::new();
                    let mut has_interp = false;
                    while let Some(c) = self.peek() {
                        if c == '"' {
                            self.pos += 1;
                            break;
                        }
                        if c == '\\' {
                            self.pos += 1;
                            if let Some(n) = self.peek() {
                                self.pos += 1;
                                match n {
                                    'n' => current.push('\n'),
                                    't' => current.push('\t'),
                                    'r' => current.push('\r'),
                                    '"' => current.push('"'),
                                    '\\' => current.push('\\'),
                                    '$' => current.push('$'),
                                    '{' => current.push('{'),
                                    'x' => {
                                        if let Some(ch) = self.parse_hex_escape() {
                                            current.push(ch);
                                        }
                                    }
                                    'o' => {
                                        if let Some(ch) = self.parse_octal_escape() {
                                            current.push(ch);
                                        }
                                    }
                                    'c' => {
                                        if let Some(ch) = self.parse_named_char_escape() {
                                            current.push(ch);
                                        }
                                    }
                                    '0' => current.push('\0'),
                                    'a' => current.push('\x07'),
                                    'e' => current.push('\x1B'),
                                    _ => current.push(n),
                                }
                            }
                        } else if c == '$' {
                            self.pos += 1;
                            if !current.is_empty() {
                                parts.push(DStrPart::Lit(current.clone()));
                                current.clear();
                            }
                            let mut var_name = String::new();
                            // Handle twigils: $*FOO, $!foo, $?foo
                            if matches!(self.peek(), Some('*') | Some('!') | Some('?')) {
                                var_name.push(self.peek().unwrap());
                                self.pos += 1;
                            }
                            while let Some(vc) = self.peek() {
                                if vc.is_ascii_alphanumeric()
                                    || vc == '_'
                                    || self.is_ident_hyphen(vc)
                                {
                                    var_name.push(vc);
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            if !var_name.is_empty() {
                                parts.push(DStrPart::Var(var_name));
                                has_interp = true;
                            } else {
                                current.push('$');
                            }
                        } else if c == '@' {
                            self.pos += 1;
                            if !current.is_empty() {
                                parts.push(DStrPart::Lit(current.clone()));
                                current.clear();
                            }
                            let mut var_name = String::new();
                            if matches!(self.peek(), Some('*')) {
                                var_name.push(self.peek().unwrap());
                                self.pos += 1;
                            }
                            while let Some(vc) = self.peek() {
                                if vc.is_ascii_alphanumeric()
                                    || vc == '_'
                                    || self.is_ident_hyphen(vc)
                                {
                                    var_name.push(vc);
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            if !var_name.is_empty() {
                                parts.push(DStrPart::Var(format!("@{}", var_name)));
                                has_interp = true;
                            } else {
                                current.push('@');
                            }
                        } else if c == '{' {
                            self.pos += 1;
                            if !current.is_empty() {
                                parts.push(DStrPart::Lit(current.clone()));
                                current.clear();
                            }
                            let mut block = String::new();
                            let mut depth = 1usize;
                            while let Some(bc) = self.peek() {
                                self.pos += 1;
                                if bc == '{' {
                                    depth += 1;
                                    block.push(bc);
                                } else if bc == '}' {
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                    block.push(bc);
                                } else {
                                    block.push(bc);
                                }
                            }
                            parts.push(DStrPart::Block(block));
                            has_interp = true;
                        } else {
                            self.pos += 1;
                            current.push(c);
                        }
                    }
                    if !current.is_empty() {
                        parts.push(DStrPart::Lit(current));
                    }
                    if has_interp {
                        TokenKind::DStr(parts)
                    } else {
                        // No interpolation, just a plain string
                        let s = parts
                            .into_iter()
                            .map(|p| match p {
                                DStrPart::Lit(s) => s,
                                _ => String::new(),
                            })
                            .collect::<String>();
                        TokenKind::Str(s)
                    }
                }
                '\'' => {
                    let mut s = String::new();
                    while let Some(c) = self.peek() {
                        self.pos += 1;
                        if c == '\'' {
                            break;
                        }
                        if c == '\\' {
                            if let Some(n) = self.peek() {
                                self.pos += 1;
                                match n {
                                    'n' => s.push('\n'),
                                    't' => s.push('\t'),
                                    '\'' => s.push('\''),
                                    '\\' => s.push('\\'),
                                    _ => s.push(n),
                                }
                            }
                        } else {
                            s.push(c);
                        }
                    }
                    TokenKind::Str(s)
                }
                '\u{ff62}' => {
                    let mut s = String::new();
                    while let Some(c) = self.peek() {
                        self.pos += 1;
                        if c == '\u{ff63}' {
                            break;
                        }
                        s.push(c);
                    }
                    TokenKind::Str(s)
                }
                '$' => {
                    if self.peek() == Some('<') {
                        self.pos += 1;
                        let mut name = String::new();
                        while let Some(c) = self.peek() {
                            self.pos += 1;
                            if c == '>' {
                                break;
                            }
                            name.push(c);
                        }
                        TokenKind::CaptureVar(name)
                    } else if self.peek() == Some('!') {
                        // Check if $!attr (attribute access) vs $! (error variable)
                        if self.src.get(self.pos + 1).is_some_and(|c| {
                            c.is_ascii_alphabetic() || c.is_alphabetic() || *c == '_'
                        }) {
                            self.pos += 1; // skip '!'
                            let name = self.read_ident();
                            TokenKind::Var(format!("!{}", name))
                        } else {
                            self.pos += 1;
                            TokenKind::Var("!".to_string())
                        }
                    } else if self.peek() == Some('/') {
                        // $/ - match variable
                        self.pos += 1;
                        TokenKind::Var("/".to_string())
                    } else if self.peek() == Some('.') {
                        // Check if $.attr (public attribute accessor)
                        if self.src.get(self.pos + 1).is_some_and(|c| {
                            c.is_ascii_alphabetic() || c.is_alphabetic() || *c == '_'
                        }) {
                            self.pos += 1; // skip '.'
                            let name = self.read_ident();
                            TokenKind::Var(format!(".{}", name))
                        } else {
                            let ident = self.read_ident();
                            TokenKind::Var(ident)
                        }
                    } else if self.peek() == Some('=') {
                        // $=finish and other pod variables
                        self.pos += 1; // skip '='
                        let name = self.read_ident();
                        TokenKind::Var(format!("={}", name))
                    } else {
                        let ident = self.read_ident();
                        TokenKind::Var(ident)
                    }
                }
                '@' => {
                    let ident = self.read_ident();
                    TokenKind::ArrayVar(ident)
                }
                '%' => {
                    if self.match_char('%') {
                        TokenKind::PercentPercent
                    } else if self.peek().is_none_or(|c| {
                        c.is_ascii_alphabetic() || c.is_alphabetic() || c == '_' || c == '*'
                    }) {
                        let ident = self.read_ident();
                        TokenKind::HashVar(ident)
                    } else {
                        TokenKind::Percent
                    }
                }
                '+' => {
                    if self.match_char('+') {
                        TokenKind::PlusPlus
                    } else if self.match_char('=') {
                        TokenKind::PlusEq
                    } else if self.match_char('&') {
                        TokenKind::BitAnd
                    } else if self.match_char('|') {
                        TokenKind::BitOr
                    } else if self.match_char('^') {
                        TokenKind::BitXor
                    } else if self.peek() == Some('<') && self.peek_next() != Some('<') {
                        self.pos += 1;
                        TokenKind::BitShiftLeft
                    } else if self.peek() == Some('>') && self.peek_next() != Some('>') {
                        self.pos += 1;
                        TokenKind::BitShiftRight
                    } else {
                        TokenKind::Plus
                    }
                }
                '-' => {
                    if self.match_char('-') {
                        TokenKind::MinusMinus
                    } else if self.match_char('>') {
                        TokenKind::Arrow
                    } else if self.match_char('=') {
                        TokenKind::MinusEq
                    } else {
                        TokenKind::Minus
                    }
                }
                '*' => {
                    if self.match_char('=') {
                        TokenKind::StarEq
                    } else if self.match_char('*') {
                        TokenKind::StarStar
                    } else {
                        TokenKind::Star
                    }
                }
                '/' => {
                    if self.match_char('/') {
                        TokenKind::SlashSlash
                    } else if !self.last_was_term {
                        // In term position (after operator/delimiter), `/` starts a regex
                        let regex = self.read_regex_literal();
                        TokenKind::Regex(regex)
                    } else if let Some(regex) = self.try_read_regex_literal() {
                        TokenKind::Regex(regex)
                    } else {
                        TokenKind::Slash
                    }
                }
                '~' => {
                    if self.match_char('~') {
                        TokenKind::SmartMatch
                    } else if self.match_char('=') {
                        TokenKind::TildeEq
                    } else {
                        TokenKind::Tilde
                    }
                }
                '=' => {
                    if self.match_char('=') {
                        if self.match_char('=') {
                            TokenKind::EqEqEq
                        } else {
                            TokenKind::EqEq
                        }
                    } else if self.match_char('~') {
                        TokenKind::MatchAssign
                    } else if self.match_char('>') {
                        TokenKind::FatArrow
                    } else {
                        TokenKind::Eq
                    }
                }
                '!' => {
                    if self.match_char('!') {
                        TokenKind::BangBang
                    } else if self.match_char('=') {
                        TokenKind::BangEq
                    } else if self.match_char('~') {
                        self.match_char('~');
                        TokenKind::BangTilde
                    } else {
                        TokenKind::Bang
                    }
                }
                '<' => {
                    if self.match_char('<') {
                        TokenKind::HyperLeft
                    } else if self.match_char('=') {
                        if self.match_char('>') {
                            TokenKind::LtEqGt
                        } else {
                            TokenKind::Lte
                        }
                    } else {
                        TokenKind::Lt
                    }
                }
                '>' => {
                    if self.match_char('>') {
                        TokenKind::HyperRight
                    } else if self.match_char('=') {
                        TokenKind::Gte
                    } else {
                        TokenKind::Gt
                    }
                }
                '&' => {
                    if self.peek() == Some('?') {
                        self.pos += 1;
                        let first = self.bump();
                        let ident = self.read_ident_start(first);
                        if ident == "ROUTINE" {
                            TokenKind::RoutineMagic
                        } else if ident == "BLOCK" {
                            TokenKind::BlockMagic
                        } else {
                            TokenKind::Ident(format!("?{}", ident))
                        }
                    } else if self.match_char('&') {
                        TokenKind::AndAnd
                    } else if self
                        .peek()
                        .is_some_and(|c| c.is_ascii_alphabetic() || c.is_alphabetic() || c == '_')
                    {
                        let mut name = String::new();
                        while let Some(c) = self.peek() {
                            if c.is_ascii_alphanumeric()
                                || c.is_alphabetic()
                                || c == '_'
                                || c == '-'
                            {
                                name.push(c);
                                self.pos += 1;
                            } else {
                                break;
                            }
                        }
                        TokenKind::CodeVar(name)
                    } else {
                        TokenKind::Ampersand
                    }
                }
                '?' => {
                    if self.match_char('?') {
                        TokenKind::QuestionQuestion
                    } else {
                        TokenKind::Question
                    }
                }
                '|' => {
                    if self.match_char('|') {
                        TokenKind::OrOr
                    } else {
                        TokenKind::Pipe
                    }
                }
                '(' => {
                    if self.try_match_str("elem)") {
                        TokenKind::SetElem
                    } else if self.try_match_str("cont)") {
                        TokenKind::SetCont
                    } else if self.try_match_str("<=)") {
                        TokenKind::SetSubset
                    } else if self.try_match_str(">=)") {
                        TokenKind::SetSuperset
                    } else if self.try_match_str("|)") {
                        TokenKind::SetUnion
                    } else if self.try_match_str("&)") {
                        TokenKind::SetIntersect
                    } else if self.try_match_str("-)") {
                        TokenKind::SetDiff
                    } else if self.try_match_str("^)") {
                        TokenKind::SetSymDiff
                    } else if self.try_match_str("<)") {
                        TokenKind::SetStrictSubset
                    } else if self.try_match_str(">)") {
                        TokenKind::SetStrictSuperset
                    } else {
                        TokenKind::LParen
                    }
                }
                ')' => TokenKind::RParen,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                ',' => TokenKind::Comma,
                ':' => {
                    if self.match_char('=') {
                        TokenKind::Bind
                    } else {
                        TokenKind::Colon
                    }
                }
                ';' => TokenKind::Semicolon,
                '^' => {
                    if self.peek() == Some('.') && self.peek_next() == Some('.') {
                        self.pos += 2; // skip ".."
                        if self.peek() == Some('^') {
                            self.pos += 1;
                            TokenKind::CaretDotDotCaret
                        } else {
                            TokenKind::CaretDotDot
                        }
                    } else {
                        TokenKind::Caret
                    }
                }
                '\u{2208}' => TokenKind::SetElem,           // ∈
                '\u{2209}' => TokenKind::SetElem,           // ∉ (handled as negated in parser)
                '\u{220B}' => TokenKind::SetCont,           // ∋
                '\u{222A}' => TokenKind::SetUnion,          // ∪
                '\u{2229}' => TokenKind::SetIntersect,      // ∩
                '\u{2216}' => TokenKind::SetDiff,           // ∖
                '\u{2296}' => TokenKind::SetSymDiff,        // ⊖
                '\u{2286}' => TokenKind::SetSubset,         // ⊆
                '\u{2287}' => TokenKind::SetSuperset,       // ⊇
                '\u{2282}' => TokenKind::SetStrictSubset,   // ⊂
                '\u{2283}' => TokenKind::SetStrictSuperset, // ⊃
                '\u{00ab}' => TokenKind::HyperLeft,         // «
                '\u{00bb}' => TokenKind::HyperRight,        // »
                _ => {
                    if ch == 'v' && self.peek().is_some_and(|c| c.is_ascii_digit()) {
                        self.read_version_literal()
                    } else if ch.is_ascii_alphabetic() || ch.is_alphabetic() || ch == '_' {
                        let ident = self.read_ident_start(ch);
                        // Handle Q:to<MARKER> heredoc (completely literal)
                        if ident == "Q" && self.peek() == Some(':') {
                            let save_pos = self.pos;
                            self.pos += 1; // skip ':'
                            let mut adverb = String::new();
                            while let Some(c) = self.peek() {
                                if c.is_ascii_alphabetic() {
                                    adverb.push(c);
                                    self.pos += 1;
                                } else {
                                    break;
                                }
                            }
                            if adverb == "to" || adverb == "heredoc" {
                                while self.peek() == Some(' ') {
                                    self.pos += 1;
                                }
                                self.last_was_term = true;
                                return Token {
                                    kind: self.read_heredoc(false, true),
                                    line: token_line,
                                };
                            }
                            self.pos = save_pos;
                        }
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            "Any" => TokenKind::Ident("Any".to_string()),
                            "or" => TokenKind::OrWord,
                            "orelse" => TokenKind::OrElse,
                            "andthen" => TokenKind::AndThen,
                            "notandthen" => TokenKind::NotAndThen,
                            _ => TokenKind::Ident(ident),
                        }
                    } else {
                        continue;
                    }
                }
            };
            self.last_was_term = matches!(
                kind,
                TokenKind::Number(_)
                    | TokenKind::BigNumber(_)
                    | TokenKind::Float(_)
                    | TokenKind::Imaginary(_)
                    | TokenKind::Str(_)
                    | TokenKind::DStr(_)
                    | TokenKind::Regex(_)
                    | TokenKind::Ident(_)
                    | TokenKind::Var(_)
                    | TokenKind::ArrayVar(_)
                    | TokenKind::HashVar(_)
                    | TokenKind::CaptureVar(_)
                    | TokenKind::CodeVar(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Nil
                    | TokenKind::RParen
                    | TokenKind::RBracket
                    | TokenKind::RBrace
                    | TokenKind::HyperRight
            );
            return Token {
                kind,
                line: token_line,
            };
        }
    }

    fn read_ident(&mut self) -> String {
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

    fn read_version_literal(&mut self) -> TokenKind {
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

    fn read_ident_start(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);
        self.read_ident_tail(&mut ident);
        ident
    }

    fn read_ident_tail(&mut self, ident: &mut String) {
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

    fn skip_ws_and_comments(&mut self) {
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

    fn matching_bracket(open: char) -> Option<char> {
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

    fn skip_bracketed_comment(&mut self, open: char, close: char) {
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

    fn bump(&mut self) -> char {
        let c = self.src[self.pos];
        self.pos += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.src.get(self.pos + 1).copied()
    }

    fn is_ident_hyphen(&self, c: char) -> bool {
        c == '-'
            && self
                .peek_next()
                .map(|n| n.is_ascii_alphabetic() || n.is_alphabetic())
                .unwrap_or(false)
    }

    fn try_read_regex_literal(&mut self) -> Option<String> {
        let mut i = self.pos;
        let mut found = false;
        let mut escaped = false;
        let mut in_bracket = 0usize;
        let mut in_quote: Option<char> = None;
        while i < self.src.len() {
            let c = self.src[i];
            if !escaped {
                if let Some(q) = in_quote {
                    if c == q {
                        in_quote = None;
                    }
                } else if c == '\'' || c == '"' {
                    in_quote = Some(c);
                } else if c == '[' {
                    in_bracket += 1;
                } else if c == ']' && in_bracket > 0 {
                    in_bracket -= 1;
                }
            }
            if !escaped && in_bracket == 0 && in_quote.is_none() && c == '/' {
                found = true;
                break;
            }
            if !escaped && c == '\\' {
                escaped = true;
                i += 1;
                continue;
            }
            escaped = false;
            if c.is_whitespace() || matches!(c, ',' | ')' | '}' | ';') {
                break;
            }
            i += 1;
        }
        if !found {
            return None;
        }
        let mut s = String::new();
        let mut escaped = false;
        let mut in_bracket = 0usize;
        let mut in_quote: Option<char> = None;
        while let Some(c) = self.peek() {
            self.pos += 1;
            if !escaped {
                if let Some(q) = in_quote {
                    if c == q {
                        in_quote = None;
                    }
                } else if c == '\'' || c == '"' {
                    in_quote = Some(c);
                } else if c == '[' {
                    in_bracket += 1;
                } else if c == ']' && in_bracket > 0 {
                    in_bracket -= 1;
                }
            }
            if !escaped && in_bracket == 0 && in_quote.is_none() && c == '/' {
                break;
            }
            if !escaped && c == '\\' {
                escaped = true;
                s.push(c);
                continue;
            }
            escaped = false;
            s.push(c);
        }
        Some(s)
    }

    fn read_brace_regex_literal(&mut self) -> String {
        let mut s = String::new();
        let mut depth = 1usize;
        let mut escaped = false;
        while let Some(c) = self.peek() {
            self.pos += 1;
            if escaped {
                escaped = false;
                s.push(c);
                continue;
            }
            if c == '\\' {
                escaped = true;
                s.push(c);
                continue;
            }
            if c == '{' {
                depth += 1;
                s.push(c);
            } else if c == '}' {
                depth -= 1;
                if depth == 0 {
                    break;
                }
                s.push(c);
            } else {
                s.push(c);
            }
        }
        s
    }

    fn read_regex_literal(&mut self) -> String {
        let mut s = String::new();
        let mut escaped = false;
        let mut in_bracket = 0usize;
        let mut in_brace = 0usize;
        let mut in_quote: Option<char> = None;
        while let Some(c) = self.peek() {
            self.pos += 1;
            if !escaped {
                if let Some(q) = in_quote {
                    if c == q {
                        in_quote = None;
                    }
                } else if c == '\'' || c == '"' {
                    in_quote = Some(c);
                } else if c == '[' {
                    in_bracket += 1;
                } else if c == ']' && in_bracket > 0 {
                    in_bracket -= 1;
                } else if c == '{' {
                    in_brace += 1;
                } else if c == '}' && in_brace > 0 {
                    in_brace -= 1;
                }
            }
            if !escaped && in_bracket == 0 && in_brace == 0 && in_quote.is_none() && c == '/' {
                break;
            }
            if !escaped && c == '\\' {
                escaped = true;
                s.push(c);
                continue;
            }
            escaped = false;
            s.push(c);
        }
        s
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn try_match_str(&mut self, expected: &str) -> bool {
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

    fn read_delimited_string(&mut self, delim: char) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            self.pos += 1;
            if c == '\\'
                && let Some(n) = self.peek()
            {
                self.pos += 1;
                if n == delim {
                    s.push(n);
                } else {
                    s.push('\\');
                    s.push(n);
                }
                continue;
            }
            if c == delim {
                break;
            }
            if c == '\n' {
                self.line += 1;
            }
            s.push(c);
        }
        s
    }

    fn read_interpolated_string(&mut self, close: char, open: Option<char>) -> TokenKind {
        let mut parts: Vec<DStrPart> = Vec::new();
        let mut current = String::new();
        let mut has_interp = false;
        let mut depth = 1usize;
        while let Some(c) = self.peek() {
            if let Some(o) = open {
                if c == o {
                    depth += 1;
                    self.pos += 1;
                    current.push(c);
                    continue;
                }
                if c == close {
                    depth -= 1;
                    if depth == 0 {
                        self.pos += 1;
                        break;
                    }
                    self.pos += 1;
                    current.push(c);
                    continue;
                }
            } else if c == close {
                self.pos += 1;
                break;
            }
            if c == '\\' {
                self.pos += 1;
                if let Some(n) = self.peek() {
                    self.pos += 1;
                    match n {
                        'n' => current.push('\n'),
                        't' => current.push('\t'),
                        'r' => current.push('\r'),
                        '\\' => current.push('\\'),
                        '$' => current.push('$'),
                        '{' => current.push('{'),
                        'x' => {
                            if let Some(ch) = self.parse_hex_escape() {
                                current.push(ch);
                            }
                        }
                        'o' => {
                            if let Some(ch) = self.parse_octal_escape() {
                                current.push(ch);
                            }
                        }
                        'c' => {
                            if let Some(ch) = self.parse_named_char_escape() {
                                current.push(ch);
                            }
                        }
                        '0' => current.push('\0'),
                        'a' => current.push('\x07'),
                        'e' => current.push('\x1B'),
                        _ => {
                            if n == close {
                                current.push(n);
                            } else {
                                current.push('\\');
                                current.push(n);
                            }
                        }
                    }
                }
            } else if c == '$' {
                self.pos += 1;
                if !current.is_empty() {
                    parts.push(DStrPart::Lit(current.clone()));
                    current.clear();
                }
                let mut var_name = String::new();
                if matches!(self.peek(), Some('*') | Some('!') | Some('?')) {
                    var_name.push(self.peek().unwrap());
                    self.pos += 1;
                }
                while let Some(vc) = self.peek() {
                    if vc.is_ascii_alphanumeric() || vc == '_' || self.is_ident_hyphen(vc) {
                        var_name.push(vc);
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                if !var_name.is_empty() {
                    parts.push(DStrPart::Var(var_name));
                    has_interp = true;
                } else {
                    current.push('$');
                }
            } else if c == '@' {
                self.pos += 1;
                if !current.is_empty() {
                    parts.push(DStrPart::Lit(current.clone()));
                    current.clear();
                }
                let mut var_name = String::new();
                if matches!(self.peek(), Some('*')) {
                    var_name.push(self.peek().unwrap());
                    self.pos += 1;
                }
                while let Some(vc) = self.peek() {
                    if vc.is_ascii_alphanumeric() || vc == '_' || self.is_ident_hyphen(vc) {
                        var_name.push(vc);
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                if !var_name.is_empty() {
                    parts.push(DStrPart::Var(format!("@{}", var_name)));
                    has_interp = true;
                } else {
                    current.push('@');
                }
            } else {
                self.pos += 1;
                if c == '\n' {
                    self.line += 1;
                }
                current.push(c);
            }
        }
        if !current.is_empty() {
            parts.push(DStrPart::Lit(current));
        }
        if has_interp {
            TokenKind::DStr(parts)
        } else {
            let s = parts
                .into_iter()
                .map(|p| match p {
                    DStrPart::Lit(s) => s,
                    _ => String::new(),
                })
                .collect::<String>();
            TokenKind::Str(s)
        }
    }

    fn read_bracketed_string(&mut self, open: char, close: char) -> String {
        let mut s = String::new();
        let mut depth = 1usize;
        while let Some(c) = self.peek() {
            self.pos += 1;
            if c == open {
                depth += 1;
                s.push(c);
            } else if c == close {
                depth -= 1;
                if depth == 0 {
                    break;
                }
                s.push(c);
            } else {
                if c == '\n' {
                    self.line += 1;
                }
                s.push(c);
            }
        }
        s
    }

    /// Compute the column width of a whitespace prefix, expanding tabs to multiples of 8.
    fn compute_column(s: &str) -> usize {
        let mut col = 0;
        for c in s.chars() {
            if c == '\t' {
                col = (col / 8 + 1) * 8;
            } else {
                col += 1;
            }
        }
        col
    }

    /// Strip `margin` columns of indentation from a line.
    /// Tabs expand to the next multiple of 8 columns.
    fn strip_indent(line: &str, margin: usize) -> String {
        if margin == 0 || line.is_empty() {
            return line.to_string();
        }
        let mut col = 0;
        let mut char_iter = line.chars().peekable();
        while col < margin {
            match char_iter.peek() {
                Some(&' ') => {
                    char_iter.next();
                    col += 1;
                }
                Some(&'\t') => {
                    char_iter.next();
                    let next_tab = (col / 8 + 1) * 8;
                    if next_tab > margin {
                        // Tab partially consumed; emit spaces for the remainder
                        let spaces = next_tab - margin;
                        let mut result = " ".repeat(spaces);
                        result.extend(char_iter);
                        return result;
                    }
                    col = next_tab;
                }
                _ => break, // non-whitespace or end
            }
        }
        char_iter.collect()
    }

    /// Process q-string escape sequences (only `\\` → `\`).
    fn process_q_escapes(body: &str) -> String {
        let mut result = String::new();
        let mut chars = body.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\\' {
                if chars.peek() == Some(&'\\') {
                    chars.next();
                    result.push('\\');
                } else {
                    result.push('\\');
                }
            } else {
                result.push(c);
            }
        }
        result
    }

    /// Read a heredoc body. Called after the adverb (to/heredoc) has been consumed
    /// and optional whitespace has been skipped. The cursor is at the opening delimiter.
    /// `is_qq` = true for qq:to (interpolating), false for q:to.
    /// `is_literal` = true for Q:to (completely literal, no escape processing).
    fn read_heredoc(&mut self, is_qq: bool, is_literal: bool) -> TokenKind {
        // Read delimiter
        let open = self.peek().unwrap_or('/');
        let close = match open {
            '/' => '/',
            '(' => ')',
            '[' => ']',
            '<' => '>',
            '\'' => '\'',
            _ => '/',
        };
        self.pos += 1;
        let mut marker = String::new();
        while let Some(c) = self.peek() {
            if c == close {
                self.pos += 1;
                break;
            }
            marker.push(c);
            self.pos += 1;
        }
        // Save rest of current line (after heredoc delimiter), then skip to next line.
        // In Raku, the rest of the line (e.g., ");") is still valid code that must be
        // tokenized AFTER the heredoc body. We splice it back in after reading the body.
        let mut rest_of_line: Vec<char> = Vec::new();
        while let Some(c) = self.peek() {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                break;
            }
            rest_of_line.push(c);
        }
        // Read lines until marker
        let mut lines: Vec<String> = Vec::new();
        let mut terminator_indent = String::new();
        loop {
            let mut line = String::new();
            let mut at_eof = true;
            while let Some(c) = self.peek() {
                self.pos += 1;
                at_eof = false;
                if c == '\n' {
                    self.line += 1;
                    break;
                }
                line.push(c);
            }
            if line.trim() == marker {
                // Capture leading whitespace from terminator line
                let trimmed_start = line.len() - line.trim_start().len();
                terminator_indent = line[..trimmed_start].to_string();
                break;
            }
            lines.push(line);
            if at_eof {
                break;
            }
        }
        // Splice rest-of-line back into source at current position
        // so the lexer can tokenize it after the heredoc token.
        if !rest_of_line.is_empty() {
            // Add a newline after the rest-of-line to maintain line structure
            rest_of_line.push('\n');
            let insert_pos = self.pos;
            self.src.splice(insert_pos..insert_pos, rest_of_line);
        }

        // Compute margin column from terminator's indentation
        let margin = Self::compute_column(&terminator_indent);
        // Build body with indentation stripped
        let mut body = String::new();
        for line in &lines {
            let stripped = Self::strip_indent(line, margin);
            body.push_str(&stripped);
            body.push('\n');
        }

        if is_literal {
            // Q:to — completely literal, no escape processing
            TokenKind::Str(body)
        } else if is_qq {
            // qq:to — full interpolation
            self.parse_heredoc_qq_body(&body)
        } else {
            // q:to — process only \\ -> \ escapes
            TokenKind::Str(Self::process_q_escapes(&body))
        }
    }

    fn parse_hex_from_chars(chars: &[char], i: &mut usize) -> Option<char> {
        if *i < chars.len() && chars[*i] == '[' {
            *i += 1;
            let mut hex = String::new();
            while *i < chars.len() && chars[*i] != ']' {
                hex.push(chars[*i]);
                *i += 1;
            }
            if *i < chars.len() {
                *i += 1;
            } // skip ']'
            u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
        } else {
            let mut hex = String::new();
            while *i < chars.len() && chars[*i].is_ascii_hexdigit() {
                hex.push(chars[*i]);
                *i += 1;
            }
            if hex.is_empty() {
                return None;
            }
            u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
        }
    }

    fn parse_octal_from_chars(chars: &[char], i: &mut usize) -> Option<char> {
        if *i < chars.len() && chars[*i] == '[' {
            *i += 1;
            let mut oct = String::new();
            while *i < chars.len() && chars[*i] != ']' {
                oct.push(chars[*i]);
                *i += 1;
            }
            if *i < chars.len() {
                *i += 1;
            } // skip ']'
            u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
        } else {
            let mut oct = String::new();
            while *i < chars.len()
                && chars[*i].is_ascii_digit()
                && chars[*i] != '8'
                && chars[*i] != '9'
            {
                oct.push(chars[*i]);
                *i += 1;
            }
            if oct.is_empty() {
                return None;
            }
            u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
        }
    }

    fn parse_named_char_from_chars(chars: &[char], i: &mut usize) -> Option<char> {
        if *i < chars.len() && chars[*i] == '[' {
            *i += 1;
            let mut name = String::new();
            while *i < chars.len() && chars[*i] != ']' {
                name.push(chars[*i]);
                *i += 1;
            }
            if *i < chars.len() {
                *i += 1;
            } // skip ']'
            lookup_unicode_char_by_name(&name)
        } else {
            None
        }
    }

    /// Parse a heredoc body string as an interpolated (qq) string.
    /// Handles $var, @var, {block}, and escape sequences.
    fn parse_heredoc_qq_body(&self, body: &str) -> TokenKind {
        let mut parts: Vec<DStrPart> = Vec::new();
        let mut current = String::new();
        let mut has_interp = false;
        let chars: Vec<char> = body.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            let c = chars[i];
            if c == '\\' {
                i += 1;
                if i < chars.len() {
                    let n = chars[i];
                    i += 1;
                    match n {
                        'n' => current.push('\n'),
                        't' => current.push('\t'),
                        'r' => current.push('\r'),
                        '\\' => current.push('\\'),
                        '$' => current.push('$'),
                        '{' => current.push('{'),
                        'x' => {
                            if let Some(ch) = Self::parse_hex_from_chars(&chars, &mut i) {
                                current.push(ch);
                            }
                        }
                        'o' => {
                            if let Some(ch) = Self::parse_octal_from_chars(&chars, &mut i) {
                                current.push(ch);
                            }
                        }
                        'c' => {
                            if let Some(ch) = Self::parse_named_char_from_chars(&chars, &mut i) {
                                current.push(ch);
                            }
                        }
                        '0' => current.push('\0'),
                        'a' => current.push('\x07'),
                        'e' => current.push('\x1B'),
                        _ => current.push(n),
                    }
                }
            } else if c == '$' {
                i += 1;
                if !current.is_empty() {
                    parts.push(DStrPart::Lit(current.clone()));
                    current.clear();
                }
                let mut var_name = String::new();
                // Handle twigils: $*FOO, $!foo, $?foo
                if i < chars.len() && matches!(chars[i], '*' | '!' | '?') {
                    var_name.push(chars[i]);
                    i += 1;
                }
                while i < chars.len() {
                    let vc = chars[i];
                    if vc.is_ascii_alphanumeric() || vc == '_' || vc == '-' {
                        var_name.push(vc);
                        i += 1;
                    } else {
                        break;
                    }
                }
                if !var_name.is_empty() {
                    parts.push(DStrPart::Var(var_name));
                    has_interp = true;
                } else {
                    current.push('$');
                }
            } else if c == '@' {
                i += 1;
                if !current.is_empty() {
                    parts.push(DStrPart::Lit(current.clone()));
                    current.clear();
                }
                let mut var_name = String::new();
                if i < chars.len() && chars[i] == '*' {
                    var_name.push(chars[i]);
                    i += 1;
                }
                while i < chars.len() {
                    let vc = chars[i];
                    if vc.is_ascii_alphanumeric() || vc == '_' || vc == '-' {
                        var_name.push(vc);
                        i += 1;
                    } else {
                        break;
                    }
                }
                if !var_name.is_empty() {
                    parts.push(DStrPart::Var(format!("@{}", var_name)));
                    has_interp = true;
                } else {
                    current.push('@');
                }
            } else if c == '{' {
                i += 1;
                if !current.is_empty() {
                    parts.push(DStrPart::Lit(current.clone()));
                    current.clear();
                }
                let mut block = String::new();
                let mut depth = 1usize;
                while i < chars.len() {
                    let bc = chars[i];
                    i += 1;
                    if bc == '{' {
                        depth += 1;
                        block.push(bc);
                    } else if bc == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        block.push(bc);
                    } else {
                        block.push(bc);
                    }
                }
                parts.push(DStrPart::Block(block));
                has_interp = true;
            } else {
                i += 1;
                current.push(c);
            }
        }
        if !current.is_empty() {
            parts.push(DStrPart::Lit(current));
        }
        if has_interp {
            TokenKind::DStr(parts)
        } else {
            let s = parts
                .into_iter()
                .map(|p| match p {
                    DStrPart::Lit(s) => s,
                    _ => String::new(),
                })
                .collect::<String>();
            TokenKind::Str(s)
        }
    }
}
