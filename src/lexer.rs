#[derive(Debug, Clone, PartialEq)]
pub(crate) enum DStrPart {
    Lit(String),
    Var(String),
    Block(String),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenKind {
    Number(i64),
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
    Semicolon,
    Eof,
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
}

pub(crate) struct Lexer {
    src: Vec<char>,
    pos: usize,
    line: usize,
}

impl Lexer {
    pub(crate) fn new(input: &str) -> Self {
        Self {
            src: input.chars().collect(),
            pos: 0,
            line: 1,
        }
    }

    pub(crate) fn next_token(&mut self) -> Token {
        loop {
            self.skip_ws_and_comments();
            if self.pos >= self.src.len() {
                return Token {
                    kind: TokenKind::Eof,
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
                    } else if self.peek() == Some('q') && !matches!(self.peek_next(), Some(':')) {
                        // qq form: qq/string/, qq(string), qq[string], qq<string>, qq{string}
                        if let Some(delim) = self.src.get(self.pos + 1).copied() {
                            if matches!(delim, '/' | '(' | '[' | '{' | '<') {
                                self.pos += 1; // skip second 'q'
                                self.pos += 1; // skip delimiter
                                if matches!(delim, '(' | '[' | '{' | '<') {
                                    let close = match delim {
                                        '(' => ')',
                                        '[' => ']',
                                        '{' => '}',
                                        '<' => '>',
                                        _ => '/',
                                    };
                                    self.read_interpolated_string(close, Some(delim))
                                } else {
                                    self.read_interpolated_string(delim, None)
                                }
                            } else {
                                let ident = self.read_ident_start(ch);
                                match ident.as_str() {
                                    "True" => TokenKind::True,
                                    "False" => TokenKind::False,
                                    "Nil" | "Mu" => TokenKind::Nil,
                                    "Any" => TokenKind::Ident("Any".to_string()),
                                    "or" => TokenKind::OrWord,
                                    _ => TokenKind::Ident(ident),
                                }
                            }
                        } else {
                            let ident = self.read_ident_start(ch);
                            TokenKind::Ident(ident)
                        }
                    } else if self.peek() == Some(':')
                        || (self.peek() == Some('q') && self.peek_next() == Some(':'))
                    {
                        // q:to/MARKER/ or qq:to/MARKER/ heredoc
                        let is_qq = self.peek() == Some('q');
                        if is_qq {
                            self.pos += 1;
                        } // skip second 'q'
                        self.pos += 1; // skip ':'
                        // Read the adverb (e.g., "to")
                        let mut adverb = String::new();
                        while let Some(c) = self.peek() {
                            if c == '/' || c == '(' || c == '[' || c == '<' {
                                break;
                            }
                            adverb.push(c);
                            self.pos += 1;
                        }
                        if adverb == "to" || adverb == "heredoc" {
                            // Read delimiter
                            let open = self.peek().unwrap_or('/');
                            let close = match open {
                                '/' => '/',
                                '(' => ')',
                                '[' => ']',
                                '<' => '>',
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
                            // Skip to end of current line
                            while let Some(c) = self.peek() {
                                self.pos += 1;
                                if c == '\n' {
                                    self.line += 1;
                                    break;
                                }
                            }
                            // Read lines until marker
                            let mut body = String::new();
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
                                    break;
                                }
                                body.push_str(&line);
                                body.push('\n');
                                if at_eof {
                                    break;
                                }
                            }
                            // Remove trailing newline if present
                            if body.ends_with('\n') {
                                body.pop();
                            }
                            if is_qq {
                                // qq heredoc: for now treat as plain string
                                TokenKind::Str(body)
                            } else {
                                TokenKind::Str(body)
                            }
                        } else {
                            // Unknown adverb, treat as identifier
                            let ident = format!("q:{}", adverb);
                            TokenKind::Ident(ident)
                        }
                    } else {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" | "Mu" => TokenKind::Nil,
                                    "Any" => TokenKind::Ident("Any".to_string()),
                            "or" => TokenKind::OrWord,
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
                'm' => {
                    if self.peek() == Some('/') {
                        self.pos += 1;
                        let regex = self.read_regex_literal();
                        TokenKind::Regex(regex)
                    } else {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" | "Mu" => TokenKind::Nil,
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
                            "Nil" | "Mu" => TokenKind::Nil,
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
                            "Nil" | "Mu" => TokenKind::Nil,
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
                            return Token {
                                kind: TokenKind::Number(value),
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
                            return Token {
                                kind: TokenKind::Number(value),
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
                            return Token {
                                kind: TokenKind::Number(value),
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
                        && self.peek_next().map_or(false, |c| c.is_ascii_digit())
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
                        } else {
                            let value = num.parse::<i64>().unwrap_or(0);
                            if self.peek() == Some('i') {
                                self.pos += 1;
                                TokenKind::Imaginary(value as f64)
                            } else {
                                TokenKind::Number(value)
                            }
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
                                    '"' => current.push('"'),
                                    '\\' => current.push('\\'),
                                    '$' => current.push('$'),
                                    '{' => current.push('{'),
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
                        if self
                            .src
                            .get(self.pos + 1)
                            .map_or(false, |c| c.is_ascii_alphabetic() || *c == '_')
                        {
                            self.pos += 1; // skip '!'
                            let name = self.read_ident();
                            TokenKind::Var(format!("!{}", name))
                        } else {
                            self.pos += 1;
                            TokenKind::Var("!".to_string())
                        }
                    } else if self.peek() == Some('.') {
                        // Check if $.attr (public attribute accessor)
                        if self
                            .src
                            .get(self.pos + 1)
                            .map_or(false, |c| c.is_ascii_alphabetic() || *c == '_')
                        {
                            self.pos += 1; // skip '.'
                            let name = self.read_ident();
                            TokenKind::Var(format!(".{}", name))
                        } else {
                            let ident = self.read_ident();
                            TokenKind::Var(ident)
                        }
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
                    } else if self
                        .peek()
                        .map_or(true, |c| c.is_ascii_alphabetic() || c == '_' || c == '*')
                    {
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
                        .map_or(false, |c| c.is_ascii_alphabetic() || c == '_')
                    {
                        let mut name = String::new();
                        while let Some(c) = self.peek() {
                            if c.is_ascii_alphanumeric() || c == '_' || c == '-' {
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
                    if ch.is_ascii_alphabetic() || ch == '_' {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" | "Mu" => TokenKind::Nil,
                                    "Any" => TokenKind::Ident("Any".to_string()),
                            "or" => TokenKind::OrWord,
                            _ => TokenKind::Ident(ident),
                        }
                    } else {
                        continue;
                    }
                }
            };
            return Token { kind };
        }
    }

    fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        if matches!(self.peek(), Some('*') | Some('~') | Some('?') | Some('^')) {
            if let Some(c) = self.peek() {
                ident.push(c);
                self.pos += 1;
            }
        }
        self.read_ident_tail(&mut ident);
        ident
    }

    fn read_ident_start(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);
        self.read_ident_tail(&mut ident);
        ident
    }

    fn read_ident_tail(&mut self, ident: &mut String) {
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' || self.is_ident_hyphen(c) {
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
                } else if c == '\u{feff}' {
                    self.pos += 1;
                } else if c.is_whitespace() {
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
            }
            if self.peek() == Some('#') {
                self.pos += 1;
                // Check for embedded comment #`(...)
                if self.peek() == Some('`') {
                    self.pos += 1;
                    if let Some(open) = self.peek() {
                        if let Some(close) = Self::matching_bracket(open) {
                            self.pos += 1;
                            self.skip_bracketed_comment(open, close);
                            continue;
                        }
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
                .map(|n| n.is_ascii_alphabetic())
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

    fn read_regex_literal(&mut self) -> String {
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
            if c == '\\' {
                if let Some(n) = self.peek() {
                    self.pos += 1;
                    if n == delim {
                        s.push(n);
                    } else {
                        s.push('\\');
                        s.push(n);
                    }
                    continue;
                }
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
            } else {
                if c == close {
                    self.pos += 1;
                    break;
                }
            }
            if c == '\\' {
                self.pos += 1;
                if let Some(n) = self.peek() {
                    self.pos += 1;
                    match n {
                        'n' => current.push('\n'),
                        't' => current.push('\t'),
                        '\\' => current.push('\\'),
                        '$' => current.push('$'),
                        '{' => current.push('{'),
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
}
