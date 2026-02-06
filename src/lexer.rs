#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenKind {
    Number(i64),
    Str(String),
    Ident(String),
    Var(String),
    HashVar(String),
    RoutineMagic,
    BlockMagic,
    ArrayVar(String),
    True,
    False,
    Nil,
    Plus,
    Minus,
    Star,
    Slash,
    Tilde,
    Eq,
    EqEq,
    FatArrow,
    MatchAssign,
    Dot,
    DotDot,
    DotDotCaret,
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
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Bind,
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
        Self { src: input.chars().collect(), pos: 0, line: 1 }
    }

    pub(crate) fn next_token(&mut self) -> Token {
        loop {
            self.skip_ws_and_comments();
            if self.pos >= self.src.len() {
                return Token { kind: TokenKind::Eof };
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
                    } else {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            "or" => TokenKind::OrWord,
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
                'r' => {
                    if self.peek() == Some('x') && self.peek_next() == Some('/') {
                        self.pos += 1;
                        self.pos += 1;
                        let regex = self.read_regex_literal();
                        TokenKind::Str(regex)
                    } else {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
            '0'..='9' => {
                let mut num = ch.to_string();
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        num.push(c);
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let value = num.parse::<i64>().unwrap_or(0);
                if self.peek() == Some('i') {
                    self.pos += 1;
                }
                TokenKind::Number(value)
            }
            '"' => {
                let mut s = String::new();
                while let Some(c) = self.peek() {
                    self.pos += 1;
                    if c == '"' {
                        break;
                    }
                    if c == '\\' {
                        if let Some(n) = self.peek() {
                            self.pos += 1;
                            match n {
                                'n' => s.push('\n'),
                                't' => s.push('\t'),
                                '"' => s.push('"'),
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
                if self.peek() == Some('!') {
                    self.pos += 1;
                    TokenKind::Var("!".to_string())
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
                let ident = self.read_ident();
                TokenKind::HashVar(ident)
            }
            '+' => TokenKind::Plus,
            '-' => {
                if self.match_char('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '*' => TokenKind::Star,
            '/' => {
                if let Some(regex) = self.try_read_regex_literal() {
                    TokenKind::Str(regex)
                } else {
                    TokenKind::Slash
                }
            }
            '~' => {
                if self.match_char('~') {
                    TokenKind::SmartMatch
                } else {
                    TokenKind::Tilde
                }
            }
            '=' => {
                if self.match_char('=') {
                    TokenKind::EqEq
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
                if self.match_char('=') {
                    TokenKind::Lte
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.match_char('=') {
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
                } else {
                    TokenKind::Ampersand
                }
            }
            '?' => {
                if self.match_char('?') {
                    TokenKind::QuestionQuestion
                } else {
                    continue;
                }
            }
            '|' => {
                if self.match_char('|') {
                    TokenKind::OrOr
                } else {
                    TokenKind::OrOr
                }
            }
            '(' => TokenKind::LParen,
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
            },
            ';' => TokenKind::Semicolon,
            '^' => {
                continue;
            }
            _ => {
                    if ch.is_ascii_alphabetic() || ch == '_' {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
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
        if matches!(self.peek(), Some('*') | Some('~') | Some('?')) {
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
            if self.peek() == Some('=') && (self.pos == 0 || self.src.get(self.pos - 1) == Some(&'\n')) {
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
            '\u{ff62}' => Some('\u{ff63}'), // Japanese corner brackets
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
        c == '-' && self.peek_next().map(|n| n.is_ascii_alphabetic()).unwrap_or(false)
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
}
