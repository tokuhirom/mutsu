use super::*;

impl Lexer {
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
                    } else if let Some(d) = self.peek()
                        && !d.is_alphanumeric()
                        && !d.is_whitespace()
                        && d != '_'
                        && d != '('
                        && d != ')'
                    {
                        self.pos += 1;
                        let regex = self.read_regex_with_delim(d);
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
                    } else if let Some(d) = self.peek()
                        && !d.is_alphanumeric()
                        && !d.is_whitespace()
                        && d != '_'
                        && d != '('
                        && d != ')'
                        && d != '{'
                    {
                        self.pos += 1;
                        let pattern = self.read_regex_with_delim(d);
                        let replacement = self.read_delimited_string(d);
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
                    } else if !self.last_was_term {
                        // Quote-word list: < word1 word2 ... >
                        // Read raw content until matching >
                        self.read_angle_bracket_words()
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
                '\u{228E}' => TokenKind::Plus,              // ⊎ (baggy addition)
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
                    | TokenKind::QWords(_)
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
                    | TokenKind::Gt
                    | TokenKind::HyperRight
            );
            return Token {
                kind,
                line: token_line,
            };
        }
    }
}
