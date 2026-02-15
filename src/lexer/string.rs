use super::*;

impl Lexer {
    pub(super) fn read_delimited_string(&mut self, delim: char) -> String {
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

    pub(super) fn read_interpolated_string(
        &mut self,
        close: char,
        open: Option<char>,
    ) -> TokenKind {
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

    pub(super) fn read_bracketed_string(&mut self, open: char, close: char) -> String {
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

    /// Read a quote-word list: content between < and >.
    /// Splits on whitespace, returns QWords token or a single Str for one word.
    pub(super) fn read_angle_bracket_words(&mut self) -> TokenKind {
        let mut words = Vec::new();
        let mut current = String::new();
        while let Some(c) = self.peek() {
            if c == '>' {
                self.pos += 1;
                break;
            }
            self.pos += 1;
            if c.is_whitespace() {
                if !current.is_empty() {
                    words.push(std::mem::take(&mut current));
                }
            } else if c == '\\' {
                // Backslash escape: next char is literal
                if let Some(next) = self.peek() {
                    self.pos += 1;
                    current.push(next);
                }
            } else {
                current.push(c);
            }
        }
        if !current.is_empty() {
            words.push(current);
        }
        TokenKind::QWords(words)
    }
}
