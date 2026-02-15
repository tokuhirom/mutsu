use super::*;

impl Lexer {
    pub(super) fn try_read_regex_literal(&mut self) -> Option<String> {
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

    pub(super) fn read_brace_regex_literal(&mut self) -> String {
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

    pub(super) fn read_regex_literal(&mut self) -> String {
        self.read_regex_with_delim('/')
    }

    pub(super) fn read_regex_with_delim(&mut self, delim: char) -> String {
        let mut s = String::new();
        let mut escaped = false;
        let mut in_bracket = 0usize;
        let mut in_brace = 0usize;
        let mut in_quote: Option<char> = None;
        while let Some(c) = self.peek() {
            self.pos += 1;
            // Check delimiter first (before quote tracking), so that
            // delimiters like " or ' are not mistaken for quote-starters
            if !escaped && in_bracket == 0 && in_brace == 0 && in_quote.is_none() && c == delim {
                break;
            }
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
}
