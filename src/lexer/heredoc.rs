use super::*;

impl Lexer {
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

    /// Process q-string escape sequences (only `\\` -> `\`).
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
    pub(super) fn read_heredoc(&mut self, is_qq: bool, is_literal: bool) -> TokenKind {
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
