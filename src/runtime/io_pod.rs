use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn make_pod_block(contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block"), attrs)
    }

    pub(crate) fn make_pod_named(name: &str, contents: Vec<Value>) -> Value {
        Self::make_pod_named_with_config(name, contents, HashMap::new())
    }

    pub(crate) fn make_pod_named_with_config(
        name: &str,
        contents: Vec<Value>,
        config: HashMap<String, Value>,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(name.to_string()));
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Block::Named"), attrs)
    }

    pub(crate) fn make_pod_heading(level: &str, contents: Vec<Value>) -> Value {
        Self::make_pod_heading_with_config(level, contents, HashMap::new())
    }

    pub(crate) fn make_pod_heading_with_config(
        level: &str,
        contents: Vec<Value>,
        config: HashMap<String, Value>,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("level".to_string(), Value::str(level.to_string()));
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Heading"), attrs)
    }

    /// Strip the `# ` / `#` abbreviated-block alias for `:numbered` from the
    /// start of a directive tail. Returns `(is_numbered, remainder)`.
    pub(crate) fn extract_numbered_alias(rest: &str) -> (bool, &str) {
        let trimmed = rest.trim_start();
        if let Some(after) = trimmed.strip_prefix('#') {
            // Must be followed by whitespace or end of line to count as alias.
            if after.is_empty()
                || after.starts_with(' ')
                || after.starts_with('\t')
                || after.starts_with('\n')
            {
                return (true, after.trim_start());
            }
        }
        (false, rest)
    }

    pub(crate) fn make_pod_comment(content: String) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(vec![Value::str(content)]),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Comment"), attrs)
    }

    pub(crate) fn make_pod_para(lines: Vec<String>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(lines.into_iter().map(Value::str).collect::<Vec<_>>()),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Para"), attrs)
    }

    /// Create a Pod::Block::Para whose contents may include Pod::FormattingCode
    /// nodes when the text contains `C<...>`, `B<...>`, etc.
    pub(crate) fn make_pod_para_with_formatting(text: &str) -> Value {
        let contents = Self::parse_formatting_codes(text);
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Para"), attrs)
    }

    /// Parse Pod formatting codes (e.g. `C<code>`, `B<bold>`) within text.
    /// Returns a list of Value items: plain strings and Pod::FormattingCode instances.
    fn parse_formatting_codes(text: &str) -> Vec<Value> {
        let result = Self::parse_formatting_codes_inner(text);
        // Merge adjacent strings
        let mut merged: Vec<Value> = Vec::new();
        for val in result {
            if let Value::Str(s) = &val
                && let Some(Value::Str(prev)) = merged.last()
            {
                let combined = format!("{}{}", &**prev, &**s);
                let len = merged.len();
                merged[len - 1] = Value::str(combined);
                continue;
            }
            merged.push(val);
        }
        merged
    }

    /// Inner recursive parser for formatting codes.
    fn parse_formatting_codes_inner(text: &str) -> Vec<Value> {
        let mut result = Vec::new();
        let mut rest = text;
        while let Some(pos) = rest.find(|c: char| c.is_ascii_uppercase())
            && pos < rest.len()
        {
            let after_letter = &rest[pos + 1..];
            let letter = &rest[pos..pos + 1];
            // Check for double-angle `<<` delimiter
            if let Some(stripped) = after_letter.strip_prefix("<<")
                && let Some(close) = stripped.find(">>")
            {
                let before = &rest[..pos];
                if !before.is_empty() {
                    result.push(Value::str(before.to_string()));
                }
                let inner = &stripped[..close];
                result.push(Self::make_formatting_code(letter, inner));
                rest = &stripped[close + 2..];
                continue;
            }
            // Check for single-angle `<` delimiter
            if let Some(inside) = after_letter.strip_prefix('<')
                && let Some(close) = Self::find_formatting_close(inside)
            {
                let before = &rest[..pos];
                if !before.is_empty() {
                    result.push(Value::str(before.to_string()));
                }
                let inner = &inside[..close];
                result.push(Self::make_formatting_code(letter, inner));
                rest = &inside[close + 1..];
                continue;
            }
            // Not a formatting code, include up to and past the letter
            let end = pos + 1;
            result.push(Value::str(rest[..end].to_string()));
            rest = &rest[end..];
        }
        if !rest.is_empty() {
            result.push(Value::str(rest.to_string()));
        }
        result
    }

    /// Create a Pod::FormattingCode value from a type letter and inner text.
    /// For V<> (verbatim), returns a plain string Value instead.
    fn make_formatting_code(letter: &str, inner: &str) -> Value {
        // V<> is special: it produces plain text, not a FormattingCode
        if letter == "V" {
            return Value::str(inner.to_string());
        }

        let mut fc_attrs = HashMap::new();
        fc_attrs.insert("type".to_string(), Value::str(letter.to_string()));
        fc_attrs.insert("config".to_string(), Value::hash(HashMap::new()));

        match letter {
            "L" => {
                // Link: split on `|` — left is display contents, right is meta
                if let Some(pipe_pos) = Self::find_unescaped_pipe(inner) {
                    let display = &inner[..pipe_pos];
                    let meta = &inner[pipe_pos + 1..];
                    let contents = Self::parse_formatting_codes(display);
                    fc_attrs.insert("contents".to_string(), Value::array(contents));
                    fc_attrs.insert("meta".to_string(), Value::str(meta.to_string()));
                } else {
                    let contents = Self::parse_formatting_codes(inner);
                    fc_attrs.insert("contents".to_string(), Value::array(contents));
                    fc_attrs.insert("meta".to_string(), Value::str(String::new()));
                }
            }
            "E" => {
                // Escape code: convert to the actual character
                let ch = Self::resolve_pod_escape(inner);
                fc_attrs.insert("contents".to_string(), Value::array(vec![Value::str(ch)]));
            }
            _ => {
                // All other codes: recursively parse contents
                let contents = Self::parse_formatting_codes(inner);
                fc_attrs.insert("contents".to_string(), Value::array(contents));
            }
        }

        Value::make_instance(Symbol::intern("Pod::FormattingCode"), fc_attrs)
    }

    /// Find `|` in formatting code inner text not inside nested `<>`.
    fn find_unescaped_pipe(text: &str) -> Option<usize> {
        let mut depth = 0usize;
        for (i, ch) in text.char_indices() {
            match ch {
                '<' => depth += 1,
                '>' => {
                    depth = depth.saturating_sub(1);
                }
                '|' if depth == 0 => return Some(i),
                _ => {}
            }
        }
        None
    }

    /// Resolve a Pod E<> escape code to the actual character string.
    fn resolve_pod_escape(code: &str) -> String {
        let trimmed = code.trim();
        // Decimal integer
        if let Ok(n) = trimmed.parse::<u32>()
            && let Some(ch) = char::from_u32(n)
        {
            return ch.to_string();
        }
        // Hex integer (0x...)
        if let Some(hex) = trimmed.strip_prefix("0x")
            && let Ok(n) = u32::from_str_radix(hex, 16)
            && let Some(ch) = char::from_u32(n)
        {
            return ch.to_string();
        }
        // Octal integer (0o...)
        if let Some(oct) = trimmed.strip_prefix("0o")
            && let Ok(n) = u32::from_str_radix(oct, 8)
            && let Some(ch) = char::from_u32(n)
        {
            return ch.to_string();
        }
        // Binary integer (0b...)
        if let Some(bin) = trimmed.strip_prefix("0b")
            && let Ok(n) = u32::from_str_radix(bin, 2)
            && let Some(ch) = char::from_u32(n)
        {
            return ch.to_string();
        }
        // HTML5 named entities
        if let Some(ch) = Self::resolve_html5_entity(trimmed) {
            return ch;
        }
        // Unicode character name lookup
        if let Some(ch) = Self::resolve_unicode_name(trimmed) {
            return ch.to_string();
        }
        // Fallback: return the code itself
        trimmed.to_string()
    }

    /// Resolve common HTML5 named entities.
    fn resolve_html5_entity(name: &str) -> Option<String> {
        let ch = match name {
            "amp" => "&",
            "lt" => "<",
            "gt" => ">",
            "quot" => "\"",
            "apos" => "'",
            "nbsp" => "\u{00A0}",
            "mdash" => "\u{2014}",
            "ndash" => "\u{2013}",
            "laquo" => "\u{00AB}",
            "raquo" => "\u{00BB}",
            "bull" => "\u{2022}",
            "hellip" => "\u{2026}",
            "copy" => "\u{00A9}",
            "reg" => "\u{00AE}",
            "trade" => "\u{2122}",
            "hearts" => "\u{2665}",
            "spades" => "\u{2660}",
            "clubs" => "\u{2663}",
            "diams" => "\u{2666}",
            "Assign" => "\u{2254}",
            "sup1" => "\u{00B9}",
            "sup2" => "\u{00B2}",
            "sup3" => "\u{00B3}",
            "frac12" => "\u{00BD}",
            "frac14" => "\u{00BC}",
            "frac34" => "\u{00BE}",
            "times" => "\u{00D7}",
            "divide" => "\u{00F7}",
            "lsquo" => "\u{2018}",
            "rsquo" => "\u{2019}",
            "ldquo" => "\u{201C}",
            "rdquo" => "\u{201D}",
            "larr" => "\u{2190}",
            "rarr" => "\u{2192}",
            "uarr" => "\u{2191}",
            "darr" => "\u{2193}",
            "harr" => "\u{2194}",
            _ => return None,
        };
        Some(ch.to_string())
    }

    /// Resolve a Unicode character name to a char.
    fn resolve_unicode_name(name: &str) -> Option<char> {
        let upper = name.to_uppercase();
        match upper.as_str() {
            "LATIN CAPITAL LETTER A" => Some('A'),
            "LATIN CAPITAL LETTER B" => Some('B'),
            "LATIN CAPITAL LETTER C" => Some('C'),
            "LATIN SMALL LETTER A" => Some('a'),
            "LATIN SMALL LETTER B" => Some('b'),
            "LATIN SMALL LETTER C" => Some('c'),
            "SPACE" => Some(' '),
            "LINE FEED" | "LINE FEED (LF)" => Some('\n'),
            "CARRIAGE RETURN" | "CARRIAGE RETURN (CR)" => Some('\r'),
            "HORIZONTAL TABULATION" | "CHARACTER TABULATION" => Some('\t'),
            _ => None,
        }
    }

    /// Find the closing `>` for a formatting code, accounting for nested `<>`.
    fn find_formatting_close(text: &str) -> Option<usize> {
        let mut depth = 0usize;
        for (i, ch) in text.char_indices() {
            match ch {
                '<' => depth += 1,
                '>' => {
                    if depth == 0 {
                        return Some(i);
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }
        None
    }

    pub(crate) fn make_pod_code(text: String) -> Value {
        Self::make_pod_code_with_config(text, HashMap::new())
    }

    pub(crate) fn make_pod_code_with_config(text: String, config: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        let contents = if config.contains_key("allow") {
            Self::parse_formatting_codes(&text)
        } else {
            vec![Value::str(text)]
        };
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Block::Code"), attrs)
    }

    pub(crate) fn make_pod_config(type_name: &str, config: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("type".to_string(), Value::str(type_name.to_string()));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Config"), attrs)
    }

    pub(crate) fn make_pod_item(level: i64, contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        attrs.insert("level".to_string(), Value::Int(level));
        Value::make_instance(Symbol::intern("Pod::Item"), attrs)
    }

    fn make_pod_defn(term: String, contents: Vec<Value>, config: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("term".to_string(), Value::str(term));
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Defn"), attrs)
    }

    /// Build a Pod::Defn from a single paragraph (used by `=for defn` and abbreviated `=defn`).
    /// `inline` is the text appearing on the directive line after the directive token
    /// (and after stripping config adverbs). For `=defn`, it may begin with `# ` to
    /// request `:numbered`. The first non-blank line of the paragraph is the term;
    /// remaining lines form a single Pod::Block::Para.
    pub(crate) fn build_pod_defn_paragraph(
        lines: &[&str],
        start_idx: usize,
        inline: &str,
        mut config: HashMap<String, Value>,
        end_target: Option<&str>,
    ) -> (Value, usize) {
        // Collect the paragraph lines (until blank line / pod directive).
        let mut all_lines: Vec<String> = Vec::new();
        let inline_trimmed = inline.trim();
        if !inline_trimmed.is_empty() {
            all_lines.push(inline_trimmed.to_string());
        }
        let mut idx = start_idx;
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || Self::active_pod_directive(lines[idx], end_target).is_some() {
                break;
            }
            all_lines.push(lines[idx].trim().to_string());
            idx += 1;
        }
        // First non-empty line is the term; if it begins with `# `, set :numbered.
        let mut term = String::new();
        let mut term_idx = None;
        for (i, line) in all_lines.iter().enumerate() {
            if !line.is_empty() {
                term = line.clone();
                term_idx = Some(i);
                break;
            }
        }
        if let Some(rest) = term.strip_prefix('#') {
            let rest = rest.trim_start();
            term = rest.to_string();
            config
                .entry("numbered".to_string())
                .or_insert(Value::Bool(true));
        }
        let body_lines: Vec<String> = match term_idx {
            Some(i) => all_lines[i + 1..].to_vec(),
            None => Vec::new(),
        };
        let mut contents: Vec<Value> = Vec::new();
        if !body_lines.is_empty() {
            let text = Self::normalize_pod_text(&body_lines);
            let payload = if text.is_empty() {
                Vec::new()
            } else {
                vec![text]
            };
            contents.push(Self::make_pod_para(payload));
        }
        (Self::make_pod_defn(term, contents, config), idx)
    }

    /// Build a Pod::Defn from a `=begin defn ... =end defn` block.
    /// First non-blank line of the first paragraph is the term; the remainder of
    /// that paragraph (and each subsequent paragraph separated by blank lines)
    /// becomes a Pod::Block::Para in `contents`.
    pub(crate) fn build_pod_defn_delimited(
        lines: &[&str],
        start_idx: usize,
        config: HashMap<String, Value>,
    ) -> (Value, usize) {
        let mut idx = start_idx;
        // Collect paragraphs until `=end defn`.
        let mut paragraphs: Vec<Vec<String>> = Vec::new();
        let mut current: Vec<String> = Vec::new();
        while idx < lines.len() {
            let line = lines[idx];
            if let Some((directive, rest)) = Self::active_pod_directive(line, Some("defn")) {
                if directive == "end"
                    && rest.split_whitespace().next().unwrap_or_default() == "defn"
                {
                    idx += 1;
                    break;
                }
                // Other directives inside a defn block: stop collecting (paragraph break).
                if !current.is_empty() {
                    paragraphs.push(std::mem::take(&mut current));
                }
                idx += 1;
                continue;
            }
            if line.trim().is_empty() {
                if !current.is_empty() {
                    paragraphs.push(std::mem::take(&mut current));
                }
                idx += 1;
                continue;
            }
            current.push(line.trim().to_string());
            idx += 1;
        }
        if !current.is_empty() {
            paragraphs.push(current);
        }
        let mut term = String::new();
        let mut contents: Vec<Value> = Vec::new();
        let mut first = true;
        for para in paragraphs {
            if first {
                first = false;
                if para.is_empty() {
                    continue;
                }
                term = para[0].clone();
                let rest = &para[1..];
                if !rest.is_empty() {
                    let text = Self::normalize_pod_text(rest);
                    let payload = if text.is_empty() {
                        Vec::new()
                    } else {
                        vec![text]
                    };
                    contents.push(Self::make_pod_para(payload));
                }
            } else {
                let text = Self::normalize_pod_text(&para);
                let payload = if text.is_empty() {
                    Vec::new()
                } else {
                    vec![text]
                };
                contents.push(Self::make_pod_para(payload));
            }
        }
        (Self::make_pod_defn(term, contents, config), idx)
    }
}
