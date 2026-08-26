use super::*;
use crate::symbol::Symbol;

/// Pod formatting codes (`C<...>`, `B<...>`, `L<display|target>`, `E<65>`, ...)
/// and the `Pod::FormattingCode` objects they build. Split out of `io_pod.rs`
/// to keep both files under the 500-line limit.
impl Interpreter {
    /// Parse Pod formatting codes (e.g. `C<code>`, `B<bold>`) within text.
    /// Returns a list of Value items: plain strings and Pod::FormattingCode instances.
    pub(super) fn parse_formatting_codes(text: &str) -> Vec<Value> {
        let result = Self::parse_formatting_codes_inner(text);
        // Merge adjacent strings
        let mut merged: Vec<Value> = Vec::new();
        for val in result {
            if let ValueView::Str(s) = val.view()
                && let Some(ValueView::Str(prev)) = merged.last().map(Value::view)
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

        // `Pod::FormattingCode.meta` is always a `Positional` in rakudo — empty
        // for the codes that carry no metadata — so every branch below sets it.
        let mut meta: Vec<Value> = Vec::new();
        match letter {
            "L" => {
                // Link: split on the first `|` — left is the display contents,
                // right is the single meta entry (`L<d|http://y|z>` keeps the
                // second `|` in the target).
                match Self::find_unescaped_pipe(inner) {
                    Some(pipe_pos) => {
                        let contents = Self::parse_formatting_codes(&inner[..pipe_pos]);
                        fc_attrs.insert("contents".to_string(), Value::real_array(contents));
                        meta.push(Value::str(inner[pipe_pos + 1..].to_string()));
                    }
                    None => {
                        let contents = Self::parse_formatting_codes(inner);
                        fc_attrs.insert("contents".to_string(), Value::real_array(contents));
                    }
                }
            }
            "X" => {
                // Index entry: `X<display|a,b;c>` — the meta is a list of
                // `;`-separated entries, each a `,`-separated list of levels.
                match Self::find_unescaped_pipe(inner) {
                    Some(pipe_pos) => {
                        let contents = Self::parse_formatting_codes(&inner[..pipe_pos]);
                        fc_attrs.insert("contents".to_string(), Value::real_array(contents));
                        for entry in inner[pipe_pos + 1..].split(';') {
                            let levels: Vec<Value> = entry
                                .split(',')
                                .map(|lvl| Value::str(lvl.trim().to_string()))
                                .collect();
                            meta.push(Value::real_array(levels));
                        }
                    }
                    None => {
                        let contents = Self::parse_formatting_codes(inner);
                        fc_attrs.insert("contents".to_string(), Value::real_array(contents));
                    }
                }
            }
            "E" => {
                // Escape code: the contents are the resolved character(s), the
                // meta the code as written (an `Int` for a decimal codepoint).
                let ch = Self::resolve_pod_escape(inner);
                fc_attrs.insert(
                    "contents".to_string(),
                    Value::real_array(vec![Value::str(ch)]),
                );
                let code = inner.trim();
                meta.push(match code.parse::<i64>() {
                    Ok(n) => Value::int(n),
                    Err(_) => Value::str(code.to_string()),
                });
            }
            _ => {
                // All other codes: recursively parse contents
                let contents = Self::parse_formatting_codes(inner);
                fc_attrs.insert("contents".to_string(), Value::real_array(contents));
            }
        }
        fc_attrs.insert("meta".to_string(), Value::real_array(meta));

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
}
