use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn resolved_current_executable_path() -> std::path::PathBuf {
        std::env::current_exe()
            .map(|path| {
                let is_cargo_test_binary = path
                    .parent()
                    .and_then(|parent| parent.file_name())
                    .is_some_and(|name| name == "deps")
                    && path
                        .file_stem()
                        .and_then(|stem| stem.to_str())
                        .is_some_and(|stem| stem.starts_with("mutsu-"));
                if is_cargo_test_binary
                    && let Some(target_dir) = path.parent().and_then(|parent| parent.parent())
                {
                    let sibling = target_dir.join("mutsu");
                    if sibling.is_file() {
                        return sibling;
                    }
                }
                path
            })
            .unwrap_or_else(|_| std::path::PathBuf::from("target/debug/mutsu"))
    }

    fn dynamic_name_alias(name: &str) -> Option<String> {
        if let Some(rest) = name.strip_prefix("$*") {
            return Some(format!("*{}", rest));
        }
        if let Some(rest) = name.strip_prefix('*') {
            return Some(format!("$*{}", rest));
        }
        None
    }

    fn make_pod_block(contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block"), attrs)
    }

    fn make_pod_named(name: &str, contents: Vec<Value>) -> Value {
        Self::make_pod_named_with_config(name, contents, HashMap::new())
    }

    fn make_pod_named_with_config(
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

    fn make_pod_heading(level: &str, contents: Vec<Value>) -> Value {
        Self::make_pod_heading_with_config(level, contents, HashMap::new())
    }

    fn make_pod_heading_with_config(
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
    fn extract_numbered_alias(rest: &str) -> (bool, &str) {
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

    fn make_pod_comment(content: String) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(vec![Value::str(content)]),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Comment"), attrs)
    }

    fn make_pod_para(lines: Vec<String>) -> Value {
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
    fn make_pod_para_with_formatting(text: &str) -> Value {
        let contents = Self::parse_formatting_codes(text);
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Para"), attrs)
    }

    /// Parse Pod formatting codes (e.g. `C<code>`, `B<bold>`) within text.
    /// Returns a list of Value items: plain strings and Pod::FormattingCode instances.
    fn parse_formatting_codes(text: &str) -> Vec<Value> {
        let mut result = Vec::new();
        let mut rest = text;
        while let Some(pos) = rest.find(|c: char| c.is_ascii_uppercase())
            && pos < rest.len()
        {
            let after_letter = &rest[pos + 1..];
            if let Some(inside) = after_letter.strip_prefix('<') {
                let letter = &rest[pos..pos + 1];
                // Find matching '>' accounting for nesting
                if let Some(close) = Self::find_formatting_close(inside) {
                    let before = &rest[..pos];
                    if !before.is_empty() {
                        result.push(Value::str(before.to_string()));
                    }
                    let inner = &inside[..close];
                    let mut fc_attrs = HashMap::new();
                    fc_attrs.insert("type".to_string(), Value::str(letter.to_string()));
                    fc_attrs.insert(
                        "contents".to_string(),
                        Value::array(vec![Value::str(inner.to_string())]),
                    );
                    fc_attrs.insert("config".to_string(), Value::hash(HashMap::new()));
                    result.push(Value::make_instance(
                        Symbol::intern("Pod::FormattingCode"),
                        fc_attrs,
                    ));
                    rest = &inside[close + 1..]; // skip past '>'
                    continue;
                }
            }
            // Not a formatting code, include up to and past the letter
            let end = pos + 1;
            // Continue scanning from next position
            result.push(Value::str(rest[..end].to_string()));
            rest = &rest[end..];
        }
        if !rest.is_empty() {
            result.push(Value::str(rest.to_string()));
        }
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

    fn make_pod_code(text: String) -> Value {
        Self::make_pod_code_with_config(text, HashMap::new())
    }

    fn make_pod_code_with_config(text: String, config: HashMap<String, Value>) -> Value {
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

    fn make_pod_config(type_name: &str, config: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("type".to_string(), Value::str(type_name.to_string()));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Config"), attrs)
    }

    fn make_pod_item(level: i64, contents: Vec<Value>) -> Value {
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
    fn build_pod_defn_paragraph(
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
    fn build_pod_defn_delimited(
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

    /// Parse Pod config adverbs from a directive tail.
    /// Supports all Raku Pod config value forms including `:key<str>`, `:key(val)`,
    /// `:key[val]`, `:key{k=>v}`, `:!key`, `:NNNkey`, lists, hashes, typed scalars.
    fn parse_pod_config(input: &str) -> (HashMap<String, Value>, &str) {
        let mut config: HashMap<String, Value> = HashMap::new();
        let mut s = input.trim_start();
        loop {
            if !s.starts_with(':') {
                break;
            }
            let rest = &s[1..];
            let (negated, rest) = if let Some(r) = rest.strip_prefix('!') {
                (true, r)
            } else {
                (false, rest)
            };
            // Check for `:NNNkey` numeric prefix shorthand
            let digit_prefix_len = rest.bytes().take_while(|b| b.is_ascii_digit()).count();
            if digit_prefix_len > 0 && !negated {
                let after_digits = &rest[digit_prefix_len..];
                let name_len = after_digits
                    .chars()
                    .take_while(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_')
                    .map(char::len_utf8)
                    .sum::<usize>();
                if name_len > 0 {
                    let name = &after_digits[..name_len];
                    let num_str = &rest[..digit_prefix_len];
                    let val = num_str
                        .parse::<i64>()
                        .map(Value::Int)
                        .unwrap_or_else(|_| Value::str(num_str.to_string()));
                    config.insert(name.to_string(), val);
                    s = after_digits[name_len..].trim_start();
                    continue;
                }
            }
            // Read identifier
            let name_len = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_')
                .map(char::len_utf8)
                .sum::<usize>();
            if name_len == 0 {
                break;
            }
            let name = &rest[..name_len];
            let after_name = &rest[name_len..];
            let (value, after_val) = if let Some(after_open) = after_name.strip_prefix('<') {
                // :key<value> -- angle bracket form (always Str)
                if let Some(close_idx) = after_open.find('>') {
                    let raw = &after_open[..close_idx];
                    (Value::str(raw.to_string()), &after_open[close_idx + 1..])
                } else {
                    break;
                }
            } else if after_name.starts_with('(') || after_name.starts_with('[') {
                let (open, close) = if after_name.starts_with('(') {
                    ('(', ')')
                } else {
                    ('[', ']')
                };
                let after_open = &after_name[1..];
                if let Some(close_idx) = Self::find_balanced_close(after_open, open, close) {
                    let inner = &after_open[..close_idx];
                    let v = Self::parse_pod_config_value(inner);
                    (v, &after_open[close_idx + 1..])
                } else {
                    break;
                }
            } else if let Some(after_brace) = after_name.strip_prefix('{') {
                if let Some(close_idx) = Self::find_balanced_close(after_brace, '{', '}') {
                    let inner = &after_brace[..close_idx];
                    let v = Self::parse_pod_config_hash(inner);
                    (v, &after_brace[close_idx + 1..])
                } else {
                    break;
                }
            } else {
                (Value::Bool(!negated), after_name)
            };
            config.insert(name.to_string(), value);
            s = after_val.trim_start();
        }
        (config, s)
    }

    fn find_balanced_close(input: &str, open: char, close: char) -> Option<usize> {
        let mut depth = 0i32;
        let mut chars = input.char_indices();
        let mut in_single_quote = false;
        let mut in_double_quote = false;
        while let Some((i, c)) = chars.next() {
            if in_single_quote {
                if c == '\'' {
                    in_single_quote = false;
                }
                continue;
            }
            if in_double_quote {
                if c == '\\' {
                    chars.next();
                    continue;
                }
                if c == '"' {
                    in_double_quote = false;
                }
                continue;
            }
            if c == '\'' {
                in_single_quote = true;
                continue;
            }
            if c == '"' {
                in_double_quote = true;
                continue;
            }
            if c == open {
                depth += 1;
            } else if c == close {
                if depth == 0 {
                    return Some(i);
                }
                depth -= 1;
            }
        }
        None
    }

    fn parse_pod_config_value(inner: &str) -> Value {
        let trimmed = inner.trim();
        if Self::pod_config_is_list(trimmed) {
            let items = Self::parse_pod_config_list_items(trimmed);
            if items.len() == 1 {
                return items.into_iter().next().unwrap();
            }
            return Value::array(items);
        }
        Self::parse_pod_config_scalar(trimmed)
    }

    fn pod_config_is_list(input: &str) -> bool {
        let mut in_single = false;
        let mut in_double = false;
        let mut depth = 0i32;
        let mut chars = input.chars();
        while let Some(c) = chars.next() {
            if in_single {
                if c == '\'' {
                    in_single = false;
                }
                continue;
            }
            if in_double {
                if c == '\\' {
                    chars.next();
                    continue;
                }
                if c == '"' {
                    in_double = false;
                }
                continue;
            }
            if c == '\'' {
                in_single = true;
            } else if c == '"' {
                in_double = true;
            } else if c == '(' || c == '[' || c == '{' {
                depth += 1;
            } else if c == ')' || c == ']' || c == '}' {
                depth -= 1;
            } else if c == ',' && depth == 0 {
                return true;
            }
        }
        false
    }

    fn parse_pod_config_list_items(input: &str) -> Vec<Value> {
        let mut items = Vec::new();
        let mut start = 0;
        let mut in_single = false;
        let mut in_double = false;
        let mut depth = 0i32;
        let bytes = input.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let c = input[i..].chars().next().unwrap();
            let clen = c.len_utf8();
            if in_single {
                if c == '\'' {
                    in_single = false;
                }
                i += clen;
                continue;
            }
            if in_double {
                if c == '\\' {
                    i += clen;
                    if i < bytes.len() {
                        i += input[i..].chars().next().map_or(1, |c2| c2.len_utf8());
                    }
                    continue;
                }
                if c == '"' {
                    in_double = false;
                }
                i += clen;
                continue;
            }
            if c == '\'' {
                in_single = true;
            } else if c == '"' {
                in_double = true;
            } else if c == '(' || c == '[' || c == '{' {
                depth += 1;
            } else if c == ')' || c == ']' || c == '}' {
                depth -= 1;
            } else if c == ',' && depth == 0 {
                items.push(Self::parse_pod_config_scalar(input[start..i].trim()));
                start = i + 1;
                i += 1;
                continue;
            }
            i += clen;
        }
        if start < input.len() {
            items.push(Self::parse_pod_config_scalar(input[start..].trim()));
        }
        items
    }

    fn parse_pod_config_scalar(raw: &str) -> Value {
        if raw.is_empty() {
            return Value::str(String::new());
        }
        if raw == "True" {
            return Value::Bool(true);
        }
        if raw == "False" {
            return Value::Bool(false);
        }
        if raw.starts_with('\'') && raw.ends_with('\'') && raw.len() >= 2 {
            return Value::str(raw[1..raw.len() - 1].to_string());
        }
        if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
            let inner = &raw[1..raw.len() - 1];
            let mut result = String::new();
            let mut chars = inner.chars();
            while let Some(c) = chars.next() {
                if c == '\\' {
                    if let Some(next) = chars.next() {
                        match next {
                            'n' => result.push('\n'),
                            't' => result.push('\t'),
                            '"' => result.push('"'),
                            '\\' => result.push('\\'),
                            _ => {
                                result.push('\\');
                                result.push(next);
                            }
                        }
                    }
                } else {
                    result.push(c);
                }
            }
            return Value::str(result);
        }
        if raw.starts_with("Q[") && raw.ends_with(']') {
            return Value::str(raw[2..raw.len() - 1].to_string());
        }
        let num_str = raw.strip_prefix('+').unwrap_or(raw);
        if !num_str.is_empty()
            && (num_str.starts_with('-') || num_str.as_bytes()[0].is_ascii_digit())
        {
            let digits_str = num_str.strip_prefix('-').unwrap_or(num_str);
            if !digits_str.is_empty() && digits_str.chars().all(|c| c.is_ascii_digit()) {
                if let Ok(n) = num_str.parse::<i64>() {
                    return Value::Int(n);
                }
                if let Ok(n) = num_str.parse::<num_bigint::BigInt>() {
                    return Value::BigInt(std::sync::Arc::new(n));
                }
                return Value::str(raw.to_string());
            }
            if Self::looks_like_num(num_str)
                && let Ok(f) = num_str.parse::<f64>()
            {
                return Value::Num(f);
            }
        }
        Value::str(raw.to_string())
    }

    fn looks_like_num(s: &str) -> bool {
        let s = s.strip_prefix('-').unwrap_or(s);
        if s.is_empty() {
            return false;
        }
        let mut has_dot = false;
        let mut has_e = false;
        let mut chars = s.chars().peekable();
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                chars.next();
            } else if c == '.' && !has_dot && !has_e {
                has_dot = true;
                chars.next();
            } else if (c == 'e' || c == 'E') && !has_e {
                has_e = true;
                chars.next();
                if chars.peek().is_some_and(|&c2| c2 == '+' || c2 == '-') {
                    chars.next();
                }
            } else {
                return false;
            }
        }
        has_dot || has_e
    }

    fn parse_pod_config_hash(inner: &str) -> Value {
        let mut map: HashMap<String, Value> = HashMap::new();
        let items = Self::split_pod_config_top_level(inner, ',');
        let mut pending_key: Option<String> = None;
        for item in items {
            let item = item.trim();
            if item.is_empty() {
                continue;
            }
            if let Some(arrow_pos) = Self::find_fat_arrow(item) {
                pending_key = None;
                let key = Self::unquote_pod_config_key(item[..arrow_pos].trim());
                let val_str = item[arrow_pos + 2..].trim();
                map.insert(key, Self::parse_pod_config_scalar(val_str));
            } else if let Some(key) = pending_key.take() {
                map.insert(key, Self::parse_pod_config_scalar(item));
            } else {
                pending_key = Some(Self::unquote_pod_config_key(item));
            }
        }
        Value::hash(map)
    }

    fn find_fat_arrow(input: &str) -> Option<usize> {
        let mut in_single = false;
        let mut in_double = false;
        let bytes = input.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let c = input[i..].chars().next().unwrap();
            let clen = c.len_utf8();
            if in_single {
                if c == '\'' {
                    in_single = false;
                }
                i += clen;
                continue;
            }
            if in_double {
                if c == '\\' {
                    i += clen;
                    if i < bytes.len() {
                        i += input[i..].chars().next().map_or(1, |c2| c2.len_utf8());
                    }
                    continue;
                }
                if c == '"' {
                    in_double = false;
                }
                i += clen;
                continue;
            }
            if c == '\'' {
                in_single = true;
                i += clen;
                continue;
            }
            if c == '"' {
                in_double = true;
                i += clen;
                continue;
            }
            if c == '=' && i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                return Some(i);
            }
            i += clen;
        }
        None
    }

    fn unquote_pod_config_key(s: &str) -> String {
        if s.len() >= 2
            && ((s.starts_with('\'') && s.ends_with('\''))
                || (s.starts_with('"') && s.ends_with('"')))
        {
            s[1..s.len() - 1].to_string()
        } else {
            s.to_string()
        }
    }

    fn split_pod_config_top_level(input: &str, delim: char) -> Vec<&str> {
        let mut parts = Vec::new();
        let mut start = 0;
        let mut in_single = false;
        let mut in_double = false;
        let mut depth = 0i32;
        let mut i = 0;
        let input_bytes = input.as_bytes();
        while i < input_bytes.len() {
            let c = input[i..].chars().next().unwrap();
            let clen = c.len_utf8();
            if in_single {
                if c == '\'' {
                    in_single = false;
                }
                i += clen;
                continue;
            }
            if in_double {
                if c == '\\' {
                    i += clen;
                    if i < input_bytes.len() {
                        i += input[i..].chars().next().map_or(1, |c2| c2.len_utf8());
                    }
                    continue;
                }
                if c == '"' {
                    in_double = false;
                }
                i += clen;
                continue;
            }
            if c == '\'' {
                in_single = true;
            } else if c == '"' {
                in_double = true;
            } else if c == '(' || c == '[' || c == '{' {
                depth += 1;
            } else if c == ')' || c == ']' || c == '}' {
                depth -= 1;
            } else if c == delim && depth == 0 {
                parts.push(&input[start..i]);
                start = i + clen;
            }
            i += clen;
        }
        if start <= input.len() {
            parts.push(&input[start..]);
        }
        parts
    }

    fn make_pod_table_with_config(rows: Vec<Vec<String>>, config: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        let contents = rows
            .into_iter()
            .map(|row| Value::array(row.into_iter().map(Value::str).collect::<Vec<_>>()))
            .collect::<Vec<_>>();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("headers".to_string(), Value::array(Vec::new()));
        attrs.insert("caption".to_string(), Value::str(String::new()));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Block::Table"), attrs)
    }

    fn is_pod_table_separator(line: &str) -> bool {
        let trimmed = line.trim();
        !trimmed.is_empty()
            && trimmed.contains('-')
            && trimmed
                .chars()
                .all(|c| matches!(c, '-' | '+' | '|' | ':' | ' ' | '\t'))
    }

    fn collect_table_rows(lines: &[&str], mut idx: usize) -> (Vec<Vec<String>>, usize) {
        let mut rows = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || trimmed.starts_with('=') {
                break;
            }
            if Self::is_pod_table_separator(trimmed) {
                idx += 1;
                continue;
            }
            if !trimmed.contains('|') {
                break;
            }
            let row = trimmed
                .split('|')
                .map(|cell| cell.trim().to_string())
                .collect::<Vec<_>>();
            rows.push(row);
            idx += 1;
        }
        (rows, idx)
    }

    fn collect_paragraph(lines: &[&str], mut idx: usize) -> (String, usize) {
        let mut text = String::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || trimmed.starts_with('=') {
                break;
            }
            text.push_str(lines[idx]);
            text.push('\n');
            idx += 1;
        }
        (text, idx)
    }

    fn normalize_pod_text(parts: &[String]) -> String {
        // Join all parts, then collapse runs of breaking whitespace into single spaces.
        // Non-breaking whitespace (U+00A0, U+202F, U+2060, U+FEFF) is preserved as-is.
        let joined = parts.join(" ");
        let mut result = String::new();
        let mut in_breaking_ws = false;
        for ch in joined.chars() {
            if Self::is_breaking_whitespace(ch) {
                if !in_breaking_ws && !result.is_empty() {
                    result.push(' ');
                }
                in_breaking_ws = true;
            } else {
                in_breaking_ws = false;
                result.push(ch);
            }
        }
        // Trim trailing space
        if result.ends_with(' ') {
            result.pop();
        }
        result
    }

    /// Returns true for whitespace characters that should be normalized (collapsed)
    /// in Pod text. Non-breaking spaces (U+00A0, U+202F, U+2060, U+FEFF) are NOT
    /// considered breaking and are preserved as-is.
    fn is_breaking_whitespace(ch: char) -> bool {
        matches!(
            ch,
            ' ' | '\t' | '\n' | '\r' | '\x0B' | '\x0C' | '\u{1680}' | '\u{180E}' | '\u{2000}'
                ..='\u{200A}' | '\u{2028}' | '\u{2029}' | '\u{205F}' | '\u{3000}'
        )
    }

    /// Collect a paragraph and parse formatting codes in the text.
    fn collect_pod_para_formatted(
        lines: &[&str],
        mut idx: usize,
        end_target: Option<&str>,
    ) -> (Value, usize) {
        let allows_code_blocks = matches!(end_target, Some("pod"))
            || end_target.is_some_and(|t| Self::parse_item_level(t).is_some());
        let mut para_lines = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || Self::active_pod_directive(lines[idx], end_target).is_some() {
                break;
            }
            // Stop if line is indented (code block follows) — only in pod/item blocks
            if allows_code_blocks {
                let indent = lines[idx].len() - trimmed.len();
                if indent > 0 {
                    break;
                }
            }
            para_lines.push(lines[idx].trim().to_string());
            idx += 1;
        }
        let text = Self::normalize_pod_text(&para_lines);
        if text.is_empty() {
            (Self::make_pod_para(Vec::new()), idx)
        } else {
            // Check if text contains formatting codes
            let has_formatting = text
                .as_bytes()
                .windows(2)
                .any(|w| w[0].is_ascii_uppercase() && w[1] == b'<');
            if has_formatting {
                (Self::make_pod_para_with_formatting(&text), idx)
            } else {
                (Self::make_pod_para(vec![text]), idx)
            }
        }
    }

    /// Collect consecutive indented lines as a Pod::Block::Code.
    /// Groups lines with the same base indentation level.
    /// When indentation changes, this returns and lets the caller create a new block.
    fn collect_pod_code_block(
        lines: &[&str],
        mut idx: usize,
        end_target: Option<&str>,
    ) -> (Value, usize) {
        // Determine base indentation from first line
        let base_indent = lines[idx].len() - lines[idx].trim_start().len();
        let mut code_lines: Vec<&str> = Vec::new();

        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if Self::active_pod_directive(lines[idx], end_target).is_some() {
                break;
            }
            if trimmed.is_empty() {
                // Blank line: include it if the next non-blank line has the same
                // base indentation. Peek ahead.
                let mut peek = idx + 1;
                while peek < lines.len() && lines[peek].trim().is_empty() {
                    peek += 1;
                }
                if peek < lines.len() {
                    let next_trimmed = lines[peek].trim_start();
                    if !next_trimmed.is_empty() {
                        let next_indent = lines[peek].len() - next_trimmed.len();
                        if next_indent == base_indent {
                            // Include the blank line(s) and continue
                            code_lines.push("");
                            idx += 1;
                            continue;
                        }
                    }
                }
                // End of this code block
                break;
            }
            let indent = lines[idx].len() - trimmed.len();
            if indent == 0 {
                // Not indented → not a code block line
                break;
            }
            if indent < base_indent {
                // Different base indentation → different code block
                break;
            }
            // Strip the base indentation
            if lines[idx].len() >= base_indent {
                code_lines.push(&lines[idx][base_indent..]);
            } else {
                code_lines.push(trimmed);
            }
            idx += 1;
        }

        // Remove trailing empty lines
        while code_lines.last().is_some_and(|l| l.is_empty()) {
            code_lines.pop();
        }

        let text = code_lines.join("\n");
        (Self::make_pod_code(text), idx)
    }

    fn collect_pod_para_with_inline(
        lines: &[&str],
        mut idx: usize,
        inline: &str,
        end_target: Option<&str>,
    ) -> (Option<Value>, usize) {
        let mut para_lines = Vec::new();
        if !inline.trim().is_empty() {
            para_lines.push(inline.trim().to_string());
        }
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || Self::active_pod_directive(lines[idx], end_target).is_some() {
                break;
            }
            para_lines.push(lines[idx].trim().to_string());
            idx += 1;
        }
        if para_lines.is_empty() {
            return (None, idx);
        }
        let text = Self::normalize_pod_text(&para_lines);
        let payload = if text.is_empty() {
            Vec::new()
        } else {
            vec![text]
        };
        (Some(Self::make_pod_para(payload)), idx)
    }

    fn pod_block_allows_flush_directives(end_target: Option<&str>) -> bool {
        match end_target {
            None => true,
            Some("pod") => true,
            Some(target) => Self::parse_item_level(target).is_some(),
        }
    }

    fn active_pod_directive<'a>(
        line: &'a str,
        end_target: Option<&str>,
    ) -> Option<(&'a str, &'a str)> {
        let trimmed = line.trim_start();
        let (directive, rest) = Self::parse_pod_directive_line(trimmed)?;
        let has_indent = trimmed.len() != line.len();

        if directive == "end" {
            let target = rest.split_whitespace().next().unwrap_or_default();
            if end_target.is_some_and(|expected| expected == target) {
                return Some((directive, rest));
            }
        }

        if Self::pod_block_allows_flush_directives(end_target) || has_indent {
            Some((directive, rest))
        } else {
            None
        }
    }

    fn parse_pod_directive_line(line: &str) -> Option<(&str, &str)> {
        let trimmed = line.trim_start();
        let token = trimmed.split_whitespace().next()?;
        let directive = token.strip_prefix('=')?;
        let first = directive.as_bytes().first().copied()?;
        if !first.is_ascii_alphabetic() {
            return None;
        }
        if !directive
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        {
            return None;
        }
        let rest = trimmed[token.len()..].trim_start();
        Some((directive, rest))
    }

    fn parse_heading_level(directive: &str) -> Option<&str> {
        let level = directive.strip_prefix("head")?;
        if level.is_empty() || !level.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        Some(level)
    }

    fn collect_pod_entries(
        lines: &[&str],
        mut idx: usize,
        end_target: Option<&str>,
    ) -> (Vec<Value>, usize) {
        let mut entries = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() {
                idx += 1;
                continue;
            }
            if let Some((directive, rest)) = Self::active_pod_directive(lines[idx], end_target) {
                if directive == "end" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if end_target.is_some_and(|expected| expected == target) {
                        return (entries, idx + 1);
                    }
                    idx += 1;
                    continue;
                }
                if directive == "comment" {
                    let (text, next_idx) = Self::collect_paragraph(lines, idx + 1);
                    entries.push(Self::make_pod_comment(text));
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "config" {
                    let type_name = rest.split_whitespace().next().unwrap_or_default();
                    let after_type = rest
                        .strip_prefix(type_name)
                        .map(str::trim_start)
                        .unwrap_or_default();
                    let (cfg, _) = Self::parse_pod_config(after_type);
                    entries.push(Self::make_pod_config(type_name, cfg));
                    idx += 1;
                    continue;
                }
                if directive == "table" {
                    let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                    let (mut config, _) = Self::parse_pod_config(rest_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::Bool(true));
                    }
                    let (rows, next_idx) = Self::collect_table_rows(lines, idx + 1);
                    if !rows.is_empty() || numbered || !config.is_empty() {
                        entries.push(Self::make_pod_table_with_config(rows, config));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "for" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if target.is_empty() {
                        idx += 1;
                        continue;
                    }
                    let inline = rest
                        .strip_prefix(target)
                        .map(str::trim_start)
                        .unwrap_or_default();
                    if target == "comment" {
                        let mut text = String::new();
                        if !inline.is_empty() {
                            text.push_str(inline);
                            text.push('\n');
                        }
                        let (tail, next_idx) = Self::collect_paragraph(lines, idx + 1);
                        text.push_str(&tail);
                        entries.push(Self::make_pod_comment(text));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "defn" {
                        let (config, leftover) = Self::parse_pod_config(inline);
                        let (defn, next_idx) = Self::build_pod_defn_paragraph(
                            lines,
                            idx + 1,
                            leftover,
                            config,
                            end_target,
                        );
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "table" {
                        let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                        let (mut config, _) = Self::parse_pod_config(inline_after);
                        if numbered {
                            config.insert("numbered".to_string(), Value::Bool(true));
                        }
                        let (rows, next_idx) = Self::collect_table_rows(lines, idx + 1);
                        entries.push(Self::make_pod_table_with_config(rows, config));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                    let (mut config, leftover) = Self::parse_pod_config(inline_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::Bool(true));
                    }
                    let mut cont_idx = idx + 1;
                    while cont_idx < lines.len() {
                        let cont = lines[cont_idx].trim_start();
                        if cont.starts_with("= ") || cont.starts_with("=\t") {
                            let cont_str = cont[1..].trim_start();
                            let (more, _) = Self::parse_pod_config(cont_str);
                            config.extend(more);
                            cont_idx += 1;
                        } else {
                            break;
                        }
                    }
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(lines, cont_idx, leftover, end_target);
                    let mut contents = Vec::new();
                    if let Some(para) = para {
                        contents.push(para);
                    }
                    if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading_with_config(level, contents, config));
                    } else {
                        entries.push(Self::make_pod_named_with_config(target, contents, config));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "begin" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if target.is_empty() {
                        idx += 1;
                        continue;
                    }
                    if target == "comment" {
                        idx += 1;
                        let mut raw = String::new();
                        while idx < lines.len() {
                            if let Some((end_directive, end_rest)) =
                                Self::active_pod_directive(lines[idx], Some("comment"))
                                && end_directive == "end"
                                && end_rest.split_whitespace().next().unwrap_or_default()
                                    == "comment"
                            {
                                idx += 1;
                                break;
                            }
                            raw.push_str(lines[idx]);
                            raw.push('\n');
                            idx += 1;
                        }
                        entries.push(Self::make_pod_block(vec![Value::str(raw)]));
                        continue;
                    }
                    if target == "defn" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (config, _) = Self::parse_pod_config(after_target);
                        let (defn, next_idx) =
                            Self::build_pod_defn_delimited(lines, idx + 1, config);
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "code" {
                        // =begin code ... =end code → Pod::Block::Code
                        // Collect raw text (no inner directive parsing)
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (code_config, _) = Self::parse_pod_config(after_target);
                        idx += 1;
                        let mut code_lines: Vec<&str> = Vec::new();
                        while idx < lines.len() {
                            if let Some((ed, er)) =
                                Self::active_pod_directive(lines[idx], Some("code"))
                                && ed == "end"
                                && er.split_whitespace().next().unwrap_or_default() == "code"
                            {
                                idx += 1;
                                break;
                            }
                            code_lines.push(lines[idx]);
                            idx += 1;
                        }
                        // Strip common leading indentation
                        let min_indent = code_lines
                            .iter()
                            .filter(|l| !l.trim().is_empty())
                            .map(|l| l.len() - l.trim_start().len())
                            .min()
                            .unwrap_or(0);
                        let text: String = code_lines
                            .iter()
                            .map(|l| {
                                if l.len() >= min_indent {
                                    &l[min_indent..]
                                } else {
                                    l.trim_start()
                                }
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        // Trim trailing newlines
                        let text = text.trim_end_matches('\n').to_string();
                        entries.push(Self::make_pod_code_with_config(text, code_config));
                        continue;
                    }
                    if target == "table" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (tbl_config, _) = Self::parse_pod_config(after_target);
                        idx += 1;
                        let mut table_lines: Vec<&str> = Vec::new();
                        while idx < lines.len() {
                            if let Some((ed, er)) =
                                Self::active_pod_directive(lines[idx], Some("table"))
                                && ed == "end"
                                && er.split_whitespace().next().unwrap_or_default() == "table"
                            {
                                idx += 1;
                                break;
                            }
                            table_lines.push(lines[idx]);
                            idx += 1;
                        }
                        let mut rows = Vec::new();
                        for line in &table_lines {
                            let tl = line.trim();
                            if tl.is_empty() || Self::is_pod_table_separator(tl) {
                                continue;
                            }
                            let cells: Vec<String> = if tl.contains('|') {
                                tl.split('|').map(|c| c.trim().to_string()).collect()
                            } else {
                                tl.split_whitespace().map(|c| c.to_string()).collect()
                            };
                            rows.push(cells);
                        }
                        entries.push(Self::make_pod_table_with_config(rows, tbl_config));
                        continue;
                    }
                    if let Some(level) = Self::parse_item_level(target) {
                        let (item_contents, next_idx) =
                            Self::collect_pod_entries(lines, idx + 1, Some(target));
                        entries.push(Self::make_pod_item(level, item_contents));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (contents, next_idx) =
                        Self::collect_pod_entries(lines, idx + 1, Some(target));
                    if target == "pod" {
                        entries.push(Self::make_pod_block(contents));
                    } else if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading(level, contents));
                    } else {
                        entries.push(Self::make_pod_named(target, contents));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if let Some((level, inline)) = Self::parse_item_directive(trimmed) {
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(lines, idx + 1, inline, end_target);
                    let mut item_contents = Vec::new();
                    if let Some(para) = para {
                        item_contents.push(para);
                    }
                    entries.push(Self::make_pod_item(level, item_contents));
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "defn" {
                    let (config, leftover) = Self::parse_pod_config(rest);
                    let (defn, next_idx) = Self::build_pod_defn_paragraph(
                        lines,
                        idx + 1,
                        leftover,
                        config,
                        end_target,
                    );
                    entries.push(defn);
                    idx = next_idx.max(idx + 1);
                    continue;
                }

                let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                let mut config = HashMap::new();
                if numbered {
                    config.insert("numbered".to_string(), Value::Bool(true));
                }
                let (para, next_idx) =
                    Self::collect_pod_para_with_inline(lines, idx + 1, rest_after, end_target);
                let mut contents = Vec::new();
                if let Some(para) = para {
                    contents.push(para);
                }
                if let Some(level) = Self::parse_heading_level(directive) {
                    entries.push(Self::make_pod_heading_with_config(level, contents, config));
                } else {
                    entries.push(Self::make_pod_named_with_config(
                        directive, contents, config,
                    ));
                }
                idx = next_idx.max(idx + 1);
                continue;
            }

            // Check if this line is indented → code block (only in pod/item blocks)
            let indent = lines[idx].len() - lines[idx].trim_start().len();
            let allows_code_blocks = matches!(end_target, Some("pod"))
                || end_target.is_some_and(|t| Self::parse_item_level(t).is_some());
            if indent > 0 && allows_code_blocks {
                let (code, next_idx) = Self::collect_pod_code_block(lines, idx, end_target);
                entries.push(code);
                idx = next_idx.max(idx + 1);
            } else {
                let (para, next_idx) = Self::collect_pod_para_formatted(lines, idx, end_target);
                entries.push(para);
                idx = next_idx.max(idx + 1);
            }
        }
        (entries, idx)
    }

    fn parse_item_level(token: &str) -> Option<i64> {
        let suffix = token
            .strip_prefix("=item")
            .or_else(|| token.strip_prefix("item"))?;
        if suffix.is_empty() {
            return Some(1);
        }
        suffix.parse::<i64>().ok().filter(|n| *n > 0)
    }

    fn parse_item_directive(line: &str) -> Option<(i64, &str)> {
        let trimmed = line.trim_start();
        let rest = trimmed.strip_prefix("=item")?;
        let digit_len = rest.bytes().take_while(|b| b.is_ascii_digit()).count();
        let level = if digit_len == 0 {
            1
        } else {
            rest[..digit_len].parse::<i64>().ok().filter(|n| *n > 0)?
        };
        let after = &rest[digit_len..];
        if let Some(ch) = after.chars().next()
            && !ch.is_whitespace()
        {
            return None;
        }
        Some((level, after.trim_start()))
    }

    pub(super) fn collect_pod_blocks(&mut self, input: &str) {
        let lines: Vec<&str> = input.lines().collect();
        let mut entries = Vec::new();
        let mut idx = 0usize;
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if let Some((directive, rest)) = Self::active_pod_directive(lines[idx], None) {
                if directive == "end" {
                    idx += 1;
                    continue;
                }
                if directive == "comment" {
                    let (text, next_idx) = Self::collect_paragraph(&lines, idx + 1);
                    entries.push(Self::make_pod_comment(text));
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "config" {
                    let type_name = rest.split_whitespace().next().unwrap_or_default();
                    let after_type = rest
                        .strip_prefix(type_name)
                        .map(str::trim_start)
                        .unwrap_or_default();
                    let (cfg, _) = Self::parse_pod_config(after_type);
                    entries.push(Self::make_pod_config(type_name, cfg));
                    idx += 1;
                    continue;
                }
                if directive == "table" {
                    let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                    let (mut config, _) = Self::parse_pod_config(rest_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::Bool(true));
                    }
                    let (rows, next_idx) = Self::collect_table_rows(&lines, idx + 1);
                    if !rows.is_empty() || numbered || !config.is_empty() {
                        entries.push(Self::make_pod_table_with_config(rows, config));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "for" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if target.is_empty() {
                        idx += 1;
                        continue;
                    }
                    let inline = rest
                        .strip_prefix(target)
                        .map(str::trim_start)
                        .unwrap_or_default();
                    if target == "comment" {
                        let mut text = String::new();
                        if !inline.is_empty() {
                            text.push_str(inline);
                            text.push('\n');
                        }
                        let (tail, next_idx) = Self::collect_paragraph(&lines, idx + 1);
                        text.push_str(&tail);
                        entries.push(Self::make_pod_comment(text));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "defn" {
                        let (config, leftover) = Self::parse_pod_config(inline);
                        let (defn, next_idx) =
                            Self::build_pod_defn_paragraph(&lines, idx + 1, leftover, config, None);
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                    let (mut config, leftover) = Self::parse_pod_config(inline_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::Bool(true));
                    }
                    let mut cont_idx = idx + 1;
                    while cont_idx < lines.len() {
                        let cont = lines[cont_idx].trim_start();
                        if cont.starts_with("= ") || cont.starts_with("=\t") {
                            let cont_str = cont[1..].trim_start();
                            let (more, _) = Self::parse_pod_config(cont_str);
                            config.extend(more);
                            cont_idx += 1;
                        } else {
                            break;
                        }
                    }
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(&lines, cont_idx, leftover, None);
                    let mut contents = Vec::new();
                    if let Some(para) = para {
                        contents.push(para);
                    }
                    if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading_with_config(level, contents, config));
                    } else {
                        entries.push(Self::make_pod_named_with_config(target, contents, config));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "begin" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if target.is_empty() {
                        idx += 1;
                        continue;
                    }
                    if target == "comment" {
                        idx += 1;
                        let mut raw = String::new();
                        while idx < lines.len() {
                            if let Some((end_directive, end_rest)) =
                                Self::active_pod_directive(lines[idx], Some("comment"))
                                && end_directive == "end"
                                && end_rest.split_whitespace().next().unwrap_or_default()
                                    == "comment"
                            {
                                idx += 1;
                                break;
                            }
                            raw.push_str(lines[idx]);
                            raw.push('\n');
                            idx += 1;
                        }
                        entries.push(Self::make_pod_block(vec![Value::str(raw)]));
                        continue;
                    }
                    if target == "defn" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (config, _) = Self::parse_pod_config(after_target);
                        let (defn, next_idx) =
                            Self::build_pod_defn_delimited(&lines, idx + 1, config);
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "code" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (code_config, _) = Self::parse_pod_config(after_target);
                        idx += 1;
                        let mut code_lines: Vec<&str> = Vec::new();
                        while idx < lines.len() {
                            if let Some((ed, er)) =
                                Self::active_pod_directive(lines[idx], Some("code"))
                                && ed == "end"
                                && er.split_whitespace().next().unwrap_or_default() == "code"
                            {
                                idx += 1;
                                break;
                            }
                            code_lines.push(lines[idx]);
                            idx += 1;
                        }
                        let min_indent = code_lines
                            .iter()
                            .filter(|l| !l.trim().is_empty())
                            .map(|l| l.len() - l.trim_start().len())
                            .min()
                            .unwrap_or(0);
                        let text: String = code_lines
                            .iter()
                            .map(|l| {
                                if l.len() >= min_indent {
                                    &l[min_indent..]
                                } else {
                                    l.trim_start()
                                }
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        let text = text.trim_end_matches('\n').to_string();
                        entries.push(Self::make_pod_code_with_config(text, code_config));
                        continue;
                    }
                    if target == "table" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (tbl_config, _) = Self::parse_pod_config(after_target);
                        idx += 1;
                        let mut table_lines: Vec<&str> = Vec::new();
                        while idx < lines.len() {
                            if let Some((ed, er)) =
                                Self::active_pod_directive(lines[idx], Some("table"))
                                && ed == "end"
                                && er.split_whitespace().next().unwrap_or_default() == "table"
                            {
                                idx += 1;
                                break;
                            }
                            table_lines.push(lines[idx]);
                            idx += 1;
                        }
                        let mut rows = Vec::new();
                        for line in &table_lines {
                            let tl = line.trim();
                            if tl.is_empty() || Self::is_pod_table_separator(tl) {
                                continue;
                            }
                            let cells: Vec<String> = if tl.contains('|') {
                                tl.split('|').map(|c| c.trim().to_string()).collect()
                            } else {
                                tl.split_whitespace().map(|c| c.to_string()).collect()
                            };
                            rows.push(cells);
                        }
                        entries.push(Self::make_pod_table_with_config(rows, tbl_config));
                        continue;
                    }
                    if let Some(level) = Self::parse_item_level(target) {
                        let (item_contents, next_idx) =
                            Self::collect_pod_entries(&lines, idx + 1, Some(target));
                        entries.push(Self::make_pod_item(level, item_contents));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (contents, next_idx) =
                        Self::collect_pod_entries(&lines, idx + 1, Some(target));
                    if target == "pod" {
                        entries.push(Self::make_pod_block(contents));
                    } else if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading(level, contents));
                    } else {
                        entries.push(Self::make_pod_named(target, contents));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if let Some((level, inline)) = Self::parse_item_directive(trimmed) {
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(&lines, idx + 1, inline, None);
                    let mut item_contents = Vec::new();
                    if let Some(para) = para {
                        item_contents.push(para);
                    }
                    entries.push(Self::make_pod_item(level, item_contents));
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "defn" {
                    let (config, leftover) = Self::parse_pod_config(rest);
                    let (defn, next_idx) =
                        Self::build_pod_defn_paragraph(&lines, idx + 1, leftover, config, None);
                    entries.push(defn);
                    idx = next_idx.max(idx + 1);
                    continue;
                }

                let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                let mut config = HashMap::new();
                if numbered {
                    config.insert("numbered".to_string(), Value::Bool(true));
                }
                let (para, next_idx) =
                    Self::collect_pod_para_with_inline(&lines, idx + 1, rest_after, None);
                let mut contents = Vec::new();
                if let Some(para) = para {
                    contents.push(para);
                }
                if let Some(level) = Self::parse_heading_level(directive) {
                    entries.push(Self::make_pod_heading_with_config(level, contents, config));
                } else {
                    entries.push(Self::make_pod_named_with_config(
                        directive, contents, config,
                    ));
                }
                idx = next_idx.max(idx + 1);
                continue;
            }
            idx += 1;
        }
        self.env.insert("=pod".to_string(), Value::array(entries));
    }

    pub(super) fn init_io_environment(&mut self) {
        let stdout = self.create_handle(
            IoHandleTarget::Stdout,
            IoHandleMode::Write,
            Some("STDOUT".to_string()),
        );
        self.env.insert("$*OUT".to_string(), stdout.clone());
        self.env.insert("*OUT".to_string(), stdout);
        let stderr = self.create_handle(
            IoHandleTarget::Stderr,
            IoHandleMode::Write,
            Some("STDERR".to_string()),
        );
        self.env.insert("$*ERR".to_string(), stderr.clone());
        self.env.insert("*ERR".to_string(), stderr);
        let stdin = self.create_handle(
            IoHandleTarget::Stdin,
            IoHandleMode::Read,
            Some("STDIN".to_string()),
        );
        self.env.insert("$*IN".to_string(), stdin.clone());
        self.env.insert("*IN".to_string(), stdin);
        let argfiles = self.create_handle(
            IoHandleTarget::ArgFiles,
            IoHandleMode::Read,
            Some("$*ARGFILES".to_string()),
        );
        self.env.insert("$*ARGFILES".to_string(), argfiles.clone());
        self.env.insert("*ARGFILES".to_string(), argfiles);
        let spec = self.make_io_spec_instance();
        self.env.insert("$*SPEC".to_string(), spec.clone());
        self.env.insert("*SPEC".to_string(), spec);
        #[cfg(not(target_arch = "wasm32"))]
        let cwd_str = env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .to_string_lossy()
            .to_string();
        #[cfg(target_arch = "wasm32")]
        let cwd_str = "/".to_string();
        let cwd_val = self.make_io_path_instance(&cwd_str);
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        #[cfg(not(target_arch = "wasm32"))]
        let tmpdir_str = env::temp_dir().to_string_lossy().to_string();
        #[cfg(target_arch = "wasm32")]
        let tmpdir_str = "/tmp".to_string();
        let tmpdir_val = self.make_io_path_instance(&tmpdir_str);
        self.env.insert("$*TMPDIR".to_string(), tmpdir_val.clone());
        self.env.insert("*TMPDIR".to_string(), tmpdir_val);
        #[cfg(not(target_arch = "wasm32"))]
        let home_val = if let Ok(home) = env::var("HOME") {
            self.make_io_path_instance(&home)
        } else {
            Value::Nil
        };
        #[cfg(target_arch = "wasm32")]
        let home_val = Value::Nil;
        self.env.insert("$*HOME".to_string(), home_val.clone());
        self.env.insert("*HOME".to_string(), home_val);
        // $*EXECUTABLE - path to the interpreter binary
        #[cfg(not(target_arch = "wasm32"))]
        let exe_path = Self::resolved_current_executable_path()
            .to_string_lossy()
            .to_string();
        #[cfg(target_arch = "wasm32")]
        let exe_path = "mutsu".to_string();
        let exe_io = self.make_io_path_instance(&exe_path);
        self.env.insert("$*EXECUTABLE".to_string(), exe_io.clone());
        self.env.insert("*EXECUTABLE".to_string(), exe_io);
        self.env.insert(
            "$*EXECUTABLE-NAME".to_string(),
            Value::str(
                std::path::Path::new(&exe_path)
                    .file_name()
                    .map(|f| f.to_string_lossy().to_string())
                    .unwrap_or_else(|| exe_path.clone()),
            ),
        );
        let exec_name = self.env.get("$*EXECUTABLE-NAME").cloned().unwrap();
        self.env.insert("*EXECUTABLE-NAME".to_string(), exec_name);
        let distro = Self::make_distro_instance();
        self.env.insert("*DISTRO".to_string(), distro.clone());
        self.env.insert("?DISTRO".to_string(), distro);
        let perl = Self::make_perl_instance();
        self.env.insert("*PERL".to_string(), perl.clone());
        self.env.insert("?PERL".to_string(), perl);
        let raku = Self::make_perl_instance();
        self.env.insert("*RAKU".to_string(), raku.clone());
        self.env.insert("?RAKU".to_string(), raku);
        let vm = Self::make_vm_instance();
        self.env.insert("$*VM".to_string(), vm.clone());
        self.env.insert("*VM".to_string(), vm.clone());
        self.env.insert("?VM".to_string(), vm);
        let kernel = Self::make_kernel_instance();
        self.env.insert("*KERNEL".to_string(), kernel.clone());
        self.env.insert("?KERNEL".to_string(), kernel);
    }

    pub(super) fn create_handle(
        &mut self,
        target: IoHandleTarget,
        mode: IoHandleMode,
        path: Option<String>,
    ) -> Value {
        let id = self.next_handle_id;
        self.next_handle_id += 1;
        let state = IoHandleState {
            target,
            mode,
            path: path.clone(),
            line_separators: self.default_line_separators(),
            line_chomp: true,
            encoding: "utf-8".to_string(),
            file: None,
            socket: None,
            listener: None,
            closed: false,
            out_buffer_capacity: None,
            out_buffer_pending: Vec::new(),
            bin: false,
            nl_out: "\n".to_string(),
            bytes_written: 0,
            read_attempted: false,
            argfiles_index: 0,
            argfiles_reader: None,
        };
        self.handles.insert(id, state);
        self.make_handle_instance(id)
    }

    pub(super) fn make_handle_instance(&self, handle_id: usize) -> Value {
        self.make_handle_instance_with_bin(handle_id, false)
    }

    pub(super) fn make_handle_instance_with_bin(&self, handle_id: usize, bin: bool) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::Int(handle_id as i64));
        if let Some(state) = self.handles.get(&handle_id) {
            if let Some(path) = &state.path {
                attrs.insert("path".to_string(), Value::str(path.clone()));
            }
            attrs.insert(
                "mode".to_string(),
                Value::str(Self::mode_name(state.mode).to_string()),
            );
            // Store handle state attributes so IO::Handle.open can inherit them
            attrs.insert("chomp".to_string(), Value::Bool(state.line_chomp));
            attrs.insert("nl-out".to_string(), Value::str(state.nl_out.clone()));
            // Store nl-in
            if state.line_separators.len() == 1 {
                attrs.insert(
                    "nl-in".to_string(),
                    Value::str(String::from_utf8_lossy(&state.line_separators[0]).to_string()),
                );
            } else {
                let items: Vec<Value> = state
                    .line_separators
                    .iter()
                    .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                    .collect();
                attrs.insert("nl-in".to_string(), Value::real_array(items));
            }
        }
        if bin {
            attrs.insert("bin".to_string(), Value::Bool(true));
        }
        Value::make_instance(Symbol::intern("IO::Handle"), attrs)
    }

    pub(super) fn mode_name(mode: IoHandleMode) -> &'static str {
        match mode {
            IoHandleMode::Read => "r",
            IoHandleMode::Write => "w",
            IoHandleMode::Append => "a",
            IoHandleMode::ReadWrite => "rw",
        }
    }

    pub(super) fn make_io_spec_instance(&self) -> Value {
        let attrs = HashMap::new();
        let class_name = if cfg!(target_os = "windows") {
            "IO::Spec::Win32"
        } else {
            "IO::Spec::Unix"
        };
        Value::make_instance(Symbol::intern(class_name), attrs)
    }

    pub(super) fn make_distro_instance() -> Value {
        let os = std::env::consts::OS;
        let (name, auth, version_str, desc, release) = match os {
            "macos" => {
                let product_version = Command::new("sw_vers")
                    .arg("-productVersion")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let build_version = Command::new("sw_vers")
                    .arg("-buildVersion")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let product_name = Command::new("sw_vers")
                    .arg("-productName")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let major = product_version.split('.').next().unwrap_or("").to_string();
                let desc_str = if product_name.is_empty() {
                    format!("macOS {}", major)
                } else {
                    // sw_vers returns "macOS" as the product name
                    format!("{} {}", product_name, major)
                };
                (
                    "macos".to_string(),
                    "Apple Inc.".to_string(),
                    product_version,
                    desc_str,
                    build_version,
                )
            }
            "linux" => {
                use std::sync::OnceLock;
                static DISTRO_UNAME_R: OnceLock<String> = OnceLock::new();
                let kernel_release = DISTRO_UNAME_R
                    .get_or_init(|| {
                        Command::new("uname")
                            .arg("-r")
                            .output()
                            .ok()
                            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                            .unwrap_or_default()
                    })
                    .clone();
                let mut distro_desc = String::new();
                if let Ok(content) = std::fs::read_to_string("/etc/os-release") {
                    for line in content.lines() {
                        if let Some(val) = line.strip_prefix("PRETTY_NAME=") {
                            distro_desc = val.trim_matches('"').to_string();
                            break;
                        }
                    }
                }
                if distro_desc.is_empty() {
                    distro_desc = "Linux".to_string();
                }
                (
                    "linux".to_string(),
                    "unknown".to_string(),
                    kernel_release.clone(),
                    distro_desc,
                    kernel_release,
                )
            }
            "windows" => {
                let ver = Command::new("cmd")
                    .args(["/C", "ver"])
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                (
                    "mswin32".to_string(),
                    "Microsoft".to_string(),
                    String::new(),
                    ver.clone(),
                    ver,
                )
            }
            _ => {
                use std::sync::OnceLock;
                static FALLBACK_UNAME_R: OnceLock<String> = OnceLock::new();
                let kernel_release = FALLBACK_UNAME_R
                    .get_or_init(|| {
                        Command::new("uname")
                            .arg("-r")
                            .output()
                            .ok()
                            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                            .unwrap_or_default()
                    })
                    .clone();
                (
                    os.to_string(),
                    "unknown".to_string(),
                    kernel_release.clone(),
                    os.to_string(),
                    kernel_release,
                )
            }
        };

        // Parse version string into Value::Version
        let version = Self::parse_version_string(&version_str);

        let path_sep = if cfg!(windows) {
            ";".to_string()
        } else {
            ":".to_string()
        };

        let is_win = cfg!(windows);

        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(name));
        attrs.insert("auth".to_string(), Value::str(auth));
        attrs.insert("version".to_string(), version);
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), HashMap::new()),
        );
        attrs.insert("desc".to_string(), Value::str(desc));
        attrs.insert("release".to_string(), Value::str(release));
        attrs.insert("path-sep".to_string(), Value::str(path_sep));
        attrs.insert("is-win".to_string(), Value::Bool(is_win));

        Value::make_instance(Symbol::intern("Distro"), attrs)
    }

    pub(super) fn parse_version_string(s: &str) -> Value {
        use crate::value::VersionPart;
        let parts: Vec<VersionPart> = s
            .split('.')
            .filter_map(|p| p.parse::<i64>().ok().map(VersionPart::Num))
            .collect();
        if parts.is_empty() {
            Value::Version {
                parts: vec![VersionPart::Num(0)],
                plus: false,
                minus: false,
            }
        } else {
            Value::Version {
                parts,
                plus: false,
                minus: false,
            }
        }
    }

    pub(super) fn make_perl_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str_from("Raku"));
        attrs.insert("auth".to_string(), Value::str_from("The Perl Foundation"));
        attrs.insert(
            "version".to_string(),
            Value::Version {
                parts: vec![crate::value::VersionPart::Num(6)],
                plus: false,
                minus: false,
            },
        );
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), {
                let mut a = HashMap::new();
                a.insert("values".to_string(), Value::array(vec![Value::Int(0)]));
                a
            }),
        );
        attrs.insert(
            "desc".to_string(),
            Value::str_from("Raku Programming Language"),
        );
        attrs.insert(
            "DISTROnames".to_string(),
            Value::array(vec![
                Value::str_from("macos"),
                Value::str_from("linux"),
                Value::str_from("freebsd"),
                Value::str_from("mswin32"),
                Value::str_from("openbsd"),
                Value::str_from("dragonfly"),
                Value::str_from("netbsd"),
                Value::str_from("browser"),
            ]),
        );
        attrs.insert(
            "VMnames".to_string(),
            Value::array(vec![
                Value::str_from("mutsu"),
                Value::str_from("moar"),
                Value::str_from("jvm"),
                Value::str_from("js"),
            ]),
        );
        attrs.insert(
            "KERNELnames".to_string(),
            Value::array(vec![
                Value::str_from("darwin"),
                Value::str_from("linux"),
                Value::str_from("freebsd"),
                Value::str_from("openbsd"),
                Value::str_from("netbsd"),
                Value::str_from("dragonfly"),
                Value::str_from("sunos"),
                Value::str_from("win32"),
                Value::str_from("browser"),
            ]),
        );
        Value::make_instance(Symbol::intern("Perl"), attrs)
    }

    pub(super) fn make_vm_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str_from("mutsu"));
        attrs.insert("auth".to_string(), Value::str_from("github.com/tokuhirom"));
        attrs.insert(
            "version".to_string(),
            Value::Version {
                parts: vec![
                    crate::value::VersionPart::Num(0),
                    crate::value::VersionPart::Num(1),
                    crate::value::VersionPart::Num(0),
                ],
                plus: false,
                minus: false,
            },
        );
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), {
                let mut a = HashMap::new();
                a.insert("values".to_string(), Value::array(vec![Value::Int(0)]));
                a
            }),
        );
        attrs.insert("desc".to_string(), Value::str_from("mutsu virtual machine"));
        attrs.insert("precomp-ext".to_string(), Value::str_from("mutsu"));
        attrs.insert("precomp-target".to_string(), Value::str_from("mutsu"));
        attrs.insert("prefix".to_string(), Value::str_from("mutsu"));
        // properties: a non-empty hash so the value is truthy.
        let mut props = HashMap::new();
        props.insert("name".to_string(), Value::str_from("mutsu"));
        attrs.insert("properties".to_string(), Value::Hash(props.into()));
        // config: a non-empty hash so the value is truthy.
        let mut config = HashMap::new();
        config.insert("name".to_string(), Value::str_from("mutsu"));
        attrs.insert("config".to_string(), Value::Hash(config.into()));
        Value::make_instance(Symbol::intern("VM"), attrs)
    }

    pub(super) fn make_kernel_instance() -> Value {
        use std::sync::OnceLock;
        static UNAME_R: OnceLock<String> = OnceLock::new();
        static UNAME_M: OnceLock<String> = OnceLock::new();

        let os = std::env::consts::OS;
        let arch = std::env::consts::ARCH;

        // Kernel name (e.g., "linux", "darwin", "win32")
        let name = match os {
            "macos" => "darwin".to_string(),
            "windows" => "win32".to_string(),
            _ => os.to_string(),
        };

        // Kernel release (e.g., "6.18.7-76061807-generic") — cached
        let release = UNAME_R
            .get_or_init(|| {
                Command::new("uname")
                    .arg("-r")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default()
            })
            .clone();

        // Hardware (e.g., "x86_64") — cached
        let hardware = UNAME_M
            .get_or_init(|| {
                Command::new("uname")
                    .arg("-m")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_else(|| arch.to_string())
            })
            .clone();

        // Architecture (mapped from Rust's ARCH constant)
        let arch_str = match arch {
            "x86_64" => "x86_64",
            "x86" => "i386",
            "aarch64" => "aarch64",
            "arm" => "arm",
            _ => arch,
        }
        .to_string();

        // Bits
        let bits: i64 = if arch == "x86_64" || arch == "aarch64" || arch == "powerpc64" {
            64
        } else {
            32
        };

        // Hostname — cached
        static HOSTNAME: OnceLock<String> = OnceLock::new();
        let hostname = HOSTNAME
            .get_or_init(|| {
                Command::new("hostname")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default()
            })
            .clone();

        // Version from release string
        let version = Self::parse_version_string(&release);

        // Build signals list (first 32 standard POSIX signals)
        let signal_names = [
            "", "HUP", "INT", "QUIT", "ILL", "TRAP", "ABRT", "BUS", "FPE", "KILL", "USR1", "SEGV",
            "USR2", "PIPE", "ALRM", "TERM", "STKFLT", "CHLD", "CONT", "STOP", "TSTP", "TTIN",
            "TTOU", "URG", "XCPU", "XFSZ", "VTALRM", "PROF", "WINCH", "IO", "PWR", "SYS",
        ];
        let signals: Vec<Value> = (0..32)
            .map(|i| {
                if i < signal_names.len() && !signal_names[i].is_empty() {
                    Value::str(format!("SIG{}", signal_names[i]))
                } else {
                    Value::Nil
                }
            })
            .collect();

        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(name));
        attrs.insert("auth".to_string(), Value::str_from("unknown"));
        attrs.insert("version".to_string(), version);
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), HashMap::new()),
        );
        attrs.insert("desc".to_string(), Value::Str(String::new().into()));
        attrs.insert("release".to_string(), Value::str(release));
        attrs.insert("hardware".to_string(), Value::str(hardware));
        attrs.insert("arch".to_string(), Value::str(arch_str));
        attrs.insert("bits".to_string(), Value::Int(bits));
        attrs.insert("hostname".to_string(), Value::str(hostname));
        attrs.insert("signals".to_string(), Value::array(signals));

        Value::make_instance(Symbol::intern("Kernel"), attrs)
    }

    pub(super) fn get_dynamic_handle(&self, name: &str) -> Option<Value> {
        self.env.get(name).cloned().or_else(|| {
            Self::dynamic_name_alias(name).and_then(|alias| self.env.get(&alias).cloned())
        })
    }

    pub(super) fn default_input_handle(&self) -> Option<Value> {
        self.get_dynamic_handle("$*ARGFILES")
            .or_else(|| self.get_dynamic_handle("$*IN"))
    }

    pub(crate) fn write_to_named_handle(
        &mut self,
        name: &str,
        text: &str,
        newline: bool,
    ) -> Result<(), RuntimeError> {
        if let Some(handle) = self.get_dynamic_handle(name) {
            if Self::handle_id_from_value(&handle).is_some() {
                return self.write_to_handle_value(&handle, text, newline);
            }
            let payload = if newline {
                format!("{}\n", text)
            } else {
                text.to_string()
            };
            if self
                .call_method_with_values(handle, "print", vec![Value::str(payload.clone())])
                .is_ok()
            {
                return Ok(());
            }
            if name == "$*ERR" {
                self.stderr_output.push_str(&payload);
            }
            self.emit_output(&payload);
            return Ok(());
        }
        let payload = if newline {
            format!("{}\n", text)
        } else {
            text.to_string()
        };
        if name == "$*ERR" {
            self.stderr_output.push_str(&payload);
        }
        self.emit_output(&payload);
        Ok(())
    }

    pub(crate) fn render_gist_value(&mut self, value: &Value) -> String {
        self.call_method_with_values(value.clone(), "gist", vec![])
            .map(|result| result.to_string_value())
            .unwrap_or_else(|_| crate::runtime::gist_value(value))
    }

    /// Stringify a value by calling .Str method (used by put/print).
    /// Falls back to to_string_value() if .Str method dispatch fails.
    pub(crate) fn render_str_value(&mut self, value: &Value) -> String {
        self.call_method_with_values(value.clone(), "Str", vec![])
            .map(|result| result.to_string_value())
            .unwrap_or_else(|_| value.to_string_value())
    }

    pub(super) fn get_dynamic_string(&self, name: &str) -> Option<String> {
        self.get_dynamic_handle(name).and_then(|value| match value {
            Value::Str(s) => Some(s.to_string()),
            Value::Instance { attributes, .. } => {
                // Support IO::Path instances (e.g., $*CWD)
                attributes.get("path").map(|v| v.to_string_value())
            }
            _ => None,
        })
    }

    pub(super) fn get_cwd_path(&self) -> PathBuf {
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            return PathBuf::from(cwd);
        }
        env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    }

    pub(super) fn resolve_path(&self, path: &str) -> PathBuf {
        let pb = PathBuf::from(path);
        if pb.is_absolute() {
            self.apply_chroot(pb)
        } else {
            let cwd = self.get_cwd_path();
            self.apply_chroot(cwd.join(pb))
        }
    }

    pub(super) fn apply_chroot(&self, path: PathBuf) -> PathBuf {
        if let Some(root) = &self.chroot_root {
            if path.starts_with(root) {
                return path;
            }
            if path.is_absolute() {
                if let Ok(stripped_root) = path.strip_prefix(root) {
                    return root.join(stripped_root);
                }
                if let Ok(stripped_slash) = path.strip_prefix("/") {
                    return root.join(stripped_slash);
                }
                return root.join(path);
            }
        }
        path
    }

    pub(super) fn stringify_path(path: &Path) -> String {
        path.to_string_lossy().to_string()
    }

    pub(crate) fn make_io_path_instance(&self, path: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("path".to_string(), Value::str(path.to_string()));
        // Inherit $*SPEC if set (check both env lookup styles)
        let spec = self
            .env
            .get("$*SPEC")
            .or_else(|| self.env.get("*SPEC"))
            .cloned()
            .or_else(|| self.get_dynamic_var("*SPEC").ok());
        if let Some(spec) = spec
            && !matches!(&spec, Value::Nil)
        {
            attrs.insert("SPEC".to_string(), spec);
        }
        // Set CWD from $*CWD if available
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            attrs.insert("cwd".to_string(), Value::str(cwd));
        }
        Value::make_instance(Symbol::intern("IO::Path"), attrs)
    }

    pub(super) fn collect_doc_comments(&mut self, input: &str) {
        use super::DocComment;

        self.doc_comments.clear();
        self.doc_comment_list.clear();
        let mut pending_leading: Option<String> = None;
        // The last declaration that can receive trailing #= comments
        let mut last_declarant: Option<(String, super::DocDeclKind)> = None;
        // Track the current class scope for method doc keys
        let mut current_class: Option<String> = None;
        // Stack of class scopes for nested classes
        let mut class_stack: Vec<Option<String>> = Vec::new();
        // Per-function counter for uniquifying multi sub/method doc keys
        let mut multi_counters: HashMap<String, usize> = HashMap::new();
        // Track current function name for scoping parameter docs
        let mut current_sub: Option<String> = None;
        // Counter for uniquifying anonymous sub doc keys
        let mut anon_sub_counter: usize = 0;

        fn extract_ident(s: &str) -> String {
            s.trim_start()
                .split(|c: char| !c.is_alphanumeric() && c != '_' && c != '-' && c != ':')
                .next()
                .unwrap_or("")
                .to_string()
        }

        fn append_doc_text(existing: Option<String>, text: &str) -> Option<String> {
            if text.is_empty() {
                return existing;
            }
            Some(match existing {
                Some(mut prev) => {
                    prev.push(' ');
                    prev.push_str(text);
                    prev
                }
                None => text.to_string(),
            })
        }

        fn try_extract_param_name(s: &str) -> Option<String> {
            let chars: Vec<(usize, char)> = s.char_indices().collect();
            let len = chars.len();
            let mut idx = 0usize;
            while idx < len {
                let (byte_pos, ch) = chars[idx];
                if matches!(ch, '$' | '@' | '%' | '&') {
                    let start = byte_pos;
                    idx += 1;
                    let mut end_idx = idx;
                    while end_idx < len {
                        let (_, next) = chars[end_idx];
                        if next.is_alphanumeric() || next == '_' || next == '-' {
                            end_idx += 1;
                        } else {
                            break;
                        }
                    }
                    let end_byte = if end_idx < len {
                        chars[end_idx].0
                    } else {
                        s.len()
                    };
                    // Allow both named params ($x) and anonymous ($, @, %)
                    return Some(s[start..end_byte].to_string());
                }
                idx += 1;
            }
            None
        }

        fn matching_bracket(c: char) -> Option<char> {
            match c {
                '(' => Some(')'),
                '[' => Some(']'),
                '{' => Some('}'),
                '<' => Some('>'),
                '\u{00AB}' => Some('\u{00BB}'), // << >>
                '\u{2018}' => Some('\u{2019}'),
                '\u{201C}' => Some('\u{201D}'),
                '\u{300C}' => Some('\u{300D}'),
                '\u{300E}' => Some('\u{300F}'),
                '\u{FF08}' => Some('\u{FF09}'),
                '\u{300A}' => Some('\u{300B}'),
                '\u{3008}' => Some('\u{3009}'),
                '\u{169B}' => Some('\u{169C}'),
                '\u{2045}' => Some('\u{2046}'),
                '\u{207D}' => Some('\u{207E}'),
                '\u{2768}' => Some('\u{2769}'),
                '\u{276E}' => Some('\u{276F}'),
                '\u{2770}' => Some('\u{2771}'),
                '\u{2772}' => Some('\u{2773}'),
                '\u{27E6}' => Some('\u{27E7}'),
                '\u{2985}' => Some('\u{2986}'),
                '\u{2993}' => Some('\u{2994}'),
                '\u{2995}' => Some('\u{2996}'),
                _ => None,
            }
        }

        fn parse_doc_comment(
            lines: &[&str],
            start: usize,
            prefix: &str,
        ) -> Option<(String, usize, bool)> {
            let trimmed = lines.get(start)?.trim_start();
            let rest = trimmed.strip_prefix(prefix)?;
            let rest = rest.trim_start();
            if rest.is_empty() {
                return Some((String::new(), start + 1, false));
            }

            let mut chars = rest.chars();
            let open = chars.next()?;
            let Some(close) = matching_bracket(open) else {
                return Some((rest.trim().to_string(), start + 1, false));
            };

            let mut count = 1usize;
            let mut scan = chars.as_str();
            while scan.starts_with(open) {
                count += 1;
                scan = &scan[open.len_utf8()..];
            }

            let open_seq: String = std::iter::repeat_n(open, count).collect();
            let close_seq: String = std::iter::repeat_n(close, count).collect();
            let mut depth = 1i32;
            let mut idx = start;
            let mut current = scan;
            let mut payload = String::new();

            loop {
                if count == 1 {
                    while !current.is_empty() {
                        let ch = current.chars().next().unwrap();
                        if ch == open {
                            depth += 1;
                            payload.push(ch);
                            current = &current[ch.len_utf8()..];
                            continue;
                        }
                        if ch == close {
                            depth -= 1;
                            if depth == 0 {
                                let normalized =
                                    payload.split_whitespace().collect::<Vec<_>>().join(" ");
                                return Some((normalized, idx + 1, true));
                            }
                            payload.push(ch);
                            current = &current[ch.len_utf8()..];
                            continue;
                        }
                        payload.push(ch);
                        current = &current[ch.len_utf8()..];
                    }
                } else {
                    while !current.is_empty() {
                        if current.starts_with(&close_seq[..]) {
                            depth -= 1;
                            if depth == 0 {
                                let normalized =
                                    payload.split_whitespace().collect::<Vec<_>>().join(" ");
                                return Some((normalized, idx + 1, true));
                            }
                            payload.push_str(&close_seq);
                            current = &current[close_seq.len()..];
                            continue;
                        }
                        if current.starts_with(&open_seq[..]) {
                            depth += 1;
                            payload.push_str(&open_seq);
                            current = &current[open_seq.len()..];
                            continue;
                        }
                        let ch = current.chars().next().unwrap();
                        payload.push(ch);
                        current = &current[ch.len_utf8()..];
                    }
                }

                idx += 1;
                if idx >= lines.len() {
                    let normalized = payload.split_whitespace().collect::<Vec<_>>().join(" ");
                    return Some((normalized, idx, true));
                }
                payload.push('\n');
                current = lines[idx];
            }
        }

        /// Check if a line (after stripping optional prefix keywords) contains
        /// a trailing #= comment on the same line. Returns (line_without_trailing, trailing_text).
        fn extract_inline_trailing(line: &str) -> (String, Option<String>) {
            // Look for #= not inside strings
            let mut in_sq = false;
            let mut in_dq = false;
            let bytes = line.as_bytes();
            let mut i = 0;
            while i < bytes.len() {
                let b = bytes[i];
                if b == b'\'' && !in_dq {
                    in_sq = !in_sq;
                } else if b == b'"' && !in_sq {
                    in_dq = !in_dq;
                } else if b == b'#'
                    && !in_sq
                    && !in_dq
                    && i + 1 < bytes.len()
                    && bytes[i + 1] == b'='
                {
                    // Check it's not #== (comparison)
                    if i + 2 < bytes.len() && bytes[i + 2] == b'=' {
                        i += 1;
                        continue;
                    }
                    let before = line[..i].to_string();
                    let raw = line[i + 2..].trim();
                    // Handle block form #={...} / #=(...)  / #=[...] / #=<...>
                    let after = if let Some(inner) = raw.strip_prefix('{') {
                        inner.strip_suffix('}').unwrap_or(inner).trim().to_string()
                    } else if let Some(inner) = raw.strip_prefix('(') {
                        inner.strip_suffix(')').unwrap_or(inner).trim().to_string()
                    } else if let Some(inner) = raw.strip_prefix('[') {
                        inner.strip_suffix(']').unwrap_or(inner).trim().to_string()
                    } else if let Some(inner) = raw.strip_prefix('<') {
                        inner.strip_suffix('>').unwrap_or(inner).trim().to_string()
                    } else {
                        raw.to_string()
                    };
                    return (before, Some(after));
                }
                i += 1;
            }
            (line.to_string(), None)
        }

        /// Dispatch prefix for proto/multi/only declarations.
        #[derive(Clone, Copy, PartialEq)]
        enum DispatchPrefix {
            None,
            Proto,
            Multi,
            Only,
        }

        /// Try to extract a declarant name from a line, handling various declaration patterns.
        /// Returns (name, is_class_like, kind, dispatch_prefix)
        fn try_extract_declarant(
            trimmed: &str,
            current_class: &Option<String>,
        ) -> Option<(String, bool, super::DocDeclKind, DispatchPrefix)> {
            use super::DocDeclKind;
            // Strip optional scope declarators
            let s = trimmed
                .strip_prefix("my ")
                .or_else(|| trimmed.strip_prefix("our "))
                .unwrap_or(trimmed);

            // class/module/package/role/grammar declarations
            for kw in &["class ", "module ", "package ", "grammar "] {
                if let Some(rest) = s.strip_prefix(kw) {
                    let name = extract_ident(rest);
                    if !name.is_empty() {
                        let full_name = if name.contains("::") {
                            name
                        } else if let Some(class) = current_class {
                            format!("{}::{}", class, name)
                        } else {
                            name
                        };
                        return Some((full_name, true, DocDeclKind::Package, DispatchPrefix::None));
                    }
                }
            }

            // role (may have parametric [...])
            if let Some(rest) = s.strip_prefix("role ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    let full_name = if name.contains("::") {
                        name
                    } else if let Some(class) = current_class {
                        format!("{}::{}", class, name)
                    } else {
                        name
                    };
                    return Some((full_name, true, DocDeclKind::Package, DispatchPrefix::None));
                }
            }

            // sub/method/submethod/token/rule/regex declarations
            let s2 = s
                .strip_prefix("multi ")
                .or_else(|| s.strip_prefix("proto "))
                .or_else(|| s.strip_prefix("only "))
                .unwrap_or(s);

            let dispatch_prefix = if s.starts_with("multi ") {
                DispatchPrefix::Multi
            } else if s.starts_with("proto ") {
                DispatchPrefix::Proto
            } else if s.starts_with("only ") {
                DispatchPrefix::Only
            } else {
                DispatchPrefix::None
            };

            let had_dispatch_prefix = dispatch_prefix != DispatchPrefix::None;

            // Also handle "anon" keyword (e.g., "anon Str sub {}")
            let s3 = s2.strip_prefix("anon ").unwrap_or(s2);
            // Strip optional return type between anon/dispatch and sub keyword
            let s4 = if s3 != s2 {
                // After "anon ", there may be a type name before "sub"
                if let Some(sub_pos) = s3.find("sub ") {
                    &s3[sub_pos..]
                } else {
                    s3
                }
            } else {
                s3
            };

            for kw in &["sub ", "method ", "submethod ", "token ", "rule ", "regex "] {
                if let Some(rest) = s4.strip_prefix(kw) {
                    let name = extract_ident(rest);
                    let is_grammar_rule = kw.starts_with("token")
                        || kw.starts_with("rule")
                        || kw.starts_with("regex");
                    let is_method_like =
                        kw.starts_with("method") || kw.starts_with("submethod") || is_grammar_rule;
                    let full_name = if is_method_like {
                        if let Some(class) = current_class {
                            if name.is_empty() {
                                // Anonymous method
                                format!("{}::<anon>", class)
                            } else {
                                format!("{}::{}", class, name)
                            }
                        } else if name.is_empty() {
                            "<anon>".to_string()
                        } else {
                            name
                        }
                    } else if name.is_empty() {
                        // Anonymous sub (like "anon Str sub {}")
                        "&<anon>".to_string()
                    } else {
                        // Prefix sub names with & to avoid collision with package names
                        format!("&{}", name)
                    };
                    let kind = if is_grammar_rule {
                        DocDeclKind::GrammarRule
                    } else {
                        DocDeclKind::Sub
                    };
                    return Some((full_name, false, kind, dispatch_prefix));
                }
            }

            // multi/proto/only without explicit sub/method keyword: treat as sub
            if had_dispatch_prefix {
                let name = extract_ident(s2);
                if !name.is_empty() {
                    return Some((
                        format!("&{}", name),
                        false,
                        DocDeclKind::Sub,
                        dispatch_prefix,
                    ));
                }
            }

            // has $.attr declarations
            if let Some(rest) = s.strip_prefix("has ") {
                let rest = rest.trim_start();
                let attr_rest =
                    if rest.starts_with('$') || rest.starts_with('@') || rest.starts_with('%') {
                        rest
                    } else {
                        rest.find(['$', '@', '%'])
                            .map(|i| &rest[i..])
                            .unwrap_or(rest)
                    };
                if let Some(after_sigil) = attr_rest
                    .strip_prefix("$.")
                    .or_else(|| attr_rest.strip_prefix("$!"))
                    .or_else(|| attr_rest.strip_prefix("@."))
                    .or_else(|| attr_rest.strip_prefix("@!"))
                    .or_else(|| attr_rest.strip_prefix("%."))
                    .or_else(|| attr_rest.strip_prefix("%!"))
                {
                    let name = extract_ident(after_sigil);
                    if !name.is_empty() {
                        let full_name = if let Some(class) = current_class {
                            format!("{}::$!{}", class, name)
                        } else {
                            format!("$!{}", name)
                        };
                        return Some((full_name, false, DocDeclKind::Attr, DispatchPrefix::None));
                    }
                }
            }

            // enum declaration
            if let Some(rest) = s.strip_prefix("enum ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((name, false, DocDeclKind::Package, DispatchPrefix::None));
                }
            }

            // subset declaration
            if let Some(rest) = s.strip_prefix("subset ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((name, false, DocDeclKind::Package, DispatchPrefix::None));
                }
            }

            // unit module
            if let Some(rest) = s.strip_prefix("unit module") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((name, false, DocDeclKind::Package, DispatchPrefix::None));
                }
            }

            // Handle "anon sub" in assignments like "my $x = anon Str sub {}"
            if let Some(anon_pos) = s.find("anon ") {
                let after_anon = &s[anon_pos + 5..];
                // Skip optional type name before "sub"
                let sub_rest = if let Some(sub_pos) = after_anon.find("sub ") {
                    &after_anon[sub_pos + 4..]
                } else if let Some(sub_pos) = after_anon.find("sub{") {
                    &after_anon[sub_pos + 3..]
                } else {
                    ""
                };
                if !sub_rest.is_empty()
                    || after_anon.starts_with("sub ")
                    || after_anon.starts_with("sub{")
                {
                    return Some((
                        "&<anon>".to_string(),
                        false,
                        DocDeclKind::Sub,
                        DispatchPrefix::None,
                    ));
                }
            }

            // Handle bare anonymous sub anywhere in the line (e.g. "is sub {")
            if s.contains("sub {") || s.contains("sub{") {
                return Some((
                    "&<anon>".to_string(),
                    false,
                    DocDeclKind::Sub,
                    DispatchPrefix::None,
                ));
            }

            // Handle block assignments: "my $var = {" or "my $var = {;"
            // This allows doc comments to attach to blocks assigned to variables.
            if let Some(eq_pos) = s.find('=') {
                let after_eq = s[eq_pos + 1..].trim_start();
                if after_eq.starts_with('{') || after_eq.starts_with("{;") {
                    // Extract the variable name from before the '='
                    let before_eq = s[..eq_pos].trim();
                    if let Some(dollar_pos) = before_eq.rfind('$') {
                        let var_name = &before_eq[dollar_pos..];
                        let var_name = var_name
                            .split(|c: char| {
                                !c.is_alphanumeric() && c != '_' && c != '-' && c != '$'
                            })
                            .next()
                            .unwrap_or(var_name);
                        if !var_name.is_empty() {
                            return Some((
                                format!("block:{}", var_name),
                                false,
                                DocDeclKind::Sub,
                                DispatchPrefix::None,
                            ));
                        }
                    }
                }
            }

            None
        }

        let lines: Vec<&str> = input.lines().collect();
        let mut idx = 0usize;
        while idx < lines.len() {
            let line = lines[idx];
            let trimmed = line.trim_start();

            // Leading doc comment (#|)
            if let Some((text, next_idx, _is_block)) = parse_doc_comment(&lines, idx, "#|") {
                pending_leading = append_doc_text(pending_leading.take(), &text);
                last_declarant = None;
                idx = next_idx;
                continue;
            }

            // Trailing doc comment (#=) on its own line
            if let Some((text, next_idx, _is_block)) = parse_doc_comment(&lines, idx, "#=") {
                if !text.is_empty()
                    && let Some((ref name, ref kind)) = last_declarant
                {
                    let entry =
                        self.doc_comments
                            .entry(name.clone())
                            .or_insert_with(|| DocComment {
                                wherefore_name: name.clone(),
                                kind: kind.clone(),
                                ..Default::default()
                            });
                    entry.trailing = append_doc_text(entry.trailing.take(), &text);
                }
                idx = next_idx;
                continue;
            }

            // Not a doc comment line — check for declarations
            // Skip embedded block comments (#`(...), #`[...], #`{...}, #`<...>)
            // These can span multiple lines and may contain #| or #= that should
            // NOT be treated as doc comments.
            if let Some(after_backtick) = trimmed.strip_prefix("#`")
                && let Some(open_char) = after_backtick.chars().next()
                && let Some(close_char) = matching_bracket(open_char)
            {
                // Count consecutive open brackets
                let mut count = 1usize;
                let mut scan = &after_backtick[open_char.len_utf8()..];
                while scan.starts_with(open_char) {
                    count += 1;
                    scan = &scan[open_char.len_utf8()..];
                }
                let open_seq: String = std::iter::repeat_n(open_char, count).collect();
                let close_seq: String = std::iter::repeat_n(close_char, count).collect();
                let mut depth = 1i32;
                let mut current = scan;
                let mut block_idx = idx;
                loop {
                    if count == 1 {
                        while !current.is_empty() {
                            let ch = current.chars().next().unwrap();
                            if ch == open_char {
                                depth += 1;
                            } else if ch == close_char {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            current = &current[ch.len_utf8()..];
                        }
                    } else {
                        while !current.is_empty() {
                            if current.starts_with(&close_seq[..]) {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                                current = &current[close_seq.len()..];
                            } else if current.starts_with(&open_seq[..]) {
                                depth += 1;
                                current = &current[open_seq.len()..];
                            } else {
                                let ch = current.chars().next().unwrap();
                                current = &current[ch.len_utf8()..];
                            }
                        }
                    }
                    if depth == 0 {
                        idx = block_idx + 1;
                        break;
                    }
                    block_idx += 1;
                    if block_idx >= lines.len() {
                        idx = block_idx;
                        break;
                    }
                    current = lines[block_idx];
                }
                continue;
            }
            // Skip empty lines and plain comments (but not #| or #=)
            if trimmed.is_empty()
                || (trimmed.starts_with('#')
                    && !trimmed.starts_with("#|")
                    && !trimmed.starts_with("#="))
            {
                // Don't reset last_declarant for empty/comment lines within a class body
                // but do reset pending_leading if we see a regular comment
                if trimmed.starts_with('#')
                    && !trimmed.starts_with("#|")
                    && !trimmed.starts_with("#=")
                {
                    // Regular comment — don't affect doc comments
                }
                idx += 1;
                continue;
            }

            // Closing brace tracking for class scope
            // Handle lines that are just "}" or start with "}" (possibly with trailing content)
            if let Some(after_close) = trimmed.strip_prefix('}') {
                if let Some(prev) = class_stack.pop() {
                    current_class = prev;
                } else {
                    current_class = None;
                }
                last_declarant = None;
                // If the line is just "}" or "};" or "} # comment", skip it entirely
                let after_brace = after_close.trim();
                if after_brace.is_empty() || after_brace == ";" || after_brace.starts_with('#') {
                    idx += 1;
                    continue;
                }
                // Otherwise fall through to process the rest of the line
            }

            // Check for inline trailing #= on the same line as a declaration
            let (line_without_trailing, inline_trailing) = extract_inline_trailing(trimmed);
            let check_line = line_without_trailing.trim();

            // When a line contains multiple declarations separated by ';',
            // try the last segment first so trailing #= attaches to the last
            // declarant (e.g. "has $.first; has $.second; #= comment").
            let last_seg = if check_line.contains(';') {
                check_line.split(';').rev().find_map(|seg| {
                    let seg = seg.trim();
                    if !seg.is_empty() {
                        try_extract_declarant(seg, &current_class)
                    } else {
                        None
                    }
                })
            } else {
                None
            };

            if let Some((name, is_class_like, kind, dispatch)) =
                last_seg.or_else(|| try_extract_declarant(check_line, &current_class))
            {
                // For multi declarations, generate a unique key to avoid
                // overwriting proto or other multi variants.
                // For anonymous subs, also uniquify to avoid collisions.
                let doc_key = if dispatch == DispatchPrefix::Multi {
                    let counter = multi_counters.entry(name.clone()).or_insert(0);
                    let key = format!("{}/multi.{}", name, counter);
                    *counter += 1;
                    key
                } else if name == "&<anon>" {
                    let key = format!("&<anon>.{}", anon_sub_counter);
                    anon_sub_counter += 1;
                    key
                } else {
                    name.clone()
                };

                let leading = pending_leading.take();
                let has_leading = leading.is_some();
                let has_inline_trailing = inline_trailing.is_some();
                // Only create doc_comments entry if there's actual doc content
                if has_leading || has_inline_trailing {
                    let entry =
                        self.doc_comments
                            .entry(doc_key.clone())
                            .or_insert_with(|| DocComment {
                                wherefore_name: name.clone(),
                                kind: kind.clone(),
                                is_proto: false,
                                ..Default::default()
                            });
                    entry.kind = kind.clone();
                    entry.is_proto = dispatch == DispatchPrefix::Proto;
                    entry.source_line = Some((idx + 1) as u32);
                    if has_leading {
                        entry.leading = leading;
                    }
                    if let Some(ref trail) = inline_trailing {
                        entry.trailing = append_doc_text(entry.trailing.take(), trail);
                    }
                    // Extract return type for "anon Type sub {}" patterns
                    if name == "&<anon>"
                        && let Some(anon_pos) = check_line.find("anon ")
                    {
                        let after_anon = &check_line[anon_pos + 5..];
                        if let Some(sub_pos) = after_anon.find("sub") {
                            let type_part = after_anon[..sub_pos].trim();
                            if !type_part.is_empty() {
                                entry.return_type = Some(type_part.to_string());
                            }
                        }
                    }
                }
                // Always set last_declarant so trailing #= on the next line can attach
                last_declarant = Some((doc_key, kind.clone()));
                // Track the current function for parameter scoping
                if kind == super::DocDeclKind::Sub {
                    current_sub = Some(name.clone());
                }
                // Check for inline #| on the same line as a sub declaration
                // (e.g. "sub foo( #| leading for param") — sets pending_leading
                // for the next parameter
                if kind == super::DocDeclKind::Sub
                    && let Some(hash_pipe_pos) = trimmed.find("#|")
                {
                    let doc_text = trimmed[hash_pipe_pos + 2..].trim();
                    if !doc_text.is_empty() {
                        pending_leading = append_doc_text(pending_leading.take(), doc_text);
                    }
                }
                if is_class_like {
                    class_stack.push(current_class.take());
                    current_class = Some(name);
                    // Check if the scope opens and closes on the same line
                    // e.g. "class Simple { }" — count braces to detect this
                    let mut depth: i32 = 0;
                    for ch in trimmed.chars() {
                        if ch == '{' {
                            depth += 1;
                        } else if ch == '}' {
                            depth -= 1;
                        }
                    }
                    if depth <= 0 {
                        // Self-closing: pop the scope we just pushed
                        if let Some(prev) = class_stack.pop() {
                            current_class = prev;
                        } else {
                            current_class = None;
                        }
                    }
                }
            } else if let Some(param_name) = try_extract_param_name(check_line) {
                if let Some(leading) = pending_leading.take() {
                    // Key parameter docs by "sub_name::param_name" to avoid
                    // collisions when the same param name appears in different subs
                    let param_key = if let Some(ref sub_name) = current_sub {
                        format!("{}::{}", sub_name, param_name)
                    } else {
                        param_name.clone()
                    };
                    self.doc_comments.insert(
                        param_key.clone(),
                        DocComment {
                            wherefore_name: param_name.clone(),
                            leading: Some(leading),
                            trailing: None,
                            kind: super::DocDeclKind::Param,
                            is_proto: false,
                            return_type: None,
                            source_line: Some((idx + 1) as u32),
                        },
                    );
                }
                last_declarant = None;
            } else {
                // Not a recognized declaration — discard pending leading
                pending_leading = None;
                last_declarant = None;
            }

            idx += 1;
        }

        // Build ordered doc_comment_list: collect names in order during a simple re-scan
        self.doc_comment_list.clear();
        let mut seen_names = std::collections::HashSet::new();
        let _ = &self.doc_comments; // used below
        // Collect keys that have actual content, in source order
        // Since HashMap doesn't preserve order, re-scan declarations
        {
            let mut cur_class2: Option<String> = None;
            let mut cls_stack2: Vec<Option<String>> = Vec::new();
            let mut cur_sub2: Option<String> = None;
            let mut idx2 = 0usize;
            while idx2 < lines.len() {
                let trimmed2 = lines[idx2].trim_start();
                // Skip embedded block comments (#`(...), #`[...], etc.)
                if let Some(after_bt) = trimmed2.strip_prefix("#`")
                    && let Some(oc) = after_bt.chars().next()
                    && let Some(cc) = matching_bracket(oc)
                {
                    let mut cnt = 1usize;
                    let mut sc = &after_bt[oc.len_utf8()..];
                    while sc.starts_with(oc) {
                        cnt += 1;
                        sc = &sc[oc.len_utf8()..];
                    }
                    let os: String = std::iter::repeat_n(oc, cnt).collect();
                    let cs: String = std::iter::repeat_n(cc, cnt).collect();
                    let mut dp = 1i32;
                    let mut cur = sc;
                    let mut bi = idx2;
                    loop {
                        if cnt == 1 {
                            while !cur.is_empty() {
                                let ch = cur.chars().next().unwrap();
                                if ch == oc {
                                    dp += 1;
                                } else if ch == cc {
                                    dp -= 1;
                                    if dp == 0 {
                                        break;
                                    }
                                }
                                cur = &cur[ch.len_utf8()..];
                            }
                        } else {
                            while !cur.is_empty() {
                                if cur.starts_with(&cs[..]) {
                                    dp -= 1;
                                    if dp == 0 {
                                        break;
                                    }
                                    cur = &cur[cs.len()..];
                                } else if cur.starts_with(&os[..]) {
                                    dp += 1;
                                    cur = &cur[os.len()..];
                                } else {
                                    let ch = cur.chars().next().unwrap();
                                    cur = &cur[ch.len_utf8()..];
                                }
                            }
                        }
                        if dp == 0 {
                            idx2 = bi + 1;
                            break;
                        }
                        bi += 1;
                        if bi >= lines.len() {
                            idx2 = bi;
                            break;
                        }
                        cur = lines[bi];
                    }
                    continue;
                }
                if parse_doc_comment(&lines, idx2, "#|").is_some()
                    || parse_doc_comment(&lines, idx2, "#=").is_some()
                {
                    // Skip doc comment lines — they were handled in first pass
                    let (_, next_idx, _) = parse_doc_comment(&lines, idx2, "#|")
                        .or_else(|| parse_doc_comment(&lines, idx2, "#="))
                        .unwrap();
                    idx2 = next_idx;
                    continue;
                }
                if trimmed2.is_empty() || (trimmed2.starts_with('#')) {
                    idx2 += 1;
                    continue;
                }
                if let Some(after_close2) = trimmed2.strip_prefix('}') {
                    if let Some(prev) = cls_stack2.pop() {
                        cur_class2 = prev;
                    } else {
                        cur_class2 = None;
                    }
                    let after_brace = after_close2.trim();
                    if after_brace.is_empty() || after_brace == ";" || after_brace.starts_with('#')
                    {
                        idx2 += 1;
                        continue;
                    }
                }
                let (lwt, _) = extract_inline_trailing(trimmed2);
                let ck = lwt.trim();
                if let Some((name, is_cls, kind, dispatch)) = try_extract_declarant(ck, &cur_class2)
                {
                    // Generate the same key as the first pass
                    let doc_key = if dispatch == DispatchPrefix::Multi {
                        // Find the next multi key that hasn't been seen
                        let mut mi = 0usize;
                        loop {
                            let candidate_key = format!("{}/multi.{}", name, mi);
                            if !seen_names.contains(&candidate_key)
                                && self.doc_comments.contains_key(&candidate_key)
                            {
                                break candidate_key;
                            }
                            mi += 1;
                            if mi > 100 {
                                break format!("{}/multi.{}", name, mi);
                            }
                        }
                    } else {
                        name.clone()
                    };
                    if !seen_names.contains(&doc_key)
                        && let Some(dc) = self.doc_comments.get(&doc_key)
                    {
                        seen_names.insert(doc_key);
                        self.doc_comment_list.push(dc.clone());
                    }
                    if kind == super::DocDeclKind::Sub {
                        cur_sub2 = Some(name.clone());
                    }
                    if is_cls {
                        cls_stack2.push(cur_class2.take());
                        cur_class2 = Some(name);
                        // Self-closing scope detection
                        let mut depth: i32 = 0;
                        for ch in trimmed2.chars() {
                            if ch == '{' {
                                depth += 1;
                            } else if ch == '}' {
                                depth -= 1;
                            }
                        }
                        if depth <= 0 {
                            if let Some(prev) = cls_stack2.pop() {
                                cur_class2 = prev;
                            } else {
                                cur_class2 = None;
                            }
                        }
                    }
                } else if let Some(param_name) = try_extract_param_name(ck) {
                    // Check for parameter doc entries
                    let param_key = if let Some(ref sub_name) = cur_sub2 {
                        format!("{}::{}", sub_name, param_name)
                    } else {
                        param_name.clone()
                    };
                    if !seen_names.contains(&param_key)
                        && let Some(dc) = self.doc_comments.get(&param_key)
                    {
                        seen_names.insert(param_key);
                        self.doc_comment_list.push(dc.clone());
                    }
                }
                idx2 += 1;
            }
        }
    }

    /// Add Pod::Block::Declarator entries to $=pod from doc_comment_list.
    pub(super) fn add_declarator_pod_entries(&mut self) {
        use super::DocDeclKind;
        // Get existing $=pod entries
        let mut pod_entries: Vec<Value> = if let Some(Value::Array(arr, _)) = self.env.get("=pod") {
            arr.iter().cloned().collect()
        } else {
            Vec::new()
        };
        // Add declarator doc entries
        // TODO: For Sub/Method/Attr, the WHEREFORE should ideally be the actual
        // runtime object, not a Package placeholder. Currently we use the type
        // name as a Package value so .^name returns the correct type.
        for dc in &self.doc_comment_list {
            let wherefore = match dc.kind {
                DocDeclKind::Package => {
                    Value::Package(crate::symbol::Symbol::intern(&dc.wherefore_name))
                }
                DocDeclKind::Sub => {
                    // Block declarations (from "my $var = {") use "Block" type
                    if dc.wherefore_name.starts_with("block:") {
                        Value::Package(crate::symbol::Symbol::intern("Block"))
                    } else {
                        // Proto/only subs (not methods) have ^name "Routine",
                        // regular subs and methods have "Sub" in mutsu.
                        // Methods have wherefore_name containing "::" (e.g., "C::meth")
                        // or not starting with "&".
                        let is_standalone_sub =
                            dc.wherefore_name.starts_with('&') || !dc.wherefore_name.contains("::");
                        let base_type = if dc.is_proto && is_standalone_sub {
                            "Routine"
                        } else {
                            "Sub"
                        };
                        // For subs with return types (e.g., "anon Str sub {}"),
                        // produce "Sub+{Callable[Str]}" format
                        let type_name = if let Some(ref rt) = dc.return_type {
                            format!("{}+{{Callable[{}]}}", base_type, rt)
                        } else {
                            base_type.to_string()
                        };
                        Value::Package(crate::symbol::Symbol::intern(&type_name))
                    }
                }
                DocDeclKind::GrammarRule => Value::Package(crate::symbol::Symbol::intern("Regex")),
                DocDeclKind::Attr => Value::Package(crate::symbol::Symbol::intern("Attribute")),
                DocDeclKind::Param => Value::Package(crate::symbol::Symbol::intern("Parameter")),
            };
            let pod_entry = Interpreter::make_pod_declarator(dc, wherefore);
            pod_entries.push(pod_entry);
        }
        self.env
            .insert("=pod".to_string(), Value::array(pod_entries));
    }
}
