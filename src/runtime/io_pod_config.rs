use super::*;

impl Interpreter {
    /// Parse Pod config adverbs from a directive tail.
    /// Supports all Raku Pod config value forms including `:key<str>`, `:key(val)`,
    /// `:key[val]`, `:key{k=>v}`, `:!key`, `:NNNkey`, lists, hashes, typed scalars.
    pub(crate) fn parse_pod_config(input: &str) -> (HashMap<String, Value>, &str) {
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
}
