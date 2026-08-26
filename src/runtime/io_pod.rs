use super::*;
use crate::symbol::Symbol;

impl Interpreter {
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
        attrs.insert("contents".to_string(), Value::real_array(contents));
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
        // `Pod::Heading.level` is an `Int` in rakudo (`=head2` -> `level => 2`),
        // which `Pod::To::Text`'s `given $pod.level { when 1 {...} }` and any
        // arithmetic on it rely on. `parse_heading_level` only ever hands us
        // ASCII digits, so the fallback is unreachable in practice.
        let level_value = match level.parse::<i64>() {
            Ok(n) => Value::int(n),
            Err(_) => Value::str(level.to_string()),
        };
        attrs.insert("level".to_string(), level_value);
        attrs.insert("contents".to_string(), Value::real_array(contents));
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
            Value::real_array(vec![Value::str(content)]),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Comment"), attrs)
    }

    pub(crate) fn make_pod_para(lines: Vec<String>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::real_array(lines.into_iter().map(Value::str).collect::<Vec<_>>()),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Para"), attrs)
    }

    /// Create a Pod::Block::Para whose contents may include Pod::FormattingCode
    /// nodes when the text contains `C<...>`, `B<...>`, etc.
    pub(crate) fn make_pod_para_with_formatting(text: &str) -> Value {
        let contents = Self::parse_formatting_codes(text);
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::real_array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Para"), attrs)
    }

    /// An *implicit* (indented) code block: rakudo keeps its body as one
    /// joined string in `contents` (roast S26-documentation/04-code.t pins
    /// `is $r.contents[1].contents, "While this is not\nThis is a code block"`).
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
        attrs.insert("contents".to_string(), Value::real_array(contents));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Block::Code"), attrs)
    }

    /// An *explicitly*-marked code block (`=begin code` / `=for code` /
    /// `=code`). Unlike the indented form, rakudo keeps one `contents`
    /// element per source line followed by a literal `"\n"` element, so the
    /// block's trailing newline survives `Pod::To::Text`'s
    /// `$pod.contents>>.&pod2text.join`. With `:allow`, each line is parsed
    /// for the permitted formatting codes first.
    pub(crate) fn make_pod_code_block(
        code_lines: Vec<String>,
        config: HashMap<String, Value>,
    ) -> Value {
        let allow = config.contains_key("allow");
        let mut contents: Vec<Value> = Vec::new();
        for line in code_lines {
            if allow {
                contents.extend(Self::parse_formatting_codes(&line));
            } else if !line.is_empty() {
                contents.push(Value::str(line));
            }
            contents.push(Value::str("\n".to_string()));
        }
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::real_array(contents));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Block::Code"), attrs)
    }

    /// Strip the common leading indentation from an explicitly-marked code
    /// block's lines and drop its trailing blank lines.
    pub(crate) fn dedent_pod_code_lines(code_lines: &[&str]) -> Vec<String> {
        let min_indent = code_lines
            .iter()
            .filter(|l| !l.trim().is_empty())
            .map(|l| l.len() - l.trim_start().len())
            .min()
            .unwrap_or(0);
        let mut out: Vec<String> = code_lines
            .iter()
            .map(|l| {
                if l.len() >= min_indent {
                    l[min_indent..].to_string()
                } else {
                    l.trim_start().to_string()
                }
            })
            .collect();
        while out.last().is_some_and(|l| l.trim().is_empty()) {
            out.pop();
        }
        out
    }

    /// Collect the verbatim body of a code *paragraph* (`=for code` / the
    /// abbreviated `=code`): the remainder of the directive line, then every
    /// line up to the next blank line or Pod directive, dedented.
    pub(crate) fn collect_pod_code_paragraph(
        lines: &[&str],
        mut idx: usize,
        inline: &str,
        end_target: Option<&str>,
    ) -> (Vec<String>, usize) {
        let mut body: Vec<&str> = Vec::new();
        while idx < lines.len() {
            if lines[idx].trim().is_empty()
                || Self::active_pod_directive(lines[idx], end_target).is_some()
            {
                break;
            }
            body.push(lines[idx]);
            idx += 1;
        }
        let mut out = Self::dedent_pod_code_lines(&body);
        let inline = inline.trim_end();
        if !inline.is_empty() {
            out.insert(0, inline.to_string());
        }
        (out, idx)
    }

    pub(crate) fn make_pod_config(type_name: &str, config: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("type".to_string(), Value::str(type_name.to_string()));
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Config"), attrs)
    }

    pub(crate) fn make_pod_item(level: i64, contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::real_array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        attrs.insert("level".to_string(), Value::int(level));
        Value::make_instance(Symbol::intern("Pod::Item"), attrs)
    }

    fn make_pod_defn(term: String, contents: Vec<Value>, config: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("term".to_string(), Value::str(term));
        attrs.insert("contents".to_string(), Value::real_array(contents));
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
            config.entry("numbered".to_string()).or_insert(Value::TRUE);
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
