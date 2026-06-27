use super::*;

impl Interpreter {
    pub(super) fn collect_doc_comments(&mut self, input: &str) {
        use super::DocComment;

        self.doc_comments.clear();
        self.doc_comment_list.clear();
        let mut pending_leading: Option<String> = None;
        // The last declaration that can receive trailing #= comments
        // (doc_key, kind, decl_line, callable_type_override, is_proto, return_type)
        #[allow(clippy::type_complexity)]
        let mut last_declarant: Option<(
            String,
            super::DocDeclKind,
            u32,
            Option<&'static str>,
            bool,
            Option<String>,
        )> = None;
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
        // Track role declaration counts for uniquifying parametric role variants
        let mut role_counters: HashMap<String, usize> = HashMap::new();

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
        /// Returns (name, is_class_like, kind, dispatch_prefix, callable_type_override)
        fn try_extract_declarant(
            trimmed: &str,
            current_class: &Option<String>,
        ) -> Option<(
            String,
            bool,
            super::DocDeclKind,
            DispatchPrefix,
            Option<&'static str>,
        )> {
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
                        return Some((
                            full_name,
                            true,
                            DocDeclKind::Package,
                            DispatchPrefix::None,
                            None,
                        ));
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
                    return Some((
                        full_name,
                        true,
                        DocDeclKind::Package,
                        DispatchPrefix::None,
                        None,
                    ));
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
                    let callable_type = if kw.starts_with("submethod") {
                        Some("Submethod")
                    } else if kw.starts_with("method") {
                        Some("Method")
                    } else {
                        None
                    };
                    return Some((full_name, false, kind, dispatch_prefix, callable_type));
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
                        None,
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
                        return Some((
                            full_name,
                            false,
                            DocDeclKind::Attr,
                            DispatchPrefix::None,
                            None,
                        ));
                    }
                }
            }

            // enum declaration
            if let Some(rest) = s.strip_prefix("enum ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((
                        name,
                        false,
                        DocDeclKind::Package,
                        DispatchPrefix::None,
                        None,
                    ));
                }
            }

            // subset declaration
            if let Some(rest) = s.strip_prefix("subset ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((
                        name,
                        false,
                        DocDeclKind::Package,
                        DispatchPrefix::None,
                        None,
                    ));
                }
            }

            // unit module
            if let Some(rest) = s.strip_prefix("unit module") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((
                        name,
                        false,
                        DocDeclKind::Package,
                        DispatchPrefix::None,
                        None,
                    ));
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
                        None,
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
                    None,
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
                                None,
                            ));
                        }
                    }
                }
            }

            None
        }

        /// Extract heredoc terminators from a line containing `:to/DELIM/` patterns.
        fn extract_heredoc_terminators(line: &str) -> Vec<String> {
            let mut terminators = Vec::new();
            let mut search = line;
            while let Some(pos) = search.find(":to").or_else(|| search.find(":heredoc")) {
                let kw_len = if search[pos..].starts_with(":heredoc") {
                    8
                } else {
                    3
                };
                let after = &search[pos + kw_len..];
                if let Some(open) = after.chars().next() {
                    let close = match open {
                        '/' => '/',
                        '<' => '>',
                        '\u{00AB}' => '\u{00BB}',
                        '(' => ')',
                        '[' => ']',
                        '{' => '}',
                        _ => {
                            search = &search[pos + kw_len..];
                            continue;
                        }
                    };
                    let body = &after[open.len_utf8()..];
                    if let Some(end) = body.find(close) {
                        let term = body[..end].to_string();
                        if !term.is_empty() {
                            terminators.push(term);
                        }
                    }
                    search = if after.len() > open.len_utf8() {
                        &after[open.len_utf8()..]
                    } else {
                        ""
                    };
                } else {
                    break;
                }
            }
            terminators
        }

        let lines: Vec<&str> = input.lines().collect();
        let mut idx = 0usize;
        let mut heredoc_terminators: Vec<String> = Vec::new();
        while idx < lines.len() {
            let line = lines[idx];
            let trimmed = line.trim_start();

            // Skip heredoc body lines -- do not scan for doc comments
            if !heredoc_terminators.is_empty() {
                heredoc_terminators.retain(|t| trimmed != t.as_str());
                idx += 1;
                continue;
            }

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
                    && let Some((
                        ref name,
                        ref kind,
                        decl_line,
                        callable_type_ovr,
                        is_proto,
                        ref return_type,
                    )) = last_declarant
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
                    // Carry the declaration's callable type / proto / return type
                    // so a standalone trailing #= produces the correct WHEREFORE
                    // type in $=pod (e.g. Method/Submethod/Routine, not Sub).
                    if entry.callable_type_override.is_none()
                        && let Some(ct) = callable_type_ovr
                    {
                        entry.callable_type_override = Some(ct.to_string());
                    }
                    if is_proto {
                        entry.is_proto = true;
                    }
                    if entry.return_type.is_none()
                        && let Some(rt) = return_type
                    {
                        entry.return_type = Some(rt.clone());
                    }
                    // Ensure source_line is set so proximity matching works
                    if entry.source_line.is_none() {
                        entry.source_line = Some(decl_line);
                    }
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

            if let Some((name, is_class_like, kind, dispatch, callable_type_ovr)) =
                last_seg.or_else(|| try_extract_declarant(check_line, &current_class))
            {
                // For multi declarations, generate a unique key to avoid
                // overwriting proto or other multi variants.
                // For anonymous subs, also uniquify to avoid collisions.
                // For re-declared roles (parametric variants), uniquify too.
                let is_role_decl = {
                    let s = check_line
                        .strip_prefix("my ")
                        .or_else(|| check_line.strip_prefix("our "))
                        .unwrap_or(check_line);
                    s.starts_with("role ")
                };
                let doc_key = if dispatch == DispatchPrefix::Multi {
                    let counter = multi_counters.entry(name.clone()).or_insert(0);
                    let key = format!("{}/multi.{}", name, counter);
                    *counter += 1;
                    key
                } else if name == "&<anon>" {
                    let key = format!("&<anon>.{}", anon_sub_counter);
                    anon_sub_counter += 1;
                    key
                } else if is_role_decl {
                    let counter = role_counters.entry(name.clone()).or_insert(0);
                    let key = if *counter == 0 {
                        name.clone()
                    } else {
                        format!("{}/role.{}", name, counter)
                    };
                    *counter += 1;
                    key
                } else {
                    name.clone()
                };

                let leading = pending_leading.take();
                let has_leading = leading.is_some();
                let has_inline_trailing = inline_trailing.is_some();
                // Extract return type for "anon Type sub {}" patterns. Computed
                // unconditionally so a standalone trailing #= (which attaches via
                // last_declarant) can carry the return type into $=pod.
                let return_type_ovr: Option<String> = if name == "&<anon>" {
                    check_line.find("anon ").and_then(|anon_pos| {
                        let after_anon = &check_line[anon_pos + 5..];
                        after_anon.find("sub").and_then(|sub_pos| {
                            let type_part = after_anon[..sub_pos].trim();
                            if type_part.is_empty() {
                                None
                            } else {
                                Some(type_part.to_string())
                            }
                        })
                    })
                } else {
                    None
                };
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
                    if let Some(ct) = callable_type_ovr {
                        entry.callable_type_override = Some(ct.to_string());
                    }
                    if has_leading {
                        entry.leading = leading;
                    }
                    if let Some(ref trail) = inline_trailing {
                        entry.trailing = append_doc_text(entry.trailing.take(), trail);
                    }
                    if entry.return_type.is_none()
                        && let Some(ref rt) = return_type_ovr
                    {
                        entry.return_type = Some(rt.clone());
                    }
                }
                // Always set last_declarant so trailing #= on the next line can attach
                last_declarant = Some((
                    doc_key,
                    kind.clone(),
                    (idx + 1) as u32,
                    callable_type_ovr,
                    dispatch == DispatchPrefix::Proto,
                    return_type_ovr,
                ));
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
                // Key parameter docs by "sub_name::param_name" to avoid
                // collisions when the same param name appears in different subs
                let param_key = if let Some(ref sub_name) = current_sub {
                    format!("{}::{}", sub_name, param_name)
                } else {
                    param_name.clone()
                };
                let leading = pending_leading.take();
                let has_leading = leading.is_some();
                let has_inline_trailing = inline_trailing.is_some();
                if has_leading || has_inline_trailing {
                    let entry = self
                        .doc_comments
                        .entry(param_key.clone())
                        .or_insert_with(|| DocComment {
                            wherefore_name: param_name.clone(),
                            kind: super::DocDeclKind::Param,
                            ..Default::default()
                        });
                    entry.source_line = Some((idx + 1) as u32);
                    if has_leading {
                        entry.leading = leading;
                    }
                    if let Some(ref trail) = inline_trailing {
                        entry.trailing = append_doc_text(entry.trailing.take(), trail);
                    }
                }
                // Set last_declarant so trailing #= on the next line can attach
                last_declarant = Some((
                    param_key,
                    super::DocDeclKind::Param,
                    (idx + 1) as u32,
                    None,
                    false,
                    None,
                ));
            } else {
                // Not a recognized declaration — discard pending leading
                pending_leading = None;
                last_declarant = None;
            }

            // Detect heredoc starters on the current line so body lines are skipped
            let hd_terms = extract_heredoc_terminators(line);
            if !hd_terms.is_empty() {
                heredoc_terminators = hd_terms;
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
                // Also try last segment for multi-declaration lines
                let last_seg2 = if ck.contains(';') {
                    ck.split(';').rev().find_map(|seg| {
                        let seg = seg.trim();
                        if !seg.is_empty() {
                            try_extract_declarant(seg, &cur_class2)
                        } else {
                            None
                        }
                    })
                } else {
                    None
                };
                if let Some((name, is_cls, kind, dispatch, _callable_type_ovr)) =
                    last_seg2.or_else(|| try_extract_declarant(ck, &cur_class2))
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
                    } else if name == "&<anon>" {
                        // Find the next anon key that hasn't been seen
                        let mut ai = 0usize;
                        loop {
                            let candidate_key = format!("&<anon>.{}", ai);
                            if !seen_names.contains(&candidate_key)
                                && self.doc_comments.contains_key(&candidate_key)
                            {
                                break candidate_key;
                            }
                            ai += 1;
                            if ai > 100 {
                                break format!("&<anon>.{}", ai);
                            }
                        }
                    } else if {
                        let s_check = ck
                            .strip_prefix("my ")
                            .or_else(|| ck.strip_prefix("our "))
                            .unwrap_or(ck);
                        s_check.starts_with("role ")
                    } && seen_names.contains(&name)
                    {
                        // Role re-declaration (parametric variant) — find the next role key
                        let mut ri = 1usize;
                        loop {
                            let candidate_key = format!("{}/role.{}", name, ri);
                            if !seen_names.contains(&candidate_key)
                                && self.doc_comments.contains_key(&candidate_key)
                            {
                                break candidate_key;
                            }
                            ri += 1;
                            if ri > 100 {
                                break format!("{}/role.{}", name, ri);
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
                        // Use callable_type_override if set (Method, Submethod)
                        let is_standalone_sub =
                            dc.wherefore_name.starts_with('&') || !dc.wherefore_name.contains("::");
                        let base_type = if let Some(ref ct) = dc.callable_type_override {
                            ct.as_str()
                        } else if dc.is_proto && is_standalone_sub {
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
