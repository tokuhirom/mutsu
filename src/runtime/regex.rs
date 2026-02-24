use super::*;

impl Interpreter {
    fn regex_escape_literal(text: &str) -> String {
        let mut out = String::new();
        for ch in text.chars() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                out.push(ch);
            } else {
                out.push('\\');
                out.push(ch);
            }
        }
        out
    }

    fn extract_sym_adverb(name: &str) -> Option<String> {
        let marker = ":sym<";
        let start = name.find(marker)? + marker.len();
        let rest = &name[start..];
        let end = rest.find('>')?;
        Some(rest[..end].to_string())
    }

    fn instantiate_token_pattern(def: &FunctionDef, pattern: &str) -> String {
        let Some(sym) = Self::extract_sym_adverb(&def.name) else {
            return pattern.to_string();
        };
        let escaped = Self::regex_escape_literal(&sym);
        pattern
            .replace("<.sym>", &escaped)
            .replace("<sym>", &escaped)
    }

    fn token_pattern_from_def(def: &FunctionDef) -> Option<String> {
        match def.body.last() {
            Some(Stmt::Expr(Expr::Literal(Value::Regex(p)))) => {
                Some(Self::instantiate_token_pattern(def, p))
            }
            Some(Stmt::Expr(Expr::Literal(Value::Str(s)))) => {
                Some(Self::instantiate_token_pattern(def, s))
            }
            Some(Stmt::Return(Expr::Literal(Value::Regex(p)))) => {
                Some(Self::instantiate_token_pattern(def, p))
            }
            Some(Stmt::Return(Expr::Literal(Value::Str(s)))) => {
                Some(Self::instantiate_token_pattern(def, s))
            }
            _ => None,
        }
    }

    fn resolve_token_patterns_static_in_pkg(&self, name: &str, pkg: &str) -> Vec<(String, String)> {
        let mut out = Vec::new();
        if name.contains("::") {
            if let Some(defs) = self.token_defs.get(name) {
                for def in defs {
                    if let Some(p) = Self::token_pattern_from_def(def) {
                        out.push((p, def.package.clone()));
                    }
                }
            }
            let sym_prefix = format!("{name}:sym<");
            let mut sym_keys: Vec<&String> = self
                .token_defs
                .keys()
                .filter(|key| key.starts_with(&sym_prefix))
                .collect();
            sym_keys.sort();
            for key in sym_keys {
                if let Some(defs) = self.token_defs.get(key) {
                    for def in defs {
                        if let Some(p) = Self::token_pattern_from_def(def) {
                            out.push((p, def.package.clone()));
                        }
                    }
                }
            }
            return out;
        }
        if !pkg.is_empty() {
            let local = format!("{}::{}", pkg, name);
            if let Some(defs) = self.token_defs.get(&local) {
                for def in defs {
                    if let Some(p) = Self::token_pattern_from_def(def) {
                        out.push((p, def.package.clone()));
                    }
                }
            }
            let sym_prefix = format!("{pkg}::{name}:sym<");
            let mut sym_keys: Vec<&String> = self
                .token_defs
                .keys()
                .filter(|key| key.starts_with(&sym_prefix))
                .collect();
            sym_keys.sort();
            for key in sym_keys {
                if let Some(defs) = self.token_defs.get(key) {
                    for def in defs {
                        if let Some(p) = Self::token_pattern_from_def(def) {
                            out.push((p, def.package.clone()));
                        }
                    }
                }
            }
        }
        let global = format!("GLOBAL::{}", name);
        if let Some(defs) = self.token_defs.get(&global) {
            for def in defs {
                if let Some(p) = Self::token_pattern_from_def(def) {
                    out.push((p, def.package.clone()));
                }
            }
        }
        let sym_prefix = format!("GLOBAL::{name}:sym<");
        let mut sym_keys: Vec<&String> = self
            .token_defs
            .keys()
            .filter(|key| key.starts_with(&sym_prefix))
            .collect();
        sym_keys.sort();
        for key in sym_keys {
            if let Some(defs) = self.token_defs.get(key) {
                for def in defs {
                    if let Some(p) = Self::token_pattern_from_def(def) {
                        out.push((p, def.package.clone()));
                    }
                }
            }
        }
        out
    }

    #[allow(dead_code)]
    pub(super) fn regex_is_match(&self, pattern: &str, text: &str) -> bool {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return false,
        };
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self.regex_match_from_in_pkg(&parsed, &chars, 0, &pkg);
        }
        for start in 0..=chars.len() {
            if self.regex_match_from_in_pkg(&parsed, &chars, start, &pkg) {
                return true;
            }
        }
        false
    }

    pub(super) fn regex_match_with_captures(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<RegexCaptures> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_caps_in_pkg(&parsed, &chars, 0, &pkg)
                .map(|(end, mut caps)| {
                    caps.from = 0;
                    caps.to = end;
                    caps.matched = chars[0..end].iter().collect();
                    caps
                });
        }
        for start in 0..=chars.len() {
            if let Some((end, mut caps)) =
                self.regex_match_end_from_caps_in_pkg(&parsed, &chars, start, &pkg)
            {
                caps.from = start;
                caps.to = end;
                caps.matched = chars[start..end].iter().collect();
                return Some(caps);
            }
        }
        None
    }

    pub(super) fn regex_match_all_with_captures(
        &self,
        pattern: &str,
        text: &str,
    ) -> Vec<RegexCaptures> {
        let Some(parsed) = self.parse_regex(pattern) else {
            return Vec::new();
        };
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        let mut out = Vec::new();
        let mut starts = Vec::new();
        if parsed.anchor_start {
            starts.push(0usize);
        } else {
            starts.extend(0..=chars.len());
        }
        for start in starts {
            for (end, mut caps) in
                self.regex_match_ends_from_caps_in_pkg(&parsed, &chars, start, &pkg)
            {
                caps.from = start;
                caps.to = end;
                caps.matched = chars[start..end].iter().collect();
                out.push(caps);
            }
        }
        out.sort_by_key(|caps| (caps.from, caps.to, caps.positional.len(), caps.named.len()));
        out
    }

    pub(super) fn regex_find_first(&self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
                .map(|end| (0, end));
        }
        for start in 0..=chars.len() {
            if let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, start, &pkg) {
                return Some((start, end));
            }
        }
        None
    }

    pub(super) fn regex_match_len_at_start(&self, pattern: &str, text: &str) -> Option<usize> {
        let parsed = self.parse_regex(pattern)?;
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        self.regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
    }

    fn regex_match_len_at_start_in_pkg(
        &self,
        pattern: &str,
        text: &str,
        pkg: &str,
    ) -> Option<usize> {
        let parsed = self.parse_regex(pattern)?;
        let chars: Vec<char> = text.chars().collect();
        self.regex_match_end_from_in_pkg(&parsed, &chars, 0, pkg)
    }

    fn regex_match_end_from_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Option<usize> {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return Some(pos);
                    }
                } else {
                    return Some(pos);
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    stack.push((idx + 1, pos));
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        None
    }

    fn regex_match_end_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Option<(usize, RegexCaptures)> {
        let mut stack = Vec::new();
        stack.push((0usize, start, RegexCaptures::default()));
        while let Some((idx, pos, caps)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return Some((pos, caps));
                    }
                } else {
                    return Some((pos, caps));
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some((next, new_caps)) = self.regex_match_atom_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next, new_caps));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    stack.push((idx + 1, pos, caps.clone()));
                    if let Some((next, new_caps)) = self.regex_match_atom_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next, new_caps));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push((pos, caps.clone()));
                    let mut current = pos;
                    let mut current_caps = caps.clone();
                    while let Some((next, new_caps)) = self.regex_match_atom_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        &current_caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        current_caps = new_caps.clone();
                        positions.push((next, new_caps));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
                RegexQuant::OneOrMore => {
                    let (mut current, mut current_caps) = match self
                        .regex_match_atom_with_capture_in_pkg(
                            &token.atom,
                            chars,
                            pos,
                            &caps,
                            pkg,
                            pattern.ignore_case,
                        ) {
                        Some((next, new_caps)) => (next, new_caps),
                        None => continue,
                    };
                    let mut positions = Vec::new();
                    positions.push((current, current_caps.clone()));
                    while let Some((next, new_caps)) = self.regex_match_atom_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        &current_caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        current_caps = new_caps.clone();
                        positions.push((next, new_caps));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
            }
        }
        None
    }

    fn regex_match_ends_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Vec<(usize, RegexCaptures)> {
        let mut stack = Vec::new();
        stack.push((0usize, start, RegexCaptures::default()));
        let mut matches = Vec::new();
        while let Some((idx, pos, caps)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if !pattern.anchor_end || pos == chars.len() {
                    matches.push((pos, caps));
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    for (next, new_caps) in self.regex_match_atom_all_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next, new_caps));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    stack.push((idx + 1, pos, caps.clone()));
                    for (next, new_caps) in self.regex_match_atom_all_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next, new_caps));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push((pos, caps.clone()));
                    let mut current = pos;
                    let mut current_caps = caps.clone();
                    while let Some((next, new_caps)) = self.regex_match_atom_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        &current_caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        current_caps = new_caps.clone();
                        positions.push((next, new_caps));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
                RegexQuant::OneOrMore => {
                    let (mut current, mut current_caps) = match self
                        .regex_match_atom_with_capture_in_pkg(
                            &token.atom,
                            chars,
                            pos,
                            &caps,
                            pkg,
                            pattern.ignore_case,
                        ) {
                        Some((next, new_caps)) => (next, new_caps),
                        None => continue,
                    };
                    let mut positions = Vec::new();
                    positions.push((current, current_caps.clone()));
                    while let Some((next, new_caps)) = self.regex_match_atom_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        &current_caps,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        current_caps = new_caps.clone();
                        positions.push((next, new_caps));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
            }
        }
        matches
    }

    fn regex_match_atom_all_with_capture_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        if let RegexAtom::Named(name) = atom {
            let (silent, lookup_name) = if let Some(stripped) = name.strip_prefix('.') {
                (true, stripped)
            } else {
                (false, name.as_str())
            };
            let candidates = self.resolve_token_patterns_static_in_pkg(lookup_name, pkg);
            if !candidates.is_empty() {
                let tail: Vec<char> = chars[pos..].to_vec();
                let mut out = Vec::new();
                for (sub_pat, sub_pkg) in candidates {
                    if let Some(parsed) = self.parse_regex(&sub_pat) {
                        for (inner_end, inner_caps) in
                            self.regex_match_ends_from_caps_in_pkg(&parsed, &tail, 0, &sub_pkg)
                        {
                            let end = pos + inner_end;
                            let mut new_caps = current_caps.clone();
                            for (k, v) in inner_caps.named {
                                new_caps.named.entry(k).or_default().extend(v);
                            }
                            for v in inner_caps.positional {
                                new_caps.positional.push(v);
                            }
                            if !silent {
                                let captured: String = chars[pos..end].iter().collect();
                                new_caps
                                    .named
                                    .entry(lookup_name.to_string())
                                    .or_default()
                                    .push(captured);
                            }
                            out.push((end, new_caps));
                        }
                    }
                }
                out.sort_by_key(|(end, caps)| (*end, caps.positional.len(), caps.named.len()));
                out.dedup_by(|(end_a, _), (end_b, _)| end_a == end_b);
                return out;
            }
        }
        self.regex_match_atom_with_capture_in_pkg(atom, chars, pos, current_caps, pkg, ignore_case)
            .into_iter()
            .collect()
    }

    #[allow(dead_code)]
    fn regex_match_from_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> bool {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return true;
                    }
                } else {
                    return true;
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    stack.push((idx + 1, pos));
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        current,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        if next == current {
                            break;
                        }
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        false
    }

    fn regex_match_atom_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        pkg: &str,
        ignore_case: bool,
    ) -> Option<usize> {
        // Group, CaptureGroup, Alternation, ZeroWidth, CodeAssertion can match zero-width
        match atom {
            RegexAtom::Group(pattern) => {
                return self.regex_match_end_from_in_pkg(pattern, chars, pos, pkg);
            }
            RegexAtom::CaptureGroup(pattern) => {
                return self.regex_match_end_from_in_pkg(pattern, chars, pos, pkg);
            }
            RegexAtom::Alternation(alternatives) => {
                for alt in alternatives {
                    if let Some(end) = self.regex_match_end_from_in_pkg(alt, chars, pos, pkg) {
                        return Some(end);
                    }
                }
                return None;
            }
            RegexAtom::ZeroWidth => {
                return Some(pos); // Matches at any position without consuming
            }
            RegexAtom::CodeAssertion { .. } => {
                // In non-capture mode, code assertions always succeed
                // (we can't evaluate them without capture context)
                return Some(pos);
            }
            RegexAtom::UnicodePropAssert { name, negated } => {
                // Zero-width assertion: check next char but don't consume
                if pos >= chars.len() {
                    // At end of string, negated assertion succeeds
                    return if *negated { Some(pos) } else { None };
                }
                let c = chars[pos];
                let prop_match = check_unicode_property(name, c);
                let result = if *negated { !prop_match } else { prop_match };
                return if result { Some(pos) } else { None };
            }
            _ => {}
        }
        if let RegexAtom::Named(name) = atom {
            let (silent, lookup_name) = if let Some(stripped) = name.strip_prefix('.') {
                (true, stripped)
            } else {
                (false, name.as_str())
            };
            let candidates = self.resolve_token_patterns_static_in_pkg(lookup_name, pkg);
            if !candidates.is_empty() {
                let remaining: String = chars[pos..].iter().collect();
                let mut best_len: Option<usize> = None;
                for (sub_pat, sub_pkg) in candidates {
                    if let Some(len) =
                        self.regex_match_len_at_start_in_pkg(&sub_pat, &remaining, &sub_pkg)
                    {
                        let better = best_len.map(|current| len > current).unwrap_or(true);
                        if better {
                            best_len = Some(len);
                        }
                    }
                }
                return best_len.map(|len| pos + len);
            }
            if lookup_name == "ws" {
                let _ = silent;
                let mut next = pos;
                while next < chars.len() && chars[next].is_whitespace() {
                    next += 1;
                }
                return Some(next);
            }
        }
        if pos >= chars.len() {
            return None;
        }
        let c = chars[pos];
        // Newline needs special handling: \r\n is a single logical newline (2 chars)
        match atom {
            RegexAtom::Newline => {
                // Raku \n matches: \r\n (CR+LF), \n (LF), \r (CR), \x85 (NEL), \x2028 (LINE SEP)
                if c == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
                    return Some(pos + 2); // CR/LF
                }
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return Some(pos + 1);
                }
                return None;
            }
            RegexAtom::NotNewline => {
                // Matches anything that is NOT a newline character
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return None;
                }
                return Some(pos + 1);
            }
            _ => {}
        }
        let matched = match atom {
            RegexAtom::Literal(ch) => {
                if ignore_case {
                    ch.to_lowercase().to_string() == c.to_lowercase().to_string()
                } else {
                    *ch == c
                }
            }
            RegexAtom::Named(name) => {
                let name_chars: Vec<char> = name.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    false
                } else {
                    let slice = &chars[pos..pos + name_chars.len()];
                    if ignore_case {
                        slice.iter().collect::<String>().to_lowercase()
                            == name_chars.iter().collect::<String>().to_lowercase()
                    } else {
                        *slice == name_chars[..]
                    }
                }
            }
            RegexAtom::Any => true,
            RegexAtom::CharClass(class) => self.regex_match_class(class, c),
            RegexAtom::UnicodeProp {
                name,
                negated,
                args,
            } => {
                let prop_match = if let Some(arg_str) = args {
                    check_unicode_property_with_args(name, arg_str, c)
                } else {
                    check_unicode_property(name, c)
                };
                if *negated { !prop_match } else { prop_match }
            }
            RegexAtom::CompositeClass { positive, negative } => {
                let pos_match = positive.iter().any(|item| match item {
                    ClassItem::NamedBuiltin(n) => matches_named_builtin(n, c),
                    ClassItem::UnicodePropItem { name, negated } => {
                        let m = check_unicode_property(name, c);
                        if *negated { !m } else { m }
                    }
                    _ => self.regex_match_class(
                        &CharClass {
                            items: vec![item.clone()],
                            negated: false,
                        },
                        c,
                    ),
                });
                let neg_match = negative.iter().any(|item| match item {
                    ClassItem::NamedBuiltin(n) => matches_named_builtin(n, c),
                    ClassItem::UnicodePropItem { name, negated } => {
                        let m = check_unicode_property(name, c);
                        if *negated { !m } else { m }
                    }
                    _ => self.regex_match_class(
                        &CharClass {
                            items: vec![item.clone()],
                            negated: false,
                        },
                        c,
                    ),
                });
                pos_match && !neg_match
            }
            RegexAtom::Group(_)
            | RegexAtom::CaptureGroup(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::Newline
            | RegexAtom::NotNewline
            | RegexAtom::ZeroWidth
            | RegexAtom::CodeAssertion { .. }
            | RegexAtom::UnicodePropAssert { .. } => unreachable!(),
        };
        if matched {
            match atom {
                RegexAtom::Named(name) => Some(pos + name.chars().count()),
                _ => Some(pos + 1),
            }
        } else {
            None
        }
    }

    fn regex_match_atom_with_capture_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
    ) -> Option<(usize, RegexCaptures)> {
        // Handle zero-width and group atoms before the length check
        match atom {
            RegexAtom::Group(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. } => {
                return self
                    .regex_match_atom_in_pkg(atom, chars, pos, pkg, ignore_case)
                    .map(|next| (next, current_caps.clone()));
            }
            RegexAtom::CaptureGroup(pattern) => {
                // Match the inner pattern and capture the matched text
                if let Some((end, inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                {
                    let captured: String = chars[pos..end].iter().collect();
                    let mut new_caps = current_caps.clone();
                    // Merge inner captures (nested capture groups)
                    for (k, v) in &inner_caps.named {
                        new_caps
                            .named
                            .entry(k.clone())
                            .or_default()
                            .extend(v.clone());
                    }
                    for v in &inner_caps.positional {
                        new_caps.positional.push(v.clone());
                    }
                    // Add this capture group's match
                    new_caps.positional.push(captured);
                    return Some((end, new_caps));
                }
                return None;
            }
            RegexAtom::CodeAssertion { code, negated } => {
                // Evaluate the code with current captures available
                let result = self.eval_regex_code_assertion(code, current_caps);
                let pass = if *negated { !result } else { result };
                return if pass {
                    Some((pos, current_caps.clone()))
                } else {
                    None
                };
            }
            _ => {}
        }
        if let RegexAtom::Named(name) = atom {
            let (silent, lookup_name) = if let Some(stripped) = name.strip_prefix('.') {
                (true, stripped)
            } else {
                (false, name.as_str())
            };
            let candidates = self.resolve_token_patterns_static_in_pkg(lookup_name, pkg);
            if !candidates.is_empty() {
                let tail: Vec<char> = chars[pos..].to_vec();
                let mut best: Option<(usize, RegexCaptures)> = None;
                for (sub_pat, sub_pkg) in candidates {
                    if let Some(parsed) = self.parse_regex(&sub_pat)
                        && let Some((inner_end, inner_caps)) =
                            self.regex_match_end_from_caps_in_pkg(&parsed, &tail, 0, &sub_pkg)
                    {
                        let better = best
                            .as_ref()
                            .map(|(best_end, _)| inner_end > *best_end)
                            .unwrap_or(true);
                        if better {
                            best = Some((inner_end, inner_caps));
                        }
                    }
                }
                if let Some((inner_end, inner_caps)) = best {
                    let end = pos + inner_end;
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
                    for v in inner_caps.positional {
                        new_caps.positional.push(v);
                    }
                    if !silent {
                        let captured: String = chars[pos..end].iter().collect();
                        new_caps
                            .named
                            .entry(lookup_name.to_string())
                            .or_default()
                            .push(captured);
                    }
                    return Some((end, new_caps));
                }
                return None;
            }
            if lookup_name == "ws" {
                let mut end = pos;
                while end < chars.len() && chars[end].is_whitespace() {
                    end += 1;
                }
                let mut new_caps = current_caps.clone();
                if !silent {
                    let captured: String = chars[pos..end].iter().collect();
                    new_caps
                        .named
                        .entry(lookup_name.to_string())
                        .or_default()
                        .push(captured);
                }
                return Some((end, new_caps));
            }
        }
        if pos >= chars.len() {
            return None;
        }
        match atom {
            RegexAtom::Named(name) => {
                let name_chars: Vec<char> = name.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    return None;
                }
                if chars[pos..pos + name_chars.len()] == name_chars[..] {
                    let captured: String = name_chars.iter().collect();
                    let mut new_caps = current_caps.clone();
                    new_caps
                        .named
                        .entry(name.clone())
                        .or_default()
                        .push(captured);
                    return Some((pos + name_chars.len(), new_caps));
                }
                None
            }
            _ => self
                .regex_match_atom_in_pkg(atom, chars, pos, pkg, ignore_case)
                .map(|next| (next, current_caps.clone())),
        }
    }

    /// Evaluate a code assertion inside a regex.
    /// Sets up $0, $1, etc. from current captures, evaluates the code,
    /// and returns whether the result is truthy.
    fn eval_regex_code_assertion(&self, code: &str, caps: &RegexCaptures) -> bool {
        // We need to evaluate in a mutable context but `self` is &self.
        // Use unsafe interior mutability pattern via a clone-and-eval approach.
        // Parse the code first.
        let (stmts, _) = match crate::parse_dispatch::parse_source(code) {
            Ok(result) => result,
            Err(_) => return false,
        };
        // Create a minimal interpreter with env set up
        let mut env = self.env.clone();
        // Set positional capture variables ($0, $1, etc.)
        for (i, val) in caps.positional.iter().enumerate() {
            env.insert(i.to_string(), Value::Str(val.clone()));
        }
        // Build $/ as an array for $/[n] access
        let match_list: Vec<Value> = caps
            .positional
            .iter()
            .map(|s| Value::Str(s.clone()))
            .collect();
        env.insert("/".to_string(), Value::array(match_list));
        // Set named captures
        for (k, v) in &caps.named {
            let value = if v.len() == 1 {
                Value::Str(v[0].clone())
            } else {
                Value::array(v.iter().cloned().map(Value::Str).collect())
            };
            env.insert(format!("<{}>", k), value);
        }
        // Evaluate the code in a fresh interpreter with this env
        let mut interp = Interpreter {
            env,
            functions: self.functions.clone(),
            current_package: self.current_package.clone(),
            ..Default::default()
        };
        match interp.eval_block_value(&stmts) {
            Ok(val) => val.truthy(),
            Err(_) => false,
        }
    }

    pub(super) fn regex_match_class(&self, class: &CharClass, c: char) -> bool {
        let mut matched = false;
        for item in &class.items {
            match item {
                ClassItem::Range(a, b) => {
                    if *a <= c && c <= *b {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Char(ch) => {
                    if *ch == c {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Digit => {
                    if c.is_ascii_digit() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Word => {
                    if c.is_alphanumeric() || c == '_' {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Space => {
                    if c.is_whitespace() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NamedBuiltin(name) => {
                    if matches_named_builtin(name, c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::UnicodePropItem { name, negated } => {
                    let prop_match = check_unicode_property(name, c);
                    if if *negated { !prop_match } else { prop_match } {
                        matched = true;
                        break;
                    }
                }
            }
        }
        if class.negated { !matched } else { matched }
    }

    /// Find all non-overlapping regex matches, returning (start, end) char-index pairs.
    pub(super) fn regex_find_all(&self, pattern: &str, text: &str) -> Vec<(usize, usize)> {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let pkg = self.current_package.clone();
        let chars: Vec<char> = text.chars().collect();
        let mut results = Vec::new();
        let mut pos = 0;
        while pos <= chars.len() {
            let search_start = if parsed.anchor_start { 0 } else { pos };
            let mut found = None;
            if parsed.anchor_start {
                if pos == 0
                    && let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
                {
                    found = Some((0, end));
                }
            } else {
                for start in search_start..=chars.len() {
                    if let Some(end) =
                        self.regex_match_end_from_in_pkg(&parsed, &chars, start, &pkg)
                    {
                        found = Some((start, end));
                        break;
                    }
                }
            }
            match found {
                Some((start, end)) => {
                    results.push((start, end));
                    // Advance past the match (at least 1 to avoid infinite loop)
                    pos = if end > start { end } else { start + 1 };
                }
                None => break,
            }
            if parsed.anchor_start {
                break;
            }
        }
        results
    }
}

/// Check if a character matches a named builtin character class.
pub(super) fn matches_named_builtin(name: &str, c: char) -> bool {
    match name {
        "alpha" => c.is_alphabetic() || c == '_',
        "upper" => check_unicode_property("Uppercase_Letter", c),
        "lower" => check_unicode_property("Lowercase_Letter", c),
        "digit" => c.is_ascii_digit(),
        "xdigit" => c.is_ascii_hexdigit(),
        "space" | "ws" => c.is_whitespace(),
        "alnum" => c.is_alphabetic() || c == '_' || c.is_ascii_digit(),
        "blank" => c == '\t' || c == ' ' || c == '\u{A0}',
        "cntrl" => c.is_control(),
        "punct" => check_unicode_property("Punctuation", c),
        _ => false,
    }
}
