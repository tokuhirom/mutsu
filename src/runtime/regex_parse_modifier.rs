use super::regex_parse::*;
use super::*;
use ::regex::Regex;

impl Interpreter {
    /// Try to parse an inline scope modifier from the remaining source after ':'.
    /// Returns `Some(remaining)` if a modifier was recognized (and flags updated),
    /// or `None` if no modifier matched.
    pub(super) fn try_parse_inline_modifier<'a>(
        remaining: &'a str,
        ratchet: &mut bool,
        ignore_case: &mut bool,
        ignore_mark: &mut bool,
        sigspace: &mut bool,
    ) -> Option<&'a str> {
        fn is_word_boundary(rest: &str) -> bool {
            rest.is_empty() || !rest.starts_with(|c: char| c.is_ascii_alphanumeric() || c == '_')
        }
        fn is_short_boundary(rest: &str) -> bool {
            rest.is_empty()
                || rest.starts_with(' ')
                || rest.starts_with(':')
                || rest.starts_with('/')
        }
        // Check negated long forms first
        if let Some(rest) = remaining.strip_prefix("!ratchet")
            && is_word_boundary(rest)
        {
            *ratchet = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!ignorecase")
            && is_word_boundary(rest)
        {
            *ignore_case = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!ignoremark")
            && is_word_boundary(rest)
        {
            *ignore_mark = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!sigspace")
            && is_word_boundary(rest)
        {
            *sigspace = false;
            return Some(rest);
        }
        // Negated short forms
        if let Some(rest) = remaining.strip_prefix("!r")
            && is_short_boundary(rest)
        {
            *ratchet = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!i")
            && is_short_boundary(rest)
        {
            *ignore_case = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!s")
            && is_short_boundary(rest)
        {
            *sigspace = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!m")
            && is_short_boundary(rest)
        {
            *ignore_mark = false;
            return Some(rest);
        }
        // Positive long forms
        if let Some(rest) = remaining.strip_prefix("ratchet")
            && is_word_boundary(rest)
        {
            *ratchet = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("ignorecase")
            && is_word_boundary(rest)
        {
            *ignore_case = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("ignoremark")
            && is_word_boundary(rest)
        {
            *ignore_mark = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("sigspace")
            && is_word_boundary(rest)
        {
            *sigspace = true;
            return Some(rest);
        }
        // Positive short forms
        if let Some(rest) = remaining.strip_prefix("r")
            && is_short_boundary(rest)
        {
            *ratchet = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("i")
            && is_short_boundary(rest)
        {
            *ignore_case = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("s")
            && is_short_boundary(rest)
        {
            *sigspace = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("m")
            && is_short_boundary(rest)
        {
            *ignore_mark = true;
            return Some(rest);
        }
        None
    }

    pub(super) fn interpolate_regex_scalars(&self, pattern: &str) -> Result<String, RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            // # starts a comment — skip without interpolation.
            // #`[...] is an embedded comment; plain # is a line comment.
            if ch == '#' {
                if i + 1 < chars.len() && chars[i + 1] == '`' {
                    out.push(chars[i]);
                    i += 1;
                    out.push(chars[i]); // `
                    i += 1;
                    if i < chars.len() {
                        let bracket = chars[i];
                        let close = match bracket {
                            '[' => ']',
                            '(' => ')',
                            '{' => '}',
                            '<' => '>',
                            _ => bracket,
                        };
                        out.push(bracket);
                        i += 1;
                        let mut embed_depth = 1u32;
                        while i < chars.len() && embed_depth > 0 {
                            let c = chars[i];
                            if c == bracket && bracket != close {
                                embed_depth += 1;
                            } else if c == close {
                                embed_depth -= 1;
                            }
                            out.push(c);
                            i += 1;
                        }
                    }
                } else {
                    while i < chars.len() && chars[i] != '\n' {
                        out.push(chars[i]);
                        i += 1;
                    }
                }
                continue;
            }
            // Skip code blocks { ... } — don't interpolate variables inside them
            if ch == '{' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '{' {
                        depth += 1;
                    } else if c == '}' {
                        depth -= 1;
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            // Array interpolation in regex groups: (@name) / ( @name )
            // Expand to an alternation group from the current array value.
            if ch == '(' {
                let mut j = i + 1;
                while j < chars.len() && chars[j].is_whitespace() {
                    j += 1;
                }
                if j < chars.len() && chars[j] == '@' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    if j > name_start {
                        let mut k = j;
                        while k < chars.len() && chars[k].is_whitespace() {
                            k += 1;
                        }
                        if k < chars.len() && chars[k] == ')' {
                            let bare_name: String = chars[name_start..j].iter().collect();
                            let sigiled_name = format!("@{}", bare_name);
                            let value = self
                                .env
                                .get(&sigiled_name)
                                .cloned()
                                .or_else(|| self.env.get(&bare_name).cloned())
                                .unwrap_or(Value::Nil);
                            let value = value.into_deref();
                            let entries: Vec<String> = match value {
                                Value::Array(items, ..) => items
                                    .iter()
                                    .map(|v| {
                                        Self::escape_regex_scalar_literal(&v.to_string_value())
                                    })
                                    .collect(),
                                Value::Seq(items) | Value::Slip(items) => items
                                    .iter()
                                    .map(|v| {
                                        Self::escape_regex_scalar_literal(&v.to_string_value())
                                    })
                                    .collect(),
                                Value::Nil => Vec::new(),
                                other => {
                                    vec![Self::escape_regex_scalar_literal(
                                        &other.to_string_value(),
                                    )]
                                }
                            };
                            if entries.is_empty() {
                                out.push_str("()");
                            } else {
                                out.push('(');
                                out.push_str(&entries.join("|"));
                                out.push(')');
                            }
                            i = k + 1;
                            continue;
                        }
                    }
                }
            }
            if ch == '\\' {
                out.push(ch);
                i += 1;
                if i < chars.len() {
                    out.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            // Skip <...> angle brackets — don't interpolate variables inside them.
            // The tokenizer handles <$var>, <@var>, <{code}>, etc. directly.
            if ch == '<' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '\\' {
                        out.push(c);
                        i += 1;
                        if i < chars.len() {
                            out.push(chars[i]);
                            i += 1;
                        }
                        continue;
                    }
                    if c == '<' {
                        depth += 1;
                    } else if c == '>' {
                        depth -= 1;
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            if ch == '$' {
                let inside_sq = is_inside_single_quoted_regex_literal(&chars, i);
                let mut j = i + 1;
                if j < chars.len() && chars[j] == '{' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len() && chars[j] != '}' {
                        j += 1;
                    }
                    if j < chars.len() && j > name_start {
                        // Inside single-quoted regex literals, $ is not interpolated
                        if inside_sq {
                            out.push('$');
                            i += 1;
                            continue;
                        }
                        let name: String = chars[name_start..j].iter().collect();
                        let value = self
                            .env
                            .get(&name)
                            .cloned()
                            .or_else(|| self.env.get(&format!("${name}")).cloned())
                            .unwrap_or(Value::Nil);
                        let value = value.into_deref();
                        Self::check_hash_in_regex(&value)?;
                        Self::push_value_as_regex_pattern(&value, &mut out);
                        i = j + 1;
                        continue;
                    }
                } else if j < chars.len()
                    && (chars[j].is_alphabetic()
                        || chars[j] == '_'
                        || matches!(chars[j], '*' | '?' | '^' | '.'))
                {
                    let name_start = j;
                    // Skip twigil if present
                    if matches!(chars[j], '*' | '?' | '^' | '.') {
                        j += 1;
                    }
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    // Inside single-quoted regex literals, $ is not interpolated
                    if inside_sq {
                        out.push('$');
                        i += 1;
                        continue;
                    }
                    let name: String = chars[name_start..j].iter().collect();
                    // Reduce-time dyn-var overlay: a `$*` var written by a grammar
                    // action mid-parse takes precedence over `self.env` so the next
                    // subrule matches with the updated value (see REGEX_DYNVAR_OVERLAY).
                    let overlay_value = if name.starts_with('*')
                        && super::regex::regex_helpers::dynvar_overlay_active()
                    {
                        super::regex::regex_helpers::dynvar_mark_seen();
                        super::regex::regex_helpers::dynvar_overlay_get(&name)
                    } else {
                        None
                    };
                    let value = overlay_value
                        .or_else(|| self.env.get(&name).cloned())
                        .or_else(|| self.env.get(&format!("${name}")).cloned())
                        .unwrap_or(Value::Nil);
                    let value = value.into_deref();
                    Self::check_hash_in_regex(&value)?;
                    Self::push_value_as_regex_pattern(&value, &mut out);
                    i = j;
                    continue;
                } else if j < chars.len() && chars[j] == '(' {
                    // $( expr ) — scalar contextualizer: evaluate expr
                    // and match the result as a literal string.
                    if inside_sq {
                        out.push('$');
                        i += 1;
                        continue;
                    }
                    j += 1; // skip '('
                    let mut depth = 1usize;
                    let expr_start = j;
                    while j < chars.len() && depth > 0 {
                        if chars[j] == '(' {
                            depth += 1;
                        } else if chars[j] == ')' {
                            depth -= 1;
                        }
                        if depth > 0 {
                            j += 1;
                        }
                    }
                    let expr_str: String = chars[expr_start..j].iter().collect();
                    j += 1; // skip closing ')'
                    let val = self.eval_string_as_source(&expr_str);
                    let literal = val.to_string_value();
                    out.push_str(&Self::escape_regex_scalar_literal(&literal));
                    i = j;
                    continue;
                }
            }
            if ch == '@' {
                // Inside single-quoted regex literals, @ is not interpolated
                if is_inside_single_quoted_regex_literal(&chars, i) {
                    out.push('@');
                    i += 1;
                    continue;
                }
                let mut j = i + 1;
                // @$var — dereference scalar as array for alternation
                if j < chars.len() && chars[j] == '$' {
                    j += 1; // skip '$'
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    if j > name_start {
                        let bare_name: String = chars[name_start..j].iter().collect();
                        let value = self
                            .env
                            .get(&bare_name)
                            .cloned()
                            .or_else(|| self.env.get(&format!("${bare_name}")).cloned())
                            .unwrap_or(Value::Nil);
                        let value = value.into_deref();
                        let elements = match &value {
                            Value::Array(arr, _) => arr.as_ref().clone(),
                            Value::Seq(items) | Value::Slip(items) => {
                                crate::value::ArrayData::new((**items).clone())
                            }
                            _ => crate::value::ArrayData::new(vec![value]),
                        };
                        let mut alts = Vec::new();
                        for elt in &elements {
                            match elt {
                                Value::Regex(pat) => alts.push(pat.to_string()),
                                Value::RegexWithAdverbs(a) => alts.push(a.pattern.to_string()),
                                other => alts.push(Self::escape_regex_scalar_literal(
                                    &other.to_string_value(),
                                )),
                            }
                        }
                        Self::push_regex_interpolated_alternation(&mut out, &alts);
                        i = j;
                        continue;
                    }
                }
                if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    let bare_name: String = chars[name_start..j].iter().collect();
                    let sigiled_name = format!("@{bare_name}");
                    let value = self
                        .env
                        .get(&sigiled_name)
                        .cloned()
                        .or_else(|| self.env.get(&bare_name).cloned())
                        .unwrap_or(Value::Nil);
                    // Slice 2a: a `=`-array-shared source (`my $r = @var`) promotes
                    // `@var` to a `ContainerRef` cell; deref it so the array
                    // interpolates as alternation instead of stringifying the cell.
                    let value = value.into_deref();
                    let elements = match &value {
                        Value::Array(arr, _) => arr.as_ref().clone(),
                        Value::Seq(items) | Value::Slip(items) => {
                            crate::value::ArrayData::new((**items).clone())
                        }
                        _ => crate::value::ArrayData::new(vec![value]),
                    };
                    let mut alts = Vec::new();
                    for elt in &elements {
                        match elt {
                            Value::Regex(pat) => alts.push(pat.to_string()),
                            Value::RegexWithAdverbs(a) => alts.push(a.pattern.to_string()),
                            other => alts
                                .push(Self::escape_regex_scalar_literal(&other.to_string_value())),
                        }
                    }
                    Self::push_regex_interpolated_alternation(&mut out, &alts);
                    i = j;
                    continue;
                } else if j < chars.len() && chars[j] == '(' {
                    j += 1; // skip '('
                    let mut depth = 1usize;
                    let expr_start = j;
                    while j < chars.len() && depth > 0 {
                        if chars[j] == '(' {
                            depth += 1;
                        } else if chars[j] == ')' {
                            depth -= 1;
                        }
                        if depth > 0 {
                            j += 1;
                        }
                    }
                    let expr_str: String = chars[expr_start..j].iter().collect();
                    j += 1; // skip closing ')'
                    let val = self.eval_string_as_source(&expr_str);
                    let elements = match &val {
                        Value::Array(arr, _) => arr.as_ref().clone(),
                        Value::Seq(items) | Value::Slip(items) => {
                            crate::value::ArrayData::new((**items).clone())
                        }
                        _ => crate::value::ArrayData::new(vec![val]),
                    };
                    let mut alts = Vec::new();
                    for elt in elements.iter() {
                        match elt {
                            Value::Regex(pat) => alts.push(pat.to_string()),
                            Value::RegexWithAdverbs(a) => alts.push(a.pattern.to_string()),
                            other => alts
                                .push(Self::escape_regex_scalar_literal(&other.to_string_value())),
                        }
                    }
                    Self::push_regex_interpolated_alternation(&mut out, &alts);
                    i = j;
                    continue;
                }
            }
            out.push(ch);
            i += 1;
        }
        Ok(out)
    }

    /// Convert a Value to its regex pattern representation and push to output.
    /// Handles Nil (always-fail), Regex, Junction (alternation), and literals.
    /// Check if a pattern string contains `$varname` references to undeclared
    /// variables. Used by the `<$var>` handler to detect undeclared variables
    /// in the resolved pattern content (one level of reinterpretation).
    pub(super) fn check_undeclared_vars_in_pattern(&self, pattern: &str) -> Option<RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            if chars[i] == '$' {
                let mut j = i + 1;
                // Skip twigil if present
                if j < chars.len() && matches!(chars[j], '*' | '?' | '^' | '.') {
                    j += 1;
                }
                if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    let name_start = i + 1; // include twigil in name
                    let mut end = j;
                    while end < chars.len()
                        && (chars[end].is_alphanumeric() || chars[end] == '_' || chars[end] == '-')
                    {
                        end += 1;
                    }
                    let name: String = chars[name_start..end].iter().collect();
                    if self.env.get(&name).is_none() && self.env.get(&format!("${name}")).is_none()
                    {
                        let symbol = format!("${name}");
                        let msg = format!("Variable '{symbol}' is not declared");
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(symbol));
                        attrs.insert("message".to_string(), Value::str(msg.clone()));
                        let ex = Value::make_instance(Symbol::intern("X::Undeclared"), attrs);
                        let mut err = RuntimeError::new(&msg);
                        err.exception = Some(Box::new(ex));
                        return Some(err);
                    }
                    i = end;
                    continue;
                }
            }
            i += 1;
        }
        None
    }

    fn push_value_as_regex_pattern(value: &Value, out: &mut String) {
        match value {
            Value::Nil => out.push_str("<!>"),
            Value::Regex(pat) => out.push_str(pat),
            Value::RegexWithAdverbs(a) => out.push_str(&a.pattern),
            Value::Junction { values, .. } => {
                // Expand junction values as alternation [v1|v2|...]
                out.push('[');
                for (idx, v) in values.iter().enumerate() {
                    if idx > 0 {
                        out.push('|');
                    }
                    match v {
                        Value::Regex(pat) => out.push_str(pat),
                        Value::RegexWithAdverbs(a) => out.push_str(&a.pattern),
                        other => out
                            .push_str(&Self::escape_regex_scalar_literal(&other.to_string_value())),
                    }
                }
                out.push(']');
            }
            other => out.push_str(&Self::escape_regex_scalar_literal(&other.to_string_value())),
        }
    }

    /// Check if a value is a Hash and throw X::Syntax::Reserved if so.
    fn check_hash_in_regex(value: &Value) -> Result<(), RuntimeError> {
        if matches!(value, Value::Hash(_)) {
            let msg = "The use of hashes in regexes is reserved";
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.to_string()));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::Reserved"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        Ok(())
    }

    fn escape_regex_scalar_literal(input: &str) -> String {
        let mut out = String::new();
        for ch in input.chars() {
            if ch.is_whitespace()
                || matches!(
                    ch,
                    '\\' | '.'
                        | '^'
                        | '$'
                        | '*'
                        | '+'
                        | '?'
                        | '('
                        | ')'
                        | '['
                        | ']'
                        | '{'
                        | '}'
                        | '<'
                        | '>'
                        | '|'
                        | ':'
                        | '#'
                        | '\''
                        // `%` (and `%%`) is the separator-quantifier infix and `&`
                        // is the conjunction infix; an interpolated scalar matches
                        // *literally* (raku does not re-parse it as regex source),
                        // so e.g. a `%>` delimiter after `\h*` must not bind as a
                        // `\h* % ...` separator. Escape them to force literal match.
                        | '%'
                        | '&'
                )
            {
                out.push('\\');
            }
            out.push(ch);
        }
        out
    }

    /// Check if a regex pattern string contains dangerous code that could
    /// be used for injection attacks. Returns true if the pattern is dangerous.
    pub(super) fn contains_dangerous_regex_code(pattern: &str) -> bool {
        let s = pattern.trim();
        // Check for nested assertions: <$var>, <@var> inside a reinterpreted string
        if s.contains("<$") || s.contains("<@") {
            return true;
        }
        // Check for code interpolation patterns
        if s.contains("$(") || s.contains("@(") {
            return true;
        }
        // Check for braces: { or } could indicate code blocks
        if s.contains('{') || s.contains('}') {
            return true;
        }
        // Check for dynamic lookups: <::(...)>
        if s.contains("::(") {
            return true;
        }
        // Check for double-quoted strings with interpolation
        if s.contains('"') {
            let in_dq: Vec<&str> = s.split('"').collect();
            for (i, chunk) in in_dq.iter().enumerate() {
                if i % 2 == 1
                    && (chunk.contains('$')
                        || chunk.contains('@')
                        || chunk.contains('%')
                        || chunk.contains('&'))
                {
                    return true;
                }
            }
        }
        // Check for named rule with parens containing code: <alpha(...)>
        static NAMED_RULE_RE: std::sync::LazyLock<Regex> =
            std::sync::LazyLock::new(|| Regex::new(r"<\w+\(.*\)>").expect("valid regex"));
        if NAMED_RULE_RE.find(s).is_some() {
            return true;
        }
        // Check for :my variable declaration
        if s.contains(":my ") || s.contains(":our ") {
            return true;
        }
        // Check for "$x:(..." extended colonpair syntax
        if s.contains(":(") {
            return true;
        }
        false
    }

    /// Check if a regex pattern string contains a longname alias
    /// (e.g., `<IO::File=bar>` or `<::IO::File=bar>`). Returns true if so.
    pub(super) fn contains_longname_alias(pattern: &str) -> bool {
        // Look for <...::...=...> pattern
        let s = pattern.trim();
        if s.contains("::") && s.contains('=') {
            return true;
        }
        false
    }

    /// Validate the tail of a subrule-assertion name. Per S05, no characters
    /// other than the recognised continuations may follow the initial
    /// identifier of a `<ident...>` subrule. Returns a malformed-regex error
    /// when, for example, a bare regex metacharacter (`*`, `|`, `&`, ...)
    /// immediately follows the identifier (e.g. `<test*>`).
    pub(super) fn check_subrule_name_tail(name: &str) -> Option<RuntimeError> {
        let mut chars = name.chars().peekable();
        // The leading identifier must start with an alphabetic char or `_`.
        match chars.peek() {
            Some(c) if c.is_alphabetic() || *c == '_' => {}
            _ => return None,
        }
        // Consume the (possibly long) identifier: word characters plus the
        // intra-identifier connectors `-`, `'`, and `::` package separators.
        while let Some(&c) = chars.peek() {
            if c.is_alphanumeric() || c == '_' || c == '-' || c == '\'' || c == ':' {
                chars.next();
            } else {
                break;
            }
        }
        // Whatever remains is the tail. An empty tail or a tail beginning with
        // an allowed continuation is fine; anything else is malformed.
        match chars.peek() {
            // End of name, argument list, alias, method-args, or a passed regex.
            None | Some('(') | Some('=') => None,
            Some(c) if c.is_whitespace() => None,
            Some(_) => {
                let msg = "Unable to parse regex; couldn't find delimiter";
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("message".to_string(), Value::str(msg.to_string()));
                let ex =
                    Value::make_instance(Symbol::intern("X::Syntax::Regex::Unterminated"), attrs);
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(ex));
                Some(err)
            }
        }
    }

    /// Create an X::Syntax::Regex::Alias::LongName error.
    pub(super) fn make_longname_alias_error() -> RuntimeError {
        let msg = "Can't use a long name as a regex alias";
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.to_string()));
        let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Alias::LongName"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(ex));
        err
    }

    /// Create an X::SecurityPolicy error for prohibited regex interpolation.
    pub(super) fn make_security_policy_error() -> RuntimeError {
        let msg = "Prohibited regex interpolation";
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.to_string()));
        let ex = Value::make_instance(Symbol::intern("X::SecurityPolicy"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(ex));
        err
    }
}
