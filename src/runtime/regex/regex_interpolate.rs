use super::super::*;

/// Where in a regex code block's Raku source the parameter-baking scan
/// currently is. A `Code` frame carries the brace depth it has opened, so the
/// `}` that closes an interpolation block inside a `"..."` can be told from
/// one that merely closes a nested block.
enum BakeCtx {
    Code { depth: usize },
    Single,
    Double,
}

impl Interpreter {
    /// Heuristic: returns true if a regex subrule arg expression syntactically
    /// looks like a fat-arrow Pair (`a => 1`), a colonpair (`:b(2)`), or a
    /// `|`-flattening prefix (`|...`). For these forms we cannot easily
    /// round-trip the value through a regex source string, so we keep the
    /// original expression and let the match-time evaluator handle them.
    pub(in crate::runtime) fn regex_arg_is_complex(arg: &str) -> bool {
        let trimmed = arg.trim_start();
        if trimmed.starts_with(':') || trimmed.starts_with('|') {
            return true;
        }
        // Detect top-level `=>` (fat-arrow Pair).
        let bytes: Vec<char> = arg.chars().collect();
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        let mut i = 0;
        while i < bytes.len() {
            let ch = bytes[i];
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == q {
                    quote = None;
                }
                i += 1;
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren += 1,
                ')' => paren -= 1,
                '[' => bracket += 1,
                ']' => bracket -= 1,
                '{' => brace += 1,
                '}' => brace -= 1,
                '=' if paren == 0
                    && bracket == 0
                    && brace == 0
                    && i + 1 < bytes.len()
                    && bytes[i + 1] == '>' =>
                {
                    return true;
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// Returns true if a value can be safely formatted into a regex argument
    /// list and re-evaluated to the same value. Conservative: only simple
    /// scalar literals.
    pub(in crate::runtime) fn value_is_round_trippable(value: &Value) -> bool {
        matches!(
            value.view(),
            ValueView::Int(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Nil
        )
    }

    /// Scan a `<…>` construct whose `<` sits at `chars[start]`. Returns the body
    /// between the delimiters, whether a matching `>` was found, and the index
    /// just past the construct.
    fn scan_angle_construct(chars: &[char], start: usize) -> (String, bool, usize) {
        let mut depth = 1usize;
        let mut inner = String::new();
        let mut i = start + 1;
        let mut paren = 0usize;
        let mut bracket = 0usize;
        let mut brace = 0usize;
        let mut quote: Option<char> = None;
        let mut esc = false;
        while i < chars.len() {
            let c = chars[i];
            if let Some(q) = quote {
                if esc {
                    esc = false;
                } else if c == '\\' {
                    esc = true;
                } else if c == q {
                    quote = None;
                }
            } else {
                match c {
                    '\'' | '"' => quote = Some(c),
                    '(' => paren += 1,
                    ')' => paren = paren.saturating_sub(1),
                    '[' => bracket += 1,
                    ']' => bracket = bracket.saturating_sub(1),
                    '{' => brace += 1,
                    '}' => brace = brace.saturating_sub(1),
                    '<' if paren == 0 && bracket == 0 && brace == 0 => depth += 1,
                    '>' if paren == 0 && bracket == 0 && brace == 0 => {
                        depth -= 1;
                        if depth == 0 {
                            return (inner, true, i + 1);
                        }
                    }
                    _ => {}
                }
            }
            inner.push(c);
            i += 1;
        }
        (inner, false, i)
    }

    /// A `<…>` whose body is a *sub-pattern of the same regex* — the lookaround
    /// assertions. Split into `(keyword, body)` so a caller that rewrites the
    /// pattern text (baking bound params, interpolating them) can descend into
    /// the body. Everything else in angle brackets — `<[…]>` character classes,
    /// `<{code}>` / `<$var>` interpolations, subrule calls — is opaque here and
    /// must be left verbatim.
    fn split_lookaround_body(inner: &str) -> Option<(&str, &str)> {
        let head = super::super::regex_parse_core::lookaround_keyword_len(inner)?;
        Some(inner.split_at(head))
    }

    /// Split a code assertion (`<?{ … }>`, `<!{ … }>`, `<{ … }>`) into its
    /// `?`/`!` marker and the `{ … }` block itself.
    ///
    /// Such a body is Raku code that runs at match time, in the *caller's*
    /// env — so a rule's parameters have to be baked into it exactly like a
    /// bare `{ … }` block's. Without this, `token t($x) { <?{ $x eq 'a' }> }`
    /// saw `Nil` for its own parameter (raku prints the bound value), and an
    /// anonymous `token ($text) { <?{ … $text … }> }` — the shape
    /// HomoGlypher's `tokenize` returns — could never see its argument.
    fn split_code_assertion_body(inner: &str) -> Option<(&str, &str)> {
        let head = match inner.as_bytes().first() {
            Some(b'{') => 0,
            Some(b'?') | Some(b'!') if inner.as_bytes().get(1) == Some(&b'{') => 1,
            _ => return None,
        };
        Some(inner.split_at(head))
    }

    /// Walk a regex pattern source and, inside each top-level `{ ... }` code
    /// block, replace bare `$name` references for `param_names` with a
    /// parenthesised literal of the value bound in `self.env`. This lets
    /// regex code blocks see token parameters that would otherwise be lost
    /// when the pattern is matched in the caller's environment.
    pub(in crate::runtime) fn bake_bound_params_into_regex_code_blocks(
        &mut self,
        pattern: &str,
        param_names: &[String],
    ) -> String {
        if param_names.is_empty() {
            return pattern.to_string();
        }
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            // Subrule calls / character classes are opaque, but a lookaround's
            // body is a sub-pattern of this same regex and its `{ … }` blocks
            // need the same baking (YAMLish's `block` computes its indent in a
            // `{ … }` inside a `<?before … >`).
            if ch == '<' {
                let (inner, closed, next) = Self::scan_angle_construct(&chars, i);
                i = next;
                out.push('<');
                match Self::split_lookaround_body(&inner)
                    .or_else(|| Self::split_code_assertion_body(&inner))
                {
                    Some((keyword, body)) => {
                        out.push_str(keyword);
                        out.push_str(
                            &self.bake_bound_params_into_regex_code_blocks(body, param_names),
                        );
                    }
                    None => out.push_str(&inner),
                }
                if closed {
                    out.push('>');
                }
                continue;
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
            if ch == '{' {
                // Capture body of code block. The end is found quote-aware:
                // a brace inside a string literal (`{ say "a}b" }`) is not a
                // block delimiter.
                let j = Self::find_matching_brace_end_in_chars(&chars, i).unwrap_or(chars.len());
                let body_start = i + 1;
                let body: String = chars[body_start..j].iter().collect();
                let baked = self.bake_params_in_code_text(&body, param_names);
                out.push('{');
                out.push_str(&baked);
                if j < chars.len() {
                    out.push('}');
                    i = j + 1;
                } else {
                    i = j;
                }
                continue;
            }
            out.push(ch);
            i += 1;
        }
        out
    }

    fn bake_params_in_code_text(&self, code: &str, param_names: &[String]) -> String {
        let chars: Vec<char> = code.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        // Raku code and a Raku string are different substitution positions, and
        // a code block nested in a `"..."` is a code position again, so the
        // scan carries a stack rather than a pair of flags. Each `Code` frame
        // counts its own brace depth to know which `}` returns it to the
        // string that opened it.
        let mut stack: Vec<BakeCtx> = vec![BakeCtx::Code { depth: 0 }];
        while i < chars.len() {
            let ch = chars[i];
            let Some(frame) = stack.last_mut() else {
                // The bottom `Code` frame is never popped, so this is
                // unreachable; copying the tail keeps it a no-op rather than a
                // panic if that ever stops holding.
                out.extend(chars[i..].iter());
                break;
            };
            match frame {
                BakeCtx::Single => {
                    out.push(ch);
                    i += 1;
                    if ch == '\\' && i < chars.len() {
                        out.push(chars[i]);
                        i += 1;
                    } else if ch == '\'' {
                        stack.pop();
                    }
                }
                BakeCtx::Double => {
                    if ch == '\\' && i + 1 < chars.len() {
                        out.push(ch);
                        out.push(chars[i + 1]);
                        i += 2;
                        continue;
                    }
                    if ch == '"' {
                        out.push(ch);
                        stack.pop();
                        i += 1;
                        continue;
                    }
                    if ch == '{' {
                        out.push(ch);
                        stack.push(BakeCtx::Code { depth: 0 });
                        i += 1;
                        continue;
                    }
                    // A parameter read inside a string interpolates to the
                    // *value*, not to its `.raku`. Splicing the literal form
                    // here left the quotes visible — and worse, the spliced
                    // text was re-read by the enclosing string's own
                    // interpolation rules, so a value holding `$`, `@`, `{` or
                    // `"` did not survive the round trip.
                    match Self::param_read_at(&chars, i, param_names) {
                        Some((name, next)) => match self.env.get(&name) {
                            Some(value) => {
                                out.push_str(&Self::escape_for_double_quoted(
                                    &value.clone().to_string_value(),
                                ));
                                i = next;
                            }
                            None => {
                                out.push(ch);
                                i += 1;
                            }
                        },
                        None => {
                            out.push(ch);
                            i += 1;
                        }
                    }
                }
                BakeCtx::Code { depth } => {
                    if ch == '\\' && i + 1 < chars.len() {
                        out.push(ch);
                        out.push(chars[i + 1]);
                        i += 2;
                        continue;
                    }
                    match ch {
                        '\'' => {
                            out.push(ch);
                            stack.push(BakeCtx::Single);
                            i += 1;
                            continue;
                        }
                        '"' => {
                            out.push(ch);
                            stack.push(BakeCtx::Double);
                            i += 1;
                            continue;
                        }
                        '{' => {
                            *depth += 1;
                            out.push(ch);
                            i += 1;
                            continue;
                        }
                        '}' => {
                            out.push(ch);
                            if *depth > 0 {
                                *depth -= 1;
                            } else if stack.len() > 1 {
                                // Closes the interpolation block a `"..."`
                                // opened; the string resumes.
                                stack.pop();
                            }
                            i += 1;
                            continue;
                        }
                        _ => {}
                    }
                    match Self::param_read_at(&chars, i, param_names) {
                        Some((name, next)) => match self
                            .env
                            .get(&name)
                            .cloned()
                            .and_then(|v| Self::value_to_raku_literal(&v))
                        {
                            Some(literal) => {
                                out.push_str(&literal);
                                i = next;
                            }
                            None => {
                                out.push(ch);
                                i += 1;
                            }
                        },
                        None => {
                            out.push(ch);
                            i += 1;
                        }
                    }
                }
            }
        }
        out
    }

    /// The `$name` of a bound parameter starting at `chars[i]`, with the index
    /// just past it. `None` when this is not a `$`, or names something the
    /// caller did not bind.
    fn param_read_at(chars: &[char], i: usize, param_names: &[String]) -> Option<(String, usize)> {
        if chars[i] != '$' {
            return None;
        }
        let next = *chars.get(i + 1)?;
        if !(next.is_alphabetic() || next == '_') {
            return None;
        }
        let mut j = i + 1;
        while j < chars.len() && (chars[j].is_alphanumeric() || chars[j] == '_') {
            j += 1;
        }
        let name: String = chars[i + 1..j].iter().collect();
        param_names.contains(&name).then_some((name, j))
    }

    /// Escape a value for splicing into a double-quoted Raku string, so that
    /// what it holds is what the string yields. Every character that would
    /// otherwise start an interpolation (`$`, `@`, `%`, `&`, `{`) or end the
    /// string (`"`), plus the escape character itself, is backslashed.
    ///
    /// Escaping only `\\` and `"` — which is what both splice sites used to do
    /// — loses any value containing `$`, `@` or `{`: the spliced text is read
    /// back by the enclosing string's own interpolation rules, silently
    /// yielding a *different* string (or `Nil`) rather than the value.
    pub(in crate::runtime::regex) fn escape_for_double_quoted(text: &str) -> String {
        let mut out = String::with_capacity(text.len());
        for ch in text.chars() {
            if matches!(ch, '\\' | '"' | '$' | '@' | '%' | '&' | '{' | '}') {
                out.push('\\');
            }
            out.push(ch);
        }
        out
    }

    fn value_to_raku_literal(value: &Value) -> Option<String> {
        match value.view() {
            ValueView::Int(n) => Some(n.to_string()),
            ValueView::Num(n) => Some(format!("{n}e0")),
            ValueView::Bool(true) => Some("True".to_string()),
            ValueView::Bool(false) => Some("False".to_string()),
            ValueView::Nil => Some("Nil".to_string()),
            ValueView::Str(s) => {
                let escaped = s.replace('\\', "\\\\").replace('\'', "\\'");
                Some(format!("'{escaped}'"))
            }
            // A callable is a value, not source text. Keeping its lexical
            // reference in the code block lets the enclosing matcher provide
            // the actual closure at match time; `raku_value` would stringify
            // the Block and turn `$callback()` into a call on a Str.
            ValueView::Sub(_) | ValueView::WeakSub(_) => None,
            // Bound token parameters are evaluated before the token's code
            // blocks run.  Reparse their ordinary Raku representation in the
            // caller-side block so composite arguments (notably Pair/Hash and
            // angle-word lists) retain their value instead of becoming an
            // unresolved lexical.
            _ => Some(crate::builtins::methods_0arg::raku_repr::raku_value(value)),
        }
    }

    pub(in crate::runtime) fn interpolate_bound_regex_scalars(&self, pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            if ch == '{' {
                // A code block is opaque to text interpolation. Its end is
                // found quote-aware, so a brace inside a string literal — one
                // the baking pass may itself have written there, escaping a
                // parameter value that contains a brace — does not end it
                // early and leave the rest to be rewritten as pattern text.
                let end = Self::find_matching_brace_end_in_chars(&chars, i)
                    .map_or(chars.len(), |j| j + 1);
                out.extend(chars[i..end].iter());
                i = end;
                continue;
            }
            // As in `bake_bound_params_into_regex_code_blocks`: opaque except for
            // a lookaround, whose body is a sub-pattern of this same regex and so
            // may name the bound parameters (`<?before $indent …>`).
            if ch == '<' {
                let (inner, closed, next) = Self::scan_angle_construct(&chars, i);
                i = next;
                out.push('<');
                match Self::split_lookaround_body(&inner) {
                    Some((keyword, body)) => {
                        out.push_str(keyword);
                        out.push_str(&self.interpolate_bound_regex_scalars(body));
                    }
                    None => out.push_str(&inner),
                }
                if closed {
                    out.push('>');
                }
                continue;
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
            if ch == '$' {
                // `$$` is the end-of-line anchor (see the sibling guard in
                // `interpolate_regex_scalars`); the `{` that may follow it opens a
                // code block, not a `${name}` reference.
                if i + 1 < chars.len() && chars[i + 1] == '$' {
                    out.push('$');
                    out.push('$');
                    i += 2;
                    continue;
                }
                let start = i;
                let mut j = i + 1;
                let parsed = if j < chars.len() && chars[j] == '{' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len() && chars[j] != '}' {
                        j += 1;
                    }
                    if j < chars.len() && j > name_start {
                        let name: String = chars[name_start..j].iter().collect();
                        j += 1;
                        Some((name, j))
                    } else {
                        None
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
                    while j < chars.len() {
                        let c = chars[j];
                        // Raku identifier rule: `-` extends the name only when
                        // followed by a letter — `$p-1` is `$p` minus one, not
                        // the kebab variable "p-1".
                        let kebab = c == '-'
                            && chars
                                .get(j + 1)
                                .is_some_and(|n| n.is_alphabetic() || *n == '_');
                        if c.is_alphanumeric() || c == '_' || kebab {
                            j += 1;
                        } else {
                            break;
                        }
                    }
                    Some((chars[name_start..j].iter().collect::<String>(), j))
                } else {
                    None
                };
                if let Some((name, end)) = parsed {
                    if let Some(value) = self
                        .env
                        .get(&name)
                        .cloned()
                        .or_else(|| self.env.get(&format!("${name}")).cloned())
                    {
                        // The env this reads is the caller's, with the subrule's
                        // bound parameters layered on top, so a substitution
                        // that is not one of them is what makes a
                        // parameterized-subrule resolution depend on the
                        // caller's lexical scope rather than on its arguments.
                        super::regex_arg_purity::note_named_read(&name);
                        // ADR-0046 Decision 2 item 2: a substitution made here is
                        // a *runtime* interpolation, exactly like the general-case
                        // `interpolate_regex_scalars` ones, so it must terminate
                        // the candidate's declarative LTM prefix. Wrap the spliced
                        // span in `NON_DECLARATIVE_INTERP_MARK` so the tokenizer
                        // sets `RegexToken::from_runtime_interpolation` on every
                        // atom it builds from it. Genuine `constant`s are exempt
                        // (Rakudo inlines them at compile time — ADR-0022 §2), and
                        // so is a `"..."` regex literal, whose own tokenizer arm
                        // does not strip the mark (see the identical
                        // `// TODO:` at `regex_parse_modifier.rs`).
                        let is_const =
                            crate::runtime::regex_parse::is_inside_double_quoted_regex_literal(
                                &chars, start,
                            ) || self.is_compile_time_constant_scalar(&name);
                        if !is_const {
                            out.push(Interpreter::NON_DECLARATIVE_INTERP_MARK);
                        }
                        match value.view() {
                            ValueView::Regex(pat) => out.push_str(&pat),
                            _ => {
                                out.push_str(&Self::regex_escape_literal(&value.to_string_value()))
                            }
                        }
                        if !is_const {
                            out.push(Interpreter::NON_DECLARATIVE_INTERP_MARK);
                        }
                    } else {
                        out.extend(chars[start..end].iter());
                    }
                    i = end;
                    continue;
                }
            }
            out.push(ch);
            i += 1;
        }
        out
    }

    /// Does this argument expression mention a `:my`/`:let` lexical of the regex
    /// it appears in? Such an expression has to stay unrendered (see
    /// [`Self::instantiate_named_regex_arg_calls`]).
    fn regex_arg_names_local(arg: &str, locals: &std::collections::HashSet<String>) -> bool {
        if locals.is_empty() {
            return false;
        }
        crate::runtime::regex_parse_core::scalar_names_in_decl(arg)
            .iter()
            .any(|n| locals.contains(n))
    }

    pub(in crate::runtime) fn instantiate_named_regex_arg_calls(
        &mut self,
        pattern: &str,
    ) -> Result<String, RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let default_caps = RegexCaptures::default();
        // An argument naming an in-regex `:my`/`:let` lexical must NOT be rendered
        // here: this pass runs before the match, when that lexical has no value
        // yet, and baking the resulting `Nil` into the pattern text is permanent.
        // Left verbatim, it is re-evaluated at match time against the live
        // captures (`eval_regex_arg_list`), which is where the value exists.
        let regex_local_vars = crate::runtime::regex_parse_core::declared_regex_var_names(pattern);
        let mut i = 0usize;
        while i < chars.len() {
            if chars[i] != '<' {
                out.push(chars[i]);
                i += 1;
                continue;
            }
            // `<( ... )>` is the capture-marker construct (set match start/end),
            // NOT a subrule call `<name(args)>`. Emit the `<(` verbatim and keep
            // scanning the inner pattern (any real subrule calls inside it are
            // still processed), so its content is not mis-read as an argument
            // expression.
            if chars.get(i + 1) == Some(&'(') {
                out.push('<');
                out.push('(');
                i += 2;
                continue;
            }

            let mut depth = 1usize;
            let start = i + 1;
            i += 1;
            // Track nested parens/brackets/braces and quotes so that a
            // closing `>` inside a subrule arg list (e.g. `<.foo(:b(2))>`)
            // doesn't terminate the assertion prematurely.
            let mut paren = 0usize;
            let mut bracket = 0usize;
            let mut brace = 0usize;
            let mut quote: Option<char> = None;
            let mut esc = false;
            while i < chars.len() && depth > 0 {
                let c = chars[i];
                if let Some(q) = quote {
                    if esc {
                        esc = false;
                    } else if c == '\\' {
                        esc = true;
                    } else if c == q {
                        quote = None;
                    }
                } else {
                    match c {
                        '\'' | '"' => quote = Some(c),
                        '(' => paren += 1,
                        ')' => paren = paren.saturating_sub(1),
                        '[' => bracket += 1,
                        ']' => bracket = bracket.saturating_sub(1),
                        '{' => brace += 1,
                        '}' => brace = brace.saturating_sub(1),
                        '<' if paren == 0 && bracket == 0 && brace == 0 => depth += 1,
                        '>' if paren == 0 && bracket == 0 && brace == 0 => depth -= 1,
                        _ => {}
                    }
                }
                i += 1;
            }
            if depth != 0 {
                out.push('<');
                out.extend(chars[start..].iter());
                break;
            }

            let inner_end = i - 1;
            let inner: String = chars[start..inner_end].iter().collect();
            // A lookaround assertion (`<?before ...>` / `<!after ...>` ...)
            // wraps a nested sub-pattern: recurse so an arg-call inside it
            // (`<?before <pred($p)>>`) gets its bound params rendered too —
            // at match time the lookaround body evaluates args against a
            // default env where `$p` no longer exists (P47).
            {
                let assertion = inner
                    .strip_prefix(['?', '!', '.'])
                    .unwrap_or(inner.as_str());
                if let Some(rest) = assertion
                    .strip_prefix("before ")
                    .or_else(|| assertion.strip_prefix("after "))
                {
                    let head_len = inner.len() - rest.len();
                    let head = &inner[..head_len];
                    let rendered_inner = self.instantiate_named_regex_arg_calls(rest)?;
                    out.push('<');
                    out.push_str(head);
                    out.push_str(&rendered_inner);
                    out.push('>');
                    continue;
                }
            }
            let spec = Self::parse_named_regex_lookup_spec(&inner);
            if spec.arg_exprs.is_empty() {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            }

            let Some(open_idx) = inner.find('(') else {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            };
            let Some(close_idx) = inner.rfind(')') else {
                out.push('<');
                out.push_str(&inner);
                out.push('>');
                continue;
            };

            let mut rendered_args = Vec::new();
            for arg in &spec.arg_exprs {
                // For complex / non-round-trippable values (Pair, Slip, Array, Hash, ...),
                // keep the original argument expression so the match-time evaluator can
                // re-evaluate and route them as named arguments or flatten Slips.
                if Self::regex_arg_is_complex(arg)
                    || Self::regex_arg_names_local(arg, &regex_local_vars)
                {
                    rendered_args.push(arg.clone());
                    continue;
                }
                let Some(value) = self.eval_regex_expr_value(arg, &default_caps) else {
                    return Err(RuntimeError::new(format!(
                        "Failed to evaluate regex argument expression: {arg}"
                    )));
                };
                if Self::value_is_round_trippable(&value) {
                    rendered_args.push(Self::format_named_regex_arg_value(&value));
                } else {
                    // Fallback: keep the raw expression so it is re-evaluated later.
                    rendered_args.push(arg.clone());
                }
            }

            out.push('<');
            out.push_str(&inner[..open_idx + 1]);
            out.push_str(&rendered_args.join(", "));
            out.push_str(&inner[close_idx..]);
            out.push('>');
        }
        Ok(out)
    }

    pub(super) fn restore_env_entries(&mut self, restore: HashMap<String, Option<Value>>) {
        for (key, value) in restore {
            match value {
                Some(v) => {
                    self.env.insert(key, v);
                }
                None => {
                    self.env.remove(&key);
                }
            }
        }
    }

    pub(super) fn find_top_level_semicolon(text: &str) -> Option<usize> {
        let mut paren = 0usize;
        let mut bracket = 0usize;
        let mut brace = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (idx, ch) in text.char_indices() {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    continue;
                }
                if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' => paren += 1,
                ')' => paren = paren.saturating_sub(1),
                '[' => bracket += 1,
                ']' => bracket = bracket.saturating_sub(1),
                '{' => brace += 1,
                '}' => brace = brace.saturating_sub(1),
                ';' if paren == 0 && bracket == 0 && brace == 0 => return Some(idx),
                _ => {}
            }
        }
        None
    }

    pub(super) fn find_matching_brace_end(text: &str, open_idx: usize) -> Option<usize> {
        Self::matching_brace_end(text.char_indices().skip_while(|(i, _)| *i < open_idx))
    }

    /// [`Self::find_matching_brace_end`] for a caller that already holds the
    /// text as a `char` slice and counts in chars rather than bytes.
    pub(super) fn find_matching_brace_end_in_chars(
        chars: &[char],
        open_idx: usize,
    ) -> Option<usize> {
        Self::matching_brace_end(chars.iter().copied().enumerate().skip(open_idx))
    }

    /// The rule both spellings above share: the index of the `}` closing the
    /// `{` the iterator starts on, with braces inside `'…'` / `"…"` string
    /// literals ignored.
    ///
    /// The quote awareness is the point. A naive depth count reads the `}` in
    /// `{ say "a}b" }` as the end of the code block, slicing the body in the
    /// middle of a string literal — and the baking pass then rewrites what is
    /// left of it as if it were pattern text.
    fn matching_brace_end(it: impl Iterator<Item = (usize, char)>) -> Option<usize> {
        let mut depth = 0usize;
        let mut quote: Option<char> = None;
        let mut escaped = false;
        for (idx, ch) in it {
            if let Some(q) = quote {
                if escaped {
                    escaped = false;
                    continue;
                }
                if ch == '\\' {
                    escaped = true;
                    continue;
                }
                if ch == q {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '{' => depth += 1,
                '}' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return Some(idx);
                    }
                }
                _ => {}
            }
        }
        None
    }
}
