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

    /// ADR-0022 Slice 5: toggles `RegexToken::from_runtime_interpolation` in
    /// the tokenizer that runs immediately after this interpolation pass,
    /// within the same `parse_regex_uncached` call. Wrapping a substituted
    /// span in a pair of these (push once before, once after) marks every
    /// `RegexAtom::Literal` token the tokenizer builds from that span as
    /// non-declarative for LTM ranking. A reserved control character that
    /// can never appear in ordinary pattern source, mirroring
    /// `SILENT_ACTION_MARKER_PREFIX`'s convention — it never survives past
    /// the tokenizer, which strips every occurrence without emitting an
    /// atom for it, so it cannot leak into a matched literal or a
    /// displayed pattern string.
    pub(crate) const NON_DECLARATIVE_INTERP_MARK: char = '\u{1}';

    /// Was `name` (a bare, sigilless scalar name, e.g. `"x"` for `$x`)
    /// declared with `constant` and thus a value Rakudo inlines as a
    /// literal at compile time (ADR-0022 §2's "constants participate" —
    /// see the `__mutsu_constant_var::` marker written by
    /// `exec_set_local_op_inner`)? An ordinary `my`/`state`/param scalar
    /// answers `false` here even if it happens to never be reassigned:
    /// only a genuine `constant` is a Rakudo compile-time value.
    pub(in crate::runtime) fn is_compile_time_constant_scalar(&self, name: &str) -> bool {
        self.env
            .get(&format!("__mutsu_constant_var::{name}"))
            .is_some()
    }

    pub(super) fn interpolate_regex_scalars(&self, pattern: &str) -> Result<String, RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        // Scalar names introduced by an in-pattern `:my $v …` / `:let $v …`. A
        // later bare `$v` must NOT be pre-substituted from the outer `env` here —
        // its value is a *match-time* lexical (often set by a code block), so it
        // is left verbatim for the structural parser to lower to a `VarInterp`
        // atom (read from `caps.regex_vars` while matching). Tracked
        // left-to-right; a `:my` always precedes its uses.
        // A `:my` of an *enclosing* pattern counts too: this text may be one of
        // its sub-patterns, parsed by a nested call (see `ENCLOSING_REGEX_VARS`).
        let mut declared_my_vars: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        let is_regex_local = |name: &String, declared: &std::collections::HashSet<String>| {
            declared.contains(name) || super::regex::regex_helpers::is_enclosing_regex_var(name)
        };
        while i < chars.len() {
            let ch = chars[i];
            // # starts a comment — skip without interpolation.
            // #`[...] is an embedded comment; plain # is a line comment.
            if ch == '#' {
                let end = super::regex_parse::regex_comment_end(&chars, i)
                    .expect("comment marker was checked");
                out.extend(chars[i..end].iter());
                i = end;
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
            // Skip an embedded declaration `:my $x = …;` / `:our …;` / `:constant …;`
            // — its body is main-slang code, not a pattern, so a `%*var` / `@var`
            // in it (`:my %*PLAYED = ()`) must NOT be interpolated as a regex
            // variable reference. Copy through verbatim up to the `;`.
            if ch == ':' {
                let rest: String = chars[i + 1..].iter().collect();
                if rest.starts_with("my ")
                    || rest.starts_with("our ")
                    || rest.starts_with("constant ")
                    || rest.starts_with("let ")
                    || rest.starts_with("temp ")
                {
                    let decl_start = i;
                    while i < chars.len() {
                        let c = chars[i];
                        out.push(c);
                        i += 1;
                        if c == ';' {
                            break;
                        }
                    }
                    // Record the scalar names this declaration introduces so a
                    // later bare `$name` is preserved for match-time interpolation.
                    // `:my`/`:let` introduce a fresh regex-local lexical outright.
                    // `:our` looks like it refers to "existing storage" the same
                    // way `:temp`/`:constant` do, but it does not: its assigned
                    // value lives only in the match's `regex_vars` (written by the
                    // `VarDecl` atom at match time — see
                    // `regex_match_atom_with_capture_in_pkg`), never in `env`.
                    // Before ADR-0022 Slice 5 this went unnoticed because the
                    // LTM-measurement pass ran `:our`'s initializer for real (an
                    // ADR-0009 violation Slice 5 fixed), which happened to leave a
                    // real `env` entry behind for this fallback to find. Fixing
                    // that leak exposed this: without it, a later bare `$our`
                    // resolved against `env`, found nothing, and got replaced with
                    // the always-fails atom `<!>` (`roast/S05-modifier/my.t` test
                    // 12, `Grammar.parse` on `token TOP { :our $our = …; … $our }`).
                    // `:temp`/`:constant` keep the "existing storage" treatment —
                    // both genuinely write somewhere `env` can see (a real outer
                    // lexical for `:temp`, the `__mutsu_constant_var::` marker plus
                    // the constant's own value for `:constant`).
                    if rest.starts_with("my ")
                        || rest.starts_with("let ")
                        || rest.starts_with("our ")
                    {
                        let decl: String = chars[decl_start..i].iter().collect();
                        for name in super::regex_parse_core::scalar_names_in_decl(&decl) {
                            declared_my_vars.insert(name);
                        }
                    }
                    continue;
                }
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
                                .unwrap_or(Value::NIL);
                            let value = value.into_deref();
                            let entries: Vec<String> = match value.view() {
                                ValueView::Array(items, ..) => items
                                    .iter()
                                    .map(|v| {
                                        Self::escape_regex_scalar_literal(&v.to_string_value())
                                    })
                                    .collect(),
                                ValueView::Seq(items) => items
                                    .iter()
                                    .map(|v| {
                                        Self::escape_regex_scalar_literal(&v.to_string_value())
                                    })
                                    .collect(),
                                ValueView::Slip(items) => items
                                    .iter()
                                    .map(|v| {
                                        Self::escape_regex_scalar_literal(&v.to_string_value())
                                    })
                                    .collect(),
                                ValueView::Nil => Vec::new(),
                                _ => {
                                    vec![Self::escape_regex_scalar_literal(
                                        &value.to_string_value(),
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
                        // A `:my`-declared regex-local var: leave `${name}`
                        // verbatim for the parser's match-time `VarInterp` lowering.
                        if is_regex_local(&name, &declared_my_vars) {
                            out.extend(chars[i..=j].iter());
                            i = j + 1;
                            continue;
                        }
                        let value = self
                            .env
                            .get(&name)
                            .cloned()
                            .or_else(|| self.env.get(&format!("${name}")).cloned())
                            .unwrap_or(Value::NIL);
                        let value = value.into_deref();
                        Self::check_hash_in_regex(&value)?;
                        // A double-quoted regex literal (`"${name}..."`) is
                        // scanned by the structural parser's OWN inner loop
                        // (its `"..."` arm reads chars directly, bypassing
                        // the main token loop that consumes
                        // `NON_DECLARATIVE_INTERP_MARK`), so a mark placed
                        // inside it would leak through as a literal control
                        // character instead of being stripped. Skip marking
                        // there — such an interpolation stays declarative,
                        // same as before this slice.
                        // TODO: teach the double-quoted-literal tokenizer arm
                        // to also strip/honor the mark, so `$var` inside
                        // `"..."` gets the same non-constant treatment as
                        // everywhere else.
                        let inside_qq = is_inside_double_quoted_regex_literal(&chars, i);
                        let is_const = inside_qq || self.is_compile_time_constant_scalar(&name);
                        if !is_const {
                            out.push(Self::NON_DECLARATIVE_INTERP_MARK);
                        }
                        // A Regex value: reroute through the `<$var>` tokenizer arm
                        // (rather than splicing its pattern body here) so its
                        // captures get isolated the same way `<$var>` isolates
                        // them (see `RegexAtom::CaptureIsolatedGroup`). Not inside a
                        // double-quoted regex literal — that domain is re-read
                        // verbatim as literal text by the structural parser's own
                        // `"..."` scanner, which would turn `<$name>` into four+
                        // literal characters instead of an assertion.
                        if !inside_qq
                            && matches!(
                                value.view(),
                                ValueView::Regex(_) | ValueView::RegexWithAdverbs(_)
                            )
                        {
                            out.push_str(&format!("<${name}>"));
                        } else {
                            Self::push_value_as_regex_pattern(&value, &mut out);
                        }
                        if !is_const {
                            out.push(Self::NON_DECLARATIVE_INTERP_MARK);
                        }
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
                    // Inside a double-quoted regex literal, a `$var.method(...)`
                    // chain is a qq-string method-call interpolation (Raku
                    // interpolates `"$x.uc()"`), not a scalar followed by a
                    // match-any `.`. A bare `/ $x.foo() /` is a Raku syntax
                    // error, so this only applies within `"..."`. Evaluate the
                    // whole chain and match its result literally.
                    if let Some(chain_end) = scan_interp_method_chain(&chars, j)
                        && is_inside_double_quoted_regex_literal(&chars, i)
                    {
                        let expr_str: String = chars[i..chain_end].iter().collect();
                        let val = self.eval_string_as_source(&expr_str);
                        out.push_str(&Self::escape_regex_scalar_literal(&val.to_string_value()));
                        i = chain_end;
                        continue;
                    }
                    let name: String = chars[name_start..j].iter().collect();
                    // A `:my`-declared regex-local var: leave `$name` verbatim so
                    // the structural parser lowers it to a match-time `VarInterp`
                    // atom instead of pre-substituting an outer-scope value here.
                    if is_regex_local(&name, &declared_my_vars) {
                        out.extend(chars[i..j].iter());
                        i = j;
                        continue;
                    }
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
                    // A `$*` dynamic var resolved from the overlay is always
                    // runtime-only, regardless of any stale `constant`
                    // marker of the same bare name.
                    let is_overlay = overlay_value.is_some();
                    let value = overlay_value
                        .or_else(|| self.env.get(&name).cloned())
                        .or_else(|| self.env.get(&format!("${name}")).cloned())
                        .unwrap_or(Value::NIL);
                    let value = value.into_deref();
                    Self::check_hash_in_regex(&value)?;
                    // See the `${name}` arm above: a double-quoted regex
                    // literal is scanned by the structural parser's own
                    // inner loop, which does not strip
                    // `NON_DECLARATIVE_INTERP_MARK`, so skip marking there.
                    let inside_qq = is_inside_double_quoted_regex_literal(&chars, i);
                    let is_const =
                        !is_overlay && (inside_qq || self.is_compile_time_constant_scalar(&name));
                    if !is_const {
                        out.push(Self::NON_DECLARATIVE_INTERP_MARK);
                    }
                    // A Regex value: reroute through the `<$var>` tokenizer arm so
                    // its captures get isolated (see the `${name}` arm above and
                    // `RegexAtom::CaptureIsolatedGroup`'s doc comment). Skipped inside a
                    // double-quoted regex literal (same reason as the `${name}`
                    // arm) and for an overlay-resolved `$*` dyn-var, whose value
                    // lives in `REGEX_DYNVAR_OVERLAY` — a store the `<$var>`
                    // tokenizer arm cannot see, only `self.env`.
                    if !inside_qq
                        && !is_overlay
                        && matches!(
                            value.view(),
                            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_)
                        )
                    {
                        out.push_str(&format!("<${name}>"));
                    } else {
                        Self::push_value_as_regex_pattern(&value, &mut out);
                    }
                    if !is_const {
                        out.push(Self::NON_DECLARATIVE_INTERP_MARK);
                    }
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
                            .unwrap_or(Value::NIL);
                        let value = value.into_deref();
                        let elements = match value.view() {
                            ValueView::Array(arr, _) => arr.as_ref().clone(),
                            ValueView::Seq(items) => crate::value::ArrayData::new(items.to_vec()),
                            ValueView::Slip(items) => {
                                crate::value::ArrayData::new((**items).clone())
                            }
                            _ => crate::value::ArrayData::new(vec![value]),
                        };
                        // TODO: unlike bare `@name` below, a Regex-valued
                        // element here (`@$var` dereferences a scalar, not a
                        // named array variable) has no `<@name>` tokenizer
                        // form to reroute through, so its captures still leak
                        // into the outer match (same leak as bug 2's `<@var>`
                        // case, just not yet closed for this dereferenced
                        // form). Splicing the raw pattern text is unchanged
                        // from before this fix.
                        let mut alts = Vec::new();
                        for elt in &elements {
                            match elt.view() {
                                ValueView::Regex(pat) => alts.push(pat.to_string()),
                                ValueView::RegexWithAdverbs(a) => alts.push(a.pattern.to_string()),
                                _ => alts.push(Self::escape_regex_scalar_literal(
                                    &elt.to_string_value(),
                                )),
                            }
                        }
                        // ADR-0046 Slice 1 / ADR §2.1 probe R: `@$var` array
                        // deref interpolation terminates the declarative LTM
                        // prefix unconditionally, same as bare `@name` below --
                        // no `constant` exemption exists for the `@` sigil
                        // (probe J), unlike the `$`-scalar case.
                        Self::push_regex_interpolated_alternation_marked(&mut out, &alts);
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
                        .unwrap_or(Value::NIL);
                    // Slice 2a: a `=`-array-shared source (`my $r = @var`) promotes
                    // `@var` to a `ContainerRef` cell; deref it so the array
                    // interpolates as alternation instead of stringifying the cell.
                    let value = value.into_deref();
                    let elements = match value.view() {
                        ValueView::Array(arr, _) => arr.as_ref().clone(),
                        ValueView::Seq(items) => crate::value::ArrayData::new(items.to_vec()),
                        ValueView::Slip(items) => crate::value::ArrayData::new((**items).clone()),
                        _ => crate::value::ArrayData::new(vec![value]),
                    };
                    // A Regex-valued element: reroute the whole alternation
                    // through the `<@var>` tokenizer arm (which strips each
                    // element's captures — see `array_var_alternation_atom`)
                    // instead of splicing pattern text directly, same
                    // reasoning as the bare-`$var` arms above. String-only
                    // arrays keep the existing text-splice path unchanged.
                    if elements.iter().any(|elt| {
                        matches!(
                            elt.view(),
                            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_)
                        )
                    }) {
                        out.push_str(&format!("<{sigiled_name}>"));
                        i = j;
                        continue;
                    }
                    let mut alts = Vec::new();
                    for elt in &elements {
                        match elt.view() {
                            ValueView::Regex(pat) => alts.push(pat.to_string()),
                            ValueView::RegexWithAdverbs(a) => alts.push(a.pattern.to_string()),
                            _ => {
                                alts.push(Self::escape_regex_scalar_literal(&elt.to_string_value()))
                            }
                        }
                    }
                    // ADR-0046 Slice 1 / ADR §2.1 probe I/J: bare `@name`
                    // interpolation terminates the declarative LTM prefix
                    // unconditionally -- unlike the `$`-scalar case, there is
                    // no `constant` exemption for `@` (probe J: `constant
                    // @copts` terminates exactly like `my @opts`).
                    Self::push_regex_interpolated_alternation_marked(&mut out, &alts);
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
                    let elements = match val.view() {
                        ValueView::Array(arr, _) => arr.as_ref().clone(),
                        ValueView::Seq(items) => crate::value::ArrayData::new(items.to_vec()),
                        ValueView::Slip(items) => crate::value::ArrayData::new((**items).clone()),
                        _ => crate::value::ArrayData::new(vec![val]),
                    };
                    let mut alts = Vec::new();
                    for elt in elements.iter() {
                        match elt.view() {
                            ValueView::Regex(pat) => alts.push(pat.to_string()),
                            ValueView::RegexWithAdverbs(a) => alts.push(a.pattern.to_string()),
                            _ => {
                                alts.push(Self::escape_regex_scalar_literal(&elt.to_string_value()))
                            }
                        }
                    }
                    // ADR-0046 Slice 1 / ADR §2.1 probe Q: `@(...)`
                    // contextualizer interpolation terminates the declarative
                    // LTM prefix unconditionally, same as bare `@name` above.
                    Self::push_regex_interpolated_alternation_marked(&mut out, &alts);
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
        match value.view() {
            ValueView::Nil => out.push_str("<!>"),
            ValueView::Regex(pat) => out.push_str(&pat),
            ValueView::RegexWithAdverbs(a) => out.push_str(&a.pattern),
            ValueView::Junction { values, .. } => {
                // Expand junction values as alternation [v1|v2|...]
                // TODO: same capture leak as bug 2 of
                // `todo/tickets/stored-regex-loses-its-defining-scope-lexicals.md`
                // (out of scope for that fix): a Regex-valued junction member's
                // pattern text is spliced verbatim here, so `any(rx/(a)/, ...)`
                // interpolated into an outer regex still leaks its positional and
                // named captures into the outer match's numbering. Fixing this
                // would need the same `<$var>`-style reroute (there is no
                // `<$var>`-junction tokenizer form to reroute through yet).
                out.push('[');
                for (idx, v) in values.iter().enumerate() {
                    if idx > 0 {
                        out.push('|');
                    }
                    match v.view() {
                        ValueView::Regex(pat) => out.push_str(&pat),
                        ValueView::RegexWithAdverbs(a) => out.push_str(&a.pattern),
                        _ => out.push_str(&Self::escape_regex_scalar_literal(&v.to_string_value())),
                    }
                }
                out.push(']');
            }
            _ => out.push_str(&Self::escape_regex_scalar_literal(&value.to_string_value())),
        }
    }

    /// Check if a value is a Hash and throw X::Syntax::Reserved if so.
    fn check_hash_in_regex(value: &Value) -> Result<(), RuntimeError> {
        if matches!(value.view(), ValueView::Hash(_)) {
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
            // Whitespace cannot be backslash-escaped in regex source: `\ ` is the
            // unspace form, which raku rejects ("No unspace allowed in regex").
            // Emit the codepoint form instead so an interpolated " ", "\t" or
            // "\r\n" still matches literally.
            if ch.is_whitespace() {
                out.push_str(&format!("\\x[{:02X}]", ch as u32));
                continue;
            }
            // Escape EVERY non-identifier char, not an enumerated metachar
            // list: an interpolated scalar matches *literally* (raku does not
            // re-parse it as regex source), and the enumerated approach leaked
            // whichever metachar it forgot — `~` (the goal-match marker)
            // survived to the structural parser as a bare `TildeMarker` atom
            // and panicked the matcher (Text::CSV 55_combi with `~` as a
            // quote/sep/escape char). A backslash before any non-alphanumeric
            // char is always a literal in regex slang, so blanket-escaping is
            // safe; alphanumerics and `_` must stay bare (escaping them would
            // CREATE class shorthands like `\d`/`\w`).
            if !ch.is_alphanumeric() && ch != '_' {
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
        // Check for double-quoted strings with interpolation. A naive
        // split-on-`"` parity count misfires on a `"` that is itself just a
        // literal character inside a single-quoted literal (`'boundary="'`)
        // or a character class (`<-["]>` — matching anything but `"`), both
        // legal regex source with no double-quote region at all: verified
        // against `raku` directly, `'a' ~~ /<$p>/` for
        // `$p = Q/'boundary="' $<b>=[<-["]>+] '"'/` matches (see
        // `t/regex-interp-capture-alias.t`). Track single-quote and `[...]`
        // char-class state so a `"` inside either is never treated as a
        // double-quote delimiter.
        if s.contains('"') {
            let mut in_squote = false;
            let mut in_dquote = false;
            let mut class_depth: u32 = 0;
            let mut dq_chunk = String::new();
            for c in s.chars() {
                match c {
                    '[' if !in_squote && !in_dquote => class_depth += 1,
                    ']' if !in_squote && !in_dquote && class_depth > 0 => class_depth -= 1,
                    '\'' if !in_dquote && class_depth == 0 => in_squote = !in_squote,
                    '"' if !in_squote && class_depth == 0 => {
                        if in_dquote
                            && (dq_chunk.contains('$')
                                || dq_chunk.contains('@')
                                || dq_chunk.contains('%')
                                || dq_chunk.contains('&'))
                        {
                            return true;
                        }
                        in_dquote = !in_dquote;
                        dq_chunk.clear();
                    }
                    _ if in_dquote => dq_chunk.push(c),
                    _ => {}
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
    /// (e.g., `<IO::File=bar>` or `<::IO::File=bar>`): a `::` in the alias
    /// position, i.e. before the first `=` that is not a `=>` fat arrow.
    /// A long name on the RHS (`<dt=Foo::Bar::rule>`) is a legal aliased
    /// call to a fully-qualified subrule and must NOT be flagged.
    pub(super) fn contains_longname_alias(pattern: &str) -> bool {
        let s = pattern.trim();
        let bytes = s.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'=' {
                if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    i += 2;
                    continue;
                }
                return s[..i].contains("::");
            }
            i += 1;
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

/// Starting at `start` (just past a scanned `$var` name), scan a chain of
/// `.method(...)` calls (`.flip()`, `.flip().uc()`, `.substr(0,3)`), respecting
/// nested parens in the argument lists. Returns the end index of the chain when
/// it contains at least one parenthesized method call, else `None` (a bare
/// `.method` without parens does not interpolate). Used to interpolate a
/// `$var.method(...)` term inside a double-quoted regex literal.
fn scan_interp_method_chain(chars: &[char], start: usize) -> Option<usize> {
    let mut i = start;
    let mut saw_call = false;
    while i < chars.len() && chars[i] == '.' {
        let mut k = i + 1;
        let id_start = k;
        while k < chars.len() && (chars[k].is_alphanumeric() || chars[k] == '_' || chars[k] == '-')
        {
            k += 1;
        }
        if k == id_start || k >= chars.len() || chars[k] != '(' {
            break;
        }
        let mut depth = 0usize;
        while k < chars.len() {
            match chars[k] {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        k += 1;
                        break;
                    }
                }
                _ => {}
            }
            k += 1;
        }
        if depth != 0 {
            break;
        }
        i = k;
        saw_call = true;
    }
    saw_call.then_some(i)
}
