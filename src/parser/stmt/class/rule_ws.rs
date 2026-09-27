//! `rule` sigspace: the text pass that turns significant whitespace in a
//! `rule` body into explicit `<.ws>` calls before the regex engine sees it.

/// Insert `<.ws>` at a byte offset in `out`, adding a separating space on
/// either side only where one is not already present. Returns the number of
/// bytes inserted, so a caller applying this at several offsets left-to-right
/// can keep the later ones in sync (see [`propagate_ws_to_alt_branches`]).
fn insert_ws_marker(out: &mut String, pos: usize) -> usize {
    let mut marker = String::new();
    if !out[..pos].ends_with(' ') {
        marker.push(' ');
    }
    marker.push_str("<.ws>");
    if !out[pos..].starts_with(' ') {
        marker.push(' ');
    }
    out.insert_str(pos, &marker);
    marker.len()
}

/// Apply [`insert_ws_marker`] at each recorded top-level `|`/`||` branch
/// boundary inside a bracketed alternation, so a trailing `<.ws>` discovered
/// right before the group's closing `]`/`)` applies to every branch, not only
/// the one that happens to sit textually next to the close.
fn propagate_ws_to_alt_branches(out: &mut String, positions: &[usize]) {
    let mut shift = 0usize;
    for &pos in positions {
        shift += insert_ws_marker(out, pos + shift);
    }
}

/// An inline sigspace adverb right after a `:` — `s`, `sigspace`, `!s` or
/// `!sigspace`, ending at a non-identifier character. Returns how many chars
/// the adverb spans (not counting the `:`) and whether it turns sigspace on.
fn inline_sigspace_modifier(rest: &[char]) -> Option<(usize, bool)> {
    let (negated, body) = match rest.first() {
        Some('!') => (true, &rest[1..]),
        _ => (false, rest),
    };
    for name in ["sigspace", "s"] {
        let len = name.chars().count();
        if body.len() >= len
            && body[..len].iter().copied().eq(name.chars())
            && body
                .get(len)
                .is_none_or(|ch| !ch.is_alphanumeric() && *ch != '_' && *ch != '-')
        {
            return Some((len + usize::from(negated), !negated));
        }
    }
    None
}

pub(crate) fn inject_implicit_rule_ws(pattern: &str) -> String {
    /// True when the last character already pushed to `out` arrived via a
    /// backslash escape (`\{`, `\(`, `\|`, ...) rather than as bare regex
    /// syntax. `should_insert`'s suppression rules key off raw characters
    /// like `{`/`(`/`|` to recognize code-block and group syntax, but an
    /// ESCAPED occurrence of one of those characters is an ordinary literal
    /// atom, not the syntax it resembles — treating it the same way silently
    /// drops the `<.ws>` that should follow it (mutsu#8700: `'frame' \{ 'x'`
    /// never matched the space before `'x'` because the escaped `\{` was
    /// read as a code-block opener).
    fn last_char_is_escaped(out: &str) -> bool {
        let trimmed_end = out.trim_end_matches(char::is_whitespace);
        let mut chars = trimmed_end.chars().rev();
        if chars.next().is_none() {
            return false;
        }
        let mut backslash_count = 0usize;
        for c in chars {
            if c == '\\' {
                backslash_count += 1;
            } else {
                break;
            }
        }
        backslash_count % 2 == 1
    }

    fn should_insert(prev: char, next: char) -> bool {
        // Whitespace AFTER a term but BEFORE a closing `]`/`)` IS significant in
        // Raku sigspace (`rule r { [ <w> ]**3 }` matches "a b c", not "abc" — the
        // space before `]` becomes a required `<.ws>`). Whitespace right after an
        // opening `[`/`(` is NOT significant, so those stay suppressed.
        // Whitespace BEFORE a `|`/`||` is significant too (rakudo:
        // `rule e { a | b }` requires whitespace after `a`, `rule e { a| b }`
        // does not), so only the whitespace AFTER a pipe is suppressed.
        !matches!(
            (prev, next),
            ('|', _)
                | ('(', _)
                | ('[', _)
                | ('{', _)
                | (_, '}')
                | ('^', _)
                | (_, '$')
                | ('<', _)
                | (_, '>')
                | (_, '*')
                | (_, '+')
                | (_, '?')
                | (_, '%')
                | ('%', _)
        )
    }

    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::new();
    let mut i = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;
    let mut brace_depth = 0usize;
    // Depth of currently-open `<...>` regex-syntax regions (subrule calls,
    // `<[...]>` bracketed char classes, named captures, lookarounds, ...)
    // that are NOT code assertions (`<{ … }>` / `<?{ … }>` / `<!{ … }>`).
    // While inside one, a `{`/`}` character is just a literal atom (e.g. the
    // brace matched by `<[{]>`), not a code-block delimiter, so it must not
    // touch `brace_depth` — otherwise it desyncs `brace_depth` and silently
    // suppresses `<.ws>` insertion for the rest of the pattern (mutsu#8755).
    // Code assertions are deliberately left alone here: their own `{`/`}`
    // already balance correctly via the ordinary `brace_depth` tracking
    // below, and unlike a bracketed char class they never contain a lone,
    // unmatched brace.
    let mut angle_depth = 0usize;
    // One entry per currently-open `[`/`(` group, tracking where each of its
    // top-level `|`/`||` branches ends. A closing bracket with significant
    // trailing whitespace (see `should_insert`) uses the innermost of these
    // to give every alternative the same trailing `<.ws>` that the
    // textually-last one gets for free — see the call site below.
    struct BracketCtx {
        /// Byte offsets (into `out`) right before each `|`/`||` operator that
        /// closed a NON-EMPTY branch — see `branch_start`.
        pipe_positions: Vec<usize>,
        /// Byte offset (into `out`) where the branch currently being scanned
        /// started: right after this group's opening bracket, or right after
        /// the last `|`/`||` operator. A `|` reached while everything from
        /// `branch_start` onward is still whitespace is a purely stylistic
        /// LEADING pipe (`[ | A | B ]`, used for vertical alignment) with no
        /// branch content before it — recording a boundary there would turn
        /// the propagated `<.ws>` into a bogus zero-width first alternative
        /// ahead of `A` itself.
        branch_start: usize,
        /// The sigspace state in force when this group opened, restored when
        /// it closes: an inline `:!s` / `:s` lasts to the end of the
        /// enclosing group, exactly like the regex engine's own adverb scope.
        outer_sigspace: bool,
    }
    let mut bracket_stack: Vec<BracketCtx> = Vec::new();
    // Whether whitespace is currently significant. A `rule` starts with
    // sigspace on; an inline `:!s` / `:!sigspace` turns it off (and `:s` /
    // `:sigspace` back on) for the rest of the enclosing `[...]`/`(...)` or
    // of the whole body (raku: `rule r { ( :!s 'a' 'b' ) 'c' }` matches
    // "ab c" and captures "ab", not "a b").
    let mut sigspace = true;
    while i < chars.len() {
        let c = chars[i];
        if escaped {
            out.push(c);
            escaped = false;
            i += 1;
            continue;
        }
        if c == '\\' {
            out.push(c);
            escaped = true;
            i += 1;
            continue;
        }
        if c == '\'' && !in_double {
            in_single = !in_single;
            out.push(c);
            i += 1;
            continue;
        }
        if c == '"' && !in_single {
            in_double = !in_double;
            out.push(c);
            i += 1;
            continue;
        }
        // A line comment is regex syntax, not pattern layout.  Preserve its
        // text and terminating newline while adding rule whitespace: dropping
        // the newline makes the comment consume every following atom in the
        // generated pattern (for example `rule ruleset { <!after '@'> # ...
        // <selectors> ... }`).  Embedded `#` backtick comments end at their
        // matching delimiter and may be followed by more pattern text.
        if c == '#' && !in_single && !in_double && brace_depth == 0 && angle_depth == 0 {
            out.push(c);
            i += 1;
            if chars.get(i) == Some(&'`') {
                out.push('`');
                i += 1;
                if let Some(&open) = chars.get(i)
                    && let Some(close) = crate::parser::helpers::matching_bracket(open)
                {
                    let mut depth = 1usize;
                    out.push(open);
                    i += 1;
                    while i < chars.len() {
                        let ch = chars[i];
                        out.push(ch);
                        i += 1;
                        if ch == open {
                            depth += 1;
                        } else if ch == close {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                    }
                }
            } else {
                while i < chars.len() {
                    let ch = chars[i];
                    out.push(ch);
                    i += 1;
                    if ch == '\n' {
                        break;
                    }
                }
            }
            continue;
        }
        // Track brace depth to skip ws injection inside code blocks { ... }
        if !in_single && !in_double {
            // Track `<...>` region depth so a literal `{`/`}` inside one
            // (e.g. `<[{]>`) is not mistaken for a code-block delimiter. A
            // `<` that opens a code assertion (`<{`, `<?{`, `<!{`) is left
            // out of this tracking — its own braces already balance via the
            // ordinary `brace_depth` counter below.
            if c == '<' && brace_depth == 0 {
                let mut probe = i + 1;
                if matches!(chars.get(probe), Some('?') | Some('!')) {
                    probe += 1;
                }
                if chars.get(probe) != Some(&'{') {
                    angle_depth += 1;
                }
            } else if c == '>' && angle_depth > 0 && brace_depth == 0 {
                angle_depth -= 1;
            }
            if c == '{' && angle_depth == 0 {
                brace_depth += 1;
                out.push(c);
                i += 1;
                continue;
            }
            if c == '}' && brace_depth > 0 && angle_depth == 0 {
                brace_depth -= 1;
                out.push(c);
                i += 1;
                continue;
            }
            // An embedded declaration `:my … ;` / `:our …` / `:constant …` is
            // main-slang code, not pattern text — copy it through verbatim so a
            // `%*var` in it (`:my %*PLAYED = ()`) is not mangled by `<.ws>`
            // injection (which would corrupt both the match and later scans of
            // the pattern for its declared dynamic variables).
            if c == ':'
                && brace_depth == 0
                && angle_depth == 0
                && let Some((len, on)) = inline_sigspace_modifier(&chars[i + 1..])
            {
                out.extend(chars[i..=i + len].iter());
                i += len + 1;
                sigspace = on;
                // The whitespace right after the adverb separates it from
                // the atom it scopes over; it is never significant.
                while i < chars.len() && chars[i].is_whitespace() {
                    i += 1;
                }
                out.push(' ');
                continue;
            }
            if c == ':' && brace_depth == 0 {
                let rest: String = chars[i + 1..].iter().collect();
                if rest.starts_with("my ")
                    || rest.starts_with("our ")
                    || rest.starts_with("constant ")
                    || rest.starts_with("let ")
                    || rest.starts_with("temp ")
                {
                    while i < chars.len() {
                        let ch = chars[i];
                        out.push(ch);
                        i += 1;
                        if ch == ';' {
                            break;
                        }
                    }
                    continue;
                }
            }
        }
        // Inside a code block — pass through without ws injection
        if brace_depth > 0 {
            out.push(c);
            i += 1;
            continue;
        }
        if !in_single && !in_double {
            if matches!(c, '[' | '(') {
                out.push(c);
                i += 1;
                bracket_stack.push(BracketCtx {
                    pipe_positions: Vec::new(),
                    branch_start: out.len(),
                    outer_sigspace: sigspace,
                });
                continue;
            }
            if matches!(c, ']' | ')') {
                out.push(c);
                i += 1;
                if let Some(ctx) = bracket_stack.pop() {
                    sigspace = ctx.outer_sigspace;
                }
                continue;
            }
            // Record the branch boundary once per `|`/`||` operator — the
            // second pipe of a `||` sits right after the one just pushed, so
            // it is skipped rather than recording the same boundary twice.
            // Nothing is recorded for a purely stylistic LEADING pipe (no
            // branch content between the group's open / the previous pipe
            // and this one) — see `BracketCtx::branch_start`.
            if c == '|' {
                let is_second_of_double = out.ends_with('|');
                if !is_second_of_double
                    && let Some(ctx) = bracket_stack.last_mut()
                    && !out[ctx.branch_start..].trim().is_empty()
                {
                    ctx.pipe_positions.push(out.len());
                }
                out.push(c);
                i += 1;
                if let Some(ctx) = bracket_stack.last_mut() {
                    ctx.branch_start = out.len();
                }
                continue;
            }
        }
        if !in_single && !in_double && angle_depth > 0 && c.is_whitespace() {
            // Whitespace inside an angle assertion is regex syntax layout
            // (`<?before '}'>'`, `<foo $arg>`, ...), not sigspace between
            // matching atoms. Preserve it verbatim; inserting `<.ws>` here
            // changes the assertion's lookup text.
            let mut j = i;
            while j < chars.len() && chars[j].is_whitespace() {
                j += 1;
            }
            out.extend(chars[i..j].iter());
            i = j;
            continue;
        }
        if !in_single && !in_double && c.is_whitespace() {
            let mut j = i;
            while j < chars.len() && chars[j].is_whitespace() {
                j += 1;
            }
            if !sigspace {
                if !out.ends_with(' ') && !out.is_empty() {
                    out.push(' ');
                }
                i = j;
                continue;
            }
            let prev = out.chars().rev().find(|ch| !ch.is_whitespace());
            // `should_insert`'s prev-position rules recognize `|`, `(`, `[`,
            // `{`, `^`, `<`, `%` as syntax that suppresses the following
            // `<.ws>`. When the character we just read from `out` was
            // written as an escape (`\{` and friends), it is a literal atom
            // instead, so neutralize it before `should_insert` sees it. The
            // next-position rules never need this: an escaped upcoming
            // character always shows its backslash first in `chars[j..]`,
            // which matches none of `should_insert`'s next-position arms.
            let prev = prev.map(|p| {
                if matches!(p, '|' | '(' | '[' | '{' | '^' | '<' | '%')
                    && last_char_is_escaped(&out)
                {
                    'x'
                } else {
                    p
                }
            });
            let next = chars[j..].iter().copied().find(|ch| !ch.is_whitespace());
            if let Some(p) = prev {
                // A `$` that begins a capture alias / variable (`$<name>=…`,
                // `$0=…`, `$var`, `${…}`, or a twigilled `$*dyn` / `$?COMPILE`
                // / `$^placeholder` / `$.accessor`) is a term, not the
                // end-of-string anchor, so whitespace before it IS significant
                // and must become `<.ws>`. Normalize such a `$` to a plain term char so
                // `should_insert`'s `(_, '$')` anchor suppression does not fire
                // (while its prev-based rules — after `(`/`[`/`|` — still do).
                // A trailing whitespace run with no following atom (right
                // before the closing `}`) maps to a sentinel that matches
                // none of `should_insert`'s `(_, X)` arms, so only the
                // prev-based restrictions apply — `rule r { 'a' 'b' }` must
                // consume trailing input whitespace exactly like the `<.ws>`
                // between 'a' and 'b' does.
                let n = match next {
                    Some(n)
                        if n == '$'
                            && chars.get(j + 1).is_some_and(|c| {
                                c.is_alphanumeric()
                                    || matches!(c, '_' | '<' | '{' | '*' | '?' | '^' | '.')
                            }) =>
                    {
                        'x'
                    }
                    Some(n) => n,
                    None => '\0',
                };
                if p == '^' {
                    if !out.ends_with(' ') && !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str("<.ws>?");
                    out.push(' ');
                } else if should_insert(p, n) {
                    if !out.ends_with(' ') && !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str("<.ws>");
                    if next.is_some() {
                        out.push(' ');
                    }
                    // The `<.ws>` just inserted sits textually inside
                    // whichever alternative happens to be last before this
                    // closing `]`/`)`. When the group's content is a
                    // top-level `|`/`||` alternation, a quantifier on the
                    // group (`[ <.comment> || <line> ]+`) needs the same
                    // trailing whitespace allowance after EVERY branch — not
                    // just the one next to the close — or the `+` loop has no
                    // way to skip the separator before its next iteration
                    // whenever an earlier branch is the one that matched
                    // (mutsu#8561).
                    if matches!(next, Some(']') | Some(')'))
                        && let Some(ctx) = bracket_stack.last()
                        && !ctx.pipe_positions.is_empty()
                    {
                        propagate_ws_to_alt_branches(&mut out, &ctx.pipe_positions);
                    }
                } else if !out.ends_with(' ') && !out.is_empty() {
                    out.push(' ');
                }
            }
            i = j;
            continue;
        }
        out.push(c);
        i += 1;
    }
    out.trim().to_string()
}
