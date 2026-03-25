use super::super::*;

impl Interpreter {
    /// Transform P5 regex patterns to handle Perl 5 semantics that
    /// fancy-regex doesn't support directly:
    /// - `\Z` -> `(?=\n?\z)` (match before optional final newline)
    /// - `$` without `(?m)` -> `(?=\n?\z)` (P5 $ matches before optional trailing newline)
    /// - Fix `[[` inside character classes for compatibility
    /// - `(?(?=X)yes|no)` -> `(?:(?=X)yes|(?!X)no)` (conditional with lookahead)
    /// - `(?(?!X)yes|no)` -> `(?:(?!X)yes|(?=X)no)` (conditional with neg lookahead)
    /// - `(?(?{0})yes|no)` -> `(?:no)` (code eval returning false)
    /// - `(?(?{1})yes|no)` -> `(?:yes)` (code eval returning true)
    /// - `(?(N)yes|no)` with undefined group N -> `(?:no)`
    pub(super) fn transform_p5_pattern(pattern: &str) -> String {
        // First pass: count defined capture groups
        let num_groups = Self::count_capture_groups(pattern);
        // Second pass: transform
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0;
        let mut in_char_class = false;
        let mut multiline_active = false;
        while i < chars.len() {
            if !in_char_class && chars[i] == '\\' && i + 1 < chars.len() {
                if chars[i + 1] == 'Z' {
                    out.push_str("(?=\\n?\\z)");
                    i += 2;
                    continue;
                }
                out.push(chars[i]);
                out.push(chars[i + 1]);
                i += 2;
                continue;
            }
            if chars[i] == '[' && !in_char_class {
                in_char_class = true;
                out.push(chars[i]);
                i += 1;
                if i < chars.len() && chars[i] == '^' {
                    out.push(chars[i]);
                    i += 1;
                }
                if i < chars.len() && chars[i] == ']' {
                    out.push('\\');
                    out.push(']');
                    i += 1;
                }
                continue;
            }
            if in_char_class && chars[i] == '[' {
                out.push('\\');
                out.push('[');
                i += 1;
                continue;
            }
            if in_char_class && chars[i] == ']' {
                in_char_class = false;
                out.push(chars[i]);
                i += 1;
                continue;
            }
            // Handle Perl postponed regex (??{EXPR}): evaluate EXPR and use
            // the result as a sub-pattern.  We can't run arbitrary code, but
            // we handle the common case where EXPR is a string literal
            // (e.g. (??{"(?!)"})) by inlining the string content.  For empty
            // or non-literal EXPR we fall back to a no-op (?:).
            if !in_char_class
                && chars[i] == '('
                && i + 3 < chars.len()
                && chars[i + 1] == '?'
                && chars[i + 2] == '?'
                && chars[i + 3] == '{'
                && let Some(brace_end) = Self::find_matching_brace(&chars, i + 3)
            {
                let after = brace_end + 1;
                if after < chars.len() && chars[after] == ')' {
                    let code_body: String = chars[i + 4..brace_end].iter().collect();
                    let trimmed = code_body.trim();
                    // Extract string literal content: "..." or '...'
                    let inlined = if (trimmed.starts_with('"') && trimmed.ends_with('"'))
                        || (trimmed.starts_with('\'') && trimmed.ends_with('\''))
                    {
                        // Strip quotes, use content as regex
                        trimmed[1..trimmed.len() - 1].to_string()
                    } else if trimmed.is_empty() {
                        // Empty code block -- no-op
                        String::new()
                    } else {
                        // Unknown code -- fall back to no-op
                        String::new()
                    };
                    if inlined.is_empty() {
                        out.push_str("(?:)");
                    } else {
                        // Wrap in non-capturing group so it composes safely
                        out.push_str("(?:");
                        out.push_str(&inlined);
                        out.push(')');
                    }
                    i = after + 1;
                    continue;
                }
            }
            // Handle Perl code assertions (?{...}) as zero-width no-op.
            if !in_char_class
                && chars[i] == '('
                && i + 2 < chars.len()
                && chars[i + 1] == '?'
                && chars[i + 2] == '{'
                && let Some(end) = Self::find_matching_brace(&chars, i + 2)
            {
                let after = end + 1;
                if after < chars.len() && chars[after] == ')' {
                    out.push_str("(?:)");
                    i = after + 1;
                    continue;
                }
            }
            // Handle P5 conditional patterns: (?(COND)yes|no)
            if !in_char_class && chars[i] == '(' && i + 1 < chars.len() && chars[i + 1] == '?' {
                let rest: String = chars[i + 2..].iter().collect();
                // Track (?m) flag
                if rest.starts_with('m') {
                    multiline_active = true;
                }
                // Conditional with lookahead/lookbehind: (?(?=X)yes|no)
                if (rest.starts_with("(?=") || rest.starts_with("(?!"))
                    && let Some(transformed) =
                        Self::transform_conditional_lookahead(&chars, i, false)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
                if (rest.starts_with("(?<=") || rest.starts_with("(?<!"))
                    && let Some(transformed) =
                        Self::transform_conditional_lookahead(&chars, i, true)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
                // Conditional with code: (?(?{0})yes|no) or (?(?{1})yes|no)
                if rest.starts_with("(?{")
                    && let Some(transformed) = Self::transform_conditional_code(&chars, i)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
                // Conditional with backreference: (?(N)yes|no)
                if rest.starts_with('(')
                    && let Some(transformed) =
                        Self::transform_conditional_backref(&chars, i, num_groups)
                {
                    out.push_str(&transformed.0);
                    i = transformed.1;
                    continue;
                }
            }
            if !in_char_class && chars[i] == '$' && !multiline_active {
                let next = chars.get(i + 1);
                let is_end_anchor = next.is_none()
                    || *next.unwrap() == ')'
                    || *next.unwrap() == '|'
                    || *next.unwrap() == '/';
                if is_end_anchor {
                    out.push_str("(?=\\n?\\z)");
                    i += 1;
                    continue;
                }
            }
            out.push(chars[i]);
            i += 1;
        }
        out
    }

    /// Count the number of capture groups in a P5 regex pattern.
    pub(super) fn count_capture_groups(pattern: &str) -> usize {
        let chars: Vec<char> = pattern.chars().collect();
        let mut count = 0;
        let mut i = 0;
        let mut in_class = false;
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
                continue;
            }
            if chars[i] == '[' && !in_class {
                in_class = true;
                i += 1;
                continue;
            }
            if chars[i] == ']' && in_class {
                in_class = false;
                i += 1;
                continue;
            }
            if !in_class && chars[i] == '(' {
                if i + 1 < chars.len() && chars[i + 1] == '?' {
                    // Check for conditional: (?(N)...) or (?(...) ...)
                    // The inner (N) or (...) is NOT a capture group
                    if i + 2 < chars.len() && chars[i + 2] == '(' {
                        // Skip the condition part: (?(...)
                        i += 3; // skip (?(
                        let mut depth = 1;
                        while i < chars.len() && depth > 0 {
                            if chars[i] == '(' {
                                depth += 1;
                            } else if chars[i] == ')' {
                                depth -= 1;
                            }
                            i += 1;
                        }
                        continue;
                    }
                    // Non-capturing or special group -- don't count
                } else {
                    count += 1;
                }
            }
            i += 1;
        }
        count
    }

    /// Transform `(?(?=X)yes|no)` or `(?(?!X)yes|no)` into
    /// `(?:(?=X)yes|(?!X)no)` or `(?:(?!X)yes|(?=X)no)`.
    /// Returns (replacement_string, new_index_after_the_conditional).
    pub(super) fn transform_conditional_lookahead(
        chars: &[char],
        start: usize,
        _is_lookbehind: bool,
    ) -> Option<(String, usize)> {
        // start points to '(' of '(?(...'
        // chars[start] = '('
        // chars[start+1] = '?'
        // chars[start+2] = '(' of the condition
        let cond_start = start + 2;
        // Find matching ')' for the condition
        let cond_end = Self::find_matching_paren(chars, cond_start)?;
        let condition: String = chars[cond_start..=cond_end].iter().collect();

        // After condition, parse yes and optional no branches
        let mut pos = cond_end + 1;
        let (yes_branch, no_branch, end) = Self::parse_conditional_branches(chars, pos)?;
        pos = end;

        // Determine the negated condition
        let negated = if condition.starts_with("(?=") {
            condition.replacen("(?=", "(?!", 1)
        } else if condition.starts_with("(?!") {
            condition.replacen("(?!", "(?=", 1)
        } else if condition.starts_with("(?<=") {
            condition.replacen("(?<=", "(?<!", 1)
        } else if condition.starts_with("(?<!") {
            condition.replacen("(?<!", "(?<=", 1)
        } else {
            return None;
        };

        let result = format!("(?:{condition}{yes_branch}|{negated}{no_branch})");
        Some((result, pos))
    }

    /// Transform `(?(?{0})yes|no)` -> `(?:no)` and `(?(?{1})yes|no)` -> `(?:yes)`.
    pub(super) fn transform_conditional_code(
        chars: &[char],
        start: usize,
    ) -> Option<(String, usize)> {
        let cond_start = start + 2;
        let cond_end = Self::find_matching_paren(chars, cond_start)?;
        let condition: String = chars[cond_start + 1..cond_end].iter().collect();
        // condition is like "?{0}" or "?{1}"
        let code_value = if condition == "?{0}" {
            false
        } else if condition == "?{1}" {
            true
        } else {
            // Unknown code -- treat as false (safe default)
            false
        };

        let pos = cond_end + 1;
        let (yes_branch, no_branch, end) = Self::parse_conditional_branches(chars, pos)?;

        let result = if code_value {
            format!("(?:{yes_branch})")
        } else {
            format!("(?:{no_branch})")
        };
        Some((result, end))
    }

    /// Transform `(?(N)yes|no)` with undefined group N -> `(?:no)`.
    /// If group N IS defined, leave it as-is for fancy-regex to handle.
    pub(super) fn transform_conditional_backref(
        chars: &[char],
        start: usize,
        num_groups: usize,
    ) -> Option<(String, usize)> {
        // chars[start] = '(', chars[start+1] = '?', chars[start+2] = '('
        let mut pos = start + 3;
        let mut num_str = String::new();
        while pos < chars.len() && chars[pos].is_ascii_digit() {
            num_str.push(chars[pos]);
            pos += 1;
        }
        if num_str.is_empty() || pos >= chars.len() || chars[pos] != ')' {
            return None;
        }
        let group_num: usize = num_str.parse().ok()?;
        pos += 1; // skip ')'

        if group_num <= num_groups {
            // Group exists, fancy-regex can handle it -- don't transform
            return None;
        }

        // Group doesn't exist -- always take the "no" branch
        let (_, no_branch, end) = Self::parse_conditional_branches(chars, pos)?;
        Some((format!("(?:{no_branch})"), end))
    }

    /// Find the matching ')' for a '(' at the given position.
    pub(super) fn find_matching_paren(chars: &[char], start: usize) -> Option<usize> {
        if start >= chars.len() || chars[start] != '(' {
            return None;
        }
        let mut depth = 1;
        let mut i = start + 1;
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
                continue;
            }
            if chars[i] == '(' {
                depth += 1;
            } else if chars[i] == ')' {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            i += 1;
        }
        None
    }

    pub(super) fn find_matching_brace(chars: &[char], start: usize) -> Option<usize> {
        if start >= chars.len() || chars[start] != '{' {
            return None;
        }
        let mut depth = 1;
        let mut i = start + 1;
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
                continue;
            }
            if chars[i] == '{' {
                depth += 1;
            } else if chars[i] == '}' {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            i += 1;
        }
        None
    }

    /// Parse `yes|no)` or `yes)` branches of a conditional pattern.
    /// Returns (yes_branch, no_branch, position_after_closing_paren).
    pub(super) fn parse_conditional_branches(
        chars: &[char],
        start: usize,
    ) -> Option<(String, String, usize)> {
        let mut depth = 1; // We're inside the outer (? ... )
        let mut i = start;
        let mut pipe_pos = None;
        while i < chars.len() {
            if chars[i] == '\\' {
                i += 2;
                continue;
            }
            if chars[i] == '(' {
                depth += 1;
            } else if chars[i] == ')' {
                depth -= 1;
                if depth == 0 {
                    let yes_branch: String = if let Some(pp) = pipe_pos {
                        chars[start..pp].iter().collect()
                    } else {
                        chars[start..i].iter().collect()
                    };
                    let no_branch: String = if let Some(pp) = pipe_pos {
                        chars[pp + 1..i].iter().collect()
                    } else {
                        String::new()
                    };
                    return Some((yes_branch, no_branch, i + 1));
                }
            } else if chars[i] == '|' && depth == 1 && pipe_pos.is_none() {
                pipe_pos = Some(i);
            }
            i += 1;
        }
        None
    }
}
