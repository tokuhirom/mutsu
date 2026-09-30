use super::*;

impl Interpreter {
    /// Normalizes a leading declarator doc written inside an initializer so
    /// the line scanner in `collect_doc_comments` sees it in its usual shape.
    ///
    /// A `#|` documents the declarator that follows it, so in
    ///
    /// ```text
    /// my $anon-sub = #| Anonymous
    ///     anon Str sub {};
    /// ```
    ///
    /// the doc belongs to the anonymous sub, not to `$anon-sub` (roast
    /// S26-documentation/why-leading.t; Rakudo attaches a `#|` written above
    /// `my $x = ...` to the variable). The scanner only recognizes a `#|` that
    /// starts a line, so this rewrites the pair of lines to
    ///
    /// ```text
    /// #| Anonymous
    /// my $anon-sub = anon Str sub {};
    /// ```
    ///
    /// which it already attaches to the initializer's declarator. The line
    /// count is unchanged, so every line number the scanner records stays
    /// valid. Only a doc that follows an assignment operator and ends on its
    /// own line is moved; anything else is returned as is.
    // Cost: O(n), n = total source length.
    pub(super) fn hoist_initializer_leading_docs(lines: &[&str]) -> Vec<String> {
        let mut out: Vec<String> = lines.iter().map(|l| (*l).to_string()).collect();
        let mut idx = 0usize;
        while idx + 1 < out.len() {
            let Some((prefix, doc)) = split_initializer_leading_doc(&out[idx]) else {
                idx += 1;
                continue;
            };
            let next = out[idx + 1].trim_start();
            if next.is_empty() || next.starts_with('#') || next.starts_with('=') {
                idx += 1;
                continue;
            }
            let indent_len = out[idx].len() - out[idx].trim_start().len();
            let indent = out[idx][..indent_len].to_string();
            let joined = format!("{} {}", prefix.trim_end(), next);
            let doc_line = format!("{indent}{doc}");
            out[idx + 1] = format!("{indent}{}", joined.trim_start());
            out[idx] = doc_line;
            idx += 2;
        }
        out
    }
}

/// Splits `my $x = #| doc` into (`my $x =`, `#| doc`) when the `#|` directly
/// follows an assignment operator outside any quote, and a block-form doc
/// (`#|{...}`) closes on the same line.
fn split_initializer_leading_doc(line: &str) -> Option<(&str, &str)> {
    let trimmed = line.trim_start();
    if trimmed.starts_with('#') {
        return None;
    }
    let pos = line.find("#|")?;
    let prefix = &line[..pos];
    let code = prefix.trim_end();
    // An assignment or binding (`=`, `:=`, `::=`), not a comparison.
    if code.trim().is_empty()
        || !code.ends_with('=')
        || ["==", "<=", ">=", "!="].iter().any(|op| code.ends_with(op))
    {
        return None;
    }
    if !code.matches('"').count().is_multiple_of(2) || !code.matches('\'').count().is_multiple_of(2)
    {
        return None;
    }
    let doc = &line[pos..];
    if !block_doc_closes_on_line(&doc[2..]) {
        return None;
    }
    Some((prefix, doc))
}

/// True unless `rest` (the text after `#|`) opens a bracketed block doc that
/// does not close before the end of the line.
fn block_doc_closes_on_line(rest: &str) -> bool {
    let Some(open) = rest.chars().next() else {
        return true;
    };
    let close = match open {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => return true,
    };
    let mut depth = 0i32;
    for ch in rest.chars() {
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                return true;
            }
        }
    }
    false
}
