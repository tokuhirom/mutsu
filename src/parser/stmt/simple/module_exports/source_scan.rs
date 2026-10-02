//! Hand-written source-text scans for module declarations the best-effort
//! parse may have dropped.
//!
//! `parse_program_partial` silently drops every statement it cannot parse, so
//! the static export scan backs the AST walk with a look at the raw source.
//! These scans used to be `regex` patterns; they are written out here so the
//! parser does not need a regex engine (#10439), and they skip Pod blocks and
//! heredoc bodies, where a `sub ... is export` is prose or data, not a
//! declaration.
//!
//! They stay approximations: a declaration is recognised by its spelling, not
//! by parsing, so a string literal or comment holding one still counts.

/// `source` with every Pod block and heredoc body replaced by blank lines
/// (line structure preserved), so the scans below see only code.
// Cost: O(n), n = source length.
pub(super) fn code_text(source: &str) -> String {
    let mut out = String::with_capacity(source.len());
    let mut pod: Option<PodBlock> = None;
    let mut heredocs: Vec<String> = Vec::new();
    for line in source.split_inclusive('\n') {
        let blank = || if line.ends_with('\n') { "\n" } else { "" };
        let trimmed = line.trim();
        if let Some(term) = heredocs.first() {
            if trimmed == term {
                heredocs.remove(0);
            }
            out.push_str(blank());
            continue;
        }
        if let Some(block) = &pod {
            let ends = match block {
                PodBlock::Delimited(ident) => pod_directive(trimmed)
                    .is_some_and(|(kw, rest)| kw == "end" && first_word(rest) == ident.as_str()),
                PodBlock::Paragraph => trimmed.is_empty(),
                PodBlock::Finish => false,
            };
            if ends {
                pod = None;
            }
            out.push_str(blank());
            continue;
        }
        if let Some((kw, rest)) = pod_directive(trimmed) {
            pod = match kw {
                "begin" => Some(PodBlock::Delimited(first_word(rest).to_string())),
                "finish" => Some(PodBlock::Finish),
                // `=end` with no open block, `=config`, `=alias`: one-line
                // directives.
                "end" | "config" | "alias" => None,
                // `=for NAME` and abbreviated `=NAME` run to the next blank line.
                _ => Some(PodBlock::Paragraph),
            };
            out.push_str(blank());
            continue;
        }
        heredocs.extend(heredoc_terminators(line));
        out.push_str(line);
    }
    out
}

enum PodBlock {
    Delimited(String),
    Paragraph,
    Finish,
}

/// A Pod directive line (`=begin pod`, `=head1 ...`, `=for comment`): its
/// keyword and the rest of the line.
fn pod_directive(trimmed: &str) -> Option<(&str, &str)> {
    let body = trimmed.strip_prefix('=')?;
    let end = body
        .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-'))
        .unwrap_or(body.len());
    let (kw, rest) = body.split_at(end);
    let starts_alpha = kw.chars().next().is_some_and(char::is_alphabetic);
    (starts_alpha && (rest.is_empty() || rest.starts_with(char::is_whitespace)))
        .then(|| (kw, rest.trim_start()))
}

fn first_word(s: &str) -> &str {
    s.split_whitespace().next().unwrap_or("")
}

/// The terminators of the heredocs a code line opens (`q:to/END/`,
/// `qq:heredoc<EOF>`), in order.
fn heredoc_terminators(line: &str) -> Vec<String> {
    let mut terms = Vec::new();
    let mut pos = 0;
    while let Some(off) = line[pos..].find(':') {
        let at = pos + off + 1;
        pos = at;
        let Some(after) = line[at..]
            .strip_prefix("to")
            .or_else(|| line[at..].strip_prefix("heredoc"))
        else {
            continue;
        };
        let mut chars = after.chars();
        let close = match chars.next() {
            Some('/') => '/',
            Some('<') => '>',
            Some('(') => ')',
            Some('[') => ']',
            Some('{') => '}',
            Some('\u{AB}') => '\u{BB}',
            Some('\'') => '\'',
            Some('"') => '"',
            _ => continue,
        };
        let body = chars.as_str();
        let body_at = line.len() - body.len();
        if let Some(end) = body.find(close) {
            let term = body[..end].trim();
            if !term.is_empty() {
                terms.push(term.to_string());
            }
            pos = body_at + end;
        }
    }
    terms
}

/// Regex `\w`: alphanumerics, marks, connector punctuation and the joiners.
fn is_word(c: char) -> bool {
    use crate::ucd::gc::{GeneralCategory as Gc, general_category};
    c.is_alphanumeric()
        || matches!(general_category(c), Gc::Mn | Gc::Mc | Gc::Me | Gc::Pc)
        || c == '\u{200C}'
        || c == '\u{200D}'
}

/// A word boundary between `before` and `after` (`None` = text edge).
fn boundary(before: Option<char>, after: Option<char>) -> bool {
    before.is_some_and(is_word) != after.is_some_and(is_word)
}

fn prev_char(s: &str, at: usize) -> Option<char> {
    s[..at].chars().next_back()
}

fn next_char(s: &str, at: usize) -> Option<char> {
    s[at..].chars().next()
}

/// Skip one or more whitespace characters (`\s+`).
fn skip_ws1(s: &str, at: usize) -> Option<usize> {
    let n = s[at..]
        .char_indices()
        .find(|&(_, c)| !c.is_whitespace())
        .map_or(s.len() - at, |(i, _)| i);
    (n > 0).then_some(at + n)
}

/// `[A-Za-z_][A-Za-z0-9_'\-]*` at `at`: the end of the longest match.
fn ident_end(s: &str, at: usize) -> Option<usize> {
    let b = s.as_bytes();
    if !b
        .get(at)
        .is_some_and(|c| c.is_ascii_alphabetic() || *c == b'_')
    {
        return None;
    }
    let mut end = at + 1;
    while b
        .get(end)
        .is_some_and(|c| c.is_ascii_alphanumeric() || matches!(c, b'_' | b'\'' | b'-'))
    {
        end += 1;
    }
    Some(end)
}

/// `[A-Za-z_][A-Za-z0-9_'\-]*\b` at `at`: the longest identifier that ends on
/// a word boundary (the regex backtracks off a trailing `-`/`'`).
fn ident_end_at_boundary(s: &str, at: usize) -> Option<usize> {
    let mut end = ident_end(s, at)?;
    while end > at {
        if boundary(prev_char(s, end), next_char(s, end)) {
            return Some(end);
        }
        end -= 1;
    }
    None
}

/// `keyword\s+` at `at` (no boundary check), returning the end.
fn keyword_ws(s: &str, at: usize, keyword: &str) -> Option<usize> {
    s[at..]
        .starts_with(keyword)
        .then(|| skip_ws1(s, at + keyword.len()))
        .flatten()
}

/// `\bis\s+<word>\b` at `at`, returning the end.
fn is_trait_at(s: &str, at: usize, word: &str) -> Option<usize> {
    if !s[at..].starts_with("is") || !boundary(prev_char(s, at), Some('i')) {
        return None;
    }
    let after = skip_ws1(s, at + 2)?;
    let end = after + word.len();
    (s[after..].starts_with(word) && boundary(prev_char(s, end), next_char(s, end))).then_some(end)
}

/// Does `s` contain `\bis\s+<word>\b`?
fn contains_trait(s: &str, word: &str) -> bool {
    s.char_indices()
        .any(|(i, _)| is_trait_at(s, i, word).is_some())
}

/// Every `\b<keyword>\s+NAME\b ... \bis\s+export\b` declaration in `s`, where
/// `...` holds no `;` or `{`: the name and the text between it and the (last)
/// `is export`, in source order.
fn exported_after_keyword<'a>(s: &'a str, keyword: &str) -> Vec<(&'a str, &'a str)> {
    let mut found = Vec::new();
    let mut pos = 0;
    while let Some(off) = s[pos..].find(keyword) {
        let at = pos + off;
        pos = at + keyword.len();
        if !boundary(prev_char(s, at), next_char(s, at)) {
            continue;
        }
        let Some(name_at) = keyword_ws(s, at, keyword) else {
            continue;
        };
        let Some(name_end) = ident_end_at_boundary(s, name_at) else {
            continue;
        };
        let stop = s[name_end..]
            .find([';', '{'])
            .map_or(s.len(), |i| name_end + i);
        // Greedy `[^;{]*`: the last `is export` before the stop wins.
        let Some((trait_at, export_end)) = (name_end..stop)
            .rev()
            .filter(|&j| s.is_char_boundary(j))
            .find_map(|j| is_trait_at(s, j, "export").map(|end| (j, end)))
        else {
            continue;
        };
        found.push((&s[name_at..name_end], &s[name_end..trait_at]));
        // `(\s*\([^)]*\))?`: an export tag list ends the match.
        let after_ws = s[export_end..]
            .char_indices()
            .find(|&(_, c)| !c.is_whitespace())
            .map_or(s.len(), |(i, _)| export_end + i);
        pos = match s[after_ws..].strip_prefix('(').and_then(|r| r.find(')')) {
            Some(close) => after_ws + 1 + close + 1,
            None => export_end,
        };
    }
    found
}

/// Names declared `sub NAME ... is export` / `proto NAME ... is export`
/// anywhere in `code`, each with whether its declaration carries
/// `is test-assertion` before the `is export`.
// Cost: O(n * k), n = source length, k = declaration length.
pub(super) fn exported_names(code: &str) -> Vec<(String, bool)> {
    let mut names: std::collections::BTreeMap<String, bool> = Default::default();
    for keyword in ["sub", "proto"] {
        for (name, between) in exported_after_keyword(code, keyword) {
            let is_ta = contains_trait(between, "test-assertion");
            let entry = names.entry(name.to_string()).or_insert(false);
            *entry = *entry || is_ta;
        }
    }
    names.into_iter().collect()
}

/// At a line start: where `(?:my\s+|our\s+)?(?:<declarator>\s+)?sub\s+`
/// can leave the routine name, in the order the regex tried them (each
/// optional part present first).
fn line_start_sub(s: &str, at: usize, declarators: &[&str]) -> Vec<usize> {
    let scopes = ["my", "our"]
        .iter()
        .filter_map(|kw| keyword_ws(s, at, kw))
        .chain([at]);
    let mut name_starts = Vec::new();
    for after_scope in scopes {
        let decls = declarators
            .iter()
            .filter_map(|kw| keyword_ws(s, after_scope, kw))
            .chain([after_scope]);
        name_starts.extend(decls.filter_map(|p| keyword_ws(s, p, "sub")));
    }
    name_starts
}

/// Every line-start offset of `s`.
fn line_starts(s: &str) -> impl Iterator<Item = usize> + '_ {
    std::iter::once(0).chain(s.match_indices('\n').map(|(i, _)| i + 1))
}

/// Unit-scope routine names, approximating "unit scope" as "the declaration
/// starts at column 0": `[my|our] [proto|multi|only] sub NAME`. Qualified
/// names (`sub Foo::bar`) and `EXPORT` itself are left out.
// Cost: O(n), n = source length.
pub(super) fn unit_scope_routine_names(code: &str) -> Vec<String> {
    let mut names = Vec::new();
    let mut resume = 0;
    for at in line_starts(code) {
        if at < resume {
            continue;
        }
        let Some((name_at, mut end)) = line_start_sub(code, at, &["proto", "multi", "only"])
            .into_iter()
            .find_map(|name_at| ident_end(code, name_at).map(|end| (name_at, end)))
        else {
            continue;
        };
        while code[end..].starts_with("::") {
            match ident_end(code, end + 2) {
                Some(e) => end = e,
                None => break,
            }
        }
        resume = end;
        let name = &code[name_at..end];
        if name != "EXPORT" && !name.contains("::") {
            names.push(name.to_string());
        }
    }
    names.sort();
    names.dedup();
    names
}

/// Is there a column-0 `[my|our] sub EXPORT\b`?
// Cost: O(n), n = source length.
pub(super) fn declares_export_sub(code: &str) -> bool {
    line_starts(code).any(|at| {
        line_start_sub(code, at, &[]).into_iter().any(|name_at| {
            let end = name_at + "EXPORT".len();
            code[name_at..].starts_with("EXPORT") && boundary(Some('T'), next_char(code, end))
        })
    })
}

#[cfg(test)]
#[path = "source_scan_tests.rs"]
mod tests;
