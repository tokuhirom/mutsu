//! Placeholders written inside a regex literal (mutsu#10542).
//!
//! A match regex such as `/$^a/` reaches the AST as a regex *value* that keeps
//! only its source text (`Expr::Literal(Value::Regex("$^a"))`), so the typed
//! visitor sees no variable node in it. The source is scanned here instead,
//! telling the two places a placeholder can sit apart:
//!
//! - **interpolated** into the pattern (`/$^a/`, `/@^list/`): the regex is
//!   matched in the enclosing block's scope, so the placeholder is that
//!   block's parameter;
//! - inside a **code block** (`{ ... }`, `<?{ ... }>`, `<!{ ... }>`,
//!   `<{ ... }>`): a block of its own that takes no signature, which rakudo
//!   rejects at compile time (X::Placeholder::Block).

use super::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use crate::value::{Value, ValueView};

/// The placeholders of one regex source, in the format the placeholder
/// collectors use (`^a`, `@^a`, `%^a`, `&^a`), first occurrence first.
#[derive(Default, Debug, PartialEq, Eq)]
pub(crate) struct RegexPlaceholders {
    pub(crate) interpolated: Vec<String>,
    pub(crate) in_code_block: Vec<String>,
}

/// The pattern source a regex literal expression carries, if `expr` is one.
// Cost: O(1).
pub(crate) fn regex_literal_source(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(value)
        | Expr::MatchRegex(value)
        | Expr::RegexLiteral { value, .. }
        | Expr::MatchRegexTree { value, .. }
        | Expr::MatchRegexDynamicAdverbs { value, .. } => regex_value_source(value),
        _ => None,
    }
}

// Cost: O(1).
fn regex_value_source(value: &Value) -> Option<String> {
    match value.view() {
        ValueView::Regex(src) => Some(src.to_string()),
        ValueView::RegexWithAdverbs(adverbs) => Some(adverbs.pattern.to_string()),
        _ => None,
    }
}

/// Scan a regex source for placeholder variables. Backslash escapes,
/// single-quoted literals, character classes (`<[...]>`) and `#` comments
/// are skipped; braces open a code block (nesting counted).
// Cost: O(n), n = length of `src`.
pub(crate) fn regex_source_placeholders(src: &str) -> RegexPlaceholders {
    let chars: Vec<char> = src.chars().collect();
    let mut out = RegexPlaceholders::default();
    let mut depth = 0usize;
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '\\' => i += 2,
            '\'' if depth == 0 => {
                i += 1;
                while i < chars.len() && chars[i] != '\'' {
                    i += if chars[i] == '\\' { 2 } else { 1 };
                }
                i += 1;
            }
            '#' if depth == 0 => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '<' if depth == 0 && matches!(chars.get(i + 1), Some('[' | '-' | '+')) => {
                // A character class: its `$`, `{`, `}` are literal characters.
                while i < chars.len() && chars[i] != ']' {
                    i += if chars[i] == '\\' { 2 } else { 1 };
                }
                i += 1;
            }
            '{' => {
                depth += 1;
                i += 1;
            }
            '}' => {
                depth = depth.saturating_sub(1);
                i += 1;
            }
            sigil @ ('$' | '@' | '%' | '&')
                if chars.get(i + 1) == Some(&'^')
                    && chars.get(i + 2).is_some_and(|c| c.is_alphabetic()) =>
            {
                let start = i + 2;
                let mut j = start;
                while j < chars.len()
                    && (chars[j].is_alphanumeric() || matches!(chars[j], '_' | '-'))
                {
                    j += 1;
                }
                // A trailing `-` is not part of an identifier.
                while j > start && chars[j - 1] == '-' {
                    j -= 1;
                }
                let name: String = chars[start..j].iter().collect();
                let key = match sigil {
                    '$' => format!("^{name}"),
                    s => format!("{s}^{name}"),
                };
                let list = if depth == 0 {
                    &mut out.interpolated
                } else {
                    &mut out.in_code_block
                };
                if !list.contains(&key) {
                    list.push(key);
                }
                i = j;
            }
            _ => i += 1,
        }
    }
    out
}

/// Finds the first placeholder written inside a regex code block anywhere in
/// a compilation unit, routines included.
struct CodeBlockPlaceholderFinder {
    found: Option<String>,
}

impl<'ast> Visit<'ast> for CodeBlockPlaceholderFinder {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found.is_some() {
            return;
        }
        if let Some(src) = regex_literal_source(expr)
            && let Some(key) = regex_source_placeholders(&src)
                .in_code_block
                .into_iter()
                .next()
        {
            self.found = Some(placeholder_display_name(key));
            return;
        }
        walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_none() {
            walk_stmt(self, stmt);
        }
    }
}

/// The first placeholder (`$^a`) used inside a regex code block in `stmts`:
/// such a block takes no signature, so rakudo rejects it at compile time
/// with X::Placeholder::Block.
// Cost: O(n), n = size of `stmts`' subtree plus its regex sources.
pub(crate) fn find_regex_code_block_placeholder(stmts: &[Stmt]) -> Option<String> {
    let mut finder = CodeBlockPlaceholderFinder { found: None };
    for stmt in stmts {
        finder.visit_stmt(stmt);
    }
    finder.found
}

/// `^a` -> `$^a`; `@^a`, `%^a`, `&^a` already carry their sigil.
// Cost: O(len).
pub(crate) fn placeholder_display_name(key: String) -> String {
    if key.starts_with('^') {
        format!("${key}")
    } else {
        key
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scan(src: &str) -> (Vec<String>, Vec<String>) {
        let r = regex_source_placeholders(src);
        (r.interpolated, r.in_code_block)
    }

    #[test]
    fn interpolated_and_code_block_placeholders_are_told_apart() {
        assert_eq!(scan("$^a"), (vec!["^a".to_string()], vec![]));
        assert_eq!(scan("<?{ $^a }>"), (vec![], vec!["^a".to_string()]));
        assert_eq!(
            scan("@^xs { say $^b } %^h"),
            (
                vec!["@^xs".to_string(), "%^h".to_string()],
                vec!["^b".to_string()]
            )
        );
    }

    #[test]
    fn literal_text_is_not_a_placeholder() {
        assert_eq!(scan(r"\$^a '$^b' <[$^]> # $^c"), (vec![], vec![]));
        assert_eq!(scan("$^"), (vec![], vec![]));
    }
}
