//! `--doc` mode: extract and render Pod declarator documentation from source code.
//!
//! Handles `#|` (leading) and `#=` (trailing) declarator comments attached to
//! declarations like `sub`, `method`, `class`, `role`, `module`, `grammar`,
//! `token`, `rule`, `regex`, `enum`, `subset`, `constant`, `my`, `our`, `has`.
use crate::ast::{Expr, PhaserKind, Stmt};
use crate::{Interpreter, RuntimeError, parse_dispatch};

/// Strip the `#|` or `#=` prefix from a doc comment line.
/// Removes `#|` or `#=` and exactly one following space if present.
fn strip_doc_prefix(line: &str) -> &str {
    let trimmed = line.trim_start();
    if let Some(rest) = trimmed.strip_prefix("#|") {
        rest.strip_prefix(' ').unwrap_or(rest)
    } else if let Some(rest) = trimmed.strip_prefix("#=") {
        rest.strip_prefix(' ').unwrap_or(rest)
    } else {
        trimmed
    }
}

/// Join multiple doc comment lines into a single text.
/// Blank lines separate paragraphs, which are joined with a single space.
/// Leading/trailing whitespace on each line is trimmed.
fn join_doc_lines(lines: &[&str]) -> String {
    let mut result_parts: Vec<String> = Vec::new();
    let mut current_paragraph: Vec<String> = Vec::new();

    for line in lines {
        let stripped = strip_doc_prefix(line);
        let trimmed = stripped.trim();
        if trimmed.is_empty() {
            // Paragraph break
            if !current_paragraph.is_empty() {
                result_parts.push(current_paragraph.join(" "));
                current_paragraph.clear();
            }
        } else {
            current_paragraph.push(trimmed.to_string());
        }
    }
    if !current_paragraph.is_empty() {
        result_parts.push(current_paragraph.join(" "));
    }

    result_parts.join(" ")
}

/// Extract the signature from a declaration line.
/// For `sub foo {}` → `sub foo()`.
fn extract_signature(line: &str) -> Option<String> {
    let trimmed = line.trim();

    // Match declarators
    let declarators = [
        "sub",
        "method",
        "multi sub",
        "multi method",
        "proto sub",
        "proto method",
        "class",
        "role",
        "module",
        "grammar",
        "token",
        "rule",
        "regex",
        "enum",
        "subset",
        "constant",
    ];

    for decl in &declarators {
        if let Some(rest) = trimmed.strip_prefix(decl)
            && (rest.is_empty() || rest.starts_with(|c: char| c.is_whitespace() || c == '{'))
        {
            let rest = rest.trim_start();
            let name_end = rest
                .find(|c: char| c.is_whitespace() || c == '(' || c == '{' || c == ';')
                .unwrap_or(rest.len());
            let name = &rest[..name_end];
            let after_name = rest[name_end..].trim_start();

            let params = if after_name.starts_with('(') {
                if let Some(close) = after_name.find(')') {
                    &after_name[..=close]
                } else {
                    "()"
                }
            } else {
                "()"
            };

            return match *decl {
                "sub" | "method" | "multi sub" | "multi method" | "proto sub" | "proto method" => {
                    if name.is_empty() {
                        Some(format!("{}{}", decl, params))
                    } else {
                        Some(format!("{} {}{}", decl, name, params))
                    }
                }
                _ => {
                    if name.is_empty() {
                        Some(decl.to_string())
                    } else {
                        Some(format!("{} {}", decl, name))
                    }
                }
            };
        }
    }

    // Also match `my`/`our`/`has` with a type or variable
    for scope in &["my", "our", "has"] {
        if let Some(rest) = trimmed.strip_prefix(scope)
            && rest.starts_with(|c: char| c.is_whitespace())
        {
            return Some(format!(
                "{}{}",
                scope,
                rest.split(['=', ';', '{']).next().unwrap_or("").trim_end()
            ));
        }
    }

    None
}

/// Check if a line is a `#|` (leading declarator) comment.
fn is_leading_doc(line: &str) -> bool {
    line.trim_start().starts_with("#|")
}

/// Check if a line is a `#=` (trailing declarator) comment.
fn is_trailing_doc(line: &str) -> bool {
    line.trim_start().starts_with("#=")
}

/// Render Pod documentation from source code in `--doc` mode.
pub fn render_doc(source: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let mut output = String::new();
    let mut i = 0;

    while i < lines.len() {
        // Collect leading doc comments (#|)
        let mut leading_lines: Vec<&str> = Vec::new();
        while i < lines.len() && is_leading_doc(lines[i]) {
            leading_lines.push(lines[i]);
            i += 1;
        }

        // If we collected leading comments, look for a declaration
        if !leading_lines.is_empty() {
            // Skip blank lines between #| and declaration
            while i < lines.len() && lines[i].trim().is_empty() {
                i += 1;
            }

            if i < lines.len()
                && let Some(sig) = extract_signature(lines[i])
            {
                let leading = join_doc_lines(&leading_lines);
                i += 1;

                // Collect trailing doc comments (#=)
                let mut trailing_lines: Vec<&str> = Vec::new();
                while i < lines.len() && is_trailing_doc(lines[i]) {
                    trailing_lines.push(lines[i]);
                    i += 1;
                }

                output.push_str(&sig);
                output.push('\n');
                if !leading.is_empty() {
                    output.push_str(&leading);
                    output.push('\n');
                }
                if !trailing_lines.is_empty() {
                    let trailing = join_doc_lines(&trailing_lines);
                    if !trailing.is_empty() {
                        output.push_str(&trailing);
                        output.push('\n');
                    }
                }
                continue;
            }
        }

        i += 1;
    }

    output
}

#[derive(Debug, Clone)]
pub struct DocModeResult {
    pub output: String,
    pub status: i64,
}

fn render_begin_pod_blocks(source: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let mut output = String::new();
    let mut i = 0usize;

    while i < lines.len() {
        let trimmed = lines[i].trim_start();
        let mut words = trimmed.split_whitespace();
        if words.next() == Some("=begin") && words.next() == Some("pod") {
            i += 1;
            while i < lines.len() {
                let inner = lines[i].trim_start();
                let mut inner_words = inner.split_whitespace();
                if inner_words.next() == Some("=end") && inner_words.next() == Some("pod") {
                    i += 1;
                    break;
                }
                if let Some(rest) = inner.strip_prefix("=head") {
                    let heading = rest
                        .trim_start_matches(|c: char| c.is_ascii_digit())
                        .trim_start();
                    output.push_str(heading);
                    output.push('\n');
                    i += 1;
                    continue;
                }
                if inner.starts_with('=') {
                    i += 1;
                    continue;
                }
                output.push_str(lines[i].trim_end());
                output.push('\n');
                i += 1;
            }
            continue;
        }
        i += 1;
    }

    output
}

#[allow(clippy::result_large_err)]
fn run_doc_init_blocks(source: &str) -> Result<(String, i64, bool), RuntimeError> {
    let (stmts, _) = parse_dispatch::parse_source(source)?;
    let mut interpreter = Interpreter::new();
    let mut i = 0usize;
    while i + 1 < stmts.len() {
        let is_doc_marker = matches!(&stmts[i], Stmt::Expr(Expr::BareWord(name)) if name == "DOC");
        if is_doc_marker
            && let Stmt::Phaser {
                kind: PhaserKind::Init,
                body,
            } = &stmts[i + 1]
        {
            interpreter.eval_block_value(body)?;
            if interpreter.is_halted() {
                break;
            }
            i += 2;
            continue;
        }
        i += 1;
    }
    Ok((
        interpreter.output().to_string(),
        interpreter.exit_code(),
        interpreter.is_halted(),
    ))
}

#[allow(clippy::result_large_err)]
pub fn run_doc_mode(source: &str) -> Result<DocModeResult, RuntimeError> {
    let (mut output, status, halted) = run_doc_init_blocks(source)?;
    if !halted {
        output.push_str(&render_doc(source));
        output.push_str(&render_begin_pod_blocks(source));
    }
    Ok(DocModeResult { output, status })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_doc() {
        let source = "\
#|  line 1
#|
#| line 2
sub foo {}
#= line 3
#=
#= line 4
";
        let output = render_doc(source);
        assert_eq!(output, "sub foo()\nline 1 line 2\nline 3 line 4\n");
    }

    #[test]
    fn test_extract_signature_sub() {
        assert_eq!(
            extract_signature("sub foo {}"),
            Some("sub foo()".to_string())
        );
        assert_eq!(
            extract_signature("sub foo($x) {}"),
            Some("sub foo($x)".to_string())
        );
    }

    #[test]
    fn test_strip_doc_prefix() {
        assert_eq!(strip_doc_prefix("#|  line 1"), " line 1");
        assert_eq!(strip_doc_prefix("#| "), "");
        assert_eq!(strip_doc_prefix("#| line 2"), "line 2");
    }

    #[test]
    fn test_join_doc_lines() {
        let lines = vec!["#|  line 1", "#| ", "#| line 2"];
        assert_eq!(join_doc_lines(&lines), "line 1 line 2");
    }

    #[test]
    fn test_render_begin_pod_blocks_heading_and_text() {
        let source = "\
=begin pod

=head1 Some Heading

Some Text

=end pod
";
        let output = render_begin_pod_blocks(source);
        assert!(output.contains("Some Heading"));
        assert!(output.contains("Some Text"));
    }

    #[test]
    fn test_run_doc_mode_executes_doc_init() {
        let source = "\
=begin pod
=head1 Docs
=end pod
DOC INIT { say 'alive'; exit; }
";
        let result = run_doc_mode(source).unwrap();
        assert!(result.output.contains("alive"));
        assert!(!result.output.contains("Docs"));
    }
}
