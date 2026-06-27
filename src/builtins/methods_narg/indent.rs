#![allow(clippy::result_large_err)]

use crate::value::Value;
use num_traits::ToPrimitive;

pub(crate) fn str_indent(s: &str, arg: &Value) -> (String, Option<String>) {
    const TABSTOP: usize = 8;

    // Determine the indent amount
    let is_whatever = matches!(arg, Value::Whatever);
    let steps: i64 = if is_whatever {
        0 // will be computed below
    } else {
        match arg {
            Value::Int(i) => *i,
            Value::BigInt(bi) => bi.to_i64().unwrap_or(0),
            Value::Num(f) => *f as i64,
            Value::Bool(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            Value::Str(sv) => {
                // Coerce string to Int: supports "0x10", "0e0", etc.
                if let Some(stripped) = sv.strip_prefix("0x").or_else(|| sv.strip_prefix("0X")) {
                    i64::from_str_radix(stripped, 16).unwrap_or(0)
                } else if sv.contains('e') || sv.contains('E') {
                    sv.parse::<f64>().map(|f| f as i64).unwrap_or(0)
                } else {
                    sv.parse::<i64>().unwrap_or(0)
                }
            }
            _ => 0,
        }
    };

    if s.is_empty() {
        return (String::new(), None);
    }

    // Split into lines, preserving trailing newline
    let has_trailing_newline = s.ends_with('\n');
    let lines: Vec<&str> = if has_trailing_newline {
        s.strip_suffix('\n').unwrap().split('\n').collect()
    } else {
        s.split('\n').collect()
    };

    if is_whatever {
        // Find minimum indent of non-empty lines
        let min_indent = lines
            .iter()
            .filter(|line| !line.is_empty())
            .map(|line| visual_indent_width(line, TABSTOP))
            .min()
            .unwrap_or(0);
        if min_indent == 0 {
            return (s.to_string(), None);
        }
        let result_lines: Vec<String> = lines
            .iter()
            .map(|line| {
                if line.is_empty() {
                    String::new()
                } else {
                    indent_line_negative(line, min_indent as i64, TABSTOP)
                }
            })
            .collect();
        let mut result = result_lines.join("\n");
        if has_trailing_newline {
            result.push('\n');
        }
        return (result, None);
    }

    if steps == 0 {
        return (s.to_string(), None);
    }

    // Check for excess outdent warning
    let warning = if steps < 0 {
        let outdent = -steps as usize;
        let min_indent = lines
            .iter()
            .filter(|line| !line.is_empty())
            .map(|line| visual_indent_width(line, TABSTOP))
            .min()
            .unwrap_or(0);
        if outdent > min_indent {
            Some(format!(
                "Asked to remove {} spaces, but the shortest indent is {} spaces",
                outdent, min_indent
            ))
        } else {
            None
        }
    } else {
        None
    };

    let result_lines: Vec<String> = lines
        .iter()
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else if steps > 0 {
                indent_line_positive(line, steps as usize, TABSTOP)
            } else {
                indent_line_negative(line, -steps, TABSTOP)
            }
        })
        .collect();

    let mut result = result_lines.join("\n");
    if has_trailing_newline {
        result.push('\n');
    }
    (result, warning)
}

/// Calculate the visual width of the leading whitespace of a line.
fn visual_indent_width(line: &str, tabstop: usize) -> usize {
    let mut width = 0;
    for ch in line.chars() {
        if ch == '\t' {
            // Tab advances to next tabstop
            width = (width / tabstop + 1) * tabstop;
        } else if ch == ' ' || is_unicode_space(ch) {
            width += 1;
        } else {
            break;
        }
    }
    width
}

/// Check if a character is a Unicode space (non-ASCII whitespace used for indentation).
fn is_unicode_space(ch: char) -> bool {
    ch != ' ' && ch != '\t' && ch.is_whitespace() && ch != '\n' && ch != '\r'
}

/// Indent a line by adding `steps` whitespace characters.
/// Follows Raku's "same space" rule: if the existing leading whitespace
/// consists of a single type of character, extend with that character type.
fn indent_line_positive(line: &str, steps: usize, tabstop: usize) -> String {
    let leading: Vec<char> = line
        .chars()
        .take_while(|c| *c == ' ' || *c == '\t' || is_unicode_space(*c))
        .collect();
    let rest = &line[leading.iter().collect::<String>().len()..];

    if leading.is_empty() {
        // No existing indent: just prepend spaces
        return " ".repeat(steps) + rest;
    }

    // Check if all leading whitespace is the same character
    let first = leading[0];
    let all_same = leading.iter().all(|c| *c == first);

    if all_same {
        if first == ' ' {
            // All spaces: add more spaces
            return " ".repeat(leading.len() + steps) + rest;
        } else if first == '\t' {
            // All tabs: if steps is a multiple of tabstop, add tabs
            if steps.is_multiple_of(tabstop) {
                return "\t".repeat(leading.len() + steps / tabstop) + rest;
            }
            // Otherwise add spaces after tabs
            let leading_str: String = leading.iter().collect();
            return leading_str + &" ".repeat(steps) + rest;
        } else {
            // All same Unicode space: add more of it
            let extended: String = std::iter::repeat_n(first, leading.len() + steps).collect();
            return extended + rest;
        }
    }

    // Mixed whitespace: add spaces after existing whitespace
    let leading_str: String = leading.iter().collect();
    leading_str + &" ".repeat(steps) + rest
}

/// Outdent a line by removing `de_indent` visual columns of whitespace from the left.
/// Follows Rakudo's algorithm: work from left, remove chars, then coalesce spaces into
/// adjacent tabs when not tabstop-aligned.
fn indent_line_negative(line: &str, de_indent: i64, tabstop: usize) -> String {
    let de_indent = de_indent as usize;

    // Collect leading whitespace chars with their visual widths
    let mut indent_chars: Vec<(char, usize)> = Vec::new();
    let mut indent_size = 0usize;
    let mut byte_offset = 0;
    for ch in line.chars() {
        if ch == '\t' {
            let width = tabstop - (indent_size % tabstop);
            indent_chars.push((ch, width));
            indent_size += width;
            byte_offset += ch.len_utf8();
        } else if ch == ' ' || is_unicode_space(ch) {
            indent_chars.push((ch, 1));
            indent_size += 1;
            byte_offset += ch.len_utf8();
        } else {
            break;
        }
    }
    let rest = &line[byte_offset..];

    // Work forwards from the left, removing chars
    let mut pos = 0usize;
    let mut idx = 0;
    while idx < indent_chars.len() && pos < de_indent {
        let (ch, _width) = indent_chars[idx];
        if ch == '\t' {
            // Tab: snap pos to previous tabstop, then advance by tabstop
            pos -= pos % tabstop;
            pos += tabstop;
        } else {
            pos += 1;
        }
        idx += 1;
    }

    // Coalesce: if pos is not tabstop-aligned, look ahead for a tab
    // within the next (tabstop - pos%tabstop) chars and absorb it
    if idx < indent_chars.len() && !pos.is_multiple_of(tabstop) {
        let check_count = tabstop - (pos % tabstop);
        let look_end = (idx + check_count).min(indent_chars.len());
        // Find the first tab in the look-ahead range
        let tab_offset = indent_chars[idx..look_end]
            .iter()
            .position(|(ch, _)| *ch == '\t');
        if let Some(offset) = tab_offset {
            // Remove everything from idx to idx+offset (inclusive)
            idx += offset + 1;
            pos -= pos % tabstop;
            pos += tabstop;
        }
    }

    // Build result: remaining indent chars + overshoot spaces + rest
    let mut result = String::new();
    for (ch, _) in indent_chars.iter().skip(idx) {
        result.push(*ch);
    }
    if pos > de_indent {
        result.push_str(&" ".repeat(pos - de_indent));
    }
    result.push_str(rest);
    result
}
