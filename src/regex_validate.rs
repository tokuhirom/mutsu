use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

/// Validate regex pattern syntax at parse time.
/// Returns Ok(()) if the pattern is syntactically valid, or an appropriate
/// X::Syntax::Regex::* exception.
#[allow(clippy::result_large_err)]
pub(crate) fn validate_regex_syntax(pattern: &str) -> Result<(), RuntimeError> {
    let source = pattern.trim();
    // Skip leading inline adverbs (:i, :s, :m, :ignorecase, etc.)
    let source = skip_inline_adverbs(source);
    let mut chars = source.chars().peekable();
    let mut prev_was_quantifier = false;

    while let Some(c) = chars.next() {
        match c {
            // Whitespace is insignificant
            ' ' | '\t' | '\n' | '\r' => {
                // Don't reset prev_was_quantifier — whitespace between quantifiers
                // should still be caught (e.g., "a+ +")
                continue;
            }
            // Valid metacharacters (includes word boundary markers «»)
            '.' | '^' | '|' | '&' | '~' | '%' | '=' | ',' | '«' | '»' => {
                prev_was_quantifier = false;
            }
            // Variable interpolation: $, @
            // These can be followed by variable names, <name>, {code}, digits, etc.
            // After the variable reference, '=' is valid (capture aliasing)
            '$' | '@' => {
                prev_was_quantifier = false;
                skip_variable_ref(&mut chars);
            }
            // Quantifiers
            '*' | '+' | '?' => {
                if prev_was_quantifier {
                    return Err(make_quantifier_error(c));
                }
                prev_was_quantifier = true;
                // ** is the general quantifier (e.g., ** 2, ** 2..5)
                if c == '*' && chars.peek() == Some(&'*') {
                    chars.next(); // consume second *
                    // Skip whitespace, then skip the quantifier argument
                    while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                        chars.next();
                    }
                    // Skip digits, ranges (..), etc.
                    while chars
                        .peek()
                        .is_some_and(|ch| ch.is_ascii_digit() || *ch == '.' || *ch == '^')
                    {
                        chars.next();
                    }
                    prev_was_quantifier = false; // ** N is a complete quantifier+arg
                    continue;
                }
                // Consume trailing :, !, or ? (frugal/greedy modifiers)
                while chars.peek() == Some(&':')
                    || chars.peek() == Some(&'!')
                    || chars.peek() == Some(&'?')
                {
                    chars.next();
                }
                continue;
            }
            // Escape sequences
            '\\' => {
                prev_was_quantifier = false;
                if let Some(&esc) = chars.peek() {
                    chars.next();
                    if esc.is_ascii_alphabetic() {
                        validate_backslash_sequence(esc)?;
                    }
                    // Non-alpha escapes (e.g., \!, \{, \/) are always valid
                }
            }
            // Quoted strings — skip content
            '\'' | '"' | '\u{2018}' | '\u{201A}' | '\u{201C}' | '\u{201E}' | '\u{FF62}' => {
                prev_was_quantifier = false;
                let close = match c {
                    '\'' => '\'',
                    '"' => '"',
                    '\u{2018}' => '\u{2019}',
                    '\u{201A}' => '\u{2019}',
                    '\u{201C}' => '\u{201D}',
                    '\u{201E}' => '\u{201D}',
                    '\u{FF62}' => '\u{FF63}',
                    _ => c,
                };
                loop {
                    match chars.next() {
                        Some('\\') => {
                            chars.next();
                        }
                        Some(ch) if ch == close => break,
                        Some(_) => {}
                        None => break,
                    }
                }
            }
            // Assertions/named rules — skip balanced <...>
            '<' => {
                prev_was_quantifier = false;
                skip_balanced(&mut chars, '<', '>');
            }
            // Grouping — skip balanced content
            '[' => {
                prev_was_quantifier = false;
                skip_balanced(&mut chars, '[', ']');
            }
            '(' => {
                prev_was_quantifier = false;
                skip_balanced(&mut chars, '(', ')');
            }
            '{' => {
                prev_was_quantifier = false;
                skip_balanced(&mut chars, '{', '}');
            }
            // Closing brackets — just skip
            ']' | ')' | '}' | '>' => {
                prev_was_quantifier = false;
            }
            // Comment
            '#' => {
                prev_was_quantifier = false;
                for ch in chars.by_ref() {
                    if ch == '\n' {
                        break;
                    }
                }
            }
            // Modifier colon in regex (e.g., :i, :s, :11)
            ':' => {
                prev_was_quantifier = false;
                // Check if it's followed by digits only (unrecognized modifier)
                let mut digits = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_digit() {
                        digits.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if !digits.is_empty() {
                    // Check if followed by a modifier name
                    let mut name = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                            name.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if name.is_empty() {
                        // Just digits, no modifier name → unrecognized modifier
                        return Err(make_unrecognized_modifier_error(&digits));
                    }
                } else {
                    // Skip modifier name
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    // Skip optional argument in parens: :name(...)
                    if chars.peek() == Some(&'(') {
                        skip_balanced(&mut chars, '(', ')');
                        // consume the '(' first
                        chars.next();
                        skip_balanced(&mut chars, '(', ')');
                    }
                }
            }
            // Alphanumeric and underscore are valid as literals
            _ if c.is_alphanumeric() || c == '_' => {
                prev_was_quantifier = false;
            }
            // Bare '-' is not valid as a literal in regex
            '-' => {
                return Err(make_unrecognized_metachar_error(c));
            }
            // '!' is not valid as a bare metacharacter (only inside assertions)
            '!' => {
                return Err(make_unrecognized_metachar_error(c));
            }
            // Other non-identifier glyphs that are not recognized metacharacters
            _ if !c.is_alphanumeric() && c != '_' => {
                return Err(make_unrecognized_metachar_error(c));
            }
            _ => {
                prev_was_quantifier = false;
            }
        }
    }
    Ok(())
}

/// Skip known inline adverbs at the start of a regex pattern.
fn skip_inline_adverbs(mut source: &str) -> &str {
    loop {
        let trimmed = source.trim_start();
        if let Some(rest) = trimmed
            .strip_prefix(":ignorecase")
            .or_else(|| trimmed.strip_prefix(":ignoremark"))
            .or_else(|| trimmed.strip_prefix(":sigspace"))
        {
            source = rest;
            continue;
        }
        // Short forms: :i, :s, :m (but not :mm, :my, etc.)
        if let Some(rest) = trimmed
            .strip_prefix(":i")
            .or_else(|| trimmed.strip_prefix(":s"))
            .or_else(|| trimmed.strip_prefix(":m"))
            .filter(|r| {
                r.is_empty() || r.starts_with(' ') || r.starts_with(':') || r.starts_with('/')
            })
        {
            source = rest;
            continue;
        }
        source = trimmed;
        break;
    }
    source
}

/// Skip a variable reference after $ or @.
/// Handles: $<name>, $0, ${code}, $identifier, and following = for aliasing.
fn skip_variable_ref(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
    match chars.peek() {
        Some(&'<') => {
            chars.next();
            skip_balanced(chars, '<', '>');
        }
        Some(&'{') => {
            chars.next();
            skip_balanced(chars, '{', '}');
        }
        Some(&'(') => {
            chars.next();
            skip_balanced(chars, '(', ')');
        }
        Some(&c) if c.is_ascii_digit() => {
            while chars.peek().is_some_and(|ch| ch.is_ascii_digit()) {
                chars.next();
            }
        }
        Some(&c) if c.is_alphabetic() || c == '_' => {
            while chars
                .peek()
                .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
            {
                chars.next();
            }
        }
        _ => {}
    }
    // After variable ref, '=' is valid for capture aliasing (e.g., $<type>=(...))
    if chars.peek() == Some(&'=') {
        chars.next();
    }
}

fn skip_balanced(chars: &mut std::iter::Peekable<std::str::Chars<'_>>, open: char, close: char) {
    let mut depth = 1u32;
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            chars.next(); // skip escaped char
            continue;
        }
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                return;
            }
        }
    }
}

#[allow(clippy::result_large_err)]
fn validate_backslash_sequence(esc: char) -> Result<(), RuntimeError> {
    // Known valid backslash sequences in Raku regex
    match esc {
        'd' | 'D' | 'w' | 'W' | 's' | 'S' | 'n' | 'N' | 't' | 'r' | 'x' | 'o' | 'c' | 'C' | 'X'
        | 'v' | 'V' | 'h' | 'H' | 'e' | 'E' | 'b' | 'B' | '0' => Ok(()),
        _ if !esc.is_ascii_alphabetic() => Ok(()), // non-alpha escapes are always valid
        _ => {
            // Unknown alphabetic backslash sequence
            let msg = format!("Unrecognized backslash sequence: \\{}", esc);
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::Str(msg.clone()));
            let ex = Value::make_instance("X::Backslash::UnrecognizedSequence".to_string(), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            Err(err)
        }
    }
}

pub(crate) fn make_unrecognized_metachar_error(metachar: char) -> RuntimeError {
    let msg = format!(
        "Unrecognized regex metacharacter {} (must be quoted to match literally)",
        metachar
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::Str(msg.clone()));
    attrs.insert("metachar".to_string(), Value::Str(metachar.to_string()));
    let ex = Value::make_instance("X::Syntax::Regex::UnrecognizedMetachar".to_string(), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

fn make_unrecognized_modifier_error(modifier: &str) -> RuntimeError {
    let msg = format!("Unrecognized regex modifier :{}", modifier);
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::Str(msg.clone()));
    attrs.insert("modifier".to_string(), Value::Str(modifier.to_string()));
    let ex = Value::make_instance("X::Syntax::Regex::UnrecognizedModifier".to_string(), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

fn make_quantifier_error(quant: char) -> RuntimeError {
    let msg = format!(
        "Quantifier quantifies nothing (preceding atom is also a quantifier: {})",
        quant
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::Str(msg.clone()));
    let ex = Value::make_instance("X::Syntax::Regex::Quantifier".to_string(), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}
