use crate::symbol::Symbol;
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

    // <sym> / <.sym> can only be used in a proto regex with :sym<> adverb.
    // Token/rule bodies don't pass through this validation, so if we see
    // <sym> here it's always invalid (not inside a :sym token).
    check_bare_sym_usage(source)?;

    let mut chars = source.chars().peekable();
    let mut prev_was_quantifier = false;
    let mut prev_was_tilde = false;
    let mut prev_was_anchor = false; // Track non-quantifiable anchors (^, ^^, $, $$)

    while let Some(c) = chars.next() {
        match c {
            // Whitespace is insignificant
            ' ' | '\t' | '\n' | '\r' => {
                // Don't reset prev_was_quantifier — whitespace between quantifiers
                // should still be caught (e.g., "a+ +")
                continue;
            }
            // Anchors: ^, ^^, $, $$ — these are non-quantifiable
            '^' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                // ^^ is start-of-line, ^ is start-of-string — both non-quantifiable
                if chars.peek() == Some(&'^') {
                    chars.next(); // consume second ^
                }
                prev_was_anchor = true;
                continue;
            }
            // Valid metacharacters (includes word boundary markers «»)
            '.' | '|' | '&' | '~' | '=' | ',' | '«' | '»' => {
                prev_was_quantifier = false;
                prev_was_anchor = false;
                prev_was_tilde = c == '~';
            }
            // % is a separator quantifier modifier, but %var is a hash interpolation (reserved)
            '%' => {
                if chars
                    .peek()
                    .is_some_and(|ch| ch.is_alphabetic() || *ch == '_')
                {
                    let msg = "The use of hash variables in regexes is reserved";
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::Syntax::Reserved"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                prev_was_quantifier = false;
                prev_was_anchor = false;
            }
            // Variable interpolation: $, @
            // These can be followed by variable names, <name>, {code}, digits, etc.
            // After the variable reference, '=' is valid (capture aliasing)
            '$' | '@' => {
                prev_was_quantifier = false;
                prev_was_anchor = false;
                // $ can be an anchor (end-of-string) or $$ (end-of-line)
                if c == '$' {
                    if chars.peek() == Some(&'$') {
                        chars.next(); // consume second $
                        prev_was_anchor = true;
                        continue;
                    }
                    // Bare $ not followed by variable chars is an anchor
                    let is_var = chars.peek().is_some_and(|&ch| {
                        ch.is_alphabetic()
                            || ch == '_'
                            || ch == '<'
                            || ch == '{'
                            || ch == '('
                            || ch == '!'
                            || ch.is_ascii_digit()
                    });
                    if !is_var {
                        prev_was_anchor = true;
                        continue;
                    }
                }
                // Check for attribute interpolation: $!attr (prohibited in regex)
                if c == '$' && chars.peek() == Some(&'!') {
                    let mut symbol = String::from("$!");
                    let mut peeked = chars.clone();
                    peeked.next(); // skip '!'
                    while let Some(&ch) = peeked.peek() {
                        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                            symbol.push(ch);
                            peeked.next();
                        } else {
                            break;
                        }
                    }
                    if symbol.len() > 2 {
                        let msg = "Cannot interpolate attribute in a regex";
                        let mut attrs = HashMap::new();
                        attrs.insert("message".to_string(), Value::str(msg.to_string()));
                        attrs.insert("symbol".to_string(), Value::str(symbol.to_string()));
                        let ex = Value::make_instance(Symbol::intern("X::Attribute::Regex"), attrs);
                        let mut err = RuntimeError::new(msg);
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
                skip_variable_ref(&mut chars);
            }
            // Quantifiers
            '*' | '+' | '?' => {
                if prev_was_anchor {
                    return Err(make_non_quantifiable_error());
                }
                if prev_was_tilde {
                    let msg = "Quantifier quantifies nothing";
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.to_string()));
                    let ex = Value::make_instance(
                        Symbol::intern("X::Syntax::Regex::SolitaryQuantifier"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                if prev_was_quantifier {
                    return Err(make_quantifier_error(c));
                }
                prev_was_quantifier = true;
                prev_was_tilde = false;
                prev_was_anchor = false;
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
                prev_was_tilde = false;
                prev_was_anchor = false;
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
                prev_was_tilde = false;
                prev_was_anchor = false;
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
            // << and >> are word boundaries, not angle bracket pairs
            '<' if chars.peek() == Some(&'<') => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
                chars.next(); // consume second <
            }
            '>' if chars.peek() == Some(&'>') => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
                chars.next(); // consume second >
            }
            // Assertions/named rules — skip balanced <...>
            '<' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
                // Collect the content to check for longname aliases
                let mut content = String::new();
                let mut depth = 1u32;
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch == '<' {
                        depth += 1;
                        content.push(ch);
                    } else if ch == '>' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        content.push(ch);
                    } else {
                        content.push(ch);
                    }
                }
                // Check for attribute interpolation: <$!attr>
                let ct = content.trim();
                if ct.starts_with("$!") && ct.len() > 2 {
                    let msg = "Cannot interpolate attribute in a regex";
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.to_string()));
                    attrs.insert("symbol".to_string(), Value::str(ct.to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::Attribute::Regex"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                // Check for longname alias: <Name::Path=alias>
                if content.contains("::") && content.contains('=') {
                    let msg = "Can't use a long name as a regex alias";
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.to_string()));
                    let ex = Value::make_instance(
                        Symbol::intern("X::Syntax::Regex::Alias::LongName"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                // Validate bracket character classes for Perl 5 range syntax
                validate_bracket_class_content(ct)?;
            }
            // Grouping — skip balanced content
            '[' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
                skip_balanced(&mut chars, '[', ']');
            }
            '(' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
                skip_balanced(&mut chars, '(', ')');
            }
            '{' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
                // Collect content inside braces to check for $!attr
                let mut content = String::new();
                let mut depth = 1u32;
                while let Some(ch) = chars.next() {
                    if ch == '\\' {
                        content.push(ch);
                        if let Some(esc) = chars.next() {
                            content.push(esc);
                        }
                        continue;
                    }
                    if ch == '{' {
                        depth += 1;
                    } else if ch == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    content.push(ch);
                }
                // Check for $!attr inside the code block
                if let Some(pos) = content.find("$!") {
                    let after = &content[pos + 2..];
                    let symbol_name: String = after
                        .chars()
                        .take_while(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
                        .collect();
                    if !symbol_name.is_empty() {
                        let symbol = format!("$!{}", symbol_name);
                        let msg = "Cannot interpolate attribute in a regex";
                        let mut attrs = HashMap::new();
                        attrs.insert("message".to_string(), Value::str(msg.to_string()));
                        attrs.insert("symbol".to_string(), Value::str(symbol));
                        let ex = Value::make_instance(Symbol::intern("X::Attribute::Regex"), attrs);
                        let mut err = RuntimeError::new(msg);
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
            }
            // Closing brackets — just skip
            ']' | ')' | '}' | '>' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
            }
            // Comment
            '#' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
                for ch in chars.by_ref() {
                    if ch == '\n' {
                        break;
                    }
                }
            }
            // Modifier colon in regex (e.g., :i, :s, :11, :my $var = expr;)
            ':' => {
                prev_was_quantifier = false;
                prev_was_anchor = false;
                // Backtracking control modifier (e.g., [ ... ]:!).
                // It applies to the preceding atom and is valid as a standalone
                // modifier marker with no name.
                if chars.peek() == Some(&'!') {
                    chars.next();
                    continue;
                }
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
                    // Collect modifier name
                    let mut name = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                            name.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    // :my, :our, :constant — variable declarations in regex
                    // Skip everything up to and including the semicolon
                    if name == "my" || name == "our" || name == "constant" {
                        for ch in chars.by_ref() {
                            if ch == ';' {
                                break;
                            }
                        }
                    } else if chars.peek() == Some(&'(') {
                        // Skip optional argument in parens: :name(...)
                        chars.next();
                        skip_balanced(&mut chars, '(', ')');
                    } else if !name.is_empty()
                        && !is_known_regex_adverb(&name)
                        && chars.peek().is_none_or(|ch| {
                            ch.is_whitespace() || ch.is_alphanumeric() || *ch == '_'
                        })
                    {
                        // Unknown adverb like :iabc — need whitespace after modifier
                        return Err(make_unrecognized_modifier_error(&name));
                    }
                }
            }
            // Alphanumeric and underscore are valid as literals
            _ if c.is_alphanumeric() || c == '_' => {
                prev_was_quantifier = false;
                prev_was_tilde = false;
                prev_was_anchor = false;
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
/// Check if a name is a known regex inline adverb.
fn is_known_regex_adverb(name: &str) -> bool {
    matches!(
        name,
        "i" | "ignorecase"
            | "m"
            | "ignoremark"
            | "s"
            | "sigspace"
            | "r"
            | "ratchet"
            | "g"
            | "global"
            | "ii"
            | "samecase"
            | "ss"
            | "samespace"
            | "mm"
            | "samemark"
            | "dba"
    )
}

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
                r.is_empty()
                    || r.starts_with(|c: char| c.is_whitespace())
                    || r.starts_with(':')
                    || r.starts_with('/')
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
        'd' | 'D' | 'w' | 'W' | 's' | 'S' | 'n' | 'N' | 't' | 'T' | 'r' | 'R' | 'x' | 'o' | 'c'
        | 'C' | 'X' | 'O' | 'v' | 'V' | 'h' | 'H' | 'e' | 'E' | 'f' | 'F' | '0' => Ok(()),
        // Obsolete Perl 5 backslash sequences
        'b' => Err(RuntimeError::obsolete(
            "\\b as a word boundary",
            "<?wb> (word boundary) or <!wb> (not a word boundary)",
        )),
        'B' => Err(RuntimeError::obsolete(
            "\\B as a word boundary",
            "<?wb> (word boundary) or <!wb> (not a word boundary)",
        )),
        'A' => Err(RuntimeError::obsolete(
            "\\A as beginning-of-string matcher",
            "^",
        )),
        'Z' => Err(RuntimeError::obsolete(
            "\\Z as end-of-string matcher",
            "\\n?$",
        )),
        'z' => Err(RuntimeError::obsolete("\\z as end-of-string matcher", "$")),
        _ if !esc.is_ascii_alphabetic() => Ok(()), // non-alpha escapes are always valid
        _ => {
            // Unknown alphabetic backslash sequence
            let msg = format!("Unrecognized backslash sequence: \\{}", esc);
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex =
                Value::make_instance(Symbol::intern("X::Backslash::UnrecognizedSequence"), attrs);
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
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("metachar".to_string(), Value::str(metachar.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Regex::UnrecognizedMetachar"),
        attrs,
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

fn make_unrecognized_modifier_error(modifier: &str) -> RuntimeError {
    let msg = format!("Unrecognized regex modifier :{}", modifier);
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("modifier".to_string(), Value::str(modifier.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Regex::UnrecognizedModifier"),
        attrs,
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

fn make_non_quantifiable_error() -> RuntimeError {
    let msg = "Can only quantify a construct that produces a match".to_string();
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::NonQuantifiable"), attrs);
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
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Quantifier"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Validate bracket character class content for common errors.
/// Checks for Perl 5 range syntax (a-z instead of a..z), reversed ranges,
/// and missing `+` or `-` operators between class parts.
#[allow(clippy::result_large_err)]
fn validate_bracket_class_content(content: &str) -> Result<(), RuntimeError> {
    // Extract all bracket class bodies from the content
    // Content may be: [a-z], +[a..z]-[aeiou], etc.
    let mut remaining = content;
    while let Some(start) = remaining.find('[') {
        let after_bracket = &remaining[start + 1..];
        // Find the matching ']', handling backslash escapes and nested brackets
        let mut chars = after_bracket.chars().peekable();
        let mut body = String::new();
        let mut consumed = 0usize;
        let mut found_close = false;
        while let Some(c) = chars.next() {
            consumed += c.len_utf8();
            if c == '\\' {
                body.push(c);
                if let Some(esc) = chars.next() {
                    consumed += esc.len_utf8();
                    body.push(esc);
                    // Handle \c[...], \x[...], \o[...] — skip nested brackets
                    if (esc == 'c' || esc == 'C' || esc == 'x' || esc == 'X' || esc == 'o')
                        && chars.peek() == Some(&'[')
                    {
                        let bracket = chars.next().unwrap();
                        consumed += bracket.len_utf8();
                        body.push(bracket);
                        let mut depth = 1;
                        for ch in chars.by_ref() {
                            consumed += ch.len_utf8();
                            body.push(ch);
                            if ch == '[' {
                                depth += 1;
                            } else if ch == ']' {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                        }
                    }
                }
            } else if c == ']' {
                found_close = true;
                break;
            } else {
                body.push(c);
            }
        }
        if found_close {
            check_bracket_body_for_perl5_range(&body)?;
            check_bracket_body_for_nfg_synthetic(&body)?;
        }
        // Advance past the consumed content (bracket body + closing ']')
        remaining = &remaining[start + 1 + consumed..];
    }
    // Check for missing + or - between parts in combined class expressions.
    // E.g., <[abc] [def]>, <:Kata :Hira>, <+alpha digit>
    check_missing_operator(content)?;
    Ok(())
}

/// Check for missing `+` or `-` operator between character class parts.
/// Only applies to combined character class expressions (starting with `+`, `-`, `[`, or `:`).
#[allow(clippy::result_large_err)]
fn check_missing_operator(content: &str) -> Result<(), RuntimeError> {
    let trimmed = content.trim();
    // Only check content that looks like a character class expression.
    // Must start with [, +, -, or : followed by an uppercase letter (Unicode property).
    let is_charclass = trimmed.starts_with('[')
        || trimmed.starts_with('+')
        || trimmed.starts_with('-')
        || (trimmed.starts_with(':') && trimmed.chars().nth(1).is_some_and(|c| c.is_uppercase()));
    if !is_charclass {
        return Ok(());
    }
    // Simple bracket classes without multiple parts don't need operator checking
    if trimmed.starts_with('[') && !trimmed.contains("] ") && !trimmed.contains("]+") {
        return Ok(());
    }
    // Tokenize the content into parts to check for missing operators
    let mut remaining = trimmed;
    let mut had_part = false;
    while !remaining.is_empty() {
        let first = remaining.chars().next().unwrap();
        if first == '+' || first == '-' {
            remaining = &remaining[1..];
            remaining = remaining.trim_start();
            had_part = false;
        } else if first == '[' {
            // Skip over bracket class [...]
            if had_part {
                let msg = "Missing + or - in character class expression";
                return Err(RuntimeError::new(msg));
            }
            // Find matching ]
            remaining = &remaining[1..];
            let mut chars = remaining.chars();
            while let Some(c) = chars.next() {
                if c == '\\' {
                    chars.next(); // skip escaped
                } else if c == ']' {
                    break;
                }
            }
            remaining = chars.as_str().trim_start();
            had_part = true;
        } else if first == ':' && remaining.chars().nth(1).is_some_and(|c| c.is_uppercase()) {
            // Unicode property like :Kata, :Hira
            if had_part {
                let msg = "Missing + or - in character class expression";
                return Err(RuntimeError::new(msg));
            }
            let end = remaining
                .find(|c: char| !c.is_alphanumeric() && c != ':' && c != '-' && c != '_')
                .unwrap_or(remaining.len());
            remaining = remaining[end..].trim_start();
            had_part = true;
        } else if first.is_alphanumeric() && had_part {
            // Named class after another part without operator
            let msg = "Missing + or - in character class expression";
            return Err(RuntimeError::new(msg));
        } else if first.is_alphanumeric() {
            // Named class (e.g., alpha, digit, xdigit)
            let end = remaining
                .find(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
                .unwrap_or(remaining.len());
            remaining = remaining[end..].trim_start();
            had_part = true;
        } else {
            break;
        }
    }
    Ok(())
}

/// Check a bracket class body for Perl 5 range syntax (e.g., a-z instead of a..z).
#[allow(clippy::result_large_err)]
fn check_bracket_body_for_perl5_range(body: &str) -> Result<(), RuntimeError> {
    // Iterate through the body, skipping escape sequences like \c[...], \x[...]
    let mut chars = body.chars().peekable();
    let mut prev_was_alnum = false;
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Skip the entire escape sequence
            if let Some(&esc) = chars.peek() {
                chars.next();
                if (esc == 'c' || esc == 'C' || esc == 'x' || esc == 'X' || esc == 'o')
                    && chars.peek() == Some(&'[')
                {
                    // Skip over \c[...], \x[...], \o[...] content
                    chars.next(); // skip '['
                    let mut depth = 1;
                    for ch in chars.by_ref() {
                        if ch == '[' {
                            depth += 1;
                        } else if ch == ']' {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                    }
                }
                // After an escape, the resulting char might be alnum but we don't
                // want to trigger the range check for it
                prev_was_alnum = false;
            }
        } else if c == '-' {
            // Check if this looks like a Perl 5 range: alnum-alnum
            if prev_was_alnum {
                // Check the next char (skip spaces)
                let mut peek = chars.clone();
                while peek.peek() == Some(&' ') {
                    peek.next();
                }
                if let Some(&next_ch) = peek.peek()
                    && next_ch.is_alphanumeric()
                    && next_ch != '.'
                {
                    let msg =
                        "Unsupported use of - as character range. In Raku please use: .. for range";
                    return Err(RuntimeError::new(msg));
                }
            }
            prev_was_alnum = false;
        } else {
            prev_was_alnum = c.is_alphanumeric();
        }
    }
    Ok(())
}

/// Check a bracket class body for NFG synthetic characters (base char + combining marks).
/// These cannot be used as range endpoints.
#[allow(clippy::result_large_err)]
fn check_bracket_body_for_nfg_synthetic(body: &str) -> Result<(), RuntimeError> {
    let mut chars = body.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Skip escape sequences
            if let Some(&esc) = chars.peek() {
                chars.next();
                if matches!(esc, 'c' | 'C' | 'x' | 'X' | 'o') && chars.peek() == Some(&'[') {
                    chars.next(); // skip '['
                    let mut depth = 1;
                    for ch in chars.by_ref() {
                        if ch == '[' {
                            depth += 1;
                        } else if ch == ']' {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                    }
                }
            }
        } else if c == ' ' {
            // Skip whitespace
        } else if chars
            .peek()
            .is_some_and(|ch| unicode_normalization::char::is_combining_mark(*ch))
        {
            // Base character followed by combining mark(s) — NFG synthetic
            let mut grapheme = c.to_string();
            while chars
                .peek()
                .is_some_and(|ch| unicode_normalization::char::is_combining_mark(*ch))
            {
                grapheme.push(chars.next().unwrap());
            }
            let msg = format!(
                "Cannot use {} as a range endpoint, as it is not a single codepoint",
                grapheme
            );
            return Err(RuntimeError::new(msg));
        }
    }
    Ok(())
}

/// Check if a regex pattern contains bare `<sym>` or `<.sym>` usage
/// (which is only valid inside a proto regex with :sym<> adverb).
#[allow(clippy::result_large_err)]
fn check_bare_sym_usage(source: &str) -> Result<(), RuntimeError> {
    let mut chars = source.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            // Skip quoted strings inside regex
            '"' => {
                while let Some(ch) = chars.next() {
                    if ch == '\\' {
                        chars.next(); // skip escaped char
                    } else if ch == '"' {
                        break;
                    }
                }
            }
            '\'' => {
                while let Some(ch) = chars.next() {
                    if ch == '\\' {
                        chars.next();
                    } else if ch == '\'' {
                        break;
                    }
                }
            }
            '<' => {
                // Read content up to matching >
                let mut name = String::new();
                let mut depth = 1usize;
                for ch in chars.by_ref() {
                    if ch == '<' {
                        depth += 1;
                        name.push(ch);
                    } else if ch == '>' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        name.push(ch);
                    } else {
                        name.push(ch);
                    }
                }
                let trimmed = name.trim();
                if trimmed == "sym" || trimmed == ".sym" {
                    let msg = "Can only use \"<sym>\" token in a proto regex";
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Proto"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
            _ => {}
        }
    }
    Ok(())
}
