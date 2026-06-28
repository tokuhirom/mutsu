use super::*;

/// Parse a sub name, which can be a regular identifier or an operator-style name
/// like `infix:<+>`, `prefix:<->`, `postfix:<++>`, `circumfix:<[ ]>`.
pub(crate) fn parse_sub_name(input: &str) -> PResult<'_, String> {
    let (rest, name) = parse_sub_name_inner(input)?;
    if let Some(err) = null_operator_error(&name) {
        return Err(err);
    }
    Ok((rest, name))
}

/// Detect an operator declaration with an empty (whitespace-only) operator
/// symbol, e.g. `infix:sym< >` or `infix:< >` -> X::Syntax::Extension::Null.
pub(crate) fn null_operator_error(name: &str) -> Option<PError> {
    const CATEGORIES: &[&str] = &[
        "infix",
        "prefix",
        "postfix",
        "term",
        "circumfix",
        "postcircumfix",
        "trait_mod",
        "trait_auxiliary",
    ];
    let colon = name.find(':')?;
    let cat = &name[..colon];
    if !CATEGORIES.contains(&cat) {
        return None;
    }
    // After the category, the symbol is `<SYM>`, `«SYM»`, or `sym<SYM>`.
    let after = &name[colon + 1..];
    let after = after.strip_prefix("sym").unwrap_or(after);
    let inner = if let Some(s) = after.strip_prefix('<') {
        s.strip_suffix('>')?
    } else if let Some(s) = after.strip_prefix('\u{ab}') {
        s.strip_suffix('\u{bb}')?
    } else {
        return None;
    };
    if !inner.trim().is_empty() {
        return None;
    }
    let panic_message = "Null operator is not allowed".to_string();
    let mut panic_attrs = std::collections::HashMap::new();
    panic_attrs.insert("message".to_string(), Value::str(panic_message.clone()));
    let panic = Value::make_instance(Symbol::intern("X::Syntax::Extension::Null"), panic_attrs);

    let mut worry_attrs = std::collections::HashMap::new();
    worry_attrs.insert(
        "payload".to_string(),
        Value::str("Pair with <> really means an empty list, not null string".to_string()),
    );
    let worry = Value::make_instance(Symbol::intern("X::AdHoc"), worry_attrs);

    let group_message = panic_message.clone();
    let mut group_attrs = std::collections::HashMap::new();
    group_attrs.insert("sorrows".to_string(), Value::array(vec![]));
    group_attrs.insert("worries".to_string(), Value::array(vec![worry]));
    group_attrs.insert("panic".to_string(), panic);
    group_attrs.insert("message".to_string(), Value::str(group_message.clone()));
    let group = Value::make_instance(Symbol::intern("X::Comp::Group"), group_attrs);
    Some(PError::fatal_with_exception(group_message, Box::new(group)))
}

pub(crate) fn parse_sub_name_inner(input: &str) -> PResult<'_, String> {
    let (rest, base) = if let Ok((rest, base)) = ident(input) {
        (rest, base)
    } else {
        let (rest, base) =
            take_while1(input, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        (rest, base.to_string())
    };
    // Handle package-qualified names like List::foo or Foo::Bar::baz
    let (rest, base) = {
        let mut rest = rest;
        let mut name = base;
        while rest.starts_with("::") {
            let after_colons = &rest[2..];
            if let Ok((r, part)) = ident(after_colons) {
                name.push_str("::");
                name.push_str(&part);
                rest = r;
            } else if let Ok((r, part)) = take_while1(after_colons, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            }) {
                name.push_str("::");
                name.push_str(part);
                rest = r;
            } else {
                break;
            }
        }
        (rest, name)
    };
    // Check for colonpair adverbs like :sym<foo> or :sym«baz»
    let (rest, base) = {
        let mut rest = rest;
        let mut name = base;
        while rest.starts_with(':') && !rest.starts_with(":<") && !rest.starts_with(":<<") {
            let r = &rest[1..];
            if let Ok((r, part)) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                let mut r2 = r;
                if r2.starts_with("<<") {
                    // <<...>> delimiter (ASCII double-angle quotes)
                    let after_open = &r2[2..];
                    if let Some(end) = after_open.find(">>") {
                        // Store as «...» internally for consistency
                        name.push(':');
                        name.push_str(part);
                        name.push('\u{ab}');
                        name.push_str(&after_open[..end]);
                        name.push('\u{bb}');
                        r2 = &after_open[end + 2..];
                        rest = r2;
                        continue;
                    }
                } else if r2.starts_with('<')
                    && let Some(end) = r2.find('>')
                {
                    name.push(':');
                    name.push_str(part);
                    name.push_str(&r2[..=end]);
                    r2 = &r2[end + 1..];
                    rest = r2;
                    continue;
                } else if r2.starts_with('\u{ab}') {
                    let after_open = &r2['\u{ab}'.len_utf8()..];
                    if let Some(end) = after_open.find('\u{bb}') {
                        name.push(':');
                        name.push_str(part);
                        name.push('\u{ab}');
                        name.push_str(&after_open[..end]);
                        name.push('\u{bb}');
                        r2 = &after_open[end + '\u{bb}'.len_utf8()..];
                        rest = r2;
                        continue;
                    }
                }
            }
            break;
        }
        (rest, name)
    };
    // Check for operator category names followed by :<...>
    let is_op_category = matches!(
        base.as_str(),
        "infix"
            | "prefix"
            | "postfix"
            | "term"
            | "circumfix"
            | "postcircumfix"
            | "trait_mod"
            | "trait_auxiliary"
    );
    if is_op_category && rest.starts_with(":<") {
        // Check for :<<...>> (French-quotes / double-angle-bracket) delimiter
        // In Raku, <<>> is an alternate quoting form; the content is the operator symbol.
        // e.g. infix:<< - >> is the same as infix:<->
        if let Some(after_open) = rest.strip_prefix(":<<")
            && let Some(end_pos) = after_open.find(">>")
        {
            let raw_symbol = after_open[..end_pos].trim();
            let after_close = &after_open[end_pos + 2..];
            // Resolve compile-time constants: $var or {name}
            let op_symbol = resolve_operator_symbol(raw_symbol);
            let full_name = format!("{}:<{}>", base, op_symbol);
            return Ok((after_close, full_name));
        }
        // Scan for matching '>' — handle nested <> pairs
        let after_open = &rest[2..];
        let mut depth = 1u32;
        let mut chars = after_open.char_indices();
        while let Some((i, c)) = chars.next() {
            match c {
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        let op_symbol = &after_open[..i];
                        let after_close = &after_open[i + 1..];
                        let full_name = format!("{}:<{}>", base, op_symbol);
                        return Ok((after_close, full_name));
                    }
                }
                '<' => depth += 1,
                '\\' => {
                    chars.next();
                }
                _ => {}
            }
        }
        // If we can't find the closing '>', fall through to return the base name
    }
    // Guillemet form: infix:«...» is equivalent to infix:<...>
    // Supports backslash-escaped » inside the delimiters (e.g. «~~>\»»)
    if is_op_category && let Some(after_open) = rest.strip_prefix(":\u{ab}") {
        let mut chars = after_open.char_indices();
        let mut found_end = None;
        while let Some((i, c)) = chars.next() {
            match c {
                '\u{bb}' => {
                    found_end = Some(i);
                    break;
                }
                '\\' => {
                    chars.next(); // skip escaped character
                }
                _ => {}
            }
        }
        if let Some(end_pos) = found_end {
            let raw_symbol = after_open[..end_pos].trim();
            let after_close = &after_open[end_pos + '\u{bb}'.len_utf8()..];
            // Unescape backslash sequences (e.g. \» → »)
            let op_symbol = unescape_guillemet_content(raw_symbol);
            let full_name = format!("{}:<{}>", base, op_symbol);
            return Ok((after_close, full_name));
        }
    }
    // Handle all bracket forms: :['op'], :["op"], :["op1", "op2"], :[sym], :[sym1, sym2]
    if is_op_category
        && rest.starts_with(":[")
        && let Some(result) = parse_bracket_op_name(&rest[2..], &base)
    {
        return Ok(result);
    }
    Ok((rest, base))
}

/// Validate that circumfix/postcircumfix operators have exactly 2 delimiter parts.
pub(crate) fn validate_categorical_parts(name: &str) -> Result<(), PError> {
    // Determine the category and expected number of parts
    let (category, expected) =
        if name.starts_with("circumfix:<") || name.starts_with("postcircumfix:<") {
            let cat = if name.starts_with("circumfix") {
                "circumfix"
            } else {
                "postcircumfix"
            };
            (cat, 2usize)
        } else if name.starts_with("infix:<") {
            ("infix", 1)
        } else if name.starts_with("prefix:<") {
            ("prefix", 1)
        } else if name.starts_with("postfix:<") {
            ("postfix", 1)
        } else if name.starts_with("term:<") {
            ("term", 1)
        } else {
            return Ok(());
        };

    let delim_start = name.find(":<").unwrap() + 2;
    if let Some(delims) = name[delim_start..].strip_suffix('>') {
        let parts: Vec<&str> = delims.split_whitespace().collect();
        if parts.len() > expected {
            return Err(PError::fatal(format!(
                "X::Syntax::AddCategorical::TooManyParts: Too many symbols provided for categorical of type {}; needs only {}",
                category, expected
            )));
        }
        if parts.len() < expected {
            return Err(PError::fatal(format!(
                "X::Syntax::AddCategorical::TooFewParts: Not enough symbols provided for categorical of type {}; needs {}",
                category, expected
            )));
        }
    }
    Ok(())
}

/// Resolve compile-time constants in operator symbol names.
/// Handles `$var` (scalar constant) and `{name}` (sigilless constant).
pub(crate) fn resolve_operator_symbol(raw: &str) -> String {
    let trimmed = raw.trim();
    // $variable form: look up as compile-time constant
    if let Some(bare_name) = trimmed.strip_prefix('$') {
        // Constants are stored without the $ sigil
        if let Some(value) = super::super::simple::lookup_compile_time_constant(bare_name) {
            return value;
        }
        // Also try with the sigil
        if let Some(value) = super::super::simple::lookup_compile_time_constant(trimmed) {
            return value;
        }
    }
    // {name} form: look up sigilless constant
    if let Some(inner) = trimmed.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
        let inner = inner.trim();
        if let Some(value) = super::super::simple::lookup_compile_time_constant(inner) {
            return value;
        }
    }
    trimmed.to_string()
}

/// Unescape backslash sequences inside guillemet (« ») delimiters.
/// In this context, backslash only escapes the closing » and itself.
pub(crate) fn unescape_guillemet_content(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(next) = chars.next() {
                out.push(next);
            }
        } else {
            out.push(c);
        }
    }
    out
}
