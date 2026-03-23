use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};
use crate::token_kind::{TokenKind, lookup_unicode_char_by_name};

use crate::ast::{Expr, ParamDef, Stmt, collect_placeholders};
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::add_parse_warning;
use super::{block, block_inner, ident, keyword, parse_raku_ident};

/// Known valid parameter traits for `is <trait>` in signatures.
const VALID_PARAM_TRAITS: &[&str] = &["rw", "readonly", "copy", "required", "raw"];

/// Public wrapper for `validate_param_trait` used by control.rs.
pub(super) fn validate_param_trait_pub<'a>(
    trait_name: &str,
    existing_traits: &[String],
    input: &'a str,
) -> PResult<'a, ()> {
    validate_param_trait(trait_name, existing_traits, input)
}

/// Validate a parameter trait name, returning a fatal parse error for unknown traits.
/// Also warns on duplicate traits.
pub(super) fn validate_param_trait<'a>(
    trait_name: &str,
    existing_traits: &[String],
    input: &'a str,
) -> PResult<'a, ()> {
    if !VALID_PARAM_TRAITS.contains(&trait_name) {
        return Err(PError::fatal(format!(
            "Can't use unknown trait 'is' -> '{}' in a parameter declaration",
            trait_name
        )));
    }
    if existing_traits.iter().any(|t| t == trait_name) {
        add_parse_warning(format!(
            "Potential difficulties:\n    Duplicate 'is {}' trait",
            trait_name
        ));
    }
    Ok((input, ()))
}

fn static_default_type(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(Value::Int(_)) => Some("Int".to_string()),
        Expr::Literal(Value::Num(_)) => Some("Num".to_string()),
        Expr::Literal(Value::Rat(_, _)) => Some("Rat".to_string()),
        Expr::Literal(Value::Bool(_)) => Some("Bool".to_string()),
        Expr::Literal(Value::Str(_)) => Some("Str".to_string()),
        Expr::Literal(Value::Package(name)) => Some(name.resolve()),
        Expr::AnonSub { .. } | Expr::AnonSubParams { .. } => Some("Callable".to_string()),
        _ => None,
    }
}

fn negate_literal_value(value: &Value) -> Option<Value> {
    match value {
        Value::Int(n) => Some(Value::Int(-n)),
        Value::BigInt(n) => Some(Value::bigint(-n.as_ref())),
        Value::Num(n) => Some(Value::Num(-n)),
        Value::Rat(n, d) => Some(crate::value::make_rat(-n, *d)),
        Value::FatRat(n, d) => Some(Value::FatRat(-n, *d)),
        Value::BigRat(n, d) => Some(crate::value::make_big_rat(-n.clone(), d.clone())),
        Value::Complex(re, im) => Some(Value::Complex(-re, -im)),
        _ => None,
    }
}

pub(super) fn literal_value_from_expr(expr: &Expr) -> Option<Value> {
    match expr {
        Expr::Literal(v) => Some(v.clone()),
        Expr::Unary { op, expr } => {
            let inner = literal_value_from_expr(expr)?;
            match op {
                TokenKind::Plus => Some(inner),
                TokenKind::Minus => negate_literal_value(&inner),
                _ => None,
            }
        }
        _ => None,
    }
}

fn constraint_base_type(constraint: &str) -> &str {
    let bytes = constraint.as_bytes();
    let mut end = bytes.len();
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];
        if b == b'[' || b == b'(' {
            end = i;
            break;
        }
        if b == b':' {
            let prev_is_colon = i > 0 && bytes[i - 1] == b':';
            let next_is_colon = i + 1 < bytes.len() && bytes[i + 1] == b':';
            if !prev_is_colon && !next_is_colon {
                end = i;
                break;
            }
        }
        i += 1;
    }
    &constraint[..end]
}

fn default_type_matches_constraint(
    constraint: &str,
    default_type: &str,
    default_value: Option<&Value>,
) -> Option<bool> {
    let base = constraint_base_type(constraint);
    if matches!(base, "Any" | "Mu" | "Cool") {
        return Some(true);
    }
    if base == "UInt" {
        if let Some(value) = default_value {
            return Some(match value {
                Value::Int(n) => *n >= 0,
                Value::BigInt(n) => n.sign() != num_bigint::Sign::Minus,
                _ => false,
            });
        }
        return Some(default_type == "Int");
    }
    if base == default_type {
        return Some(true);
    }
    if base.contains("::") {
        return Some(false);
    }
    match base {
        "Int" | "UInt" | "Bool" | "Str" | "List" | "Array" | "Hash" | "Pair" | "Junction"
        | "Complex" | "Signature" | "Capture" => Some(false),
        "Numeric" | "Real" => Some(matches!(default_type, "Int" | "Num" | "Rat")),
        "Num" => Some(matches!(default_type, "Int" | "Num" | "Rat")),
        "Rat" => Some(matches!(default_type, "Int" | "Rat")),
        "Callable" | "Code" | "Block" => Some(default_type == "Callable"),
        _ => None,
    }
}

pub(super) fn validate_signature_params(params: &[ParamDef]) -> Result<(), PError> {
    let mut saw_optional_positional = false;
    for pd in params {
        // Reject $? twigil parameters (compile-time variables like $?VERSION, $?FILE)
        // but allow $! (error variable / attribute twigil) since $! is a valid param name
        if pd.name.starts_with('?') {
            let msg = format!(
                "X::Parameter::Twigil: In signature parameter $?{}, it is illegal to use the '?' twigil",
                pd.name.strip_prefix('?').unwrap_or(&pd.name)
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Parameter::Twigil"),
                attrs,
            );
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
        if pd.traits.iter().any(|t| t == "rw") && (pd.optional_marker || pd.default.is_some()) {
            return Err(PError::fatal(
                "X::Trait::Invalid: Cannot make an 'is rw' parameter optional".to_string(),
            ));
        }
        if let (Some(constraint), Some(default_expr)) = (&pd.type_constraint, &pd.default)
            && let Some(default_type) = static_default_type(default_expr)
            && matches!(
                default_type_matches_constraint(
                    constraint,
                    &default_type,
                    literal_value_from_expr(default_expr).as_ref(),
                ),
                Some(false)
            )
        {
            return Err(PError::fatal(format!(
                "X::Parameter::Default::TypeCheck: Default value type '{}' does not satisfy '{}'",
                default_type, constraint
            )));
        }
        if pd.named || pd.slurpy {
            continue;
        }
        let is_optional_positional = pd.optional_marker || pd.default.is_some();
        if saw_optional_positional && !is_optional_positional {
            return Err(PError::fatal(
                "X::Parameter::WrongOrder: required positional parameters must come before optional positional parameters"
                    .to_string(),
            ));
        }
        if is_optional_positional {
            saw_optional_positional = true;
        }
    }
    Ok(())
}

/// Parse a sub name, which can be a regular identifier or an operator-style name
/// like `infix:<+>`, `prefix:<->`, `postfix:<++>`, `circumfix:<[ ]>`.
pub(super) fn parse_sub_name(input: &str) -> PResult<'_, String> {
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
                if r2.starts_with('<')
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
    if is_op_category
        && rest.starts_with(":[")
        && let Some(after_open) = rest.strip_prefix(":['")
        && let Some(end_pos) = after_open.find("']")
    {
        let op_symbol = unescape_operator_single_quoted(&after_open[..end_pos]);
        let after_close = &after_open[end_pos + 2..];
        let full_name = format!("{}:<{}>", base, op_symbol);
        return Ok((after_close, full_name));
    }
    if is_op_category
        && rest.starts_with(":[")
        && let Some(after_open) = rest.strip_prefix(":[\"")
        && let Some(end_pos) = after_open.find("\"]")
    {
        let op_symbol = unescape_operator_double_quoted(&after_open[..end_pos]);
        let after_close = &after_open[end_pos + 2..];
        let full_name = format!("{}:<{}>", base, op_symbol);
        return Ok((after_close, full_name));
    }
    // Bare identifier form: infix:[sym] or circumfix:[sym1, sym2]
    // where the identifiers are compile-time constants
    if is_op_category
        && let Some(after_open) = rest.strip_prefix(":[")
        && let Some(end_pos) = after_open.find(']')
    {
        let content = after_open[..end_pos].trim();
        // Check if content contains comma-separated parts (for circumfix)
        let parts: Vec<&str> = content.split(',').map(|s| s.trim()).collect();
        let mut resolved_parts = Vec::new();
        let mut all_resolved = true;
        for part in &parts {
            let p = part.trim_matches(|c: char| c == '"' || c == '\'');
            // Try to resolve as a compile-time constant
            if let Some(value) = super::simple::lookup_compile_time_constant(p) {
                resolved_parts.push(value);
            } else if let Some(bare) = p.strip_prefix('$') {
                if let Some(value) = super::simple::lookup_compile_time_constant(bare) {
                    resolved_parts.push(value);
                } else if let Some(value) = super::simple::lookup_compile_time_constant(p) {
                    resolved_parts.push(value);
                } else {
                    all_resolved = false;
                    break;
                }
            } else {
                all_resolved = false;
                break;
            }
        }
        if all_resolved && !resolved_parts.is_empty() {
            let after_close = &after_open[end_pos + 1..];
            let op_symbol = resolved_parts.join(" ");
            let full_name = format!("{}:<{}>", base, op_symbol);
            return Ok((after_close, full_name));
        }
    }
    Ok((rest, base))
}

/// Validate that circumfix/postcircumfix operators have exactly 2 delimiter parts.
fn validate_categorical_parts(name: &str) -> Result<(), PError> {
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
fn resolve_operator_symbol(raw: &str) -> String {
    let trimmed = raw.trim();
    // $variable form: look up as compile-time constant
    if let Some(bare_name) = trimmed.strip_prefix('$') {
        // Constants are stored without the $ sigil
        if let Some(value) = super::simple::lookup_compile_time_constant(bare_name) {
            return value;
        }
        // Also try with the sigil
        if let Some(value) = super::simple::lookup_compile_time_constant(trimmed) {
            return value;
        }
    }
    // {name} form: look up sigilless constant
    if let Some(inner) = trimmed.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
        let inner = inner.trim();
        if let Some(value) = super::simple::lookup_compile_time_constant(inner) {
            return value;
        }
    }
    trimmed.to_string()
}

/// Unescape backslash sequences inside guillemet (« ») delimiters.
/// In this context, backslash only escapes the closing » and itself.
fn unescape_guillemet_content(s: &str) -> String {
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

fn unescape_operator_single_quoted(s: &str) -> String {
    s.replace("\\'", "'").replace("\\\\", "\\")
}

fn unescape_operator_double_quoted(s: &str) -> String {
    let mut out = String::new();
    let mut rest = s;
    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
            let c = rest.as_bytes()[1] as char;
            match c {
                'n' => {
                    out.push('\n');
                    rest = &rest[2..];
                    continue;
                }
                't' => {
                    out.push('\t');
                    rest = &rest[2..];
                    continue;
                }
                'r' => {
                    out.push('\r');
                    rest = &rest[2..];
                    continue;
                }
                '0' => {
                    out.push('\0');
                    rest = &rest[2..];
                    continue;
                }
                '"' => {
                    out.push('"');
                    rest = &rest[2..];
                    continue;
                }
                '\\' => {
                    out.push('\\');
                    rest = &rest[2..];
                    continue;
                }
                'x' => {
                    let r = &rest[2..];
                    if let Some(r2) = r.strip_prefix('[')
                        && let Some(end) = r2.find(']')
                    {
                        for part in r2[..end].split(',') {
                            if let Ok(n) = u32::from_str_radix(part.trim(), 16)
                                && let Some(ch) = char::from_u32(n)
                            {
                                out.push(ch);
                            }
                        }
                        rest = &r2[end + 1..];
                        continue;
                    }
                }
                'c' => {
                    let r = &rest[2..];
                    if let Some(r2) = r.strip_prefix('[')
                        && let Some(end) = r2.find(']')
                    {
                        let names = &r2[..end];
                        let mut ok = true;
                        for part in names.split(',') {
                            let name = part.trim();
                            if name.is_empty() {
                                continue;
                            }
                            if let Some(ch) = lookup_unicode_char_by_name(name) {
                                out.push(ch);
                            } else {
                                ok = false;
                                break;
                            }
                        }
                        if ok {
                            rest = &r2[end + 1..];
                            continue;
                        }
                    }
                }
                _ => {}
            }
            out.push('\\');
            out.push(c);
            rest = &rest[2..];
            continue;
        }
        let ch = rest
            .chars()
            .next()
            .expect("rest is non-empty when decoding operator name");
        out.push(ch);
        rest = &rest[ch.len_utf8()..];
    }
    out
}

pub(super) fn parse_indirect_decl_name(input: &str) -> PResult<'_, (String, Expr)> {
    let rest = input
        .strip_prefix("::")
        .ok_or_else(|| PError::expected("indirect declarator name"))?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = parse_char(rest, '(')?;
    let (rest, _) = ws(rest)?;
    let (rest, expr) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = parse_char(rest, ')')?;
    let name = match &expr {
        Expr::Literal(Value::Str(s)) => s.to_string(),
        Expr::BareWord(s) => s.clone(),
        _ => "__INDIRECT_DECL_NAME__".to_string(),
    };
    Ok((rest, (name, expr)))
}

/// Parse `sub` declaration.
pub(super) fn sub_decl(input: &str) -> PResult<'_, Stmt> {
    sub_decl_with_semicolon_mode(input, false)
}

pub(super) fn top_level_main_semicolon_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest_after_prefix, _) = if let Some(r) = keyword("unit", input) {
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        (input, false)
    };
    let input_len = rest_after_prefix.len();
    let (rest, stmt) = sub_decl_with_semicolon_mode(rest_after_prefix, true)?;
    let consumed_len = input_len.saturating_sub(rest.len());
    let consumed = &rest_after_prefix[..consumed_len];
    let has_semicolon_terminator = consumed.trim_end().ends_with(';');
    if has_semicolon_terminator
        && matches!(&stmt, Stmt::SubDecl { name, multi, .. } if name == "MAIN" && !*multi)
    {
        Ok((rest, stmt))
    } else {
        Err(PError::expected("unit-scoped MAIN sub declaration"))
    }
}

/// Detect anonymous routines declared with `only`, `multi`, or `proto` and
/// throw a fatal X::Anon::Multi parse error, matching Raku's behavior.
pub(super) fn anon_multi_check(input: &str) -> PResult<'_, Stmt> {
    // Try each declarator keyword
    let declarators = ["only", "multi", "proto"];
    for declarator in &declarators {
        if let Some(after_kw) = keyword(declarator, input) {
            // After the declarator, optionally consume whitespace + routine type
            let after_kw_ws = ws(after_kw).map(|(r, _)| r).unwrap_or(after_kw);
            let after_type = if let Some(r) = keyword("sub", after_kw_ws) {
                ws(r).map(|(r, _)| r).unwrap_or(r)
            } else if let Some(r) = keyword("method", after_kw_ws) {
                ws(r).map(|(r, _)| r).unwrap_or(r)
            } else {
                after_kw_ws
            };
            // If what follows is `{` or `(`, this is an anonymous routine
            if after_type.starts_with('{') || after_type.starts_with('(') {
                return Err(PError::fatal(format!(
                    "FATAL:X::Anon::Multi: An anonymous routine may not take a {} declarator",
                    declarator
                )));
            }
        }
    }
    Err(PError::expected("anonymous multi check"))
}

pub(super) fn sub_decl_with_semicolon_mode(
    input: &str,
    allow_main_semicolon_decl: bool,
) -> PResult<'_, Stmt> {
    let (input, supersede) = if let Some(r) = keyword("supersede", input) {
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        (input, false)
    };
    let (rest, multi) = if let Some(r) = keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = keyword("sub", r).unwrap_or(r);
        let (r, _) = ws(r)?;
        (r, true)
    } else if let Some(r) = keyword("only", input) {
        // `only` is functionally equivalent to a regular sub declaration
        let (r, _) = ws1(r)?;
        let r = keyword("sub", r).unwrap_or(r);
        let (r, _) = ws(r)?;
        (r, false)
    } else {
        let r = keyword("sub", input).ok_or_else(|| PError::expected("sub declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    sub_decl_body(rest, multi, supersede, allow_main_semicolon_decl)
}

pub(super) fn sub_decl_body(
    input: &str,
    multi: bool,
    supersede: bool,
    allow_main_semicolon_decl: bool,
) -> PResult<'_, Stmt> {
    let (rest, name, name_expr) = if input.starts_with("::") {
        let (rest, (name, expr)) = parse_indirect_decl_name(input)?;
        (rest, name, Some(expr))
    } else {
        let (rest, name) = parse_sub_name(input)?;
        // Validate circumfix/postcircumfix operator part count
        validate_categorical_parts(&name)?;
        // Register user-declared sub so it can be called without parens later
        super::simple::register_user_sub(&name);
        super::simple::register_user_callable_term_symbol(&name);
        (rest, name, None)
    };
    let (rest, _) = ws(rest)?;

    // Parse params
    let has_explicit_signature = rest.starts_with('(');
    let (rest, (params, param_defs, return_type)) = if has_explicit_signature {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, (pd, rt)) = parse_param_list_with_return(r)?;
        validate_signature_params(&pd)?;
        reject_invocant_in_sub(&pd)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd, rt))
    } else {
        (rest, (Vec::new(), Vec::new(), None))
    };

    let (rest, _) = ws(rest)?;
    // Parse traits (is test-assertion, is export, returns ..., etc.)
    let (rest, traits) = parse_sub_traits(rest)?;
    if let Some(assoc) = traits.associativity.as_ref() {
        super::simple::register_user_infix_assoc(&name, assoc);
    }
    if traits.is_test_assertion {
        super::simple::register_user_test_assertion_sub(&name);
    }
    // Register precedence trait if present
    if let Some((trait_name, ref_op)) = &traits.precedence_trait
        && let Some(ref_level) = super::simple::resolve_op_precedence(ref_op)
    {
        let level = match trait_name.as_str() {
            "tighter" => ref_level + 5,
            "looser" => ref_level - 5,
            _ => ref_level,
        };
        super::simple::register_op_precedence(&name, level);
    }
    let (rest, _) = ws(rest)?;
    let mut signature_alternates: Vec<(Vec<String>, Vec<ParamDef>)> = Vec::new();
    let rest = if multi {
        // Multi declarators can chain additional signatures with `| (...)`.
        let mut r = rest;
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with('|') {
                break r2;
            }
            let (r3, _) = ws(&r2[1..])?;
            if !r3.starts_with('(') {
                return Err(PError::expected("alternate signature after '|'"));
            }
            let (r4, _) = parse_char(r3, '(')?;
            let (r4, _) = ws(r4)?;
            let (r4, alt_param_defs) = parse_param_list(r4)?;
            validate_signature_params(&alt_param_defs)?;
            let (r4, _) = ws(r4)?;
            let (r4, _) = parse_char(r4, ')')?;
            let alt_params: Vec<String> = alt_param_defs.iter().map(|p| p.name.clone()).collect();
            signature_alternates.push((alt_params, alt_param_defs));
            r = r4;
        }
    } else {
        rest
    };
    // Detect forward declaration: `sub name(...);`.
    if let Some(rest) = rest.strip_prefix(';') {
        // Merge return type for forward declarations too
        let fwd_return_type = return_type.clone().or(traits.return_type.clone());
        if allow_main_semicolon_decl && name == "MAIN" && !multi {
            return Ok((
                rest,
                Stmt::SubDecl {
                    name: Symbol::intern(&name),
                    name_expr,
                    params,
                    param_defs,
                    return_type: fwd_return_type,
                    associativity: traits.associativity.clone(),
                    signature_alternates,
                    body: Vec::new(),
                    multi,
                    is_rw: traits.is_rw,
                    is_raw: traits.is_raw,
                    is_export: traits.is_export,
                    export_tags: traits.export_tags.clone(),
                    is_test_assertion: traits.is_test_assertion,
                    supersede,
                    custom_traits: traits.custom_traits.clone(),
                },
            ));
        }
        if !has_explicit_signature {
            return Err(PError::raw(
                "X::UnitScope::Invalid: A unit-scoped sub definition is not allowed except on a MAIN sub; \
                 Please use the block form. If you did not mean to declare a unit-scoped sub, \
                 perhaps you accidentally placed a semicolon after routine's definition?"
                    .to_string(),
                Some(rest.len()),
            ));
        }
        if name == "MAIN" && !allow_main_semicolon_decl {
            return Err(PError::raw(
                "X::UnitScope::Invalid: A unit-scoped sub definition is not allowed except on a MAIN sub; \
                 Please use the block form. If you did not mean to declare a unit-scoped sub, \
                 perhaps you accidentally placed a semicolon after routine's definition?"
                    .to_string(),
                Some(rest.len()),
            ));
        }
        return Ok((
            rest,
            Stmt::SubDecl {
                name: Symbol::intern(&name),
                name_expr,
                params,
                param_defs,
                return_type: fwd_return_type,
                associativity: traits.associativity.clone(),
                signature_alternates,
                body: Vec::new(),
                multi,
                is_rw: traits.is_rw,
                is_raw: traits.is_raw,
                is_export: traits.is_export,
                export_tags: traits.export_tags.clone(),
                is_test_assertion: traits.is_test_assertion,
                supersede,
                custom_traits: traits.custom_traits.clone(),
            },
        ));
    }
    if rest.is_empty() {
        return Err(PError::expected("sub body '{ ... }'"));
    }
    let (rest, body) = if param_defs.iter().any(|p| p.sigilless) {
        // When there are sigilless params, we need to register them as term
        // symbols in the block scope so they shadow keywords (e.g. \return).
        let (r, _) = parse_char(rest, '{')?;
        super::simple::push_scope();
        for pd in &param_defs {
            if pd.sigilless {
                super::simple::register_user_term_symbol(&pd.name);
            }
        }
        let result = block_inner(r);
        super::simple::pop_scope();
        result?
    } else {
        match block(rest) {
            Ok(ok) => ok,
            Err(_) if name.starts_with("trait_auxiliary:<") => consume_raw_sub_body(rest)?,
            Err(err) => return Err(err),
        }
    };
    // When no explicit signature is given, collect placeholder variables
    // ($^a, $^b, &^c, etc.) from the body as implicit parameters.
    let (params, param_defs) = if params.is_empty() && param_defs.is_empty() {
        let placeholders = collect_placeholders(&body);
        if placeholders.is_empty() {
            (params, param_defs)
        } else {
            (placeholders, Vec::new())
        }
    } else {
        (params, param_defs)
    };
    // Merge return type: `-->` from inside params has priority, then `returns`/`of` traits
    let merged_return_type = return_type.or(traits.return_type);
    Ok((
        rest,
        Stmt::SubDecl {
            name: Symbol::intern(&name),
            name_expr,
            params,
            param_defs,
            return_type: merged_return_type,
            associativity: traits.associativity,
            signature_alternates,
            body,
            multi,
            is_rw: traits.is_rw,
            is_raw: traits.is_raw,
            is_export: traits.is_export,
            export_tags: traits.export_tags,
            is_test_assertion: traits.is_test_assertion,
            supersede,
            custom_traits: traits.custom_traits,
        },
    ))
}

fn consume_raw_sub_body(input: &str) -> PResult<'_, Vec<Stmt>> {
    if !input.starts_with('{') {
        return Err(PError::expected("sub body"));
    }
    let mut depth = 0u32;
    let mut i = 0usize;
    while i < input.len() {
        let ch = input[i..]
            .chars()
            .next()
            .ok_or_else(|| PError::expected("closing '}'"))?;
        let len = ch.len_utf8();
        match ch {
            '{' => depth += 1,
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Ok((&input[i + len..], Vec::new()));
                }
            }
            '\\' => {
                i += len;
                if i < input.len() {
                    let next_len = input[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(0);
                    i += next_len;
                    continue;
                }
            }
            '\'' | '"' => {
                let quote = ch;
                i += len;
                while i < input.len() {
                    let c = input[i..]
                        .chars()
                        .next()
                        .ok_or_else(|| PError::expected("string close"))?;
                    let c_len = c.len_utf8();
                    if c == '\\' {
                        i += c_len;
                        if i < input.len() {
                            let n_len =
                                input[i..].chars().next().map(|n| n.len_utf8()).unwrap_or(0);
                            i += n_len;
                            continue;
                        }
                    }
                    i += c_len;
                    if c == quote {
                        break;
                    }
                }
                continue;
            }
            _ => {}
        }
        i += len;
    }
    Err(PError::expected("closing '}'"))
}

/// Result of parsing sub traits.
pub(crate) struct SubTraits {
    pub is_export: bool,
    pub export_tags: Vec<String>,
    pub is_test_assertion: bool,
    pub is_rw: bool,
    pub is_raw: bool,
    pub return_type: Option<String>,
    pub associativity: Option<String>,
    /// Non-builtin trait names (e.g. `me'd`) for custom `trait_mod:<is>` dispatch.
    pub custom_traits: Vec<String>,
    /// Precedence trait: (trait_name, reference_operator).
    /// trait_name is one of "tighter", "looser", "equiv".
    /// reference_operator is the operator symbol or full name (e.g. "*", "+", "infix:<+>", "prefix:<foo>").
    pub precedence_trait: Option<(String, String)>,
}

/// Parse sub/method traits like `is test-assertion`, `is export`, `returns Str`, `of Num`, etc.
/// Returns `SubTraits` indicating which traits were found.
pub(super) fn parse_sub_traits(mut input: &str) -> PResult<'_, SubTraits> {
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut is_test_assertion = false;
    let mut is_rw = false;
    let mut is_raw = false;
    let mut return_type = None;
    let mut associativity = None;
    let mut custom_traits: Vec<String> = Vec::new();
    let mut seen_traits: Vec<String> = Vec::new();
    let mut precedence_trait: Option<(String, String)> = None;
    loop {
        let (r, _) = ws(input)?;
        if r.starts_with('{') || r.is_empty() {
            return Ok((
                r,
                SubTraits {
                    is_export,
                    export_tags,
                    is_test_assertion,
                    is_rw,
                    is_raw,
                    return_type,
                    associativity,
                    custom_traits: custom_traits.clone(),
                    precedence_trait: precedence_trait.clone(),
                },
            ));
        }
        if let Some(r) = keyword("is", r) {
            let (r, _) = ws(r)?;
            // Parse the trait name (Raku identifier: may include hyphens and apostrophes)
            let (r, trait_name) = parse_raku_ident(r)?;
            if seen_traits.contains(&trait_name.to_string()) {
                add_parse_warning(format!(
                    "Potential difficulties:\n    Duplicate 'is {}' trait",
                    trait_name
                ));
            }
            seen_traits.push(trait_name.to_string());
            if trait_name == "export" {
                is_export = true;
                let (r2, tags) = parse_export_trait_tags(r)?;
                if tags.is_empty() {
                    if !export_tags.iter().any(|t| t == "DEFAULT") {
                        export_tags.push("DEFAULT".to_string());
                    }
                } else {
                    for tag in tags {
                        if !export_tags.iter().any(|t| t == &tag) {
                            export_tags.push(tag);
                        }
                    }
                }
                input = r2;
                continue;
            } else if trait_name == "test-assertion" {
                is_test_assertion = true;
            } else if trait_name == "rw" {
                is_rw = true;
            } else if trait_name == "raw" {
                is_raw = true;
            } else if trait_name == "looser" || trait_name == "tighter" || trait_name == "equiv" {
                associativity = Some(trait_name.to_string());
            } else if trait_name != "assoc"
                && trait_name != "equiv"
                && trait_name != "tighter"
                && trait_name != "looser"
                && trait_name != "readonly"
                && trait_name != "hidden-from-backtrace"
                && trait_name != "implementation-detail"
                && trait_name != "nodal"
                && trait_name != "pure"
            {
                custom_traits.push(trait_name.to_string());
            }
            let (mut r, _) = ws(r)?;
            if r.starts_with('<') {
                let (r2, arg) = parse_trait_angle_arg(r)?;
                if trait_name == "assoc" {
                    associativity = Some(arg);
                } else if trait_name == "tighter" || trait_name == "looser" || trait_name == "equiv"
                {
                    precedence_trait = Some((trait_name.to_string(), arg));
                }
                r = r2;
            }
            // Parse optional parenthesized trait args: is export(:DEFAULT), is equiv(&prefix:<+>)
            if r.starts_with('(') {
                let before_parens = r;
                r = skip_balanced_parens(r);
                if (trait_name == "tighter" || trait_name == "looser" || trait_name == "equiv")
                    && precedence_trait.is_none()
                {
                    // Extract the reference operator from parenthesized form
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    let ref_op = paren_content.trim().to_string();
                    precedence_trait = Some((trait_name.to_string(), ref_op));
                }
            }
            input = r;
            continue;
        }
        if let Some(r) = keyword("returns", r) {
            let (r, _) = ws(r)?;
            let (r, type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            return_type = Some(type_name.to_string());
            input = r;
            continue;
        }
        if let Some(r) = keyword("of", r) {
            let (r, _) = ws(r)?;
            let (r, type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            return_type = Some(type_name.to_string());
            input = r;
            continue;
        }
        if let Some(r) = r.strip_prefix("-->") {
            let (r, _) = ws(r)?;
            let (r, type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            return_type = Some(type_name.to_string());
            input = r;
            continue;
        }
        return Ok((
            r,
            SubTraits {
                is_export,
                export_tags,
                is_test_assertion,
                is_rw,
                is_raw,
                return_type,
                associativity,
                custom_traits: custom_traits.clone(),
                precedence_trait: precedence_trait.clone(),
            },
        ));
    }
}

fn parse_trait_angle_arg(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('<')
        .ok_or_else(|| PError::expected("trait angle argument"))?;
    let mut depth = 1u32;
    let mut chars = after_open.char_indices();
    while let Some((i, c)) = chars.next() {
        match c {
            '>' => {
                depth -= 1;
                if depth == 0 {
                    let arg = after_open[..i].trim().to_string();
                    let after_close = &after_open[i + 1..];
                    return Ok((after_close, arg));
                }
            }
            '<' => depth += 1,
            '\\' => {
                chars.next();
            }
            _ => {}
        }
    }
    Err(PError::expected("closing '>' in trait argument"))
}

fn parse_export_trait_tags(input: &str) -> PResult<'_, Vec<String>> {
    let mut tags = Vec::new();
    let (mut rest, _) = ws(input)?;
    if !rest.starts_with('(') {
        return Ok((rest, tags));
    }

    let after_open = &rest[1..];
    let mut depth = 1usize;
    let mut end: Option<usize> = None;
    for (i, ch) in after_open.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    end = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }
    let end = end.ok_or_else(|| PError::expected("closing ')' in export trait"))?;
    let inner = &after_open[..end];
    rest = &after_open[end + 1..];

    let mut i = 0usize;
    while i < inner.len() {
        let c = inner[i..].chars().next().unwrap_or('\0');
        let c_len = c.len_utf8();
        if c.is_whitespace() || c == ',' {
            i += c_len;
            continue;
        }
        if c == ':' {
            i += c_len;
            if let Some(next) = inner[i..].chars().next()
                && next == '!'
            {
                i += next.len_utf8();
            }
            let start = i;
            while i < inner.len() {
                let ch = inner[i..].chars().next().unwrap_or('\0');
                if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                    i += ch.len_utf8();
                } else {
                    break;
                }
            }
            if i > start {
                let tag = inner[start..i].to_string();
                if !tags.iter().any(|t| t == &tag) {
                    tags.push(tag);
                }
            }
            continue;
        }
        i += c_len;
    }

    let (rest, _) = ws(rest)?;
    Ok((rest, tags))
}

/// Reject invocant markers (':') in non-method signatures (sub, pointy block).
fn reject_invocant_in_sub(params: &[ParamDef]) -> Result<(), PError> {
    if params
        .iter()
        .any(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
    {
        let msg = "X::Syntax::Signature::InvocantNotAllowed: Invocant not allowed in sub signature"
            .to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let ex = Value::make_instance(
            Symbol::intern("X::Syntax::Signature::InvocantNotAllowed"),
            attrs,
        );
        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
    }
    Ok(())
}

/// Strip sigil prefix from a parameter name, returning the bare name.
fn strip_param_sigil(name: &str) -> &str {
    name.strip_prefix('@')
        .or_else(|| name.strip_prefix('%'))
        .or_else(|| name.strip_prefix('&'))
        .unwrap_or(name)
}

/// Check for duplicate parameter names in a signature.
/// Anonymous parameters (e.g. `$`, `@`, `%`) are excluded.
fn check_duplicate_params(params: &[ParamDef]) -> Result<(), PError> {
    let mut seen = std::collections::HashSet::new();
    // Track named parameter base names (without sigils) to detect NameClash
    let mut seen_named_bases = std::collections::HashMap::<String, String>::new();

    // Collect all variable names including those from sub-signatures of renamed params
    // (e.g. :foo($x) produces variable $x which must not clash with :$x)
    let mut all_var_names: Vec<String> = Vec::new();

    for p in params {
        let name = &p.name;
        let name_without_sigil = strip_param_sigil(name);
        if name.is_empty()
            || name_without_sigil.starts_with("__ANON_")
            || name == "__type_only__"
            || name == "__literal__"
            || name == "__subsig__"
        {
            continue;
        }
        // Reconstruct the display name with sigil for error message
        let display_name = if name.starts_with("__type_capture__") {
            name.strip_prefix("__type_capture__")
                .unwrap_or(name)
                .to_string()
        } else if name.starts_with('@')
            || name.starts_with('%')
            || name.starts_with('&')
            || p.sigilless
        {
            name.clone()
        } else {
            format!("${}", name)
        };
        if !seen.insert(display_name.clone()) {
            let msg = format!(
                "X::Redeclaration: Redeclaration of symbol '{}'",
                display_name
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("symbol".to_string(), Value::str(display_name.clone()));
            attrs.insert("what".to_string(), Value::str("symbol".to_string()));
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }

        // For named params, check that the base name (without sigil) is unique
        // e.g. :$a and :@a share base name "a" → X::Signature::NameClash
        if p.named {
            let base = name_without_sigil.to_string();
            if !base.is_empty() {
                if let Some(prev_display) = seen_named_bases.get(&base) {
                    if *prev_display != display_name {
                        let msg = format!(
                            "X::Signature::NameClash: Name {} used for more than one named parameter",
                            base
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("name".to_string(), Value::str(base));
                        attrs.insert("message".to_string(), Value::str(msg.clone()));
                        let ex =
                            Value::make_instance(Symbol::intern("X::Signature::NameClash"), attrs);
                        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
                    }
                } else {
                    seen_named_bases.insert(base, display_name.clone());
                }
            }
        }

        // Collect the variable name for this param
        all_var_names.push(display_name);

        // For named params with sub-signature (renamed params like :foo($x)),
        // also collect the inner variable names
        if p.named
            && let Some(sub_params) = &p.sub_signature
        {
            for sp in sub_params {
                let sp_name = &sp.name;
                let sp_without_sigil = strip_param_sigil(sp_name);
                if sp_name.is_empty() || sp_without_sigil.starts_with("__ANON_") {
                    continue;
                }
                let sp_display = if sp_name.starts_with('@')
                    || sp_name.starts_with('%')
                    || sp_name.starts_with('&')
                    || sp.sigilless
                {
                    sp_name.clone()
                } else {
                    format!("${}", sp_name)
                };
                all_var_names.push(sp_display);
            }
        }
    }

    // Check for duplicate variable names across all params including sub-signature vars
    let mut var_seen = std::collections::HashSet::new();
    for vn in &all_var_names {
        if !var_seen.insert(vn.clone()) {
            let msg = format!("X::Redeclaration: Redeclaration of symbol '{}'", vn);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("symbol".to_string(), Value::str(vn.clone()));
            attrs.insert("what".to_string(), Value::str("symbol".to_string()));
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }

    Ok(())
}

/// Parse parameter list inside parens.
pub(super) fn parse_param_list(input: &str) -> PResult<'_, Vec<ParamDef>> {
    let (rest, params) = parse_param_list_inner(input)?;
    check_duplicate_params(&params)?;
    Ok((rest, params))
}

fn parse_param_list_inner(input: &str) -> PResult<'_, Vec<ParamDef>> {
    let mut params = Vec::new();
    let mut multi_invocant = true;
    let mut rest = input;
    if rest.starts_with(')') || rest.starts_with(']') {
        return Ok((rest, params));
    }
    // Handle --> return type at the start (no params, just return type)
    if let Some(stripped) = rest.strip_prefix("-->") {
        let r = skip_return_type_annotation(stripped)?;
        return Ok((r, params));
    }
    if let Some(r) = rest.strip_prefix(";;") {
        multi_invocant = false;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, params));
        }
        rest = r;
    }
    if let Some((r, _invocant_type)) = parse_implicit_invocant_marker(rest) {
        rest = r;
        if rest.starts_with(')') {
            return Ok((rest, params));
        }
        let (r, mut p) = parse_single_param(rest)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    } else {
        let (r, mut p) = parse_single_param(rest)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    }
    loop {
        let (r, _) = ws(rest)?;
        if let Some(r) = r.strip_prefix(";;") {
            multi_invocant = false;
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
                return Ok((r, params));
            }
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        // Handle invocant marker ':'
        if let Some(r) = r.strip_prefix(':') {
            // Mark all params parsed so far as invocant
            for p in params.iter_mut() {
                p.is_invocant = true;
            }
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
                return Ok((r, params));
            }
            mark_params_as_invocant(&mut params);
            let (r, p) = parse_single_param(r)?;
            params.push(p);
            rest = r;
            continue;
        }
        // Handle invocant separator ';'
        if let Some(r) = r.strip_prefix(';') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                return Ok((r, params));
            }
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        if !r.starts_with(',')
            && starts_with_sigil_param(r)
            && params.last().is_some_and(is_anonymous_sigil_param)
        {
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        if !r.starts_with(',') {
            // Check for --> return type annotation at end of param list
            if let Some(stripped) = r.strip_prefix("-->") {
                let r = skip_return_type_annotation(stripped)?;
                return Ok((r, params));
            }
            return Ok((r, params));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, params));
        }
        // Handle --> return type after comma
        if let Some(stripped) = r.strip_prefix("-->") {
            let r = skip_return_type_annotation(stripped)?;
            return Ok((r, params));
        }
        let (r, mut p) = parse_single_param(r)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    }
}

/// Skip a return type annotation (--> Type) in a signature.
/// Consumes whitespace, a type name (possibly with :D/:U), and any trailing whitespace.
pub(super) fn skip_return_type_annotation(input: &str) -> Result<&str, PError> {
    let (rest, _type_name) = parse_return_type_annotation(input)?;
    Ok(rest)
}

/// Parse a return type annotation (--> Type) and return both remainder and type name.
pub(super) fn parse_return_type_annotation(input: &str) -> PResult<'_, String> {
    let (rest, _) = ws(input)?;
    let bytes = rest.as_bytes();
    let mut idx = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    while idx < bytes.len() {
        let b = bytes[idx];

        if in_single {
            if b == b'\'' {
                in_single = false;
            }
            idx += 1;
            continue;
        }
        if in_double {
            if escaped {
                escaped = false;
            } else if b == b'\\' {
                escaped = true;
            } else if b == b'"' {
                in_double = false;
            }
            idx += 1;
            continue;
        }

        match b {
            b'\'' => {
                in_single = true;
                idx += 1;
            }
            b'"' => {
                in_double = true;
                idx += 1;
            }
            b'(' => {
                paren_depth += 1;
                idx += 1;
            }
            b')' => {
                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
                    break;
                }
                paren_depth = paren_depth.saturating_sub(1);
                idx += 1;
            }
            b'[' => {
                bracket_depth += 1;
                idx += 1;
            }
            b']' => {
                bracket_depth = bracket_depth.saturating_sub(1);
                idx += 1;
            }
            b'{' => {
                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
                    break;
                }
                brace_depth += 1;
                idx += 1;
            }
            b'}' => {
                if brace_depth == 0 {
                    break;
                }
                brace_depth -= 1;
                idx += 1;
            }
            b',' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => break,
            _ => idx += 1,
        }
    }

    let annotation = rest[..idx].trim().to_string();
    if annotation.is_empty() {
        return Err(PError::expected("return type annotation"));
    }
    // Detect multiple prefix constraints in return type (e.g. `--> Int Str`)
    if let Some(space_pos) = annotation.find(' ') {
        let first = &annotation[..space_pos];
        let second = annotation[space_pos..].trim();
        if !first.is_empty()
            && !second.is_empty()
            && first.chars().next().is_some_and(|c| c.is_ascii_uppercase())
            && second
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            && first
                .chars()
                .all(|c| c.is_alphanumeric() || c == ':' || c == '_')
            && second
                .chars()
                .all(|c| c.is_alphanumeric() || c == ':' || c == '_')
        {
            return Err(PError::raw(
                "FATAL:Multiple prefix constraints not yet implemented. Sorry.".to_string(),
                Some(rest[idx..].len()),
            ));
        }
    }
    let (tail, _) = ws(&rest[idx..])?;
    Ok((tail, annotation))
}

/// Parse parameter list with return type annotation.
/// Returns (remaining input, params, optional return type).
pub(super) fn parse_param_list_with_return(
    input: &str,
) -> PResult<'_, (Vec<ParamDef>, Option<String>)> {
    let (rest, (params, return_type)) = parse_param_list_with_return_inner(input)?;
    check_duplicate_params(&params)?;
    Ok((rest, (params, return_type)))
}

fn parse_param_list_with_return_inner(input: &str) -> PResult<'_, (Vec<ParamDef>, Option<String>)> {
    let mut params = Vec::new();
    let mut return_type = None;
    let mut multi_invocant = true;
    let mut rest = input;
    if rest.starts_with(')') {
        return Ok((rest, (params, None)));
    }
    if let Some(stripped) = rest.strip_prefix("-->") {
        let (r, rt) = parse_return_type_annotation(stripped)?;
        return Ok((r, (params, Some(rt))));
    }
    if let Some(r) = rest.strip_prefix(";;") {
        multi_invocant = false;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, (params, return_type)));
        }
        rest = r;
    }
    if let Some((r, _invocant_type)) = parse_implicit_invocant_marker(rest) {
        rest = r;
        if rest.starts_with(')') {
            return Ok((rest, (params, return_type)));
        }
        let (r, mut p) = parse_single_param(rest)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    } else {
        let (r, mut p) = parse_single_param(rest)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    }
    loop {
        let (r, _) = ws(rest)?;
        if let Some(r) = r.strip_prefix(";;") {
            multi_invocant = false;
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
                return Ok((r, (params, return_type)));
            }
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        if let Some(r) = r.strip_prefix(':') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
                return Ok((r, (params, return_type)));
            }
            mark_params_as_invocant(&mut params);
            let (r, p) = parse_single_param(r)?;
            params.push(p);
            rest = r;
            continue;
        }
        if let Some(r) = r.strip_prefix(';') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                return Ok((r, (params, return_type)));
            }
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        if !r.starts_with(',')
            && starts_with_sigil_param(r)
            && params.last().is_some_and(is_anonymous_sigil_param)
        {
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        if !r.starts_with(',') {
            if let Some(stripped) = r.strip_prefix("-->") {
                let (r, rt) = parse_return_type_annotation(stripped)?;
                return_type = Some(rt);
                return Ok((r, (params, return_type)));
            }
            return Ok((r, (params, return_type)));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, (params, return_type)));
        }
        if let Some(stripped) = r.strip_prefix("-->") {
            let (r, rt) = parse_return_type_annotation(stripped)?;
            return_type = Some(rt);
            return Ok((r, (params, return_type)));
        }
        let (r, mut p) = parse_single_param(r)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    }
}

pub(super) use super::sub_param::{
    is_anonymous_sigil_param, mark_params_as_invocant, parse_implicit_invocant_marker,
    starts_with_sigil_param,
};
pub(super) use super::sub_param::{
    method_decl, method_decl_body, method_decl_body_my, submethod_decl,
};
pub(super) use super::sub_param::{parse_single_param, parse_type_constraint_expr};
