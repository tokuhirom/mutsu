use super::*;

/// Known valid parameter traits for `is <trait>` in signatures.
/// `encoded` is a NativeCall string-marshalling trait (`Str $s is encoded('utf8')`)
/// that carries a parenthesized argument; see `skip_optional_trait_arg`.
const VALID_PARAM_TRAITS: &[&str] = &["rw", "readonly", "copy", "required", "raw", "encoded"];

/// After a parameter trait name, skip an optional parenthesized argument such as
/// the `('utf8')` in `is encoded('utf8')`. Balances nested parens and ignores
/// parens inside single/double-quoted string literals. Leading whitespace before
/// the `(` is permitted. Returns the input unchanged when no `(` follows.
pub(crate) fn skip_optional_trait_arg(input: &str) -> &str {
    let trimmed = input.trim_start();
    if !trimmed.starts_with('(') {
        return input;
    }
    let bytes = trimmed.as_bytes();
    let mut depth = 0u32;
    let mut quote: Option<u8> = None;
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        match quote {
            Some(q) => {
                if c == b'\\' {
                    i += 1; // skip escaped char
                } else if c == q {
                    quote = None;
                }
            }
            None => match c {
                b'\'' | b'"' => quote = Some(c),
                b'(' => depth += 1,
                b')' => {
                    depth -= 1;
                    if depth == 0 {
                        return &trimmed[i + 1..];
                    }
                }
                _ => {}
            },
        }
        i += 1;
    }
    // Unbalanced: leave the input as-is so the normal parser reports the error.
    input
}

/// Public wrapper for `validate_param_trait` used by control.rs.
pub(crate) fn validate_param_trait_pub<'a>(
    trait_name: &str,
    existing_traits: &[String],
    input: &'a str,
) -> PResult<'a, ()> {
    validate_param_trait(trait_name, existing_traits, input)
}

/// Validate a parameter trait name, returning a fatal parse error for unknown traits.
/// Also warns on duplicate traits.
pub(crate) fn validate_param_trait<'a>(
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
    // Consume an optional parenthesized trait argument, e.g. `is encoded('utf8')`.
    Ok((skip_optional_trait_arg(input), ()))
}

pub(crate) fn static_default_type(expr: &Expr) -> Option<String> {
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

pub(crate) fn negate_literal_value(value: &Value) -> Option<Value> {
    match value {
        Value::Int(n) => Some(Value::Int(-n)),
        Value::BigInt(n) => Some(Value::bigint(-n.as_ref())),
        Value::Num(n) => Some(Value::Num(-n)),
        Value::Rat(n, d) => Some(crate::value::make_rat(-n, *d)),
        Value::FatRat(n, d) => Some(Value::FatRat(-n, *d)),
        Value::BigRat(n, d) => Some(crate::value::make_big_rat(-(**n).clone(), (**d).clone())),
        Value::Complex(re, im) => Some(Value::Complex(-re, -im)),
        _ => None,
    }
}

pub(crate) fn literal_value_from_expr(expr: &Expr) -> Option<Value> {
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

pub(crate) fn constraint_base_type(constraint: &str) -> &str {
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

pub(crate) fn default_type_matches_constraint(
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
        // `Bool` enumerates to `Int` (`enum Bool does Int`), so a `Bool` default
        // conforms to an `Int`/`Numeric`/`Real` (and `Cool`/`Any`/`Mu`, handled
        // above) parameter -- e.g. `Int :$wrap = False` is valid (used by zef's
        // CLI). It does NOT conform to `Num` or `Rat`.
        "Int" => Some(default_type == "Bool"),
        "UInt" | "Bool" | "Str" | "List" | "Array" | "Hash" | "Pair" | "Junction" | "Complex"
        | "Signature" | "Capture" => Some(false),
        "Numeric" | "Real" => Some(matches!(default_type, "Int" | "Num" | "Rat" | "Bool")),
        "Num" => Some(matches!(default_type, "Int" | "Num" | "Rat")),
        "Rat" => Some(matches!(default_type, "Int" | "Rat")),
        "Callable" | "Code" | "Block" => Some(default_type == "Callable"),
        _ => None,
    }
}

/// Reconstruct a parameter's display name with its sigil. Array/hash/code
/// parameters already store the sigil in `name` (e.g. `@x`, `%x`); scalar
/// parameters store the bare name (e.g. `x`) and get a `$` prefix.
pub(crate) fn param_display_name(name: &str) -> String {
    if name.starts_with(['@', '%', '&']) {
        name.to_string()
    } else {
        format!("${}", name)
    }
}

pub(crate) fn validate_signature_params(params: &[ParamDef]) -> Result<(), PError> {
    // The `:` invocant marker may only appear after the *first* parameter
    // (`method m($self: ...)`). On any later parameter (`sub f($x, $y:)`,
    // `method m($x, $y:)`) Raku raises X::Syntax::Signature::InvocantMarker.
    for pd in params.iter().skip(1) {
        if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
            let msg = "Can only use : as invocant marker in a signature after the first parameter"
                .to_string();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Syntax::Signature::InvocantMarker"),
                attrs,
            );
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }
    let mut saw_optional_positional = false;
    let mut saw_named = false;
    let mut saw_variadic = false;
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
        // X::Parameter::TypedSlurpy: a slurpy *array* (`*@`, `**@`, `+@`) or
        // *hash* (`*%`, `+%`) parameter may not carry an explicit type
        // constraint -- even `Mu`/`Any`. (A slurpy *scalar* `*$x` / `*&f` IS
        // allowed to be typed, so this is gated on the `@`/`%` sigil.) The
        // implicit slurpy type is recorded as `None`, so any `Some` is explicit.
        if (pd.slurpy || pd.double_slurpy || pd.onearg)
            && pd.type_constraint.is_some()
            && (pd.name.starts_with('@') || pd.name.starts_with('%'))
        {
            let kind = if pd.name.starts_with('%') {
                "named"
            } else {
                "positional"
            };
            let msg = format!(
                "Slurpy {} parameters with type constraints are not supported",
                kind
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "kind".to_string(),
                crate::value::Value::str(kind.to_string()),
            );
            attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Parameter::TypedSlurpy"),
                attrs,
            );
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
        // X::Parameter::Default: a default value is illegal on a slurpy parameter
        // (`*@a = 2`, `*@ = 2`) or a required parameter (`$x! = 3`, `:$x! = 3`).
        if pd.default.is_some() {
            let is_slurpy = pd.slurpy || pd.double_slurpy || pd.onearg;
            if is_slurpy || pd.required {
                let (how, parameter, message) = if is_slurpy {
                    let anonymous = pd.name.contains("__ANON_");
                    let param = if anonymous {
                        String::new()
                    } else {
                        param_display_name(&pd.name)
                    };
                    let message = if anonymous {
                        "Cannot put default on anonymous slurpy parameter".to_string()
                    } else {
                        format!("Cannot put default on slurpy parameter {}", param)
                    };
                    ("slurpy", param, message)
                } else {
                    let param = param_display_name(&pd.name);
                    let message = format!("Cannot put default on required parameter {}", param);
                    ("required", param, message)
                };
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("how".to_string(), crate::value::Value::str(how.to_string()));
                attrs.insert("parameter".to_string(), crate::value::Value::str(parameter));
                attrs.insert(
                    "message".to_string(),
                    crate::value::Value::str(message.clone()),
                );
                let ex = crate::value::Value::make_instance(
                    crate::symbol::Symbol::intern("X::Parameter::Default"),
                    attrs,
                );
                return Err(PError::fatal_with_exception(message, Box::new(ex)));
            }
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
        // Parameter ordering: a positional parameter may not appear after an
        // optional positional, a named, or a variadic (slurpy) parameter.
        // X::Parameter::WrongOrder reports which kind is `misplaced` and the kind
        // it was placed `after`.
        if pd.named {
            saw_named = true;
            continue;
        }
        if pd.slurpy || pd.double_slurpy || pd.onearg {
            saw_variadic = true;
            continue;
        }
        let is_optional_positional = pd.optional_marker || pd.default.is_some();
        // The `after` kind: most restrictive preceding blocker wins.
        let after = if saw_variadic {
            Some("variadic")
        } else if saw_named {
            Some("named")
        } else if !is_optional_positional && saw_optional_positional {
            Some("optional")
        } else {
            None
        };
        // An optional positional is only misplaced after a named/variadic param
        // (two optionals in a row are fine).
        let blocked = if is_optional_positional {
            saw_variadic || saw_named
        } else {
            after.is_some()
        };
        if blocked {
            let after = after.unwrap_or("optional");
            let misplaced = if is_optional_positional {
                "optional positional"
            } else {
                "required"
            };
            let display = format!("${}", pd.name);
            let msg = format!(
                "Cannot put {} parameter {} after {} parameters",
                misplaced, display, after
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "misplaced".to_string(),
                crate::value::Value::str(misplaced.to_string()),
            );
            attrs.insert(
                "after".to_string(),
                crate::value::Value::str(after.to_string()),
            );
            attrs.insert("parameter".to_string(), crate::value::Value::str(display));
            attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Parameter::WrongOrder"),
                attrs,
            );
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
        if is_optional_positional {
            saw_optional_positional = true;
        }
    }
    Ok(())
}

/// If a routine body uses a placeholder variable (`$^x`, `@_`, `$:named`, ...)
/// that is not captured by a nested signature-capable block, build the fatal
/// `X::Signature::Placeholder` error. The placeholder is reported with its
/// display name and the source line on which it appears.
///
/// `param_defs` is the routine's explicit signature; the implicit slurpy
/// placeholders `@_` / `%_` are legal when they are explicitly declared as
/// parameters (`sub f(%_) { %_<k> }`), so such declared names are skipped.
pub(crate) fn placeholder_overrides_signature_error(
    body: &[Stmt],
    param_defs: &[ParamDef],
) -> Option<PError> {
    // `$^X`, `$^O`, ... (a sigil, a caret, then a single uppercase ASCII letter)
    // are Perl 5 special variables, not Raku placeholders, so they never
    // override a signature (Rakudo reports them as "Unsupported use" instead).
    let is_perl5_caret_special = |ph: &str| {
        let b = ph.as_bytes();
        b.len() == 3
            && matches!(b[0], b'$' | b'@' | b'%' | b'&')
            && b[1] == b'^'
            && b[2].is_ascii_uppercase()
    };
    let declared: std::collections::HashSet<&str> =
        param_defs.iter().map(|p| p.name.as_str()).collect();
    let mut line: i64 = 0;
    for stmt in body {
        if let Stmt::SetLine(l) = stmt {
            line = *l;
            continue;
        }
        if let Some(ph) = crate::ast::collect_unattached_placeholders(std::slice::from_ref(stmt))
            .into_iter()
            .find(|ph| !declared.contains(ph.as_str()) && !is_perl5_caret_special(ph))
        {
            let message = format!(
                "Placeholder variable '{}' cannot override existing signature",
                ph
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("placeholder".to_string(), Value::str(ph));
            attrs.insert("line".to_string(), Value::Int(line));
            attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Signature::Placeholder"), attrs);
            return Some(PError::fatal_with_exception(message, Box::new(ex)));
        }
    }
    None
}
