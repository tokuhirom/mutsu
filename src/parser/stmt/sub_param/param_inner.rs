use crate::ast::ParamDef;
use crate::parser::expr::expression;
use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::value::Value;

pub(crate) fn parse_single_param(input: &str) -> PResult<'_, ParamDef> {
    let (rest, mut p) = parse_single_param_inner(input)?;
    // The `is required` trait is exactly the `!` required marker; normalize it
    // onto the canonical `required` flag (and drop the now-redundant trait) so
    // all binding sites enforce it identically to `:$n!` — e.g. `:$n is required`
    // must die when `n` is not passed, even alongside a `*%h` slurpy.
    if p.traits.iter().any(|t| t == "required") {
        p.required = true;
        p.traits.retain(|t| t != "required");
    }
    Ok((rest, p))
}

fn parse_single_param_inner(input: &str) -> PResult<'_, ParamDef> {
    let mut rest = input;
    let mut named = false;
    let mut slurpy = false;
    let mut type_constraint = None;

    // Array sub-signature: [Type1, Type2, ...] → anonymous @ with sub-sig
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::super::sub::parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let mut p = super::helpers::make_param("@".to_string());
        p.sub_signature = Some(sub_params);
        return Ok((r, p));
    }

    // Capture-all: (|), (|$c), (|c), or capture sub-signature forms like | ($x)
    if let Some(stripped) = rest.strip_prefix('|') {
        let (r, _) = ws(stripped)?;
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::super::sub::parse_param_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, _) = ws(r)?;
            if sub_params.len() == 1 {
                return Ok((r, sub_params[0].clone()));
            }
            let mut p = super::helpers::make_param("__subsig__".to_string());
            p.sub_signature = Some(sub_params);
            return Ok((r, p));
        }
        let (r, _) = ws(r)?;
        // Optional capture variable name with sigil, optionally followed by
        // a capture sub-signature: |$c ($a, $b?)
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            let (r, name) = super::super::var_name(r)?;
            let mut p = super::helpers::make_param(name);
            p.slurpy = true;
            let (r, _) = ws(r)?;
            if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, sub_params) = super::super::sub::parse_param_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                p.sub_signature = Some(sub_params);
                return Ok((r, p));
            }
            return Ok((r, p));
        }
        // Sigilless capture variable name: |c, |args, optionally followed by
        // a capture sub-signature: |c ($a, $b?)
        // or a where constraint: |c where { ... }
        if let Ok((r_ident, name)) = super::super::ident(r)
            && !matches!(name.as_str(), "where" | "is")
        {
            let mut p = super::helpers::make_param(name);
            p.slurpy = true;
            p.sigilless = true;
            let (r_ident, _) = ws(r_ident)?;
            if r_ident.starts_with('(') {
                let (r_ident, _) = parse_char(r_ident, '(')?;
                let (r_ident, _) = ws(r_ident)?;
                let (r_ident, sub_params) = super::super::sub::parse_param_list(r_ident)?;
                let (r_ident, _) = ws(r_ident)?;
                let (r_ident, _) = parse_char(r_ident, ')')?;
                p.sub_signature = Some(sub_params);
                return Ok((r_ident, p));
            }
            // Check for where constraint on named capture: |c where { ... }
            let (r_ident, where_constraint) = if let Some(r2) =
                super::super::keyword("where", r_ident)
            {
                let (r2, _) = ws1(r2)?;
                let (r2, constraint) = super::where_constraint::parse_where_constraint_expr(r2)?;
                (r2, Some(Box::new(constraint)))
            } else {
                (r_ident, None)
            };
            p.where_constraint = where_constraint;
            return Ok((r_ident, p));
        }
        // Bare |, possibly followed by traits/where
        let mut p = super::helpers::make_param("_capture".to_string());
        p.slurpy = true;
        // An anonymous capture absorbs both positional and named arguments,
        // exactly like a named capture (|c). Mark it sigilless so binding and
        // multi-dispatch treat it as a hash+positional slurpy.
        p.sigilless = true;
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = super::super::keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = super::super::ident(r2)?;
            let (r2, _) = super::super::sub::validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let (r, where_constraint) = if let Some(r2) = super::super::keyword("where", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, constraint) = super::where_constraint::parse_where_constraint_expr(r2)?;
            (r2, Some(Box::new(constraint)))
        } else {
            (r, None)
        };
        p.traits = param_traits;
        p.where_constraint = where_constraint;
        return Ok((r, p));
    }

    // Slurpy: *@arr or *%hash or *$scalar or *[...] (slurpy unpack)
    let mut slurpy_sigil = None;
    let mut double_slurpy = false;
    let mut onearg = false;
    // Single-argument rule slurpy marker (+@a, +%h, +$x, +&f, or sigilless +foo).
    // Treat as regular slurpy for now.
    if rest.starts_with('+')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        onearg = true;
        rest = &rest[1..];
    }
    // Sigilless single-argument rule slurpy: +foo
    if rest.starts_with('+') && rest.len() > 1 && rest.as_bytes()[1].is_ascii_alphabetic() {
        let r = &rest[1..];
        if let Ok((r, name)) = super::super::ident(r) {
            let (r, _) = ws(r)?;
            // Handle traits (is copy, is rw, etc.)
            let mut param_traits = Vec::new();
            let (mut r, _) = ws(r)?;
            while let Some(rt) = super::super::keyword("is", r) {
                let (rt, _) = ws1(rt)?;
                let (rt, trait_name) = super::super::ident(rt)?;
                let (rt, _) =
                    super::super::sub::validate_param_trait(&trait_name, &param_traits, rt)?;
                param_traits.push(trait_name);
                let (rt, _) = ws(rt)?;
                r = rt;
            }
            // Default value
            let (r, default) = if r.starts_with('=') && !r.starts_with("==") {
                let r = &r[1..];
                let (r, _) = ws(r)?;
                let (r, expr) = super::helpers::parse_param_default_expr(r)?;
                (r, Some(expr))
            } else {
                (r, None)
            };
            let mut p = super::helpers::make_param(name);
            p.slurpy = true;
            p.onearg = true;
            p.sigilless = true;
            p.named = named;
            p.default = default;
            p.type_constraint = type_constraint;
            p.traits = param_traits;
            return Ok((r, p));
        }
    }
    if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[1] as char);
        rest = &rest[1..];
    }

    // Slurpy unpack: *[$a, $b, ...] — gather remaining args then unpack
    if rest.starts_with("*[") {
        let r = &rest[1..]; // skip '*', keep '['
        let (r, _) = parse_char(r, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::super::sub::parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let mut p = super::helpers::make_param("@".to_string());
        p.slurpy = true;
        p.sub_signature = Some(sub_params);
        return Ok((r, p));
    }

    // Handle ::?CLASS and ::?ROLE pseudo-types in signatures (must come before named check)
    if rest.starts_with("::?CLASS") || rest.starts_with("::?ROLE") {
        let end = if rest.starts_with("::?CLASS") { 8 } else { 7 };
        let pseudo_type = &rest[..end];
        let r = &rest[end..];
        let (r, tc) = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            let smiley = &r[..2];
            (&r[2..], format!("{}{}", pseudo_type, smiley))
        } else {
            (r, pseudo_type.to_string())
        };
        if r.starts_with(':')
            && !r.starts_with(":D")
            && !r.starts_with(":U")
            && !r.starts_with(":_")
        {
            // The `:` here is the invocant marker. Return the invocant param
            // with rest positioned AT the `:` so parse_param_list_inner can
            // detect it and mark the param as invocant before parsing more params.
            let mut p = super::helpers::make_param("self".to_string());
            p.type_constraint = Some(tc);
            p.is_invocant = true;
            p.traits.push("invocant".to_string());
            return Ok((r, p));
        }
        let (r, _) = ws(r)?;
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            type_constraint = Some(tc);
            rest = r;
        } else {
            let mut p = super::helpers::make_param("self".to_string());
            p.type_constraint = Some(tc);
            p.is_invocant = true;
            p.traits.push("invocant".to_string());
            return Ok((r, p));
        }
    }

    // Named param marker: :$name (but not :: which is a parametric type prefix)
    if rest.starts_with(':') && !rest.starts_with("::") {
        named = true;
        rest = &rest[1..];
    }

    // Type-capture parameter: ::T $x  or bare ::T
    if let Some(after_capture) = rest.strip_prefix("::")
        && let Ok((r, capture_name)) = super::super::ident(after_capture)
    {
        type_constraint = Some(format!("::{}", capture_name));
        let (r, _) = ws(r)?;
        rest = r;
        if rest.starts_with('=') && !rest.starts_with("==") {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, default) = super::helpers::parse_param_default_expr(r)?;
            let mut p = super::helpers::make_param(format!("__type_capture__{}", capture_name));
            p.type_constraint = type_constraint;
            p.named = named;
            p.slurpy = slurpy;
            p.default = Some(default);
            return Ok((r, p));
        }
        if rest.starts_with(')')
            || rest.starts_with(']')
            || rest.starts_with(',')
            || rest.starts_with(';')
        {
            let mut p = super::helpers::make_param(format!("__type_capture__{}", capture_name));
            p.type_constraint = type_constraint;
            p.named = named;
            p.slurpy = slurpy;
            return Ok((rest, p));
        }
        if rest.starts_with(':') && !rest.starts_with("::") {
            // Check if this `:` is an invocant marker (followed by whitespace/sigil/paren)
            // rather than a named-parameter marker.
            // In `(::T: $ where Foo[T])`, the `:` after `::T` is the invocant separator.
            let after_colon = &rest[1..];
            let is_invocant_marker = after_colon.is_empty()
                || after_colon.starts_with(' ')
                || after_colon.starts_with('\t')
                || after_colon.starts_with('\n')
                || after_colon.starts_with('\r')
                || after_colon.starts_with(')')
                || after_colon.starts_with(',');
            if is_invocant_marker {
                // Return the bare ::T as a standalone param; leave `:` for the
                // outer param-list loop to handle as the invocant marker.
                let mut p = super::helpers::make_param(format!("__type_capture__{}", capture_name));
                p.type_constraint = type_constraint;
                p.named = named;
                p.slurpy = slurpy;
                return Ok((rest, p));
            }
            named = true;
            rest = &rest[1..];
        }
    }

    // Type constraint (may be qualified: IO::Path)
    // Skip type constraint parsing for named params with lowercase identifiers followed by '('
    // Parametric type constraint: ::T
    if let Some(after_colon) = rest.strip_prefix("::")
        && let Ok((r, tc_name)) = super::super::ident(after_colon)
    {
        let tc = format!("::{}", tc_name);
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
            type_constraint = Some(tc);
            rest = r2;
        }
    }

    // — those are named aliases like :x($r), not type constraints.
    let skip_type_for_named_alias = named
        && rest
            .as_bytes()
            .first()
            .is_some_and(|b| b.is_ascii_lowercase())
        && rest.contains('(');
    if type_constraint.is_none()
        && !skip_type_for_named_alias
        && let Some((r, tc)) = super::type_constraint::parse_type_constraint_expr(rest)
    {
        // Validate type smiley before proceeding
        super::type_constraint::check_invalid_type_smiley(&Some(tc.clone()))?;
        let (r2, _) = ws(r)?;

        // Check for coercion type: Int() or Int(Rat)
        // Also handles sub-signature: Type (inner-params)
        if let Some(after_open) = r2.strip_prefix('(') {
            let (after_ws, _) = ws(after_open)?;
            // Coercion type: Int() — empty parens
            if let Some(r3) = after_ws.strip_prefix(')') {
                let (r3, _) = ws(r3)?;
                // This is a coercion type like Int()
                type_constraint = Some(format!("{}()", tc));
                rest = r3;
                // Re-check named after type
                if rest.starts_with(':') {
                    named = true;
                    rest = &rest[1..];
                }
            } else if let Some((inner_r, source_type)) =
                super::type_constraint::parse_type_constraint_expr(after_ws)
            {
                let (inner_r, _) = ws(inner_r)?;
                if let Some(r3) = inner_r.strip_prefix(')') {
                    let (r3, _) = ws(r3)?;
                    // This is a coercion type like Int(Rat)
                    type_constraint = Some(format!("{}({})", tc, source_type));
                    rest = r3;
                    if rest.starts_with(':') {
                        named = true;
                        rest = &rest[1..];
                    }
                } else {
                    // Fall through to sub-signature parsing
                    let (r3, _) = parse_char(r2, '(')?;
                    let (r3, _) = ws(r3)?;
                    let (r3, sub_params) = super::super::sub::parse_param_list(r3)?;
                    let (r3, _) = ws(r3)?;
                    let (r3, _) = parse_char(r3, ')')?;
                    let (r3, _) = ws(r3)?;
                    let mut p = super::helpers::make_param("__subsig__".to_string());
                    p.type_constraint = Some(tc);
                    p.sub_signature = Some(sub_params);
                    p.named = named;
                    p.slurpy = slurpy;
                    return Ok((r3, p));
                }
            } else {
                // Parse sub-signature (destructuring)
                let (r3, _) = parse_char(r2, '(')?;
                let (r3, _) = ws(r3)?;
                let (r3, sub_params) = super::super::sub::parse_param_list(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                let mut p = super::helpers::make_param("__subsig__".to_string());
                p.type_constraint = Some(tc);
                p.sub_signature = Some(sub_params);
                p.named = named;
                p.slurpy = slurpy;
                // Handle optional (?) / required (!) suffix after sub-signature
                let (rest, required, opt_marker) = super::helpers::parse_required_suffix(r3);
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = super::super::keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = super::super::ident(r)?;
                    let (r, _) =
                        super::super::sub::validate_param_trait(&trait_name, &param_traits, r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                p.required = required;
                p.optional_marker = opt_marker;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = super::super::keyword("where", rest)
                {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = super::where_constraint::parse_where_constraint_expr(r)?;
                    (r, Some(Box::new(constraint)))
                } else {
                    (rest, None)
                };
                p.where_constraint = where_constraint;
                return Ok((rest, p));
            }
        }

        if r2.starts_with('$')
            || r2.starts_with('@')
            || r2.starts_with('%')
            || r2.starts_with('&')
            || (r2.starts_with('*')
                && r2.len() > 1
                && (r2.as_bytes()[1] == b'$'
                    || r2.as_bytes()[1] == b'@'
                    || r2.as_bytes()[1] == b'%'
                    || r2.as_bytes()[1] == b'&'))
            || r2.starts_with(':')
            || r2.starts_with('|')
            || r2.starts_with('\\')
        {
            type_constraint = Some(tc);
            rest = r2;
            // Re-check named after type
            if rest.starts_with(':') {
                named = true;
                rest = &rest[1..];
            }
        } else if r2.starts_with(')')
            || r2.starts_with(',')
            || r2.starts_with(']')
            || r2.starts_with('{')
            || r2.starts_with("-->")
            // A type-only parameter may still carry traits / a where clause:
            // `Pointer is rw` (a NativeCall out-parameter), `Int where * > 0`.
            || super::super::keyword("is", r2).is_some()
            || super::super::keyword("where", r2).is_some()
        {
            // True/False in signature position are literal Bool values, not type names.
            // In Raku, `sub f(True)` means "type Bool, smartmatched against True".
            // Smartmatch against True always succeeds, so the literal check is just
            // a Bool type constraint. Smartmatch against False always fails, so
            // sub f(False) would reject all calls (equivalent to an impossible constraint).
            if (tc == "True" || tc == "False")
                && (r2.starts_with(')')
                    || r2.starts_with(',')
                    || r2.starts_with(']')
                    || r2.starts_with('{')
                    || r2.starts_with("-->"))
            {
                crate::parser::add_parse_warning(format!(
                    "Potential difficulties:\n    Literal values in signatures are smartmatched against and smartmatch with `{}` will always {}. Use the `where` clause instead.",
                    tc,
                    if tc == "True" { "succeed" } else { "fail" }
                ));
                let mut p = super::helpers::make_param("__type_only__".to_string());
                p.type_constraint = Some("Bool".to_string());
                p.named = named;
                p.slurpy = slurpy;
                return Ok((r2, p));
            }
            // Bare identifier as type-only parameter (e.g., enum values in multi dispatch)
            // multi infix:<->(e1, e2) { ... }
            let mut p = super::helpers::make_param("__type_only__".to_string());
            p.type_constraint = Some(tc);
            p.named = named;
            p.slurpy = slurpy;
            // Optional traits (`is rw`, `is copy`, …) and a `where` clause on the
            // anonymous parameter.
            let mut param_traits = Vec::new();
            let (mut r3, _) = ws(r2)?;
            while let Some(r) = super::super::keyword("is", r3) {
                let (r, _) = ws1(r)?;
                let (r, trait_name) = super::super::ident(r)?;
                let (r, _) =
                    super::super::sub::validate_param_trait(&trait_name, &param_traits, r)?;
                param_traits.push(trait_name);
                let (r, _) = ws(r)?;
                r3 = r;
            }
            p.traits = param_traits;
            let (r3, where_constraint) = if let Some(r) = super::super::keyword("where", r3) {
                let (r, _) = ws1(r)?;
                let (r, constraint) = super::where_constraint::parse_where_constraint_expr(r)?;
                (r, Some(Box::new(constraint)))
            } else {
                (r3, None)
            };
            p.where_constraint = where_constraint;
            return Ok((r3, p));
        } else {
            // Check for multiple prefix type constraints (e.g. `Int Str $x`)
            if let Some((r3, _second_tc)) = super::type_constraint::parse_type_constraint_expr(r2) {
                let (r4, _) = ws(r3).unwrap_or((r3, ()));
                if r4.starts_with('$')
                    || r4.starts_with('@')
                    || r4.starts_with('%')
                    || r4.starts_with('&')
                    || r4.starts_with('\\')
                {
                    return Err(PError::raw(
                        "FATAL:X::Parameter::MultipleTypeConstraints: Multiple prefix type constraints are not supported. You may use a subset type instead."
                            .to_string(),
                        Some(r2.len()),
                    ));
                }
            }
        }
    }

    if let Some(tc) = type_constraint.take() {
        let (r, tc) = super::type_constraint::parse_of_type_constraint_chain(rest, tc)
            .ok_or_else(|| PError::expected("type"))?;
        type_constraint = Some(tc);
        rest = r;
    }

    // Typed capture parameter, e.g. `Capture |cap`
    if type_constraint.is_some() && rest.starts_with('|') {
        let (r, mut p) = parse_single_param(rest)?;
        if p.type_constraint.is_none() {
            p.type_constraint = type_constraint.clone();
        }
        p.named = named;
        if slurpy {
            p.slurpy = true;
        }
        return Ok((r, p));
    }

    // Double slurpy marker may appear after a type constraint:
    // e.g. `Array **@AoA`
    if rest.starts_with("**")
        && rest.len() > 2
        && (rest.as_bytes()[2] == b'@' || rest.as_bytes()[2] == b'%')
    {
        slurpy = true;
        double_slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[2] as char);
        rest = &rest[2..];
    }
    // Slurpy marker may appear after a type constraint:
    // e.g. `Code *$block`, `Int *@xs`, `Hash *%h`.
    else if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[1] as char);
        rest = &rest[1..];
    }

    // Handle literal value parameters: multi sub foo(0), foo(-١), foo(Inf), foo("x")
    if let Ok((lit_rest, lit_expr)) = expression(rest)
        && let Some(v) = super::super::sub::literal_value_from_expr(&lit_expr)
        && let Ok((after_lit, _)) = ws(lit_rest)
        && (after_lit.starts_with(')')
            || after_lit.starts_with(',')
            || after_lit.starts_with(';')
            || after_lit.starts_with(']')
            || after_lit.starts_with('{')
            || after_lit.starts_with("-->"))
    {
        let mut p = super::helpers::make_param("__literal__".to_string());
        // If no explicit type constraint, infer from the literal value type
        p.type_constraint = type_constraint.or_else(|| {
            Some(
                match &v {
                    Value::Int(_) | Value::BigInt(_) => "Int",
                    Value::Num(_) => "Num",
                    Value::Rat(..) | Value::FatRat(..) | Value::BigRat(..) => "Rat",
                    Value::Str(_) => "Str",
                    Value::Bool(_) => "Bool",
                    Value::Complex(..) => "Complex",
                    _ => return None,
                }
                .to_string(),
            )
        });
        p.literal_value = Some(v);
        return Ok((after_lit, p));
    }

    // Sigilless parameter: \name or anonymous \
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = match super::super::ident(r) {
            Ok((r, name)) => (r, name),
            Err(_) => (r, String::new()),
        };
        let (r, _) = ws(r)?;
        // `is copy`, `is rw`, `is readonly`, `is raw` traits
        let (mut r, _) = ws(r)?;
        let mut sigilless_traits = Vec::new();
        while let Some(rt) = super::super::keyword("is", r) {
            let (rt, _) = ws1(rt)?;
            let (rt, trait_name) = super::super::ident(rt)?;
            let (rt, _) =
                super::super::sub::validate_param_trait(&trait_name, &sigilless_traits, rt)?;
            sigilless_traits.push(trait_name);
            let (rt, _) = ws(rt)?;
            r = rt;
        }
        // Default value
        let (r, default) = if r.starts_with('=') && !r.starts_with("==") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = super::helpers::parse_param_default_expr(r)?;
            (r, Some(expr))
        } else {
            (r, None)
        };
        let mut p = super::helpers::make_param(name);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.sigilless = true;
        p.default = default;
        p.type_constraint = type_constraint;
        p.traits = sigilless_traits;
        return Ok((r, p));
    }

    // Sub-signature without type: just (inner-params)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::super::sub::parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = ws(r)?;
        let (r, required, opt_marker) = super::helpers::parse_required_suffix(r);
        let mut p = super::helpers::make_param("__subsig__".to_string());
        p.sub_signature = Some(sub_params);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.required = required;
        p.optional_marker = opt_marker;
        return Ok((r, p));
    }

    // Named parameter with alias: :key($var) or :value(&callback)
    if named && let Ok((r, alias_name)) = super::super::ident(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('(') {
            let (r3, _) = parse_char(r2, '(')?;
            let (r3, _) = ws(r3)?;
            // Inside could be a sub-signature or a single variable
            // Try to parse as a sub-signature first (handles nested params)
            let (r3, sub_params) = super::super::sub::parse_param_list(r3)?;
            let (r3, _) = ws(r3)?;
            let (r3, _) = parse_char(r3, ')')?;
            // If it's a single simple variable param, use its name; otherwise sub-signature
            if sub_params.len() == 1
                && sub_params[0].sub_signature.is_none()
                && !sub_params[0].name.starts_with("__")
            {
                let mut p = super::helpers::make_param(alias_name.clone());
                p.named = true;
                p.slurpy = slurpy;
                p.double_slurpy = double_slurpy;
                p.onearg = onearg;
                p.type_constraint = type_constraint;
                p.sub_signature = Some(sub_params.clone());
                // Check for a sub-signature after the alias: :x($r) (Str $g, Any $i)
                let (r3, _) = ws(r3)?;
                if r3.starts_with('(') {
                    let (r4, _) = parse_char(r3, '(')?;
                    let (r4, _) = ws(r4)?;
                    let (r4, outer_sub) = super::super::sub::parse_param_list(r4)?;
                    let (r4, _) = ws(r4)?;
                    let (r3_new, _) = parse_char(r4, ')')?;
                    p.outer_sub_signature = Some(outer_sub);
                    let (rest, alias_required, alias_opt_marker) =
                        super::helpers::parse_required_suffix(r3_new);
                    p.required = alias_required;
                    p.optional_marker = alias_opt_marker;
                    let (rest, _) = ws(rest)?;
                    let mut param_traits = Vec::new();
                    let (mut rest, _) = ws(rest)?;
                    while let Some(r) = super::super::keyword("is", rest) {
                        let (r, _) = ws1(r)?;
                        let (r, trait_name) = super::super::ident(r)?;
                        let (r, _) =
                            super::super::sub::validate_param_trait(&trait_name, &param_traits, r)?;
                        param_traits.push(trait_name);
                        let (r, _) = ws(r)?;
                        rest = r;
                    }
                    p.traits = param_traits;
                    let (rest, where_constraint) =
                        if let Some(r) = super::super::keyword("where", rest) {
                            let (r, _) = ws1(r)?;
                            let (r, constraint) =
                                super::where_constraint::parse_where_constraint_expr(r)?;
                            (r, Some(Box::new(constraint)))
                        } else {
                            (rest, None)
                        };
                    p.where_constraint = where_constraint;
                    let (rest_ws, _) = ws(rest)?;
                    let (rest, default) = if rest_ws.starts_with('=') && !rest_ws.starts_with("==")
                    {
                        let rest = &rest_ws[1..];
                        let (rest, _) = ws(rest)?;
                        let (rest, expr) = expression(rest)?;
                        (rest, Some(expr))
                    } else {
                        (rest_ws, None)
                    };
                    p.default = default;
                    return Ok((rest, p));
                }
                // Handle optional (?) / required (!) suffix after alias
                let (rest, alias_required, alias_opt_marker) =
                    super::helpers::parse_required_suffix(r3);
                p.required = alias_required;
                p.optional_marker = alias_opt_marker;
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = super::super::keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = super::super::ident(r)?;
                    let (r, _) =
                        super::super::sub::validate_param_trait(&trait_name, &param_traits, r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = super::super::keyword("where", rest)
                {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = super::where_constraint::parse_where_constraint_expr(r)?;
                    (r, Some(Box::new(constraint)))
                } else {
                    (rest, None)
                };
                p.where_constraint = where_constraint;
                let (rest_ws, _) = ws(rest)?;
                let (rest, default) = if rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
                    let rest = &rest_ws[1..];
                    let (rest, _) = ws(rest)?;
                    let (rest, expr) = expression(rest)?;
                    (rest, Some(expr))
                } else {
                    (rest_ws, None)
                };
                p.default = default;
                return Ok((rest, p));
            }
            // Multiple params or complex: treat as sub-signature
            let mut p = super::helpers::make_param(alias_name);
            p.named = true;
            p.slurpy = slurpy;
            p.double_slurpy = double_slurpy;
            p.onearg = onearg;
            p.type_constraint = type_constraint;
            p.sub_signature = Some(sub_params);
            // Handle optional (?) / required (!) suffix after sub-signature
            let (rest, required, opt_marker) = super::helpers::parse_required_suffix(r3);
            // Skip whitespace
            let (rest, _) = ws(rest)?;
            // Handle is copy, is rw, is readonly, is raw traits
            let mut param_traits = Vec::new();
            let (mut rest, _) = ws(rest)?;
            while let Some(r) = super::super::keyword("is", rest) {
                let (r, _) = ws1(r)?;
                let (r, trait_name) = super::super::ident(r)?;
                let (r, _) =
                    super::super::sub::validate_param_trait(&trait_name, &param_traits, r)?;
                param_traits.push(trait_name);
                let (r, _) = ws(r)?;
                rest = r;
            }
            p.traits = param_traits;
            p.required = required;
            p.optional_marker = opt_marker;
            // Handle where constraint
            let (rest, where_constraint) = if let Some(r) = super::super::keyword("where", rest) {
                let (r, _) = ws1(r)?;
                let (r, constraint) = super::where_constraint::parse_where_constraint_expr(r)?;
                (r, Some(Box::new(constraint)))
            } else {
                (rest, None)
            };
            p.where_constraint = where_constraint;
            let (rest_ws, _) = ws(rest)?;
            let (rest, default) = if rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
                let rest = &rest_ws[1..];
                let (rest, _) = ws(rest)?;
                let (rest, expr) = expression(rest)?;
                (rest, Some(expr))
            } else {
                (rest_ws, None)
            };
            p.default = default;
            return Ok((rest, p));
        }
    }

    // Anonymous optional sigil parameters: $?, @?, %?
    for (prefix, anon_name) in [
        ("$?", "__ANON_OPTIONAL__"),
        ("@?", "@__ANON_ARRAY__"),
        ("%?", "%__ANON_HASH__"),
    ] {
        if let Some(after_q) = rest.strip_prefix(prefix)
            && (after_q.is_empty()
                || after_q.starts_with(',')
                || after_q.starts_with(')')
                || after_q.starts_with(' ')
                || after_q.starts_with('\t')
                || after_q.starts_with('\n'))
        {
            let mut p = super::helpers::make_param(anon_name.to_string());
            p.named = named;
            p.slurpy = slurpy;
            p.double_slurpy = double_slurpy;
            p.onearg = onearg;
            p.type_constraint = type_constraint;
            p.optional_marker = true;
            return Ok((after_q, p));
        }
    }

    // Anonymous callable parameter with code signature: &:(Str --> Bool)
    if rest.starts_with("&:(") {
        let r = &rest[1..]; // skip '&'
        let (r, _) = parse_char(r, ':')?;
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, (sig_params, sig_ret)) = super::super::sub::parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, required, opt_marker) = super::helpers::parse_required_suffix(r);
        let (r, _) = ws(r)?;

        let mut p = super::helpers::make_param("&".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.code_signature = Some((sig_params, sig_ret));
        p.required = required;
        p.optional_marker = opt_marker;

        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = super::super::keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = super::super::ident(r2)?;
            let (r2, _) = super::super::sub::validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        p.traits = param_traits;

        let (r, where_constraint) = if let Some(r2) = super::super::keyword("where", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, constraint) = super::where_constraint::parse_where_constraint_expr(r2)?;
            (r2, Some(Box::new(constraint)))
        } else {
            (r, None)
        };
        p.where_constraint = where_constraint;
        return Ok((r, p));
    }

    // Bare & (anonymous callable parameter)
    if rest.starts_with('&')
        && (rest.len() == 1
            || rest.as_bytes()[1] == b','
            || rest.as_bytes()[1] == b')'
            || rest.as_bytes()[1] == b' '
            || rest.as_bytes()[1] == b'\t'
            || rest.as_bytes()[1] == b'\n'
            || rest.as_bytes()[1] == b'?'
            || rest.as_bytes()[1] == b'!')
    {
        let rest = &rest[1..];
        let (rest, required, opt_marker) = super::helpers::parse_required_suffix(rest);
        let mut p = super::helpers::make_param("&".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.required = required;
        p.optional_marker = opt_marker;
        return Ok((rest, p));
    }

    // Reject placeholder/twigil variables in signatures:
    //   $:x @:x %:x  -> X::Parameter::Placeholder (named, right => ':$x')
    //   $^x          -> X::Parameter::Placeholder (positional, right => '$x')
    //   $?x $=x $~x  -> X::Parameter::Twigil
    // ($!x / $.x are attribute twigils, handled separately as X::Syntax::NoSelf
    // or accessor params; $*x is a dynamic variable and is legal.)
    super::where_constraint::reject_placeholder_or_twigil_param(rest)?;

    // Capture the original sigil before var_name strips it
    let original_sigil = rest.as_bytes().first().copied().unwrap_or(b'$');
    let param_sigil = rest.as_bytes().first().copied();
    // Handle $.x / @.x / %.x (public accessor twigil) in parameter context.
    // The '.' twigil is only valid in signatures, not in general expressions,
    // so we handle it here rather than in var_name.
    let (rest, name) =
        if (rest.starts_with("$.") || rest.starts_with("@.") || rest.starts_with("%."))
            && rest.len() > 2
            && rest.as_bytes()[2].is_ascii_alphabetic()
        {
            let sigil_prefix = if rest.starts_with('$') {
                ""
            } else if rest.starts_with('@') {
                "@"
            } else {
                "%"
            };
            let after_dot = &rest[2..]; // skip sigil + "."
            let end = after_dot
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(after_dot.len());
            let ident = &after_dot[..end];
            (&after_dot[end..], format!("{}.{}", sigil_prefix, ident))
        } else {
            super::super::var_name(rest)?
        };

    // Shape constraint for array parameters: @a[3], @a[4,4], @a[*], @a[$n]
    let mut shape_constraints = None;
    let rest = if original_sigil == b'@' && rest.starts_with('[') {
        let (r, dims) = super::super::decl::parse_array_shape_suffix(rest)?;
        shape_constraints = Some(dims);
        r
    } else {
        rest
    };

    // Code signature constraint: &foo:(Str --> Bool)
    let mut code_sig = None;
    let rest = if original_sigil == b'&' && rest.starts_with(":(") {
        let r = &rest[1..]; // skip ':'
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, (sig_params, sig_ret)) = super::super::sub::parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        code_sig = Some((sig_params, sig_ret));
        r
    } else {
        rest
    };

    // Optional (?) / required (!) suffix
    let (rest, required, opt_marker) = super::helpers::parse_required_suffix(rest);
    let (rest, _) = ws(rest)?;

    // Sub-signature after variable: $x ($a, $b) or $ ($a, $b)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::super::sub::parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = ws(r)?;
        // Handle optional (?) / required (!) suffix after sub-signature
        let (r, post_required, post_opt_marker) = super::helpers::parse_required_suffix(r);
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = super::super::keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = super::super::ident(r2)?;
            let (r2, _) = super::super::sub::validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let param_name = if slurpy {
            match slurpy_sigil {
                Some('%') => format!("%{}", name),
                Some('@') => format!("@{}", name),
                _ => name,
            }
        } else if named && original_sigil == b'&' {
            name
        } else {
            match original_sigil {
                b'@' => format!("@{}", name),
                b'%' => format!("%{}", name),
                b'&' => format!("&{}", name),
                _ => name,
            }
        };
        let mut p = super::helpers::make_param(param_name);
        p.required = required || post_required;
        p.optional_marker = opt_marker || post_opt_marker;
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.sub_signature = Some(sub_params);
        p.traits = param_traits;
        return Ok((r, p));
    }

    // Sub-signature with brackets after variable: @a [$x, $y] or $a [$x, $y]
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::super::sub::parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let (r, _) = ws(r)?;
        // Handle optional (?) / required (!) suffix after sub-signature
        let (r, post_required, post_opt_marker) = super::helpers::parse_required_suffix(r);
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = super::super::keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = super::super::ident(r2)?;
            let (r2, _) = super::super::sub::validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let param_name = if slurpy {
            match slurpy_sigil {
                Some('%') => format!("%{}", name),
                Some('@') => format!("@{}", name),
                _ => name,
            }
        } else if named && original_sigil == b'&' {
            name
        } else {
            match original_sigil {
                b'@' => format!("@{}", name),
                b'%' => format!("%{}", name),
                b'&' => format!("&{}", name),
                _ => name,
            }
        };
        let mut p = super::helpers::make_param(param_name);
        p.required = required || post_required;
        p.optional_marker = opt_marker || post_opt_marker;
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.onearg = onearg;
        p.type_constraint = type_constraint;
        p.sub_signature = Some(sub_params);
        p.traits = param_traits;
        return Ok((r, p));
    }

    // Default value
    let mut default_src = String::new();
    let (rest, mut default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let after_eq = &rest[1..];
        let (expr_start, _) = ws(after_eq)?;
        let (rest, expr) = expression(expr_start)?;
        default_src = expr_start[..expr_start.len() - rest.len()]
            .trim()
            .to_string();
        (rest, Some(expr))
    } else {
        (rest, None)
    };

    // X::Parameter::AfterDefault: a trait (`is rw`) or post-constraint
    // (`where Int`) that appears AFTER the default value is illegal.
    if default.is_some() {
        let (after_default, _) = ws(rest)?;
        if let Some(after_is) = super::super::keyword("is", after_default) {
            let (trait_start, _) = ws1(after_is)?;
            let (_, trait_name) = super::super::ident(trait_start)?;
            return Err(super::helpers::after_default_error(
                "trait",
                &format!("is {trait_name}"),
                &default_src,
            ));
        }
        if let Some(after_where) = super::super::keyword("where", after_default) {
            let (constraint_start, _) = ws1(after_where)?;
            let (rest_after, _) =
                super::where_constraint::parse_where_constraint_expr(constraint_start)?;
            let constraint_src = constraint_start[..constraint_start.len() - rest_after.len()]
                .trim()
                .to_string();
            return Err(super::helpers::after_default_error(
                "post constraint",
                &format!("where {constraint_src}"),
                &default_src,
            ));
        }
    }

    // `is copy`, `is rw`, `is readonly`, `is raw` traits (may have multiple)
    let (mut rest, _) = ws(rest)?;
    let mut param_traits = Vec::new();
    while let Some(r) = super::super::keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = super::super::ident(r)?;
        let (r, _) = super::super::sub::validate_param_trait(&trait_name, &param_traits, r)?;
        param_traits.push(trait_name);
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `where` constraint
    let (rest, where_constraint) = if let Some(r) = super::super::keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, constraint) = super::where_constraint::parse_where_constraint_expr(r)?;
        (r, Some(Box::new(constraint)))
    } else {
        (rest, None)
    };
    let (rest_ws, _) = ws(rest)?;
    let (rest, late_default) =
        if default.is_none() && rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
            let rest = &rest_ws[1..];
            let (rest, _) = ws(rest)?;
            let (rest, expr) = expression(rest)?;
            (rest, Some(expr))
        } else {
            (rest_ws, None)
        };
    if late_default.is_some() {
        default = late_default;
    }

    // Prefix the name with the sigil so runtime can distinguish types.
    // For slurpy params, use the slurpy_sigil; for non-slurpy, use original_sigil.
    let param_name = if slurpy {
        match slurpy_sigil {
            Some('%') => format!("%{}", name),
            Some('@') => format!("@{}", name),
            _ => name,
        }
    } else if named && original_sigil == b'&' {
        name
    } else if param_sigil == Some(b'@') {
        format!("@{}", name)
    } else if param_sigil == Some(b'%') {
        format!("%{}", name)
    } else {
        match original_sigil {
            b'@' => format!("@{}", name),
            b'%' => format!("%{}", name),
            b'&' => format!("&{}", name),
            _ => name,
        }
    };
    let mut p = super::helpers::make_param(param_name);
    p.default = default;
    p.required = required;
    p.optional_marker = opt_marker;
    p.named = named;
    p.slurpy = slurpy;
    p.double_slurpy = double_slurpy;
    p.onearg = onearg;
    p.type_constraint = type_constraint;
    p.where_constraint = where_constraint;
    p.traits = param_traits;
    p.code_signature = code_sig;
    p.shape_constraints = shape_constraints;
    Ok((rest, p))
}
