use super::*;

pub(crate) fn parse_pointy_param(input: &str) -> PResult<'_, ParamDef> {
    // Sigilless parameter: \name
    if let Some(r) = input.strip_prefix('\\') {
        let (rest, name) = ident(r)?;
        return Ok((
            rest,
            ParamDef {
                name,
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy: false,
                double_slurpy: false,
                onearg: false,
                sigilless: true,
                type_constraint: None,
                literal_value: None,
                sub_signature: None,
                outer_sub_signature: None,
                code_signature: None,
                where_constraint: None,
                traits: Vec::new(),
                optional_marker: false,
                is_invocant: false,
                shape_constraints: None,
            },
        ));
    }
    // Optional type constraint before the variable
    // Use parse_type_constraint_expr to handle coercion types (e.g., Numeric(Cool)),
    // qualified names (Int::Odd), definedness markers (:D/:U), etc.
    let mut rest = input;
    let mut type_constraint = None;
    rest = if let Some((r, tc)) = super::super::sub_param::parse_type_constraint_expr(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('$')
            || r2.starts_with('@')
            || r2.starts_with('%')
            || r2.starts_with('&')
            || r2.starts_with('*')
            || (r2.starts_with(':')
                && r2.len() > 1
                && matches!(r2.as_bytes()[1], b'$' | b'@' | b'%' | b'&'))
        {
            type_constraint = Some(tc);
            r2
        } else if r2.starts_with('{') || r2.starts_with(',') || r2.starts_with("-->") {
            // Type-only parameter in pointy block (e.g., `-> True { }`, `-> Int { }`)
            let tc_for_bool = if tc == "True" || tc == "False" {
                super::super::super::add_parse_warning(format!(
                    "Potential difficulties:\n    Literal values in signatures are smartmatched against and smartmatch with `{}` will always {}. Use the `where` clause instead.",
                    tc,
                    if tc == "True" { "succeed" } else { "fail" }
                ));
                "Bool".to_string()
            } else {
                tc.clone()
            };
            return Ok((
                r2,
                ParamDef {
                    name: "__type_only__".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: false,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: Some(tc_for_bool),
                    literal_value: None,
                    sub_signature: None,
                    outer_sub_signature: None,
                    code_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    is_invocant: false,
                    shape_constraints: None,
                },
            ));
        } else {
            rest
        }
    } else {
        rest
    };

    // Slurpy marker for pointy params: *@a, *%h, *$x, *&cb, and double-slurpy variants.
    let mut slurpy = false;
    let mut double_slurpy = false;
    let mut onearg = false;
    if rest.starts_with("**")
        && rest.len() > 2
        && matches!(rest.as_bytes()[2], b'@' | b'%' | b'$' | b'&')
    {
        slurpy = true;
        double_slurpy = true;
        rest = &rest[2..];
    } else if rest.starts_with('*')
        && rest.len() > 1
        && matches!(rest.as_bytes()[1], b'@' | b'%' | b'$' | b'&')
    {
        slurpy = true;
        rest = &rest[1..];
    } else if rest.starts_with('+')
        && rest.len() > 1
        && matches!(rest.as_bytes()[1], b'@' | b'%' | b'$' | b'&')
    {
        // Single-argument-rule slurpy: `-> +@foo { ... }`.
        slurpy = true;
        onearg = true;
        rest = &rest[1..];
    }

    // Capture-all pointy parameter: -> |, -> |$c, -> |c
    if let Some(stripped) = rest.strip_prefix('|') {
        let (r, _) = ws(stripped)?;
        // Optional capture variable name with sigil
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') || r.starts_with('&') {
            let (r, name) = var_name(r)?;
            return Ok((
                r,
                ParamDef {
                    name,
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint,
                    literal_value: None,
                    sub_signature: None,
                    outer_sub_signature: None,
                    code_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    is_invocant: false,
                    shape_constraints: None,
                },
            ));
        }

        // Sigilless capture variable name
        if let Ok((r, name)) = ident(r)
            && !matches!(name.as_str(), "where" | "is")
        {
            return Ok((
                r,
                ParamDef {
                    name,
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: true,
                    type_constraint,
                    literal_value: None,
                    sub_signature: None,
                    outer_sub_signature: None,
                    code_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    is_invocant: false,
                    shape_constraints: None,
                },
            ));
        }

        // Bare capture marker. An anonymous capture absorbs both positional and
        // named arguments, exactly like a named capture (|c), so it must be
        // sigilless for binding/dispatch to treat it as a full slurpy.
        return Ok((
            r,
            ParamDef {
                name: "_capture".to_string(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy: true,
                double_slurpy: false,
                onearg: false,
                sigilless: true,
                type_constraint,
                literal_value: None,
                sub_signature: None,
                outer_sub_signature: None,
                code_signature: None,
                where_constraint: None,
                traits: Vec::new(),
                optional_marker: false,
                is_invocant: false,
                shape_constraints: None,
            },
        ));
    }

    // Anonymous callable pointy parameter with code signature: -> &:(Str) { ... }
    if rest.starts_with("&:(") {
        let r = &rest[1..]; // skip '&'
        let (r, _) = parse_char(r, ':')?;
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, (sig_params, sig_ret)) = super::sub::parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;

        let mut optional_marker = false;
        let mut required_marker = false;
        let mut rest = if let Some(after) = r.strip_prefix('?') {
            optional_marker = true;
            after
        } else if let Some(after) = r.strip_prefix('!') {
            required_marker = true;
            after
        } else {
            r
        };

        let mut traits = Vec::new();
        loop {
            let (r, _) = ws(rest)?;
            let Some(after_is) = keyword("is", r) else {
                rest = r;
                break;
            };
            let (after_is, _) = ws1(after_is)?;
            let (after_is, trait_name) = ident(after_is)?;
            sub::validate_param_trait_pub(&trait_name, &traits, after_is)?;
            traits.push(trait_name);
            rest = after_is;
        }
        return Ok((
            rest,
            ParamDef {
                name: "&".to_string(),
                default: None,
                multi_invocant: true,
                required: required_marker,
                named: false,
                slurpy,
                double_slurpy,
                onearg: false,
                sigilless: false,
                type_constraint,
                literal_value: None,
                sub_signature: None,
                outer_sub_signature: None,
                code_signature: Some((sig_params, sig_ret)),
                where_constraint: None,
                traits,
                optional_marker,
                is_invocant: false,
                shape_constraints: None,
            },
        ));
    }

    // Literal value parameter in pointy signatures, e.g. `-> 42 { ... }`
    if rest.starts_with(|c: char| c.is_ascii_digit()) {
        let mut end = 0usize;
        for (idx, ch) in rest.char_indices() {
            if ch.is_ascii_digit() || ch == '_' {
                end = idx + ch.len_utf8();
            } else {
                break;
            }
        }
        let token = &rest[..end];
        let parsed = token.replace('_', "").parse::<i64>().unwrap_or(0);
        let rest = &rest[end..];
        return Ok((
            rest,
            ParamDef {
                name: "__literal__".to_string(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy,
                double_slurpy,
                onearg: false,
                sigilless: false,
                type_constraint,
                literal_value: Some(crate::value::Value::Int(parsed)),
                sub_signature: None,
                outer_sub_signature: None,
                code_signature: None,
                where_constraint: None,
                traits: Vec::new(),
                optional_marker: false,
                is_invocant: false,
                shape_constraints: None,
            },
        ));
    }

    // Named parameter prefix: :$x, :@l, :%h
    let mut named = false;
    if rest.starts_with(':')
        && rest.len() > 1
        && matches!(rest.as_bytes()[1], b'$' | b'@' | b'%' | b'&')
    {
        named = true;
        rest = &rest[1..];
    }

    let original_sigil = rest.as_bytes().first().copied().unwrap_or(b'$');
    let (rest, name) = var_name(rest)?;

    // Shape constraint for array parameters: @a[3], @a[*]
    let mut shape_constraints = None;
    let rest = if original_sigil == b'@' && rest.starts_with('[') {
        let (r, dims) = parse_array_shape_suffix(rest)?;
        shape_constraints = Some(dims);
        r
    } else {
        rest
    };

    // Optional marker on pointy params: $x? / $x!
    let mut optional_marker = false;
    let mut required_marker = false;
    let mut rest = if let Some(after) = rest.strip_prefix('?') {
        optional_marker = true;
        after
    } else if let Some(after) = rest.strip_prefix('!') {
        required_marker = true;
        after
    } else {
        rest
    };

    // Optional parameter traits: `is rw`, `is copy`, ...
    let mut traits = Vec::new();
    loop {
        let (r, _) = ws(rest)?;
        let Some(after_is) = keyword("is", r) else {
            rest = r;
            break;
        };
        let (after_is, _) = ws1(after_is)?;
        let (after_is, trait_name) = ident(after_is)?;
        sub::validate_param_trait_pub(&trait_name, &traits, after_is)?;
        traits.push(trait_name);
        rest = after_is;
    }

    // Optional default value: `$x = expr`
    let mut default = None;
    let (r, _) = ws(rest)?;
    if let Some(after_eq) = r.strip_prefix('=')
        && !after_eq.starts_with('>')
    {
        let (after_eq, _) = ws(after_eq)?;
        let (after_default, default_expr) = expression(after_eq)?;
        default = Some(default_expr);
        rest = after_default;
    } else {
        rest = r;
    }

    // Optional unpacking sub-signature: `-> Pair $p (:$key, :$value) { ... }`
    // Parse and preserve the sub-signature for runtime binding.
    let mut sub_signature = None;
    let (r, _) = ws(rest)?;
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::sub::parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        sub_signature = Some(sub_params);
        rest = r;
    } else {
        rest = r;
    }

    // Optional where-constraint on pointy parameters: `-> $x where Int|Bool { ... }`
    let (r, _) = ws(rest)?;
    let (rest, where_constraint) = if let Some(r) = keyword("where", r) {
        let (r, _) = ws1(r)?;
        let (r, constraint) = if r.starts_with('{') {
            let (r, body) = crate::parser::primary::parse_block_body(r)?;
            (
                r,
                Expr::AnonSub {
                    body,
                    is_rw: false,
                    is_block: true,
                },
            )
        } else {
            expression(r)?
        };
        (r, Some(Box::new(constraint)))
    } else {
        (r, None)
    };

    // Prefix name with sigil for proper runtime binding
    let param_name = match original_sigil {
        b'@' => format!("@{}", name),
        b'%' => format!("%{}", name),
        b'&' => format!("&{}", name),
        _ => name,
    };
    Ok((
        rest,
        ParamDef {
            name: param_name,
            default,
            multi_invocant: true,
            required: required_marker,
            named,
            slurpy,
            double_slurpy,
            onearg,
            sigilless: false,
            type_constraint,
            literal_value: None,
            sub_signature,
            outer_sub_signature: None,
            code_signature: None,
            where_constraint,
            traits,
            optional_marker,
            is_invocant: false,
            shape_constraints,
        },
    ))
}
