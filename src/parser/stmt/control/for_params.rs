use super::*;

/// Parsed for-loop parameter info: `(param, param_def, params, rw_block, explicit_zero_params)`.
type ForParams = (
    Option<String>,
    Option<ParamDef>,
    Vec<String>,
    Vec<ParamDef>,
    bool,
    bool,
);

/// Parse for loop parameters: -> $param or -> $a, $b
/// Returns `(param, param_def, params, rw_block)`.
/// `rw_block` is `true` when `<->` is used instead of `->`.
pub(crate) fn parse_for_params(input: &str) -> PResult<'_, ForParams> {
    fn skip_pointy_return_type<'a>(mut r: &'a str) -> PResult<'a, ()> {
        let (r2, _) = ws(r)?;
        r = r2;
        if let Some(after_arrow) = r.strip_prefix("-->") {
            let (after_arrow, _) = super::super::parse_return_type_annotation_pub(after_arrow)?;
            let (after_arrow, _) = ws(after_arrow)?;
            Ok((after_arrow, ()))
        } else {
            Ok((r, ()))
        }
    }

    // Check for `<->` (rw pointy block) — must check before `->`
    let rw_block = input.starts_with("<->");
    let pointy_stripped = if rw_block {
        Some(&input[3..])
    } else {
        input.strip_prefix("->")
    };
    if let Some(stripped) = pointy_stripped {
        let (r, _) = ws(stripped)?;
        // Zero-parameter pointy block: for @a -> { ... }
        // Explicitly declares zero params — passing any arg should throw.
        if r.starts_with('{') {
            return Ok((r, (None, None, Vec::new(), Vec::new(), rw_block, true)));
        }
        // Parenthesized destructuring pointy param:
        //   -> ($a, $b) { ... }
        //   -> (:key($k), :value($v)) { ... }
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::super::parse_param_list_pub(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, _) = skip_pointy_return_type(r)?;
            let unpack_name = "__for_unpack".to_string();
            let unpack_def = ParamDef {
                name: unpack_name.clone(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy: false,
                sigilless: false,
                type_constraint: None,
                literal_value: None,
                sub_signature: Some(sub_params),
                where_constraint: None,
                traits: Vec::new(),
                double_slurpy: false,
                onearg: false,
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            };
            return Ok((
                r,
                (
                    Some(unpack_name),
                    Some(unpack_def),
                    Vec::new(),
                    Vec::new(),
                    rw_block,
                    false,
                ),
            ));
        }
        // Parenthesized pointy parameter list: -> ($a, $b) { ... }
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::super::parse_param_list_pub(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, _) = skip_pointy_return_type(r)?;
            if sub_params.is_empty() {
                return Ok((r, (None, None, Vec::new(), Vec::new(), rw_block, false)));
            }
            let unpack_name = "__for_unpack".to_string();
            let unpack_def = ParamDef {
                name: unpack_name.clone(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy: false,
                sigilless: false,
                type_constraint: None,
                literal_value: None,
                sub_signature: Some(sub_params),
                where_constraint: None,
                traits: Vec::new(),
                double_slurpy: false,
                onearg: false,
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            };
            return Ok((
                r,
                (
                    Some(unpack_name),
                    Some(unpack_def),
                    Vec::new(),
                    Vec::new(),
                    rw_block,
                    false,
                ),
            ));
        }
        // Positional destructuring pointy param: -> [$a, $b] { ... }
        if let Some(mut r) = r.strip_prefix('[') {
            let (r2, _) = ws(r)?;
            r = r2;
            let mut sub_params = Vec::new();
            if !r.starts_with(']') {
                loop {
                    let (r2, param_def) = parse_for_pointy_param(r)?;
                    sub_params.push(param_def);
                    let (r2, _) = ws(r2)?;
                    if let Some(r3) = r2.strip_prefix(',') {
                        let (r3, _) = ws(r3)?;
                        r = r3;
                        continue;
                    }
                    r = r2;
                    break;
                }
            }
            let (r, _) = parse_char(r, ']')?;
            let (r, _) = skip_pointy_return_type(r)?;
            let unpack_name = "__for_unpack".to_string();
            let unpack_def = ParamDef {
                name: unpack_name.clone(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy: false,
                sigilless: false,
                type_constraint: None,
                literal_value: None,
                sub_signature: Some(sub_params),
                where_constraint: None,
                traits: Vec::new(),
                double_slurpy: false,
                onearg: false,
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            };
            return Ok((
                r,
                (
                    Some(unpack_name),
                    Some(unpack_def),
                    Vec::new(),
                    Vec::new(),
                    rw_block,
                    false,
                ),
            ));
        }
        let (r, mut first_def) = parse_for_pointy_param(r)?;
        let first = first_def.name.clone();
        let (r, _) = ws(r)?;
        let (r, _) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::super::parse_param_list_pub(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            first_def.sub_signature = Some(sub_params);
            (r, ())
        } else if r.starts_with('[') {
            let (r, _) = parse_char(r, '[')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::super::parse_param_list_pub(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            first_def.sub_signature = Some(sub_params);
            (r, ())
        } else {
            (r, ())
        };
        let (r, _) = ws(r)?;
        if r.starts_with(',') {
            let first_param = if first_def.sigilless {
                format!("\\{}", first)
            } else {
                first
            };
            let mut params = vec![first_param];
            let mut any_rw = rw_block || first_def.traits.iter().any(|t| t == "rw");
            // Keep the full ParamDef per multi-param so the compiler can emit an
            // arity check (required params) and default-value binds (`-> $a, $b = 7`).
            let mut params_def = vec![first_def];
            let mut r = r;
            loop {
                let (r2, _) = parse_char(r, ',')?;
                let (r2, _) = ws(r2)?;
                let (r2, next) = parse_for_pointy_param(r2)?;
                if next.traits.iter().any(|t| t == "rw") {
                    any_rw = true;
                }
                // Prefix sigilless params with \\ so the compiler can
                // emit MarkSigillessReadonly for them.
                if next.sigilless {
                    params.push(format!("\\{}", next.name));
                } else {
                    params.push(next.name.clone());
                }
                params_def.push(next);
                let (r2, _) = ws(r2)?;
                if !r2.starts_with(',') {
                    r = r2;
                    break;
                }
                r = r2;
            }
            let (r, _) = skip_pointy_return_type(r)?;
            Ok((r, (None, None, params, params_def, any_rw, false)))
        } else {
            let (r, _) = skip_pointy_return_type(r)?;
            Ok((
                r,
                (
                    Some(first),
                    Some(first_def),
                    Vec::new(),
                    Vec::new(),
                    rw_block,
                    false,
                ),
            ))
        }
    } else {
        Ok((input, (None, None, Vec::new(), Vec::new(), false, false)))
    }
}

fn parse_for_pointy_param(input: &str) -> PResult<'_, ParamDef> {
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
                where_constraint: None,
                traits: Vec::new(),
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            },
        ));
    }

    let rest = input;
    let mut type_constraint = None;
    let rest = if let Ok((r, tc)) = ident(rest) {
        // Preserve type smileys :D, :U, :_ as part of the type constraint.
        let (r, tc) = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            let smiley = &r[..2];
            (&r[2..], format!("{}{}", tc, smiley))
        } else {
            (r, tc)
        };
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') || r2.starts_with('&')
        {
            type_constraint = Some(tc);
            r2
        } else {
            rest
        }
    } else {
        rest
    };

    let for_original_sigil = rest.as_bytes().first().copied().unwrap_or(b'$');
    let (r, name) = var_name(rest)?;

    // Shape constraint for array parameters
    let mut shape_constraints = None;
    let r = if for_original_sigil == b'@' && r.starts_with('[') {
        let (r2, dims) = parse_array_shape_suffix(r)?;
        shape_constraints = Some(dims);
        r2
    } else {
        r
    };

    // `$x?` marks an optional param, `$x!` an explicitly-required one. Track the
    // optional marker so the compiler does not count `$x?` toward the required
    // arity of a multi-param loop.
    let mut optional_marker = false;
    let mut rest = if let Some(after) = r.strip_prefix('?') {
        optional_marker = true;
        after
    } else if let Some(after) = r.strip_prefix('!') {
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

    let (r, _) = ws(rest)?;
    let mut default = None;
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

    let param_name = match for_original_sigil {
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
            required: false,
            named: false,
            slurpy: false,
            double_slurpy: false,
            onearg: false,
            sigilless: false,
            type_constraint,
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            traits,
            optional_marker,
            outer_sub_signature: None,
            code_signature: None,
            is_invocant: false,
            shape_constraints,
        },
    ))
}
