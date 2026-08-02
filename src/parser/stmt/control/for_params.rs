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
            // See the `[` branch below: a comma after the pattern means several
            // destructuring parameters, one per chunk element.
            let (after_comma, _) = ws(r)?;
            if after_comma.starts_with(',') {
                return parse_multi_destructuring_params(stripped, rw_block);
            }
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
                block_param: true,
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
                block_param: true,
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
            // More than one destructuring parameter (`-> [$a, $b], [$c, $d]`)
            // needs the general multi-parameter path: each pattern binds one
            // element of the chunk and unpacks it. The single-pattern shape below
            // instead destructures the whole iteration value.
            let (after_comma, _) = ws(r)?;
            if after_comma.starts_with(',') {
                return parse_multi_destructuring_params(stripped, rw_block);
            }
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
                block_param: true,
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
                // A later parameter may itself be a destructuring pattern
                // (`-> $a, [$b, $c]`); it binds one chunk element and unpacks it,
                // so give it a synthetic name for the compiler to bind first.
                let (r2, mut next) = parse_destructuring_or_plain_param(r2)?;
                if next.name.is_empty() {
                    next.name = format!("__for_unpack_{}", params_def.len());
                }
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

/// Parse a pointy parameter list in which at least one parameter is a
/// destructuring pattern and there is more than one parameter:
/// `-> [$target, $variant], [$expected, $desc] { ... }`.
///
/// Each entry binds one element of the iteration chunk, exactly like an ordinary
/// multi-parameter pointy block; a pattern entry then unpacks the element it
/// bound. It gets a synthetic name so the compiler has something to bind to
/// before destructuring it.
///
/// `input` is the text just after the `->` / `<->`.
fn parse_multi_destructuring_params(input: &str, rw_block: bool) -> PResult<'_, ForParams> {
    let mut r = input;
    let mut params = Vec::new();
    let mut params_def = Vec::new();
    let mut any_rw = rw_block;
    loop {
        let (r2, _) = ws(r)?;
        let (r2, mut def) = parse_destructuring_or_plain_param(r2)?;
        if def.sub_signature.is_some() && def.name.is_empty() {
            def.name = format!("__for_unpack_{}", params_def.len());
        }
        if def.traits.iter().any(|t| t == "rw") {
            any_rw = true;
        }
        params.push(if def.sigilless {
            format!("\\{}", def.name)
        } else {
            def.name.clone()
        });
        params_def.push(def);
        let (r2, _) = ws(r2)?;
        let Some(r3) = r2.strip_prefix(',') else {
            r = r2;
            break;
        };
        r = r3;
    }
    let (r, _) = ws(r)?;
    let r = if let Some(after_arrow) = r.strip_prefix("-->") {
        let (after_arrow, _) = super::super::parse_return_type_annotation_pub(after_arrow)?;
        let (after_arrow, _) = ws(after_arrow)?;
        after_arrow
    } else {
        r
    };
    Ok((r, (None, None, params, params_def, any_rw, false)))
}

/// One entry of a pointy parameter list: a `[...]` / `(...)` destructuring
/// pattern (returned with an empty name for the caller to fill in) or an
/// ordinary parameter.
fn parse_destructuring_or_plain_param(input: &str) -> PResult<'_, ParamDef> {
    let (open, close) = match input.as_bytes().first() {
        Some(b'[') => ('[', ']'),
        Some(b'(') => ('(', ')'),
        _ => return parse_for_pointy_param(input),
    };
    let (r, _) = parse_char(input, open)?;
    let (r, _) = ws(r)?;
    let (r, sub_params) = super::super::parse_param_list_pub(r)?;
    let (r, _) = ws(r)?;
    let (r, _) = parse_char(r, close)?;
    Ok((
        r,
        ParamDef {
            name: String::new(),
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
            block_param: true,
        },
    ))
}

fn parse_for_pointy_param(input: &str) -> PResult<'_, ParamDef> {
    let rest = input;
    let mut type_constraint = None;
    // Use parse_type_constraint_expr so coercion types (`IO()`, `Int(Str)`),
    // qualified names, generics (`Array[Int]`) and definedness smileys (:D/:U)
    // are all consumed before the loop variable (e.g. `-> IO() $current`).
    // A sigilless param (`\name`) may also carry one: `-> Mu \type { ... }`.
    let rest = if let Some((r, tc)) = super::super::sub_param::parse_type_constraint_expr(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('$')
            || r2.starts_with('@')
            || r2.starts_with('%')
            || r2.starts_with('&')
            || r2.starts_with('\\')
        {
            type_constraint = Some(tc);
            r2
        } else {
            rest
        }
    } else {
        rest
    };

    // Sigilless parameter: \name
    if let Some(r) = rest.strip_prefix('\\') {
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
                type_constraint,
                literal_value: None,
                sub_signature: None,
                where_constraint: None,
                traits: Vec::new(),
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
                block_param: true,
            },
        ));
    }

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
            block_param: true,
        },
    ))
}
