use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, parse_char};

use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;

use super::decl::parse_array_shape_suffix;
use super::sub::{
    literal_value_from_expr, parse_indirect_decl_name, parse_param_list,
    parse_param_list_with_return, parse_sub_name, parse_sub_traits, validate_param_trait,
    validate_signature_params,
};
use super::{block, ident, keyword, qualified_ident, var_name};

pub(super) fn mark_params_as_invocant(params: &mut [ParamDef]) {
    for param in params {
        if !param.traits.iter().any(|t| t == "invocant") {
            param.traits.push("invocant".to_string());
        }
    }
}

/// Helper to construct a default ParamDef with only required fields.
fn make_param(name: String) -> ParamDef {
    ParamDef {
        name,
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        sigilless: false,
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
    }
}

pub(super) fn is_anonymous_sigil_param(param: &ParamDef) -> bool {
    matches!(
        param.name.as_str(),
        "__ANON_STATE__" | "__ANON_ARRAY__" | "__ANON_HASH__" | "__ANON_CODE__"
    )
}

pub(super) fn starts_with_sigil_param(input: &str) -> bool {
    matches!(input.as_bytes().first(), Some(b'$' | b'@' | b'%' | b'&'))
}

pub(super) fn parse_implicit_invocant_marker(input: &str) -> Option<(&str, String)> {
    if input.starts_with('$')
        || input.starts_with('@')
        || input.starts_with('%')
        || input.starts_with('&')
        || input.starts_with('*')
        || input.starts_with(':')
    {
        return None;
    }
    let (mut rest, mut type_name) = qualified_ident(input).ok()?;
    while rest.starts_with('[') {
        let (r2, suffix) = parse_generic_suffix(rest).ok()?;
        type_name.push_str(&suffix);
        rest = r2;
    }
    if rest.starts_with(":D") || rest.starts_with(":U") || rest.starts_with(":_") {
        type_name.push_str(&rest[..2]);
        rest = &rest[2..];
    }
    let after_colon = rest.strip_prefix(':')?;
    if after_colon.starts_with(':') {
        return None;
    }
    let (after_colon, _) = ws(after_colon).ok()?;
    Some((after_colon, type_name))
}

/// Returns (rest, required, optional_marker).
/// `!` → required=true, optional_marker=false
/// `?` → required=false, optional_marker=true
/// neither → required=false, optional_marker=false
fn parse_required_suffix(input: &str) -> (&str, bool, bool) {
    if let Some(rest) = input.strip_prefix('!') {
        (rest, true, false)
    } else if let Some(rest) = input.strip_prefix('?') {
        (rest, false, true)
    } else {
        (input, false, false)
    }
}

pub(super) fn parse_type_constraint_expr(input: &str) -> Option<(&str, String)> {
    let (mut rest, mut type_name) = qualified_ident(input).ok()?;
    while rest.starts_with('[') {
        let (r2, suffix) = parse_generic_suffix(rest).ok()?;
        type_name.push_str(&suffix);
        rest = r2;
    }
    if rest.starts_with(":D") || rest.starts_with(":U") || rest.starts_with(":_") {
        type_name.push_str(&rest[..2]);
        rest = &rest[2..];
    }

    let (r2, _) = ws(rest).ok()?;
    if let Some(after_open) = r2.strip_prefix('(') {
        let (after_ws, _) = ws(after_open).ok()?;
        if let Some(r3) = after_ws.strip_prefix(')') {
            let (r3, _) = ws(r3).ok()?;
            return Some((r3, format!("{}()", type_name)));
        }
        if let Some((inner_r, source_type)) = parse_type_constraint_expr(after_ws) {
            let (inner_r, _) = ws(inner_r).ok()?;
            if let Some(r3) = inner_r.strip_prefix(')') {
                let (r3, _) = ws(r3).ok()?;
                return Some((r3, format!("{}({})", type_name, source_type)));
            }
        }
    }
    Some((rest, type_name))
}

fn parse_where_constraint_expr(input: &str) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    if r.starts_with('{') {
        let (r, body) = crate::parser::primary::parse_block_body(r)?;
        return Ok((r, Expr::AnonSub { body, is_rw: false }));
    }
    expression(input)
}

fn parse_generic_suffix(input: &str) -> PResult<'_, String> {
    if !input.starts_with('[') {
        return Ok((input, String::new()));
    }
    let mut depth = 0usize;
    let mut end = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return Err(PError::expected("matching ']'"));
                }
                depth -= 1;
                if depth == 0 {
                    end = idx + ch.len_utf8();
                    break;
                }
            }
            _ => {}
        }
    }
    if end == 0 {
        return Err(PError::expected("matching ']'"));
    }
    Ok((&input[end..], input[..end].to_string()))
}

pub(super) fn parse_single_param(input: &str) -> PResult<'_, ParamDef> {
    let mut rest = input;
    let mut named = false;
    let mut slurpy = false;
    let mut type_constraint = None;

    // Array sub-signature: [Type1, Type2, ...] → anonymous @ with sub-sig
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let mut p = make_param("@".to_string());
        p.sub_signature = Some(sub_params);
        return Ok((r, p));
    }

    // Capture-all: (|), (|$c), (|c), or capture sub-signature forms like | ($x)
    if let Some(stripped) = rest.strip_prefix('|') {
        let (r, _) = ws(stripped)?;
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = parse_param_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, _) = ws(r)?;
            if sub_params.len() == 1 {
                return Ok((r, sub_params[0].clone()));
            }
            let mut p = make_param("__subsig__".to_string());
            p.sub_signature = Some(sub_params);
            return Ok((r, p));
        }
        let (r, _) = ws(r)?;
        // Optional capture variable name with sigil, optionally followed by
        // a capture sub-signature: |$c ($a, $b?)
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            let (r, name) = var_name(r)?;
            let mut p = make_param(name);
            p.slurpy = true;
            let (r, _) = ws(r)?;
            if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, sub_params) = parse_param_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                p.sub_signature = Some(sub_params);
                return Ok((r, p));
            }
            return Ok((r, p));
        }
        // Sigilless capture variable name: |c, |args, optionally followed by
        // a capture sub-signature: |c ($a, $b?)
        if let Ok((r_ident, name)) = ident(r)
            && !matches!(name.as_str(), "where" | "is")
        {
            let mut p = make_param(name);
            p.slurpy = true;
            p.sigilless = true;
            let (r_ident, _) = ws(r_ident)?;
            if r_ident.starts_with('(') {
                let (r_ident, _) = parse_char(r_ident, '(')?;
                let (r_ident, _) = ws(r_ident)?;
                let (r_ident, sub_params) = parse_param_list(r_ident)?;
                let (r_ident, _) = ws(r_ident)?;
                let (r_ident, _) = parse_char(r_ident, ')')?;
                p.sub_signature = Some(sub_params);
                return Ok((r_ident, p));
            }
            return Ok((r_ident, p));
        }
        // Bare |, possibly followed by traits/where
        let mut p = make_param("_capture".to_string());
        p.slurpy = true;
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let (r, where_constraint) = if let Some(r2) = keyword("where", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, constraint) = parse_where_constraint_expr(r2)?;
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
    // Unary plus marker on parameters (e.g. +@a). Keep parsing semantics
    // aligned with regular sigiled params for now.
    if rest.starts_with('+')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        rest = &rest[1..];
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
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let mut p = make_param("@".to_string());
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
        if let Some(r) = r.strip_prefix(':') {
            // This invocant marker is handled at parse_param_list level.
            let (r, _) = ws(r)?;
            if r.starts_with(')') || r.starts_with("-->") {
                let mut p = make_param("self".to_string());
                p.type_constraint = Some(tc);
                p.is_invocant = true;
                p.traits.push("invocant".to_string());
                return Ok((r, p));
            }
            return parse_single_param(r);
        }
        let (r, _) = ws(r)?;
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            type_constraint = Some(tc);
            rest = r;
        } else {
            let mut p = make_param("self".to_string());
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
        && let Ok((r, capture_name)) = ident(after_capture)
    {
        type_constraint = Some(format!("::{}", capture_name));
        let (r, _) = ws(r)?;
        rest = r;
        if rest.starts_with(')')
            || rest.starts_with(']')
            || rest.starts_with(',')
            || rest.starts_with(';')
        {
            let mut p = make_param(format!("__type_capture__{}", capture_name));
            p.type_constraint = type_constraint;
            p.named = named;
            p.slurpy = slurpy;
            return Ok((rest, p));
        }
        if rest.starts_with(':') {
            named = true;
            rest = &rest[1..];
        }
    }

    // Type constraint (may be qualified: IO::Path)
    // Skip type constraint parsing for named params with lowercase identifiers followed by '('
    // Parametric type constraint: ::T
    if let Some(after_colon) = rest.strip_prefix("::")
        && let Ok((r, tc_name)) = ident(after_colon)
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
        && let Some((r, tc)) = parse_type_constraint_expr(rest)
    {
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
            } else if let Some((inner_r, source_type)) = parse_type_constraint_expr(after_ws) {
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
                    let (r3, sub_params) = parse_param_list(r3)?;
                    let (r3, _) = ws(r3)?;
                    let (r3, _) = parse_char(r3, ')')?;
                    let (r3, _) = ws(r3)?;
                    let mut p = make_param("__subsig__".to_string());
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
                let (r3, sub_params) = parse_param_list(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                let mut p = make_param("__subsig__".to_string());
                p.type_constraint = Some(tc);
                p.sub_signature = Some(sub_params);
                p.named = named;
                p.slurpy = slurpy;
                // Handle optional (?) / required (!) suffix after sub-signature
                let (rest, required, opt_marker) = parse_required_suffix(r3);
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = ident(r)?;
                    validate_param_trait(&trait_name, &param_traits, r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                p.required = required;
                p.optional_marker = opt_marker;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = parse_where_constraint_expr(r)?;
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
            || r2.starts_with("-->")
        {
            // Bare identifier as type-only parameter (e.g., enum values in multi dispatch)
            // multi infix:<->(e1, e2) { ... }
            let mut p = make_param("__type_only__".to_string());
            p.type_constraint = Some(tc);
            p.named = named;
            p.slurpy = slurpy;
            return Ok((r2, p));
        }
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
        && let Some(v) = literal_value_from_expr(&lit_expr)
        && let Ok((after_lit, _)) = ws(lit_rest)
        && (after_lit.starts_with(')')
            || after_lit.starts_with(',')
            || after_lit.starts_with(';')
            || after_lit.starts_with(']')
            || after_lit.starts_with("-->"))
    {
        let mut p = make_param("__literal__".to_string());
        p.type_constraint = type_constraint;
        p.literal_value = Some(v);
        return Ok((after_lit, p));
    }

    // Sigilless parameter: \name
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = ident(r)?;
        let (r, _) = ws(r)?;
        // Default value
        let (r, default) = if r.starts_with('=') && !r.starts_with("==") {
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            (r, Some(expr))
        } else {
            (r, None)
        };
        let mut p = make_param(name);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.sigilless = true;
        p.default = default;
        p.type_constraint = type_constraint;
        return Ok((r, p));
    }

    // Sub-signature without type: just (inner-params)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = ws(r)?;
        let (r, required, opt_marker) = parse_required_suffix(r);
        let mut p = make_param("__subsig__".to_string());
        p.sub_signature = Some(sub_params);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        p.required = required;
        p.optional_marker = opt_marker;
        return Ok((r, p));
    }

    // Named parameter with alias: :key($var) or :value(&callback)
    if named && let Ok((r, alias_name)) = ident(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('(') {
            let (r3, _) = parse_char(r2, '(')?;
            let (r3, _) = ws(r3)?;
            // Inside could be a sub-signature or a single variable
            // Try to parse as a sub-signature first (handles nested params)
            let (r3, sub_params) = parse_param_list(r3)?;
            let (r3, _) = ws(r3)?;
            let (r3, _) = parse_char(r3, ')')?;
            // If it's a single simple variable param, use its name; otherwise sub-signature
            if sub_params.len() == 1
                && sub_params[0].sub_signature.is_none()
                && !sub_params[0].name.starts_with("__")
            {
                let mut p = make_param(alias_name.clone());
                p.named = true;
                p.slurpy = slurpy;
                p.double_slurpy = double_slurpy;
                p.type_constraint = type_constraint;
                p.sub_signature = Some(sub_params.clone());
                // Check for a sub-signature after the alias: :x($r) (Str $g, Any $i)
                let (r3, _) = ws(r3)?;
                if r3.starts_with('(') {
                    let (r4, _) = parse_char(r3, '(')?;
                    let (r4, _) = ws(r4)?;
                    let (r4, outer_sub) = parse_param_list(r4)?;
                    let (r4, _) = ws(r4)?;
                    let (r3_new, _) = parse_char(r4, ')')?;
                    p.outer_sub_signature = Some(outer_sub);
                    let (rest, alias_required, alias_opt_marker) = parse_required_suffix(r3_new);
                    p.required = alias_required;
                    p.optional_marker = alias_opt_marker;
                    let (rest, _) = ws(rest)?;
                    let mut param_traits = Vec::new();
                    let (mut rest, _) = ws(rest)?;
                    while let Some(r) = keyword("is", rest) {
                        let (r, _) = ws1(r)?;
                        let (r, trait_name) = ident(r)?;
                        validate_param_trait(&trait_name, &param_traits, r)?;
                        param_traits.push(trait_name);
                        let (r, _) = ws(r)?;
                        rest = r;
                    }
                    p.traits = param_traits;
                    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                        let (r, _) = ws1(r)?;
                        let (r, constraint) = parse_where_constraint_expr(r)?;
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
                let (rest, alias_required, alias_opt_marker) = parse_required_suffix(r3);
                p.required = alias_required;
                p.optional_marker = alias_opt_marker;
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = ident(r)?;
                    validate_param_trait(&trait_name, &param_traits, r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = parse_where_constraint_expr(r)?;
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
            let mut p = make_param(alias_name);
            p.named = true;
            p.slurpy = slurpy;
            p.double_slurpy = double_slurpy;
            p.type_constraint = type_constraint;
            p.sub_signature = Some(sub_params);
            // Handle optional (?) / required (!) suffix after sub-signature
            let (rest, required, opt_marker) = parse_required_suffix(r3);
            // Skip whitespace
            let (rest, _) = ws(rest)?;
            // Handle is copy, is rw, is readonly, is raw traits
            let mut param_traits = Vec::new();
            let (mut rest, _) = ws(rest)?;
            while let Some(r) = keyword("is", rest) {
                let (r, _) = ws1(r)?;
                let (r, trait_name) = ident(r)?;
                validate_param_trait(&trait_name, &param_traits, r)?;
                param_traits.push(trait_name);
                let (r, _) = ws(r)?;
                rest = r;
            }
            p.traits = param_traits;
            p.required = required;
            p.optional_marker = opt_marker;
            // Handle where constraint
            let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                let (r, _) = ws1(r)?;
                let (r, constraint) = parse_where_constraint_expr(r)?;
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

    // Anonymous optional scalar parameter: $?
    if let Some(after_q) = rest.strip_prefix("$?")
        && (after_q.is_empty()
            || after_q.starts_with(',')
            || after_q.starts_with(')')
            || after_q.starts_with(' ')
            || after_q.starts_with('\t')
            || after_q.starts_with('\n'))
    {
        let mut p = make_param("__ANON_OPTIONAL__".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        return Ok((after_q, p));
    }

    // Anonymous callable parameter with code signature: &:(Str --> Bool)
    if rest.starts_with("&:(") {
        let r = &rest[1..]; // skip '&'
        let (r, _) = parse_char(r, ':')?;
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, (sig_params, sig_ret)) = parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, required, opt_marker) = parse_required_suffix(r);
        let (r, _) = ws(r)?;

        let mut p = make_param("&".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        p.code_signature = Some((sig_params, sig_ret));
        p.required = required;
        p.optional_marker = opt_marker;

        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        p.traits = param_traits;

        let (r, where_constraint) = if let Some(r2) = keyword("where", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, constraint) = parse_where_constraint_expr(r2)?;
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
        let (rest, required, opt_marker) = parse_required_suffix(rest);
        let mut p = make_param("&".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        p.required = required;
        p.optional_marker = opt_marker;
        return Ok((rest, p));
    }

    // Capture the original sigil before var_name strips it
    let original_sigil = rest.as_bytes().first().copied().unwrap_or(b'$');
    let param_sigil = rest.as_bytes().first().copied();
    let (rest, name) = var_name(rest)?;

    // Shape constraint for array parameters: @a[3], @a[4,4], @a[*], @a[$n]
    let mut shape_constraints = None;
    let rest = if original_sigil == b'@' && rest.starts_with('[') {
        let (r, dims) = parse_array_shape_suffix(rest)?;
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
        let (r, (sig_params, sig_ret)) = parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        code_sig = Some((sig_params, sig_ret));
        r
    } else {
        rest
    };

    // Optional (?) / required (!) suffix
    let (rest, required, opt_marker) = parse_required_suffix(rest);
    let (rest, _) = ws(rest)?;

    // Sub-signature after variable: $x ($a, $b) or $ ($a, $b)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = ws(r)?;
        // Handle optional (?) / required (!) suffix after sub-signature
        let (r, post_required, post_opt_marker) = parse_required_suffix(r);
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
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
        let mut p = make_param(param_name);
        p.required = required || post_required;
        p.optional_marker = opt_marker || post_opt_marker;
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        p.sub_signature = Some(sub_params);
        p.traits = param_traits;
        return Ok((r, p));
    }

    // Sub-signature with brackets after variable: @a [$x, $y] or $a [$x, $y]
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let (r, _) = ws(r)?;
        // Handle optional (?) / required (!) suffix after sub-signature
        let (r, post_required, post_opt_marker) = parse_required_suffix(r);
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            validate_param_trait(&trait_name, &param_traits, r2)?;
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
        let mut p = make_param(param_name);
        p.required = required || post_required;
        p.optional_marker = opt_marker || post_opt_marker;
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        p.sub_signature = Some(sub_params);
        p.traits = param_traits;
        return Ok((r, p));
    }

    // Default value
    let (rest, mut default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        (rest, Some(expr))
    } else {
        (rest, None)
    };

    // `is copy`, `is rw`, `is readonly`, `is raw` traits (may have multiple)
    let (mut rest, _) = ws(rest)?;
    let mut param_traits = Vec::new();
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        validate_param_trait(&trait_name, &param_traits, r)?;
        param_traits.push(trait_name);
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `where` constraint
    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, constraint) = parse_where_constraint_expr(r)?;
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
    let mut p = make_param(param_name);
    p.default = default;
    p.required = required;
    p.optional_marker = opt_marker;
    p.named = named;
    p.slurpy = slurpy;
    p.double_slurpy = double_slurpy;
    p.type_constraint = type_constraint;
    p.where_constraint = where_constraint;
    p.traits = param_traits;
    p.code_signature = code_sig;
    p.shape_constraints = shape_constraints;
    Ok((rest, p))
}

/// Parse `method` declaration.
pub(super) fn method_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, multi) = if let Some(r) = keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = keyword("method", r).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        let r = keyword("method", input).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    method_decl_body(rest, multi, false)
}

/// Parse `submethod` declaration (treated like method, not inherited by subclasses).
pub(super) fn submethod_decl(input: &str) -> PResult<'_, Stmt> {
    let r = keyword("submethod", input).ok_or_else(|| PError::expected("submethod declaration"))?;
    let (r, _) = ws1(r)?;
    method_decl_body(r, false, false)
}

pub(super) fn method_decl_body(input: &str, multi: bool, is_our: bool) -> PResult<'_, Stmt> {
    let (rest, is_private) = if let Some(rest) = input.strip_prefix('!') {
        (rest, true)
    } else {
        (input, false)
    };
    let (rest, name, name_expr) = if rest.starts_with("::") {
        let (rest, (name, expr)) = parse_indirect_decl_name(rest)?;
        (rest, name, Some(expr))
    } else {
        let (rest, name) = parse_sub_name(rest)?;
        (rest, name, None)
    };
    let (rest, _) = ws(rest)?;

    let (rest, (params, param_defs, param_return_type)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, (pd, rt)) = parse_param_list_with_return(r)?;
        validate_signature_params(&pd)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd, rt))
    } else {
        (rest, (Vec::new(), Vec::new(), None))
    };

    let (rest, traits) = parse_sub_traits(rest)?;
    let return_type = traits.return_type.or(param_return_type);
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::MethodDecl {
            name: Symbol::intern(&name),
            name_expr,
            params,
            param_defs,
            body,
            multi,
            is_rw: traits.is_rw,
            is_private,
            is_our,
            return_type,
        },
    ))
}
