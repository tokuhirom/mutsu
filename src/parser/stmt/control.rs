use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char};

use crate::ast::{AssignOp, Expr, ParamDef, Stmt, collect_placeholders};
use crate::token_kind::TokenKind;

use super::decl::parse_array_shape_suffix;
use super::sub;
use super::{block, ident, keyword, parse_comma_or_expr, statement, var_name};

fn condition_has_assignment_tail(rest: &str) -> bool {
    if rest.starts_with(":=") || rest.starts_with("::=") || rest.starts_with("⚛=") {
        return true;
    }
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        return true;
    }
    super::assign::parse_compound_assign_op(rest).is_some()
        || super::assign::parse_custom_compound_assign_op(rest).is_some()
}

fn condition_expr(input: &str) -> PResult<'_, Expr> {
    match expression(input) {
        Ok((rest, cond)) => {
            let (tail, _) = ws(rest)?;
            if condition_has_assignment_tail(tail)
                && let Ok((assign_rest, assign_cond)) = super::assign::try_parse_assign_expr(input)
            {
                return Ok((assign_rest, assign_cond));
            }
            Ok((rest, cond))
        }
        Err(_) => super::assign::try_parse_assign_expr(input),
    }
}

pub(super) fn if_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("if", input).ok_or_else(|| PError::expected("if statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    // Optional binding: `if EXPR -> $var { }`
    let (rest, binding_var) = parse_binding_var(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, then_branch) = block(rest)?;
    let (rest, _) = ws(rest)?;

    // elsif chain
    let (rest, else_branch) = parse_elsif_chain(rest)?;

    Ok((
        rest,
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var,
        },
    ))
}

/// Parse optional `-> $var` binding after an if/elsif/with condition.
fn parse_binding_var(input: &str) -> PResult<'_, Option<String>> {
    if let Some(rest) = input.strip_prefix("->") {
        let (rest, _) = ws(rest)?;
        let (rest, name) = var_name(rest)?;
        let (rest, _) = ws(rest)?;
        Ok((rest, Some(name.to_string())))
    } else {
        Ok((input, None))
    }
}

pub(super) fn parse_elsif_chain(input: &str) -> PResult<'_, Vec<Stmt>> {
    if let Some(r) = keyword("elsif", input) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, then_branch) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, else_branch) = parse_elsif_chain(r)?;
        return Ok((
            r,
            vec![Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var: None,
            }],
        ));
    }
    if let Some(r) = keyword("else", input) {
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((r, body));
    }
    Ok((input, Vec::new()))
}

/// Parse `unless` statement.
pub(super) fn unless_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unless", input).ok_or_else(|| PError::expected("unless statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    // unless cannot have else/elsif/orwith — check but consume the trailing clause
    let (check, _) = ws(rest)?;
    for kw in &["else", "elsif", "orwith"] {
        if let Some(r) = keyword(kw, check) {
            // Consume the rest of the invalid clause to produce a hard error
            let (r, _) = ws(r)?;
            // Skip condition (if any) and block
            let r = if kw != &"else" {
                if let Ok((r, _)) = expression(r) { r } else { r }
            } else {
                r
            };
            let (r, _) = ws(r)?;
            let r = if let Ok((r, _)) = block(r) { r } else { r };
            return Ok((
                r,
                Stmt::Die(Expr::Literal(crate::value::Value::Str(format!(
                    "X::Syntax::UnlessElse: unless does not allow '{kw}'"
                )))),
            ));
        }
    }
    Ok((
        rest,
        Stmt::If {
            cond: Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(cond),
            },
            then_branch: body,
            else_branch: Vec::new(),
            binding_var: None,
        },
    ))
}

/// Parse labeled statement: `LABEL: <statement>`.
/// Loop-like statements keep the label on the loop node for last/next/redo.
/// Other statements are wrapped in `Stmt::Label`.
pub(super) fn labeled_loop_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, label) = ident(input)?;
    let (rest, _) = ws(rest)?;
    if !rest.starts_with(':') || rest.starts_with("::") {
        return Err(PError::expected("labeled loop"));
    }
    let rest = &rest[1..]; // consume ':'
    let has_space_after_colon = rest.chars().next().is_some_and(char::is_whitespace);
    if !has_space_after_colon
        && !rest.starts_with('{')
        && !rest.starts_with("for")
        && !rest.starts_with("while")
        && !rest.starts_with("until")
        && !rest.starts_with("loop")
        && !rest.starts_with("do")
    {
        return Err(PError::expected("labeled loop"));
    }
    let (rest, _) = ws(rest)?;

    // Check which loop keyword follows
    if let Some(r) = keyword("for", rest) {
        let (r, _) = ws1(r)?;
        let (r, iterable) = parse_comma_or_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, param_def, params)) = parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::For {
                iterable,
                param,
                param_def: Box::new(param_def),
                params,
                body,
                label: Some(label),
                mode: crate::ast::ForMode::Normal,
            },
        ));
    }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::While {
                cond,
                body,
                label: Some(label),
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body,
                label: Some(label),
            },
        ));
    }
    if let Some(r) = keyword("loop", rest) {
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: None,
                step: None,
                body,
                repeat: false,
                label: Some(label),
            },
        ));
    }

    // Label before `do` block: `A: do { ... }`
    if keyword("do", rest).is_some() {
        // Parse the rest as a statement and wrap with label
        // TODO: Represent labeled `do { ... }` with a dedicated AST node instead of
        // lowering it to a dummy `Stmt::For` carrying `Nil`.
        let r = keyword("do", rest).unwrap();
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::For {
                iterable: Expr::ArrayLiteral(vec![Expr::Literal(crate::value::Value::Nil)]),
                param: None,
                param_def: Box::new(None),
                params: Vec::new(),
                body,
                label: Some(label),
                mode: crate::ast::ForMode::Normal,
            },
        ));
    }
    // Label before bare block: `A: { ... }`
    // TODO: Represent labeled bare blocks directly instead of lowering to
    // a dummy `Stmt::For` carrying `Nil`.
    if rest.starts_with('{') {
        let (r, body) = block(rest)?;
        return Ok((
            r,
            Stmt::For {
                iterable: Expr::ArrayLiteral(vec![Expr::Literal(crate::value::Value::Nil)]),
                param: None,
                param_def: Box::new(None),
                params: Vec::new(),
                body,
                label: Some(label),
                mode: crate::ast::ForMode::Normal,
            },
        ));
    }

    // Generic labeled statement: LABEL: <statement>
    let (r, stmt) = statement(rest)?;
    Ok((
        r,
        Stmt::Label {
            name: label,
            stmt: Box::new(stmt),
        },
    ))
}

/// Parse for loop parameters: -> $param or -> $a, $b
pub(super) fn parse_for_params(
    input: &str,
) -> PResult<'_, (Option<String>, Option<ParamDef>, Vec<String>)> {
    fn skip_pointy_return_type<'a>(mut r: &'a str) -> PResult<'a, ()> {
        let (r2, _) = ws(r)?;
        r = r2;
        if let Some(after_arrow) = r.strip_prefix("-->") {
            let (after_arrow, _) = super::parse_return_type_annotation_pub(after_arrow)?;
            let (after_arrow, _) = ws(after_arrow)?;
            Ok((after_arrow, ()))
        } else {
            Ok((r, ()))
        }
    }

    if let Some(stripped) = input.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
        // Zero-parameter pointy block: for @a -> { ... }
        if r.starts_with('{') {
            return Ok((r, (None, None, Vec::new())));
        }
        // Parenthesized destructuring pointy param:
        //   -> ($a, $b) { ... }
        //   -> (:key($k), :value($v)) { ... }
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::parse_param_list_pub(r)?;
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
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            };
            return Ok((r, (Some(unpack_name), Some(unpack_def), Vec::new())));
        }
        // Parenthesized pointy parameter list: -> ($a, $b) { ... }
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::parse_param_list_pub(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, _) = skip_pointy_return_type(r)?;
            if sub_params.is_empty() {
                return Ok((r, (None, None, Vec::new())));
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
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            };
            return Ok((r, (Some(unpack_name), Some(unpack_def), Vec::new())));
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
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
            };
            return Ok((r, (Some(unpack_name), Some(unpack_def), Vec::new())));
        }
        let (r, mut first_def) = parse_for_pointy_param(r)?;
        let first = first_def.name.clone();
        let (r, _) = ws(r)?;
        let (r, _) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = super::parse_param_list_pub(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            first_def.sub_signature = Some(sub_params);
            (r, ())
        } else {
            (r, ())
        };
        let (r, _) = ws(r)?;
        if r.starts_with(',') {
            let mut params = vec![first];
            let mut r = r;
            loop {
                let (r2, _) = parse_char(r, ',')?;
                let (r2, _) = ws(r2)?;
                let (r2, next) = parse_for_pointy_param(r2)?;
                params.push(next.name);
                let (r2, _) = ws(r2)?;
                if !r2.starts_with(',') {
                    r = r2;
                    break;
                }
                r = r2;
            }
            let (r, _) = skip_pointy_return_type(r)?;
            Ok((r, (None, None, params)))
        } else {
            let (r, _) = skip_pointy_return_type(r)?;
            Ok((r, (Some(first), Some(first_def), Vec::new())))
        }
    } else {
        Ok((input, (None, None, Vec::new())))
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

    let mut rest = if r.starts_with('?') || r.starts_with('!') {
        &r[1..]
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
    if let Some(after_eq) = r.strip_prefix('=')
        && !after_eq.starts_with('>')
    {
        let (after_eq, _) = ws(after_eq)?;
        let (after_default, _default_expr) = expression(after_eq)?;
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
            default: None,
            multi_invocant: true,
            required: false,
            named: false,
            slurpy: false,
            double_slurpy: false,
            sigilless: false,
            type_constraint,
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            traits,
            optional_marker: false,
            outer_sub_signature: None,
            code_signature: None,
            is_invocant: false,
            shape_constraints,
        },
    ))
}

pub(super) fn for_stmt(input: &str) -> PResult<'_, Stmt> {
    for_stmt_with_mode(input, crate::ast::ForMode::Normal)
}

/// Parse `race for ...` statement prefix.
pub(super) fn race_for_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("race", input).ok_or_else(|| PError::expected("race for statement"))?;
    let (rest, _) = ws1(rest)?;
    for_stmt_with_mode(rest, crate::ast::ForMode::Race)
}

/// Parse `hyper for ...` statement prefix.
pub(super) fn hyper_for_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("hyper", input).ok_or_else(|| PError::expected("hyper for statement"))?;
    let (rest, _) = ws1(rest)?;
    for_stmt_with_mode(rest, crate::ast::ForMode::Hyper)
}

fn for_stmt_with_mode(input: &str, mode: crate::ast::ForMode) -> PResult<'_, Stmt> {
    let rest = keyword("for", input).ok_or_else(|| PError::expected("for statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, iterable) = parse_comma_or_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, (param, param_def, params)) = parse_for_params(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    // When no explicit params, collect placeholder variables from the body
    let (param, params) = if param.is_none() && params.is_empty() {
        let placeholders = collect_placeholders(&body);
        if placeholders.is_empty() {
            (param, params)
        } else {
            // Use the first placeholder as the loop param, rest as extra params
            let first = placeholders[0].clone();
            let rest_params = placeholders[1..].to_vec();
            (Some(first), rest_params)
        }
    } else {
        (param, params)
    };
    Ok((
        rest,
        Stmt::For {
            iterable,
            param,
            param_def: Box::new(param_def),
            params,
            body,
            label: None,
            mode,
        },
    ))
}

pub(super) fn parse_pointy_param(input: &str) -> PResult<'_, ParamDef> {
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
    let mut rest = input;
    let mut type_constraint = None;
    rest = if let Ok((r, tc)) = ident(rest) {
        let r = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            &r[2..]
        } else {
            r
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

    // Slurpy marker for pointy params: *@a, *%h, *$x, *&cb, and double-slurpy variants.
    let mut slurpy = false;
    let mut double_slurpy = false;
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
    // Keep parse permissive and skip details for now.
    let (r, _) = ws(rest)?;
    if r.starts_with('(') {
        rest = skip_balanced_parens(r);
    } else {
        rest = r;
    }

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
            named: false,
            slurpy,
            double_slurpy,
            sigilless: false,
            type_constraint,
            literal_value: None,
            sub_signature: None,
            outer_sub_signature: None,
            code_signature: None,
            where_constraint: None,
            traits,
            optional_marker,
            is_invocant: false,
            shape_constraints,
        },
    ))
}

/// Parse `while` loop.
pub(super) fn while_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("while", input).ok_or_else(|| PError::expected("while statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param_binding) = if rest.starts_with("->") {
        let (rest, (param, _param_def, params)) = parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at("single while pointy parameter", rest));
        }
        (rest, param)
    } else {
        (rest, None::<String>)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    let while_stmt = Stmt::While {
        cond: if let Some(ref param) = param_binding {
            Expr::AssignExpr {
                name: param.clone(),
                expr: Box::new(cond),
            }
        } else {
            cond
        },
        body,
        label: None,
    };
    if let Some(param) = param_binding {
        Ok((
            rest,
            Stmt::Block(vec![
                Stmt::VarDecl {
                    name: param,
                    expr: Expr::Literal(crate::value::Value::Nil),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                },
                while_stmt,
            ]),
        ))
    } else {
        Ok((rest, while_stmt))
    }
}

/// Parse `until` loop.
pub(super) fn until_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("until", input).ok_or_else(|| PError::expected("until statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param_binding) = if rest.starts_with("->") {
        let (rest, (param, _param_def, params)) = parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at("single until pointy parameter", rest));
        }
        (rest, param)
    } else {
        (rest, None::<String>)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    let cond_expr = if let Some(ref param) = param_binding {
        Expr::AssignExpr {
            name: param.clone(),
            expr: Box::new(cond),
        }
    } else {
        cond
    };
    let while_stmt = Stmt::While {
        cond: Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond_expr),
        },
        body,
        label: None,
    };
    if let Some(param) = param_binding {
        Ok((
            rest,
            Stmt::Block(vec![
                Stmt::VarDecl {
                    name: param,
                    expr: Expr::Literal(crate::value::Value::Nil),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                },
                while_stmt,
            ]),
        ))
    } else {
        Ok((rest, while_stmt))
    }
}

/// Parse C-style `loop` or infinite loop.
pub(super) fn loop_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("loop", input).ok_or_else(|| PError::expected("loop statement"))?;
    let (rest, _) = ws(rest)?;
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        let (rest, _) = ws(rest)?;
        // init: may be comma-separated list of statements
        let (rest, init) = if let Some(rest_after_semi) = rest.strip_prefix(';') {
            (rest_after_semi, None)
        } else {
            let (mut r, first) = statement(rest)?;
            let mut stmts = vec![first];
            loop {
                let (r2, _) = ws(r)?;
                if let Some(r2_after_comma) = r2.strip_prefix(',') {
                    let (r2, _) = ws(r2_after_comma)?;
                    let (r2, s) = statement(r2)?;
                    stmts.push(s);
                    r = r2;
                } else {
                    r = r2;
                    break;
                }
            }
            if stmts.len() == 1 {
                (r, Some(Box::new(stmts.into_iter().next().unwrap())))
            } else {
                (r, Some(Box::new(Stmt::Block(stmts))))
            }
        };
        let (rest, _) = ws(rest)?;
        // cond
        let (rest, cond) = if let Some(rest) = rest.strip_prefix(';') {
            (rest, None)
        } else {
            let (r, e) = expression(rest)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ';')?;
            (r, Some(e))
        };
        let (rest, _) = ws(rest)?;
        // step: may be comma-separated list of expressions
        let (rest, step) = if rest.starts_with(')') {
            (rest, None)
        } else {
            let (mut r, first) = expression(rest)?;
            let mut exprs = vec![first];
            loop {
                let (r2, _) = ws(r)?;
                if let Some(r2_after_comma) = r2.strip_prefix(',') {
                    let (r2, _) = ws(r2_after_comma)?;
                    let (r2, e) = expression(r2)?;
                    exprs.push(e);
                    r = r2;
                } else {
                    r = r2;
                    break;
                }
            }
            if exprs.len() == 1 {
                (r, Some(exprs.into_iter().next().unwrap()))
            } else {
                // Wrap multiple step expressions in a DoBlock
                let stmts: Vec<Stmt> = exprs.into_iter().map(Stmt::Expr).collect();
                (
                    r,
                    Some(Expr::DoBlock {
                        body: stmts,
                        label: None,
                    }),
                )
            }
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        let (rest, _) = ws(rest)?;
        let (rest, body) = block(rest)?;
        return Ok((
            rest,
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat: false,
                label: None,
            },
        ));
    }
    // Infinite loop
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Loop {
            init: None,
            cond: None,
            step: None,
            body,
            repeat: false,
            label: None,
        },
    ))
}

/// Parse `repeat while/until` loop.
pub(super) fn repeat_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("repeat", input).ok_or_else(|| PError::expected("repeat statement"))?;
    let (rest, _) = ws(rest)?;

    // repeat while/until COND { BODY }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, _param_def, params)) = parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        let repeat_param = param.or_else(|| params.into_iter().next());
        let init = repeat_param.as_ref().map(|name| {
            Box::new(Stmt::VarDecl {
                name: name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            })
        });
        let step = repeat_param.map(|name| Expr::AssignExpr {
            name,
            expr: Box::new(Expr::Literal(crate::value::Value::Bool(true))),
        });
        return Ok((
            r,
            Stmt::Loop {
                init,
                cond: Some(cond),
                step,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, _param_def, params)) = parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        let repeat_param = param.or_else(|| params.into_iter().next());
        let init = repeat_param.as_ref().map(|name| {
            Box::new(Stmt::VarDecl {
                name: name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            })
        });
        let step = repeat_param.map(|name| Expr::AssignExpr {
            name,
            expr: Box::new(Expr::Literal(crate::value::Value::Bool(true))),
        });
        return Ok((
            r,
            Stmt::Loop {
                init,
                cond: Some(Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                }),
                step,
                body,
                repeat: true,
                label: None,
            },
        ));
    }

    // repeat { BODY } while/until COND
    let (rest, body) = block(rest)?;
    let (rest, _) = ws(rest)?;
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: Some(cond),
                step: None,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: Some(Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                }),
                step: None,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    Err(PError::fatal(
        "X::Syntax::Missing: \"while\" or \"until\" required after repeat".to_string(),
    ))
}

/// Parse `given`/`when`/`default`.
pub(super) fn given_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("given", input).ok_or_else(|| PError::expected("given statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, topic) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Given { topic, body }))
}

pub(super) fn when_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("when", input).ok_or_else(|| PError::expected("when statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::When { cond, body }))
}

pub(super) fn default_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("default", input).ok_or_else(|| PError::expected("default statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Default(body)))
}

/// Build a `$_ = <expr>` assignment statement for topicalization.
fn topicalize(expr: &Expr) -> Stmt {
    Stmt::Assign {
        name: "_".to_string(),
        op: AssignOp::Assign,
        expr: expr.clone(),
    }
}

/// Parse `with`/`without` statement.
pub(super) fn with_stmt(input: &str) -> PResult<'_, Stmt> {
    let is_without = keyword("without", input).is_some();
    let rest = keyword("with", input)
        .or_else(|| keyword("without", input))
        .ok_or_else(|| PError::expected("with/without statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond_expr) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;

    // Check for optional pointy block: -> $param { ... }
    let (rest, param_name) = if let Some(r) = rest.strip_prefix("->") {
        let (r, _) = ws(r)?;
        // Parse parameter like $proc
        if let Some(r_after_sigil) = r.strip_prefix('$') {
            let end = r_after_sigil
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(r_after_sigil.len());
            let name = &r_after_sigil[..end];
            let r = &r_after_sigil[end..];
            let (r, _) = ws(r)?;
            (r, Some(name.to_string()))
        } else {
            (rest, None)
        }
    } else {
        (rest, None)
    };

    let (rest, body) = block(rest)?;

    // Prepend $_ = <cond_expr> to the body for topicalization
    let mut with_body = vec![topicalize(&cond_expr)];
    // If a named parameter was given (-> $param), also assign it
    if let Some(ref pname) = param_name {
        with_body.push(Stmt::VarDecl {
            name: pname.clone(),
            expr: cond_expr.clone(),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
        });
    }
    with_body.extend(body);

    let cond = Expr::MethodCall {
        target: Box::new(cond_expr),
        name: "defined".to_string(),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    };
    let cond = if is_without {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond),
        }
    } else {
        cond
    };

    // Parse orwith / else chains
    let (rest, _) = ws(rest)?;
    let (rest, else_branch) = if keyword("orwith", rest).is_some() {
        let r = keyword("orwith", rest).unwrap();
        let (r, _) = ws1(r)?;
        let (r, orwith_cond_expr) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, orwith_body) = block(r)?;

        // Prepend $_ = <orwith_cond_expr> to the body
        let mut orwith_with_body = vec![topicalize(&orwith_cond_expr)];
        orwith_with_body.extend(orwith_body);

        let orwith_cond = Expr::MethodCall {
            target: Box::new(orwith_cond_expr),
            name: "defined".to_string(),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        };
        let (r, _) = ws(r)?;
        let (r, orwith_else) = if keyword("else", r).is_some() {
            let r2 = keyword("else", r).unwrap();
            let (r2, _) = ws(r2)?;
            let (r2, else_body) = block(r2)?;
            (r2, else_body)
        } else {
            (r, Vec::new())
        };
        (
            r,
            vec![Stmt::If {
                cond: orwith_cond,
                then_branch: orwith_with_body,
                else_branch: orwith_else,
                binding_var: None,
            }],
        )
    } else if keyword("else", rest).is_some() {
        let r = keyword("else", rest).unwrap();
        let (r, _) = ws(r)?;
        let (r, else_body) = block(r)?;
        (r, else_body)
    } else {
        (rest, Vec::new())
    };

    Ok((
        rest,
        Stmt::If {
            cond,
            then_branch: with_body,
            else_branch,
            binding_var: None,
        },
    ))
}

/// Parse `react` block.
pub(super) fn react_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("react", input).ok_or_else(|| PError::expected("react block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::React { body }))
}

/// Parse `whenever` block.
pub(super) fn whenever_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("whenever", input).ok_or_else(|| PError::expected("whenever block"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, supply) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param) = if let Some(stripped) = rest.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
        let (r, name) = var_name(r)?;
        (r, Some(name))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Whenever {
            supply,
            param,
            body,
        },
    ))
}
