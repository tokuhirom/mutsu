use super::*;
use crate::ast::{Expr, make_anon_sub};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::primary::string::{double_quoted_string, single_quoted_string};

fn skip_pointy_return_type(mut r: &str) -> PResult<'_, Option<String>> {
    let (r2, _) = ws(r)?;
    r = r2;
    if let Some(after_arrow) = r.strip_prefix("-->") {
        let (after_arrow, type_name) =
            crate::parser::stmt::parse_return_type_annotation_pub(after_arrow)?;
        Ok((after_arrow, Some(type_name)))
    } else {
        Ok((r, None))
    }
}

/// Parse capture literals: `\(...)` and `\expr`.
pub(crate) fn capture_literal(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('\\') {
        return Err(PError::expected("capture literal"));
    }
    let r = &input[1..]; // skip backslash
    if !r.starts_with('(') {
        // `\` as a capture prefix binds tightly — only capture the next
        // primary term (variable, literal, etc.), not a full expression.
        // This ensures `\3 eqv \4` parses as `(\3) eqv (\4)`.
        let (r, item) = crate::parser::primary::primary(r)?;
        return Ok((r, Expr::CaptureLiteral(vec![item])));
    }
    // Parenthesized capture literal: \(a, b, :c)
    let (r, _) = parse_char(r, '(')?;
    let (r, _) = ws(r)?;
    let (r, items) = crate::parser::primary::parse_call_arg_list(r)?;
    let (r, _) = ws(r)?;
    let (r, _) = parse_char(r, ')')?;
    Ok((r, Expr::CaptureLiteral(items)))
}

/// Parse `-> $param { body }` or `-> $a, $b { body }` arrow lambda.
/// Also handles `<-> $a, $b { body }` (rw pointy block) where all params get `is rw`.
pub(crate) fn arrow_lambda(input: &str) -> PResult<'_, Expr> {
    let is_rw_block = input.starts_with("<->");
    if !is_rw_block && !input.starts_with("->") {
        return Err(PError::expected("arrow lambda"));
    }
    // Capture the definition line of the pointy block (at the `->` position)
    let arrow_line = crate::parser::primary::current_line_number(input);
    let arrow_is_original = crate::parser::primary::is_within_original_source(input);
    let r = if is_rw_block {
        &input[3..]
    } else {
        &input[2..]
    };
    let (r, _) = ws(r)?;
    // Zero-param pointed block: -> { body }
    if r.starts_with('{') {
        let (r, mut body) = parse_block_body(r)?;
        if arrow_is_original {
            body.insert(0, crate::ast::Stmt::SetLine(arrow_line));
        }
        return Ok((
            r,
            Expr::AnonSubParams {
                params: Vec::new(),
                param_defs: Vec::new(),
                return_type: None,
                body,
                is_rw: false,
                is_whatever_code: false,
            },
        ));
    }
    // Zero-param with explicit return spec: -> --> 42 { body }
    if r.starts_with("-->") {
        let (r, return_type) = skip_pointy_return_type(r)?;
        let (r, mut body) = parse_block_body(r)?;
        if arrow_is_original {
            body.insert(0, crate::ast::Stmt::SetLine(arrow_line));
        }
        return Ok((
            r,
            Expr::AnonSubParams {
                params: Vec::new(),
                param_defs: Vec::new(),
                return_type,
                body,
                is_rw: false,
                is_whatever_code: false,
            },
        ));
    }
    // Sub-signature destructuring: -> ($a, $b) { body }.
    // In Raku, `-> ($a, $b)` is ONE parameter that binds a single list/Capture
    // argument and unpacks it — NOT two separate parameters (`-> $a, $b`). So a
    // multi-element parenthesized signature must become a single destructuring
    // `__subsig__` param (matching `sub f(($a,$b))`), so `.map(-> ($k,$v){...})`
    // destructures each list element instead of chunking the list 2-at-a-time.
    // A single-element paren (`-> ($a)`) keeps the existing behavior of a lone
    // param (mirrors the capture sub-sig `len == 1` special case in sub_param.rs).
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, mut sub_params) = crate::parser::stmt::parse_param_list_pub(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, return_type) = skip_pointy_return_type(r)?;
        let (r, mut body) = parse_block_body_with_sigilless(&sub_params, r)?;
        if arrow_is_original {
            body.insert(0, crate::ast::Stmt::SetLine(arrow_line));
        }
        if is_rw_block {
            inject_rw_trait(&mut sub_params);
        }
        let param_defs = if sub_params.len() > 1 {
            let mut p = crate::parser::stmt::sub_param::make_param("__subsig__".to_string());
            p.sub_signature = Some(sub_params);
            vec![p]
        } else {
            sub_params
        };
        let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
        return Ok((
            r,
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw: false,
                is_whatever_code: false,
            },
        ));
    }
    // Parse params
    let (r, first) = crate::parser::stmt::parse_pointy_param_pub(r)?;
    let (r, _) = ws(r)?;
    if r.starts_with(',') {
        // Multi-param: -> $a, $b { body }
        let mut param_defs = vec![first];
        let mut r = r;
        loop {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            let (r2, next) = crate::parser::stmt::parse_pointy_param_pub(r2)?;
            param_defs.push(next);
            let (r2, _) = ws(r2)?;
            if !r2.starts_with(',') {
                r = r2;
                break;
            }
            r = r2;
        }
        let (r, return_type) = skip_pointy_return_type(r)?;
        let (r, mut body) = parse_block_body_with_sigilless(&param_defs, r)?;
        if arrow_is_original {
            body.insert(0, crate::ast::Stmt::SetLine(arrow_line));
        }
        if is_rw_block {
            inject_rw_trait(&mut param_defs);
        }
        let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
        Ok((
            r,
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw: false,
                is_whatever_code: false,
            },
        ))
    } else {
        // Single param: -> $n { body }
        let mut first = first;
        if is_rw_block {
            inject_rw_trait(std::slice::from_mut(&mut first));
        }
        let (r, return_type) = skip_pointy_return_type(r)?;
        let (r, mut body) = parse_block_body_with_sigilless(std::slice::from_ref(&first), r)?;
        if arrow_is_original {
            body.insert(0, crate::ast::Stmt::SetLine(arrow_line));
        }
        let simple_single = first.traits.is_empty()
            && first.shape_constraints.is_none()
            && !first.named
            && !first.slurpy
            && !first.double_slurpy
            && first.default.is_none()
            && !first.optional_marker
            && first.type_constraint.is_none()
            && first.where_constraint.is_none()
            && first.sub_signature.is_none()
            && first.outer_sub_signature.is_none()
            && first.code_signature.is_none();
        if simple_single {
            // Strip sigil prefix for Lambda (it handles sigils internally)
            let lambda_name = first
                .name
                .strip_prefix('@')
                .or_else(|| first.name.strip_prefix('%'))
                .or_else(|| first.name.strip_prefix('&'))
                .unwrap_or(&first.name)
                .to_string();
            Ok((
                r,
                Expr::Lambda {
                    param: lambda_name,
                    body,
                    is_whatever_code: false,
                },
            ))
        } else {
            Ok((
                r,
                Expr::AnonSubParams {
                    params: vec![first.name.clone()],
                    param_defs: vec![first],
                    return_type,
                    body,
                    is_rw: false,
                    is_whatever_code: false,
                },
            ))
        }
    }
}

/// For `<->` blocks, add `is rw` trait to params that don't already have
/// an explicit `is readonly` trait.
fn inject_rw_trait(params: &mut [crate::ast::ParamDef]) {
    for p in params.iter_mut() {
        if !p.traits.iter().any(|t| t == "readonly") && !p.traits.iter().any(|t| t == "rw") {
            p.traits.push("rw".to_string());
        }
    }
}

/// Parse a block `{ stmts }` as AnonSub or `{}` / `{ key => val, ... }` as Hash.
pub(crate) fn block_or_hash_expr(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('{') {
        return Err(PError::expected("block or hash"));
    }
    let r = &input[1..];
    let (r, _) = ws_inner(r);

    // Empty hash: {}
    if let Some(rest) = r.strip_prefix('}') {
        return Ok((rest, Expr::Hash(Vec::new())));
    }

    // Try to detect if this is a hash literal: { key => val, ... }
    // Heuristic: if after ws we see `ident =>` or `"str" =>` or `'str' =>`, it's a hash.
    // However, placeholder variables ($^x, @^x, %^x) force it to be a block.
    if is_hash_literal_start(r) && !body_has_placeholder_vars(r) {
        return super::hash::parse_hash_literal_body(r);
    }

    // Otherwise parse as a block (anonymous sub)
    crate::parser::stmt::simple::push_scope();
    let result = (|| -> PResult<'_, Expr> {
        let (r, stmts) = crate::parser::stmt::stmt_list_pub(r)?;
        let (r, _) = ws_inner(r);
        if !r.starts_with('}') {
            return Err(PError::expected("'}'"));
        }
        let r = &r[1..];
        Ok((r, make_anon_sub(stmts)))
    })();
    crate::parser::stmt::simple::pop_scope();
    result
}

pub(crate) fn parse_block_body(input: &str) -> PResult<'_, Vec<crate::ast::Stmt>> {
    let (r, _) = parse_char(input, '{')?;
    // Reject {YOU_ARE_HERE} outside of a setting (X::Syntax::Reserved)
    {
        let trimmed = r.trim_start();
        if let Some(after) = trimmed.strip_prefix("YOU_ARE_HERE") {
            let after_trimmed = after.trim_start();
            if after_trimmed.starts_with('}') {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    crate::value::Value::str(
                        "The use of {YOU_ARE_HERE} outside of a setting is reserved".to_string(),
                    ),
                );
                let exc = crate::value::Value::make_instance(
                    crate::symbol::Symbol::intern("X::Syntax::Reserved"),
                    attrs,
                );
                return Err(PError::fatal_with_exception(
                    "The use of {YOU_ARE_HERE} outside of a setting is reserved".to_string(),
                    Box::new(exc),
                ));
            }
        }
    }
    crate::parser::stmt::simple::push_scope();
    let result = (|| -> PResult<'_, Vec<crate::ast::Stmt>> {
        let (r, stmts) = crate::parser::stmt::stmt_list_pub(r)?;
        let (r, _) = ws_inner(r);
        let (r, _) = parse_char(r, '}')?;
        Ok((r, stmts))
    })();
    crate::parser::stmt::simple::pop_scope();
    result
}

/// Like `parse_block_body`, but registers any sigilless parameters as term symbols
/// before parsing the body.  This prevents bare names like `s` or `q` from being
/// misinterpreted as substitution / quoting operators inside arrow-lambda bodies.
fn parse_block_body_with_sigilless<'a>(
    param_defs: &[crate::ast::ParamDef],
    input: &'a str,
) -> PResult<'a, Vec<crate::ast::Stmt>> {
    let has_sigilless = param_defs.iter().any(|p| p.sigilless);
    if !has_sigilless {
        return parse_block_body(input);
    }
    let (r, _) = parse_char(input, '{')?;
    crate::parser::stmt::simple::push_scope();
    for pd in param_defs {
        if pd.sigilless {
            crate::parser::stmt::simple::register_user_term_symbol(&pd.name);
        }
    }
    let result = (|| -> PResult<'_, Vec<crate::ast::Stmt>> {
        let (r, stmts) = crate::parser::stmt::stmt_list_pub(r)?;
        let (r, _) = ws_inner(r);
        let (r, _) = parse_char(r, '}')?;
        Ok((r, stmts))
    })();
    crate::parser::stmt::simple::pop_scope();
    result
}

/// Scan the body (between { and matching }) for placeholder variables ($^x, @^x, %^x).
/// If any are found, the block should be treated as a Block, not a Hash.
fn body_has_placeholder_vars(input: &str) -> bool {
    let mut depth = 1u32;
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            '$' | '@' | '%' => {
                if let Some(&'^') = chars.peek() {
                    chars.next(); // consume '^'
                    if let Some(&next) = chars.peek()
                        && (next.is_alphabetic() || next == '_')
                    {
                        return true;
                    }
                }
            }
            // Skip string contents to avoid false positives
            '\'' => {
                for sc in chars.by_ref() {
                    if sc == '\'' {
                        break;
                    }
                }
            }
            _ => {}
        }
    }
    false
}

/// Check if the input looks like a hash literal start.
fn is_hash_literal_start(input: &str) -> bool {
    // %hash variable at start indicates hash literal: {%hash, ...}
    // Also handles twigil forms like %*dyn, %!attr, %?config, etc.
    // But {%hash{$_}} or {%hash.foo} is a block containing a hash expression.
    if let Some(stripped) = input.strip_prefix('%') {
        // Skip optional twigil (* ! ? ^)
        let after_twigil = if stripped.starts_with('*')
            || stripped.starts_with('!')
            || stripped.starts_with('?')
            || stripped.starts_with('^')
        {
            &stripped[1..]
        } else {
            stripped
        };
        if let Ok((r, _)) = crate::parser::stmt::ident_pub(after_twigil) {
            let (r, _) = ws_inner(r);
            // If followed by comma, closing brace, or fat arrow, it's a hash literal
            if r.starts_with(',') || r.starts_with('}') || r.starts_with("=>") {
                return true;
            }
        }
    }
    // ident => or "str" => or 'str' =>
    if let Ok((r, _)) = crate::parser::stmt::ident_pub(input) {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    // numeric key => val or numeric R=> val (reverse fat arrow, produces a Pair)
    if let Ok((r, _)) = crate::parser::primary::number::integer(input) {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") || r.starts_with("R=>") {
            return true;
        }
    }
    // Quoted key => val
    if (input.starts_with('"') || input.starts_with('\''))
        && let Ok((r, _)) = single_quoted_string(input).or_else(|_| double_quoted_string(input))
    {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    // Colon pair: :name(expr) or :name or :!name or :Nname — indicates a hash literal
    if input.starts_with(':') && !input.starts_with("::") {
        let r = &input[1..];
        let digit_end = r
            .char_indices()
            .take_while(|(_, c)| {
                crate::builtins::unicode::unicode_decimal_digit_value(*c).is_some()
            })
            .last()
            .map(|(idx, c)| idx + c.len_utf8())
            .unwrap_or(0);
        if digit_end > 0 && r[digit_end..].starts_with('<') {
            return false;
        }
        // :!name
        if r.starts_with('!') && crate::parser::stmt::ident_pub(&r[1..]).is_ok() {
            return true;
        }
        // :Nname — numeric colon pair (e.g., :1status)
        if let Some(first) = r.chars().next()
            && first.is_ascii_digit()
        {
            let digit_end = r.find(|c: char| !c.is_ascii_digit()).unwrap_or(r.len());
            if digit_end < r.len() && crate::parser::stmt::ident_pub(&r[digit_end..]).is_ok() {
                return true;
            }
        }
        // :$var / :@var / :%var
        if (r.starts_with('$') || r.starts_with('@') || r.starts_with('%'))
            && crate::parser::stmt::ident_pub(&r[1..]).is_ok()
        {
            return true;
        }
        // :name or :name(expr) or :name[expr]
        if let Ok((_r, name)) = crate::parser::stmt::ident_pub(r)
            && !matches!(
                name.as_str(),
                "my" | "our"
                    | "has"
                    | "if"
                    | "unless"
                    | "for"
                    | "while"
                    | "until"
                    | "loop"
                    | "given"
                    | "when"
                    | "return"
            )
            && !name.starts_with(|c: char| c.is_ascii_digit())
        {
            return true;
        }
    }
    false
}
