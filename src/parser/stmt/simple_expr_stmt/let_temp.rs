use crate::ast::{Expr, Stmt};
use crate::parser::expr::expression;
use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::stmt::idents::{ident, keyword};
use crate::parser::stmt::modifier::parse_statement_modifier;
use crate::parser::stmt::pub_shims::ident_pub;
use crate::value::Value;

/// Parse `let` statement: `let $var = expr`, `let $var`, `let @arr[idx] = expr`.
pub(crate) fn let_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("let", input).ok_or_else(|| PError::expected("let statement"))?;
    let (rest, _) = ws1(rest)?;
    // Parse sigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after let"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after let"));
    }
    let rest_after_sigil = &rest[1..];
    let (rest, var_name) = ident(rest_after_sigil)?;
    // Env key: scalars strip $, arrays/hashes keep sigil
    let full_name = if sigil == '$' {
        var_name.clone()
    } else {
        format!("{}{}", sigil, var_name)
    };
    let (rest, _) = ws(rest)?;

    // Check for index: @arr[idx]
    if let Some(idx_rest) = rest.strip_prefix('[') {
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, idx_expr) = expression(idx_rest)?;
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, _) = parse_char(idx_rest, ']')?;
        let (idx_rest, _) = ws(idx_rest)?;
        if idx_rest.starts_with('=') && !idx_rest.starts_with("==") {
            let val_rest = &idx_rest[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: Some(Box::new(idx_expr)),
                    value: Some(Box::new(val_expr)),
                    is_temp: false,
                    undefine_first: false,
                },
            );
        }
        return parse_statement_modifier(
            idx_rest,
            Stmt::Let {
                name: full_name,
                index: Some(Box::new(idx_expr)),
                value: None,
                is_temp: false,
                undefine_first: false,
            },
        );
    }

    // Check for hash key: let %hash<key> = expr
    if let Some(key_rest) = rest.strip_prefix('<')
        && let Some(end_pos) = key_rest.find('>')
    {
        let key_str = &key_rest[..end_pos];
        let after_key = &key_rest[end_pos + 1..];
        let (after_key, _) = ws(after_key)?;
        let key_expr = Expr::Literal(Value::str(key_str.to_string()));
        if after_key.starts_with('=') && !after_key.starts_with("==") {
            let val_rest = &after_key[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: Some(Box::new(key_expr)),
                    value: Some(Box::new(val_expr)),
                    is_temp: false,
                    undefine_first: false,
                },
            );
        }
        return parse_statement_modifier(
            after_key,
            Stmt::Let {
                name: full_name,
                index: Some(Box::new(key_expr)),
                value: None,
                is_temp: false,
                undefine_first: false,
            },
        );
    }

    // Check for assignment: let $var = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
                is_temp: false,
                undefine_first: false,
            },
        );
    }

    // Bare let: let $var / let @arr / let %hash
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
            is_temp: false,
            undefine_first: false,
        },
    )
}

/// Parse a variable from `undefine(...)` or `undefine $var` inside a `temp` context.
fn parse_temp_undefine_var(input: &str) -> Result<(&str, String), PError> {
    if let Some(inner) = input.strip_prefix('(') {
        let (inner, _) = ws(inner)?;
        let s = inner.chars().next().unwrap_or(' ');
        if s == '$' || s == '@' || s == '%' {
            let after_s = &inner[1..];
            let (after_id, id) = ident(after_s)?;
            let (after_id, _) = ws(after_id)?;
            let after_id = after_id
                .strip_prefix(')')
                .ok_or_else(|| PError::expected("closing paren"))?;
            let full = if s == '$' { id } else { format!("{}{}", s, id) };
            Ok((after_id, full))
        } else {
            Err(PError::expected("variable after undefine("))
        }
    } else {
        let s = input.chars().next().unwrap_or(' ');
        if s == '$' || s == '@' || s == '%' {
            let after_s = &input[1..];
            let (after_id, id) = ident(after_s)?;
            let full = if s == '$' { id } else { format!("{}{}", s, id) };
            Ok((after_id, full))
        } else {
            Err(PError::expected("variable after undefine"))
        }
    }
}

/// Parse `temp` statement — same semantics as `let` (save/restore at scope exit).
pub(crate) fn temp_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("temp", input).ok_or_else(|| PError::expected("temp statement"))?;
    let (rest, _) = ws1(rest)?;
    // temp on lvalue method call: `temp $obj.method = value`
    if let Ok((expr_rest, expr)) = expression(rest) {
        let (expr_rest_ws, _) = ws(expr_rest)?;
        if expr_rest_ws.starts_with('=')
            && !expr_rest_ws.starts_with("==")
            && let Expr::MethodCall {
                target,
                name,
                args,
                modifier: _,
                quoted: _,
            } = expr
            && let Expr::Var(var_name) = target.as_ref()
        {
            let rhs_rest = &expr_rest_ws[1..];
            let (rhs_rest, _) = ws(rhs_rest)?;
            let (rhs_rest, rhs_expr) = expression(rhs_rest)?;
            return parse_statement_modifier(
                rhs_rest,
                Stmt::TempMethodAssign {
                    var_name: var_name.clone(),
                    method_name: name.resolve(),
                    method_args: args,
                    value: rhs_expr,
                },
            );
        }
    }
    // temp undefine($var) [= value]:
    // With assignment: temp saves $d, undefine, assign "baz"; restore at scope exit.
    // Without assignment: just undefine (temp on return value is a no-op).
    if let Some(after_undef) = rest.strip_prefix("undefine")
        && (after_undef.starts_with('(') || after_undef.starts_with(' '))
    {
        let (after_undef, _) = ws(after_undef)?;
        let (var_rest, var_name) = parse_temp_undefine_var(after_undef)?;
        let (var_rest, _) = ws(var_rest)?;
        let make_var_expr = |vn: &str| -> Expr {
            if let Some(stripped) = vn.strip_prefix('@') {
                Expr::ArrayVar(stripped.to_string())
            } else if let Some(stripped) = vn.strip_prefix('%') {
                Expr::HashVar(stripped.to_string())
            } else {
                Expr::Var(vn.to_string())
            }
        };
        // With assignment: temp undefine($d) = "baz"
        // Semantics: undefine $d first, then temp saves the undefined state,
        // then assign "baz". On scope exit, temp restores to undefined.
        if var_rest.starts_with('=') && !var_rest.starts_with("==") {
            let rhs_rest = &var_rest[1..];
            let (rhs_rest, _) = ws(rhs_rest)?;
            let (rhs_rest, rhs_expr) = expression(rhs_rest)?;
            return parse_statement_modifier(
                rhs_rest,
                Stmt::Let {
                    name: var_name.clone(),
                    index: None,
                    value: Some(Box::new(rhs_expr)),
                    is_temp: true,
                    undefine_first: true,
                },
            );
        }
        // Bare: temp undefine($c) — just undefine (no temp wrapping)
        return parse_statement_modifier(
            var_rest,
            Stmt::Expr(Expr::Call {
                name: crate::symbol::Symbol::intern("undefine"),
                args: vec![make_var_expr(&var_name)],
            }),
        );
    }
    // Parse sigil + optional twigil + var name
    let sigil = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("variable after temp"))?;
    if sigil != '$' && sigil != '@' && sigil != '%' {
        return Err(PError::expected("variable after temp"));
    }
    let after_sigil = &rest[1..];
    // Handle special variables: $/, $!
    if sigil == '$' && (after_sigil.starts_with('/') || after_sigil.starts_with('!')) {
        let special_char = &after_sigil[..1];
        let rest_after = &after_sigil[1..];
        let full_name = special_char.to_string();
        let (rest_after, _) = ws(rest_after)?;
        // Check for assignment: temp $/ = expr
        if rest_after.starts_with('=') && !rest_after.starts_with("==") {
            let val_rest = &rest_after[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: None,
                    value: Some(Box::new(val_expr)),
                    is_temp: true,
                    undefine_first: false,
                },
            );
        }
        // Bare temp: temp $/
        return parse_statement_modifier(
            rest_after,
            Stmt::Let {
                name: full_name,
                index: None,
                value: None,
                is_temp: true,
                undefine_first: false,
            },
        );
    }
    // Handle twigils: $*CWD, $?FILE, etc.
    let (after_twigil, twigil) = if after_sigil.starts_with('*')
        || after_sigil.starts_with('?')
        || after_sigil.starts_with('!')
    {
        (&after_sigil[1..], &after_sigil[..1])
    } else {
        (after_sigil, "")
    };
    let (mut rest, mut var_name) = ident(after_twigil)?;
    // Support package-qualified names: `temp $Foo::Bar::scalar`. Consume any
    // additional `::ident` segments and append them to var_name.
    while let Some(after_sep) = rest.strip_prefix("::") {
        if let Ok((after_seg, seg)) = ident_pub(after_sep) {
            var_name = format!("{}::{}", var_name, seg);
            rest = after_seg;
        } else {
            break;
        }
    }
    // Build full env key including twigil
    let full_name = if sigil == '$' {
        if twigil.is_empty() {
            var_name.clone()
        } else {
            format!("{}{}", twigil, var_name)
        }
    } else {
        format!("{}{}{}", sigil, twigil, var_name)
    };
    let (rest, _) = ws(rest)?;

    // Check for array index: temp @array[idx] = expr
    if let Some(idx_rest) = rest.strip_prefix('[') {
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, idx_expr) = expression(idx_rest)?;
        let (idx_rest, _) = ws(idx_rest)?;
        let (idx_rest, _) = parse_char(idx_rest, ']')?;
        let (idx_rest, _) = ws(idx_rest)?;
        if idx_rest.starts_with('=') && !idx_rest.starts_with("==") {
            let val_rest = &idx_rest[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: Some(Box::new(idx_expr)),
                    value: Some(Box::new(val_expr)),
                    is_temp: true,
                    undefine_first: false,
                },
            );
        }
        return parse_statement_modifier(
            idx_rest,
            Stmt::Let {
                name: full_name,
                index: Some(Box::new(idx_expr)),
                value: None,
                is_temp: true,
                undefine_first: false,
            },
        );
    }

    // Check for hash key: temp %hash<key> = expr
    if let Some(key_rest) = rest.strip_prefix('<')
        && let Some(end_pos) = key_rest.find('>')
    {
        let key_str = &key_rest[..end_pos];
        let after_key = &key_rest[end_pos + 1..];
        let (after_key, _) = ws(after_key)?;
        let key_expr = Expr::Literal(Value::str(key_str.to_string()));
        if after_key.starts_with('=') && !after_key.starts_with("==") {
            let val_rest = &after_key[1..];
            let (val_rest, _) = ws(val_rest)?;
            let (val_rest, val_expr) = expression(val_rest)?;
            return parse_statement_modifier(
                val_rest,
                Stmt::Let {
                    name: full_name,
                    index: Some(Box::new(key_expr)),
                    value: Some(Box::new(val_expr)),
                    is_temp: true,
                    undefine_first: false,
                },
            );
        }
        return parse_statement_modifier(
            after_key,
            Stmt::Let {
                name: full_name,
                index: Some(Box::new(key_expr)),
                value: None,
                is_temp: true,
                undefine_first: false,
            },
        );
    }

    // Check for assignment: temp $*CWD = expr
    if rest.starts_with('=') && !rest.starts_with("==") {
        let val_rest = &rest[1..];
        let (val_rest, _) = ws(val_rest)?;
        let (val_rest, val_expr) = expression(val_rest)?;
        return parse_statement_modifier(
            val_rest,
            Stmt::Let {
                name: full_name,
                index: None,
                value: Some(Box::new(val_expr)),
                is_temp: true,
                undefine_first: false,
            },
        );
    }
    // Bare temp: temp $var
    parse_statement_modifier(
        rest,
        Stmt::Let {
            name: full_name,
            index: None,
            value: None,
            is_temp: true,
            undefine_first: false,
        },
    )
}
