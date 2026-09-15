use crate::ast::{Expr, Stmt};
use crate::parser::expr::expression;
use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::stmt::idents::{ident, keyword};
use crate::parser::stmt::modifier::parse_statement_modifier;
use crate::parser::stmt::pub_shims::ident_pub;
use crate::value::Value;

/// A `let`/`temp` whose variable is followed by a COMPOUND assignment
/// (`let %h .= push: @v`, `temp $x //= 5`).
///
/// `let`/`temp` are statement prefixes over an assignment, and a compound one is
/// an assignment like any other — but only the plain `=` form had a branch, so
/// the bare `let %h` was taken alone and the `.= push` that followed became a
/// *topic* dot-assign: the push landed on `$_` and the container was left
/// untouched (`let %!r .= push: (a => 1); say %!r` answered `{}` where rakudo
/// answers `{a => 1}`). Parse the assignment as an ordinary expression from the
/// variable and run it after the save, the same lowering `temp $s[1]<k> = 23`
/// already uses.
///
/// `var_start` is the input positioned AT the variable; `rest` is what follows
/// its name. Returns `None` when no compound assignment follows.
fn let_compound_assign_stmt<'a>(
    var_start: &'a str,
    rest: &str,
    full_name: String,
    is_temp: bool,
) -> Option<PResult<'a, Stmt>> {
    if !rest.starts_with(".=")
        && crate::parser::stmt::assign::parse_compound_assign_op(rest).is_none()
    {
        return None;
    }
    Some((|| {
        let (r, assign_expr) = expression(var_start)?;
        parse_statement_modifier(
            r,
            Stmt::SyntheticBlock(vec![
                Stmt::Let {
                    name: full_name,
                    index: None,
                    value: None,
                    is_temp,
                    undefine_first: false,
                },
                Stmt::Expr(assign_expr),
            ]),
        )
    })())
}

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
    let var_start = rest;
    let rest_after_sigil = &rest[1..];
    // An ATTRIBUTE is as temporizable as a lexical: `let %!record`, `temp $!x`.
    // Only the plain spelling was recognized here, so `let $!x = 2` fell out of
    // this parser entirely and came back as the bareword `let` followed by an
    // ordinary assignment — the save was silently dropped — while the term-position
    // form `(let %!record)` (Data::Record::Map, #7954) was a hard parse error.
    // The attribute's env key carries the twigil (`%!r`, `!x`), exactly as
    // `HashVar("!r")` / `Var("!x")` spell it everywhere else.
    let (rest_after_sigil, twigil) = match rest_after_sigil.strip_prefix('!') {
        Some(after) => (after, "!"),
        None => (rest_after_sigil, ""),
    };
    let (rest, var_name) = ident(rest_after_sigil)?;
    // Env key: scalars strip $, arrays/hashes keep sigil; both keep the twigil.
    let full_name = if sigil == '$' {
        format!("{}{}", twigil, var_name)
    } else {
        format!("{}{}{}", sigil, twigil, var_name)
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

    if let Some(parsed) = let_compound_assign_stmt(var_start, rest, full_name.clone(), false) {
        return parsed;
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

/// Walk an indexed lvalue chain down to its base variable and return the env
/// key used by `LetSave` for that variable (scalars drop their `$` sigil;
/// arrays/hashes keep their `@`/`%`). Returns `None` for non-variable bases
/// (e.g. a method call), which the multi-level `temp` lowering cannot save.
fn lvalue_base_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Var(n) => Some(n.clone()),
        Expr::ArrayVar(n) => Some(format!("@{}", n)),
        Expr::HashVar(n) => Some(format!("%{}", n)),
        Expr::Index { target, .. } => lvalue_base_name(target),
        _ => None,
    }
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
    if let Ok((expr_rest, expr)) = expression(rest) {
        // temp on a *multi-level* indexed lvalue: `temp $s[1]<k>[1] = 23`.
        // `expression` parses the whole `lvalue = value` into a nested
        // `IndexAssign` (its `target` is itself an `Index`). The single-level
        // `temp @a[i] = v` / `temp %h<k> = v` forms below cannot represent this
        // chain, so handle it here: temporize the *whole* base container (saved
        // and restored at scope exit, mirroring the single-level whole-container
        // save) and then run the full nested assignment. The two run inline in
        // the current scope via a `SyntheticBlock` (no fresh let-saves mark).
        if let Expr::IndexAssign { target, .. } = &expr
            && matches!(target.as_ref(), Expr::Index { .. })
            && let Some(save_name) = lvalue_base_name(target)
        {
            let save_stmt = Stmt::Let {
                name: save_name,
                index: None,
                value: None,
                is_temp: true,
                undefine_first: false,
            };
            return parse_statement_modifier(
                expr_rest,
                Stmt::SyntheticBlock(vec![save_stmt, Stmt::Expr(expr)]),
            );
        }
        // temp on lvalue method call: `temp $obj.method = value`, where the
        // expression parser has already lowered `$obj.method = value` to the
        // `__mutsu_assign_method_lvalue` writeback call (an rw-accessor assignment
        // is a full expression). Recover the pieces to save/restore the target.
        if let Expr::Call { name, args } = &expr
            && name == "__mutsu_assign_method_lvalue"
            && args.len() == 5
            && let Expr::Var(var_name) = &args[0]
            && let Expr::Literal(mlit) = &args[1]
            && let Some(method_name) = mlit.as_str()
            && let Expr::ArrayLiteral(method_args) = &args[2]
        {
            return parse_statement_modifier(
                expr_rest,
                Stmt::TempMethodAssign {
                    var_name: var_name.clone(),
                    method_name: method_name.to_string(),
                    method_args: method_args.clone(),
                    value: args[3].clone(),
                },
            );
        }
        // temp on lvalue method call: `temp $obj.method = value`
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
    let var_start = rest;
    let after_sigil = &rest[1..];
    // Handle special variables: $/, $!
    //
    // `$!` names the error variable only when nothing follows it: `$!x` is the
    // ATTRIBUTE `x`, and reading its `!` as the special variable made
    // `temp $!x = 2` a `temp $!` followed by an assignment to a stray lexical
    // `x`, so the attribute was never written at all (`say $!x` answered 1 where
    // rakudo answers 2). The twigil branch below is what handles it.
    if sigil == '$'
        && (after_sigil.starts_with('/')
            || (after_sigil.starts_with('!')
                && !after_sigil[1..].starts_with(crate::parser::helpers::is_raku_identifier_start)))
    {
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

    if let Some(parsed) = let_compound_assign_stmt(var_start, rest, full_name.clone(), true) {
        return parsed;
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
