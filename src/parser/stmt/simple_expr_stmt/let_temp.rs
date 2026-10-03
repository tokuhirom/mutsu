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
                    nested_lvalue: false,
                },
                Stmt::Expr(assign_expr),
            ]),
        )
    })())
}

/// A `let`/`temp` of one subscripted element, `@a[i]`, `%h<k>` or `%h{EXPR}`,
/// with an optional `= value` or compound assignment. `var_start` is the input
/// at the variable, `rest` is positioned right after its name. Returns `None` when no subscript follows. The `{EXPR}` spelling was
/// missing, so `temp %!replacing{$key} = True` (Pod::To::PDF::Lite) failed to
/// parse while `temp %h<key>` worked.
fn let_subscript_stmt<'a>(
    var_start: &'a str,
    rest: &'a str,
    full_name: &str,
    is_temp: bool,
) -> Option<PResult<'a, Stmt>> {
    let (after_index, index) = if let Some(key_rest) = rest.strip_prefix('<')
        && let Some(end_pos) = key_rest.find('>')
    {
        let key = Expr::Literal(Value::str(key_rest[..end_pos].to_string()));
        (Ok(&key_rest[end_pos + 1..]), key)
    } else {
        let close = match rest.as_bytes().first() {
            Some(b'[') => ']',
            Some(b'{') => '}',
            _ => return None,
        };
        let parsed = (|| {
            let (r, _) = ws(&rest[1..])?;
            let (r, idx) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, close)?;
            Ok((r, idx))
        })();
        match parsed {
            Ok((r, idx)) => (Ok(r), idx),
            Err(e) => return Some(Err(e)),
        }
    };
    Some((|| {
        let (r, _) = ws(after_index?)?;
        // A compound assignment to the element (`temp %h<k> //= v`,
        // Net::HTTP): save the element, then run the whole assignment as an
        // ordinary expression, the lowering `let_compound_assign_stmt` uses
        // for a plain variable.
        if r.starts_with(".=") || crate::parser::stmt::assign::parse_compound_assign_op(r).is_some()
        {
            let (r, assign_expr) = expression(var_start)?;
            let save = Stmt::Let {
                name: full_name.to_string(),
                index: Some(Box::new(index)),
                value: None,
                is_temp,
                undefine_first: false,
                nested_lvalue: false,
            };
            return parse_statement_modifier(
                r,
                Stmt::SyntheticBlock(vec![save, Stmt::Expr(assign_expr)]),
            );
        }
        let (r, value) = if r.starts_with('=') && !r.starts_with("==") {
            let (r, _) = ws(&r[1..])?;
            let (r, value) = expression(r)?;
            (r, Some(Box::new(value)))
        } else {
            (r, None)
        };
        parse_statement_modifier(
            r,
            Stmt::Let {
                name: full_name.to_string(),
                index: Some(Box::new(index)),
                value,
                is_temp,
                undefine_first: false,
                nested_lvalue: false,
            },
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

    if let Some(parsed) = let_subscript_stmt(var_start, rest, &full_name, false) {
        return parsed;
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
                nested_lvalue: false,
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
            nested_lvalue: false,
        },
    )
}

/// Walk an indexed lvalue chain down to its base variable and return the env
/// key used by `LetSave` for that variable (scalars drop their `$` sigil;
/// arrays/hashes keep their `@`/`%`). Returns `None` for non-variable bases
/// (e.g. a method call), which the multi-level `temp` lowering cannot save.
///
/// Parentheses are transparent here: `temp (@a)[0]` and `temp ((@a)[0])[1]`
/// name elements of `@a` exactly as `temp @a[0]` does (#10581).
// Cost: O(d + |name|), d = subscript and paren depth.
fn lvalue_base_name(expr: &Expr) -> Option<String> {
    let mut expr = expr;
    loop {
        match expr {
            Expr::Index { target, .. } | Expr::Grouped(target) => expr = target,
            other => return other.container_var_key(),
        }
    }
}

/// Whether an element lvalue's container is anything other than a plain
/// variable — a further subscript (`$s[1]<k>`) or a parenthesized operand
/// (`(@a)[0]`). Those are the shapes the single-level `let_subscript_stmt`
/// path below cannot spell, so they go through the nested-lvalue lowering.
// Cost: O(1).
fn is_compound_elem_container(target: &Expr) -> bool {
    matches!(target, Expr::Index { .. } | Expr::Grouped(_))
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

/// `temp my $x = 1` / `temp our $out = ''`: `temp` over a declaration.
///
/// The declarator's initializer belongs to the declaration, so the declaration
/// runs first and `temp` then saves the value it left (rakudo restores `''`,
/// not the package variable's earlier value, at scope exit). Lowered to the
/// declaration followed by a bare `temp` of the declared variable. Without this
/// `temp` was left behind as a bare word statement of its own (#10257).
///
/// Returns `None` when no single-variable declaration follows.
fn temp_declaration_stmt(input: &str) -> Option<PResult<'_, Stmt>> {
    if !["my", "our", "state"]
        .iter()
        .any(|kw| keyword(kw, input).is_some())
    {
        return None;
    }
    let (rest, decl) = crate::parser::stmt::decl::my_decl_expr(input).ok()?;
    let Stmt::VarDecl { name, .. } = &decl else {
        return None;
    };
    let save = Stmt::Let {
        name: name.clone(),
        index: None,
        value: None,
        is_temp: true,
        undefine_first: false,
        nested_lvalue: false,
    };
    Some(parse_statement_modifier(
        rest,
        Stmt::SyntheticBlock(vec![decl, save]),
    ))
}

/// The variable a `temp <invocant>.method …` saves: a plain scalar `$obj`, or
/// `self` — spelled `self` or as the `$.attr` invocant carrier, which the
/// parser leaves as the collapsed anonymous-state name `__ANON_STATE__`.
fn temp_method_invocant_name(invocant: &Expr) -> Option<String> {
    match invocant {
        Expr::Var(name) if name == "__ANON_STATE__" => Some("self".to_string()),
        Expr::Var(name) => Some(name.clone()),
        Expr::BareWord(name) if name == "self" => Some("self".to_string()),
        _ => None,
    }
}

/// Parse `temp` statement — same semantics as `let` (save/restore at scope exit).
pub(crate) fn temp_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("temp", input).ok_or_else(|| PError::expected("temp statement"))?;
    let (rest, _) = ws1(rest)?;
    if let Some(parsed) = temp_declaration_stmt(rest) {
        return parsed;
    }
    if let Ok((expr_rest, expr)) = expression(rest) {
        // temp on a *multi-level* indexed lvalue: `temp $s[1]<k>[1] = 23`.
        // `expression` parses the whole `lvalue = value` into a nested
        // `IndexAssign` (its `target` is itself an `Index`). The single-level
        // `temp @a[i] = v` / `temp %h<k> = v` forms below cannot represent this
        // chain, so the `Let` carries the whole assignment as its value and the
        // compiler temporizes the element that assignment's target names.
        if let Expr::IndexAssign { target, .. } = &expr
            && is_compound_elem_container(target)
            && let Some(save_name) = lvalue_base_name(target)
        {
            return parse_statement_modifier(
                expr_rest,
                Stmt::Let {
                    name: save_name,
                    index: None,
                    value: Some(Box::new(expr)),
                    is_temp: true,
                    undefine_first: false,
                    nested_lvalue: true,
                },
            );
        }
        // A bare element temp with no assignment over the same compound
        // shapes: `temp (@a)[0];`, `temp $s[1]<k>;`. The `Let` carries the
        // element expression itself and the compiler saves that element.
        if let Expr::Index { target, .. } = &expr
            && is_compound_elem_container(target)
            && let Some(save_name) = lvalue_base_name(target)
        {
            return parse_statement_modifier(
                expr_rest,
                Stmt::Let {
                    name: save_name,
                    index: None,
                    value: Some(Box::new(expr)),
                    is_temp: true,
                    undefine_first: false,
                    nested_lvalue: true,
                },
            );
        }
        // temp on lvalue method call: `temp $obj.method = value`, where the
        // expression parser has already lowered `$obj.method = value` to the
        // `__mutsu_assign_method_lvalue` writeback call (an rw-accessor assignment
        // is a full expression). Recover the pieces to save/restore the target.
        if let Expr::Call { name, args } = &expr
            && name == "__mutsu_assign_method_lvalue"
            && args.len() == 5
            && let Some(var_name) = temp_method_invocant_name(&args[0])
            && let Expr::Literal(mlit) = &args[1]
            && let Some(method_name) = mlit.as_str()
            && let Expr::ArrayLiteral(method_args) = &args[2]
        {
            return parse_statement_modifier(
                expr_rest,
                Stmt::TempMethodAssign {
                    var_name,
                    method_name: method_name.to_string(),
                    method_args: method_args.clone(),
                    value: args[3].clone(),
                },
            );
        }
        // temp on a compound assignment through a method lvalue:
        // `temp $obj.indent ~= '  '`, `temp $.indent ~= '  '` (CSS::Writer).
        // The expanded writeback call already carries the combined value
        // (`$obj.indent ~ '  '`), so temporize the invocant exactly as the
        // plain `temp $obj.method = value` form above does.
        if let Expr::CompoundAssign {
            target, expanded, ..
        } = &expr
            && let Expr::MethodCall {
                target: invocant,
                name: method_name,
                args: method_args,
                ..
            } = target.as_ref()
            && let Some(var_name) = temp_method_invocant_name(invocant)
            && let Expr::Call { name, args } = expanded.as_ref()
            && name == "__mutsu_assign_method_lvalue"
            && args.len() >= 4
        {
            return parse_statement_modifier(
                expr_rest,
                Stmt::TempMethodAssign {
                    var_name,
                    method_name: method_name.resolve(),
                    method_args: method_args.clone(),
                    value: args[3].clone(),
                },
            );
        }
        // `temp $.attr = value` / `temp $.attr ~= value`: the `$.attr` lvalue
        // is `self.attr`, so it is the method-lvalue form on `self`. The
        // compound form's expansion is the plain assignment of the combined
        // value.
        let plain_assign = match &expr {
            Expr::CompoundAssign { expanded, .. } => expanded.as_ref(),
            other => other,
        };
        if let Expr::AssignExpr {
            name,
            expr: value,
            is_bind: false,
        } = plain_assign
            && let Some(method_name) = name.strip_prefix('.')
            && method_name.starts_with(crate::parser::helpers::is_raku_identifier_start)
        {
            return parse_statement_modifier(
                expr_rest,
                Stmt::TempMethodAssign {
                    var_name: "self".to_string(),
                    method_name: method_name.to_string(),
                    method_args: Vec::new(),
                    value: (**value).clone(),
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
                    name: var_name,
                    index: None,
                    value: Some(Box::new(rhs_expr)),
                    is_temp: true,
                    undefine_first: true,
                    nested_lvalue: false,
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
                    nested_lvalue: false,
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
                nested_lvalue: false,
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
            var_name
        } else {
            format!("{}{}", twigil, var_name)
        }
    } else {
        format!("{}{}{}", sigil, twigil, var_name)
    };
    let (rest, _) = ws(rest)?;

    if let Some(parsed) = let_subscript_stmt(var_start, rest, &full_name, true) {
        return parsed;
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
                nested_lvalue: false,
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
            nested_lvalue: false,
        },
    )
}
