use super::super::super::expr::expression;
use super::super::super::helpers::ws;
use super::super::super::parse_result::{PError, PResult, take_while1};
use super::super::{keyword, qualified_ident};
use super::constant_subset::constant_decl;
use super::helpers::{parse_sigilless_decl_name, register_term_symbol_from_decl_name};
use super::{parse_decl_type_constraint, parse_statement_modifier};
use crate::ast::{Expr, Stmt};
use crate::parser::stmt::assign::parse_colon_args;
use crate::symbol::Symbol;
use crate::value::Value;

use super::parse_assign_expr_or_comma;

/// Build the statement for a sigilless bind/declaration (`my \name := expr`,
/// `my \name ::= expr`, or `my \name = expr` — all three bind the sigilless
/// name to `expr` rather than assigning into a container, since a sigilless
/// term has none).
///
/// Mirrors the sigilled `:=` logic in `my_decl_assign::handle_binding`: when
/// the RHS can denote a CONTAINER, the sigilless name becomes an alias of it
/// through the same `MarkBind`/`WrapVarRef`/`bind_source` machinery the sigilled
/// case uses (raku: `my $a = 5; my \x := $a; x = 10; say $a` prints `10`).
///
/// The set of such shapes is a variable, an element (`@a[0]`, `%h<k>`, a
/// computed index) and a method call — the last because an `is rw` accessor
/// returns a container and the parser cannot tell it from an ordinary method.
/// Whether the binding is actually writable is therefore settled at RUN time by
/// `OpCode::MarkSigillessBind`: `my \x := $c.v` aliases the attribute, while
/// `my \x := $s.uc` binds a plain `Str` and stays immutable.
///
/// This used to be `matches!(expr, Expr::Var(_))` alone, so every element and
/// accessor bind was marked readonly outright and `x = 9` died with "Cannot
/// modify an immutable Int". A literal or any other computed rvalue keeps the
/// readonly path: it can never denote a container, and routing it through the
/// bind machinery would give it a container cell that a later same-named
/// declaration in a callee can collide with.
///
/// This does NOT depend on whether a type constraint was written — `my Mu
/// \a := $a` and `my \a := $a` behave the same way in raku.
fn build_sigilless_bind_stmt(
    name: String,
    expr: Expr,
    type_constraint: Option<String>,
    is_state: bool,
    is_our: bool,
) -> Stmt {
    let binds_a_container = match expr {
        Expr::Var(_) | Expr::Index { .. } | Expr::MethodCall { .. } => true,
        // A SIGILLESS source (`my \y := $a; my \x := y`) denotes whatever `y`
        // was bound to, so the chain must be able to reach the first alias's
        // container. A bareword is only admitted when it is a declared
        // sigilless value term -- an ordinary bareword (a type name, a listop
        // call) keeps the readonly path. The sigiled-target twin
        // (`my $x := y`) already routes this way, through the `__scalar_bind`
        // branch in `compile_stmt`'s `VarDecl` arm.
        Expr::BareWord(ref n) => crate::parser::stmt::simple::is_user_declared_value_term(n),
        _ => false,
    };
    let decl = Stmt::VarDecl {
        name: name.clone(),
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    };
    if binds_a_container {
        Stmt::SyntheticBlock(vec![Stmt::MarkBind, decl, Stmt::MarkSigilless(name)])
    } else {
        Stmt::SyntheticBlock(vec![decl, Stmt::MarkSigillessReadonly(name)])
    }
}

/// Parse a sigilless variable declaration: `my \name = expr`
pub(super) fn parse_sigilless_decl(
    input: &str,
    is_state: bool,
    is_our: bool,
    type_constraint: Option<String>,
    apply_modifier: bool,
) -> PResult<'_, Stmt> {
    let (r, name) = parse_sigilless_decl_name(input)?;
    register_term_symbol_from_decl_name(&name);
    let (r, _) = ws(r)?;
    if let Some(r) = r.strip_prefix("::=").or_else(|| r.strip_prefix(":=")) {
        let (r, _) = ws(r)?;
        let (r, expr) = parse_assign_expr_or_comma(r)?;
        let stmt = build_sigilless_bind_stmt(name, expr, type_constraint.clone(), is_state, is_our);
        if apply_modifier {
            return parse_statement_modifier(r, stmt);
        }
        return Ok((r, stmt));
    }
    // .= mutating method call: my \foo .= new => my \foo = Type.new
    if let Some(stripped) = r.strip_prefix(".=") {
        let (r, _) = ws(stripped)?;
        let (r, method_name) =
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        let r_before_ws = r;
        let (r, _) = ws(r)?;
        // Parse optional args (parenthesized, colon-form, or fake-infix adverbs)
        let (r, args) = if let Some(inner) = r.strip_prefix('(') {
            let r = inner;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let r = r.strip_prefix(')').ok_or_else(|| PError::expected(")"))?;
            (r, args)
        } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
            // Colon invocant form (no space before ':'): .=method: arg
            parse_colon_args(r_before_ws)?
        } else if r.starts_with(':') && !r.starts_with("::") {
            // Fake-infix adverb form (space before ':'): .=method :key<val>
            super::my_decl_assign::parse_fake_infix_adverbs(r)?
        } else {
            (r, Vec::new())
        };
        // Build: Type.method(args) where Type comes from type_constraint or defaults to "Mu"
        let target_name = type_constraint.as_deref().unwrap_or("Mu").to_string();
        let expr = Expr::MethodCall {
            target: Box::new(Expr::BareWord(target_name)),
            name: Symbol::intern(&method_name),
            args,
            modifier: None,
            quoted: false,
        };
        let decl = Stmt::VarDecl {
            name: name.clone(),
            expr,
            type_constraint: type_constraint.clone(),
            is_state,
            is_our,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        };
        let stmt = Stmt::SyntheticBlock(vec![decl, Stmt::MarkSigillessReadonly(name)]);
        if apply_modifier {
            return parse_statement_modifier(r, stmt);
        }
        return Ok((r, stmt));
    }
    if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
        let r = &r[1..];
        let (r, _) = ws(r)?;
        let (r, expr) = parse_assign_expr_or_comma(r)?;
        let stmt = build_sigilless_bind_stmt(name, expr, type_constraint.clone(), is_state, is_our);
        if apply_modifier {
            return parse_statement_modifier(r, stmt);
        }
        return Ok((r, stmt));
    }
    // A sigilless term declaration (`my \foo`) requires an initializer; there
    // is no implicit default like a sigilled variable has. rakudo rejects this
    // at compile time with X::Syntax::Term::MissingInitializer.
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "message".to_string(),
        Value::str("Term definition requires an initializer".to_string()),
    );
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Term::MissingInitializer"), attrs);
    Err(PError::fatal_with_exception(
        "Term definition requires an initializer".to_string(),
        Box::new(ex),
    ))
}

/// Parse optional type constraint before a variable sigil.
pub(super) fn parse_optional_type_constraint(rest: &str) -> PResult<'_, Option<String>> {
    let saved = rest;
    if let Some((r, tc)) = parse_decl_type_constraint(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('$')
            || r2.starts_with('@')
            || r2.starts_with('%')
            || r2.starts_with('&')
            || r2.starts_with('\\')
            || r2.starts_with('(')
            || keyword("constant", r2).is_some()
        {
            Ok((r2, Some(tc)))
        } else {
            // Check for multiple prefix type constraints (e.g. `my Int Str $x`)
            if let Some((r3, _second_tc)) = parse_decl_type_constraint(r2) {
                let (r4, _) = ws(r3).unwrap_or((r3, ()));
                if r4.starts_with('$')
                    || r4.starts_with('@')
                    || r4.starts_with('%')
                    || r4.starts_with('&')
                    || r4.starts_with('\\')
                {
                    return Err(PError::raw(
                        "FATAL:X::Comp::NYI: Multiple prefix constraints not yet implemented. Sorry."
                            .to_string(),
                        Some(r2.len()),
                    ));
                }
            }
            Ok((saved, None))
        }
    } else {
        Ok(parse_fallback_type_constraint(saved).unwrap_or((saved, None)))
    }
}

fn parse_fallback_type_constraint(rest: &str) -> Option<(&str, Option<String>)> {
    let (mut r, mut tc) = qualified_ident(rest).ok()?;
    if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
        tc.push_str(&r[..2]);
        r = &r[2..];
    } else if r.starts_with(':')
        && r[1..]
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
    {
        let smiley_rest = &r[1..];
        let end = smiley_rest
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(smiley_rest.len());
        tc.push_str(&r[..end + 1]);
        r = &r[end + 1..];
    }
    let (r2, _) = ws(r).ok()?;
    if r2.starts_with('$')
        || r2.starts_with('@')
        || r2.starts_with('%')
        || r2.starts_with('&')
        || r2.starts_with('\\')
        || r2.starts_with('(')
        || keyword("constant", r2).is_some()
    {
        Some((r2, Some(tc)))
    } else {
        None
    }
}

/// Check for invalid type smileys (e.g. Int:foo).
///
/// Shares the parameter parser's implementation so a declaration and a
/// signature agree on what counts as a smiley — in particular that the colon
/// inside `Array[Str:D]` is the *inner* type's, not the outer one's.
pub(super) fn check_invalid_type_smiley(type_constraint: &Option<String>) -> Result<(), PError> {
    crate::parser::stmt::sub_param::check_invalid_type_smiley(type_constraint)
}

/// Parse `my <Type> constant <name> = <expr>`.
pub(super) fn parse_typed_constant(
    rest: &str,
    type_constraint: Option<String>,
    is_our: bool,
    apply_modifier: bool,
) -> PResult<'_, Stmt> {
    let (r, mut stmt) = constant_decl(rest)?;
    if let Stmt::VarDecl {
        type_constraint: ref mut tc,
        is_our: ref mut our_flag,
        ref mut expr,
        ..
    } = stmt
    {
        if let Some(ref type_name) = type_constraint
            && let Expr::MethodCall { target, .. } = expr
            && matches!(target.as_ref(), Expr::BareWord(w) if w == "Mu")
        {
            **target = Expr::BareWord(type_name.clone());
        }
        *tc = type_constraint;
        *our_flag = is_our;
    }
    if apply_modifier {
        return parse_statement_modifier(r, stmt);
    }
    Ok((r, stmt))
}

/// Try to parse `our $.name` / `my $.name` — class-level (shared) attributes.
#[allow(clippy::too_many_arguments)]
pub(super) fn try_dot_twigil_attr<'a>(
    rest: &'a str,
    sigil: u8,
    is_array: bool,
    is_hash: bool,
    is_our: bool,
    is_state: bool,
    type_constraint: &Option<String>,
    apply_modifier: bool,
) -> Result<Option<(&'a str, Stmt)>, PError> {
    if (sigil == b'$' || is_array || is_hash)
        && rest.len() > 2
        && rest.as_bytes()[1] == b'.'
        && rest[2..]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    {
        let after_dot = &rest[2..];
        let (after_name, attr_name) = take_while1(after_dot, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let attr_name = attr_name.to_string();
        let (after_name, _) = ws(after_name)?;
        let (after_name, default) = if after_name.starts_with('=')
            && !after_name.starts_with("==")
            && !after_name.starts_with("=>")
        {
            let r = &after_name[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            (r, Some(expr))
        } else {
            (after_name, None)
        };
        let stmt = Stmt::HasDecl {
            name: Symbol::intern(&attr_name),
            is_public: true,
            default,
            handles: Vec::new(),
            is_rw: true,
            is_readonly: false,
            type_constraint: type_constraint.clone(),
            type_smiley: None,
            is_required: None,
            sigil: sigil as char,
            where_constraint: None,
            is_alias: false,
            is_our,
            is_my: !is_our && !is_state,
            is_default: None,
            is_type: None,
            deprecated_message: None,
            is_built: None,
            unknown_traits: Vec::new(),
        };
        if apply_modifier {
            return parse_statement_modifier(after_name, stmt).map(Some);
        }
        return Ok(Some((after_name, stmt)));
    }
    Ok(None)
}
