use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char, parse_char};
use super::super::parse_statement_modifier;
use super::super::{ident, keyword};
use super::helpers::register_term_symbol_from_decl_name;
use super::parse_decl_type_constraint;
use crate::ast::{Expr, Stmt};
use crate::value::Value;

use super::parse_comma_or_expr;

mod bind_arity;
mod elements;
mod named;
use elements::{collect_nested_group_vars, parse_element_traits};
use named::parse_named_destructuring;
pub(crate) mod desugar;
use crate::parser::stmt::assign::parse_comma_or_expr_no_word_logical;

use crate::ast::{SignatureDecl, SignatureInit, SignatureVar as DestructureVar};

pub(in crate::parser::stmt) fn parse_destructuring_decl(
    input: &str,
    is_state: bool,
    is_our: bool,
    type_constraint: Option<String>,
) -> PResult<'_, Stmt> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    let mut vars: Vec<DestructureVar> = Vec::new();
    // A nested group's leaves are flattened into `vars`, so once one is
    // present `vars.len()` no longer counts the declared positionals.
    let mut has_nested_group = false;
    let mut r = rest;
    loop {
        if r.starts_with(')') {
            break;
        }

        // Nested group: `my (\d, (\e, \f)) = ...`. Raku binds the corresponding
        // RHS element by recursively destructuring it; we flatten the inner
        // sigilless/sigilled targets so they are all declared and assigned
        // positionally. (The precise nested *value* binding is `#?rakudo skip`-ped
        // even on rakudo, so only flattening-without-error is required here.)
        if r.starts_with('(') {
            has_nested_group = true;
            let r2 = collect_nested_group_vars(r, &mut vars)?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
            continue;
        }

        let mut is_slurpy = false;
        let mut is_named = false;

        // Check for slurpy prefix '*'
        if let Some(after) = r.strip_prefix('*') {
            is_slurpy = true;
            r = after;
        }

        // Check for named prefix ':'
        if let Some(after) = r.strip_prefix(':') {
            is_named = true;
            r = after;
        }

        // Try to parse a type constraint before the variable (e.g. `Foo $d`)
        let mut per_var_type_constraint = None;
        if let Some((after_tc, tc)) = parse_decl_type_constraint(r) {
            let (after_tc_ws, _) = ws(after_tc)?;
            // Only treat as type if followed by a sigil or sigilless backslash
            if after_tc_ws.starts_with('$')
                || after_tc_ws.starts_with('@')
                || after_tc_ws.starts_with('%')
                || after_tc_ws.starts_with('&')
                || after_tc_ws.starts_with('\\')
            {
                // An outer declaration type (`my Int (...)`) and an inner element
                // type (`Str $x`) that disagree are X::Syntax::Variable::ConflictingTypes.
                if let Some(outer) = &type_constraint
                    && outer != &tc
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "outer".to_string(),
                        crate::value::Value::package(crate::symbol::Symbol::intern(outer)),
                    );
                    attrs.insert(
                        "inner".to_string(),
                        crate::value::Value::package(crate::symbol::Symbol::intern(&tc)),
                    );
                    let msg = format!(
                        "X::Syntax::Variable::ConflictingTypes: Variable definition of type {} (from declaration) conflicts with type {} (from inner declaration)",
                        outer, tc
                    );
                    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
                    let exception = crate::value::Value::make_instance(
                        crate::symbol::Symbol::intern("X::Syntax::Variable::ConflictingTypes"),
                        attrs,
                    );
                    return Err(PError::fatal_with_exception(msg, Box::new(exception)));
                }
                per_var_type_constraint = Some(tc);
                r = after_tc_ws;
            } else if !is_slurpy
                && !is_named
                && (after_tc_ws.starts_with(',') || after_tc_ws.starts_with(')'))
            {
                // A bare type in the list (`my ($a, Any, $b) = ...`) is an
                // anonymous typed scalar placeholder, the same as `Any $`.
                vars.push(DestructureVar {
                    name: "__ANON_STATE__".to_string(),
                    is_slurpy: false,
                    is_optional: false,
                    is_named: false,
                    default: None,
                    per_var_type_constraint: Some(tc),
                    where_constraint: None,
                    sigilless: false,
                    literal_value: None,
                    param_trait: None,
                });
                r = after_tc_ws;
                if let Some(after_comma) = r.strip_prefix(',') {
                    let (r2, _) = ws(after_comma)?;
                    r = r2;
                }
                continue;
            }
        }

        // Sigilless variable: \c or \name
        if let Some(after_backslash) = r.strip_prefix('\\') {
            let (r2, name) = ident(after_backslash)?;
            register_term_symbol_from_decl_name(&name);
            let (r2, _) = ws(r2)?;
            // Parse optional where constraint
            let (r2, where_constraint) = if keyword("where", r2).is_some() {
                let r3 = keyword("where", r2).unwrap();
                let (r3, _) = ws1(r3)?;
                let (r3, expr) = expression(r3)?;
                (r3, Some(expr))
            } else {
                (r2, None)
            };
            let (r2, _) = ws(r2)?;
            vars.push(DestructureVar {
                name,
                is_slurpy,
                is_optional: false,
                is_named,
                default: None,
                per_var_type_constraint,
                where_constraint,
                sigilless: true,
                literal_value: None,
                param_trait: None,
            });
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
            continue;
        }

        // Literal value: "foo" or 'bar' — acts as a match constraint
        if r.starts_with('"') || r.starts_with('\'') {
            let (r2, lit_expr) = expression(r)?;
            let (r2, _) = ws(r2)?;
            let anon_name = format!("__literal_match_{}", vars.len());
            vars.push(DestructureVar {
                name: anon_name,
                is_slurpy: false,
                is_optional: false,
                is_named: false,
                default: None,
                per_var_type_constraint: None,
                where_constraint: None,
                sigilless: false,
                literal_value: Some(lit_expr),
                param_trait: None,
            });
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
            continue;
        }

        let sigil = r.as_bytes().first().copied().unwrap_or(0);
        if sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&' {
            let prefix = match sigil {
                b'@' => "@",
                b'%' => "%",
                b'&' => "&",
                _ => "",
            };
            let (r2, n) = crate::parser::stmt::lexical_var_name(r)?;
            let full_name = format!("{}{}", prefix, n);
            if sigil == b'&' {
                // A `&name` destructure target (e.g. `my (&plan, &is) = ...`)
                // makes a bare `name` callable as a list-op afterwards.
                register_term_symbol_from_decl_name(&full_name);
            }
            let (r2, _) = ws(r2)?;

            // Check for optional suffix '?'
            let (r2, is_optional) = if let Some(after) = r2.strip_prefix('?') {
                (after, true)
            } else {
                (r2, false)
            };
            let (r2, _) = ws(r2)?;
            let display_name = if sigil == b'$' {
                format!("${n}")
            } else {
                full_name.clone()
            };
            let (r2, param_trait) = parse_element_traits(r2, &display_name)?;

            // Parse optional where constraint: $a where 2
            let (r2, where_constraint) = if keyword("where", r2).is_some() {
                let r3 = keyword("where", r2).unwrap();
                let (r3, _) = ws1(r3)?;
                let (r3, expr) = expression(r3)?;
                (r3, Some(expr))
            } else {
                (r2, None)
            };
            let (r2, _) = ws(r2)?;

            // Check for per-variable default value: ($x = 5)
            let (r2, default) =
                if r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>") {
                    let r3 = &r2[1..];
                    let (r3, _) = ws(r3)?;
                    let (r3, expr) = expression(r3)?;
                    (r3, Some(expr))
                } else {
                    (r2, None)
                };
            let (r2, _) = ws(r2)?;

            vars.push(DestructureVar {
                name: full_name,
                is_slurpy,
                is_optional,
                is_named,
                default,
                per_var_type_constraint,
                where_constraint,
                sigilless: false,
                literal_value: None,
                param_trait,
            });

            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
        } else {
            return Err(PError::expected(
                "variable sigil ($, @, %, &), sigilless (\\name), or literal",
            ));
        }
    }
    let (rest, _) = parse_char(r, ')')?;
    let (rest, _) = ws(rest)?;

    // Keep the sigilless source spelling available to the parser, but give the
    // sigilless `_` term the same private storage as a standalone declaration.
    // Otherwise a grouped `my (\_)` would reintroduce the topic collision.
    for dvar in &mut vars {
        if dvar.sigilless {
            dvar.name = crate::symbol::sigilless_storage_name(&dvar.name).to_string();
        }
    }

    // Parse optional `is default(expr)` trait on grouped declaration
    let mut rest = rest;
    let mut group_default_expr: Option<Expr> = None;
    if let Some(r) = keyword("is", rest)
        && let Ok((r, _)) = ws1(r)
        && let Some(r) = keyword("default", r)
    {
        let (r, _) = ws(r)?;
        if let Some(inner) = r.strip_prefix('(') {
            let (inner, _) = ws(inner)?;
            let (inner, default_expr) = expression(inner)?;
            let (inner, _) = ws(inner)?;
            let inner = inner
                .strip_prefix(')')
                .ok_or_else(|| PError::expected("closing paren in is default"))?;
            group_default_expr = Some(default_expr);
            let (r2, _) = ws(inner)?;
            rest = r2;
        }
    }

    let is_binding = rest.starts_with(":=") || rest.starts_with("::=");
    if rest.starts_with('=') || rest.starts_with("::=") || rest.starts_with(":=") {
        return parse_destructuring_with_rhs(
            rest,
            vars,
            is_state,
            is_our,
            is_binding,
            has_nested_group,
            type_constraint,
        );
    }
    // A sigilless term in a grouped declaration (`my (\a)`, `my (\a, \b)`)
    // has no implicit default and so requires an initializer, exactly like a
    // bare `my \a`. Without one, rakudo rejects it at compile time with
    // X::Syntax::Term::MissingInitializer.
    if vars.iter().any(|v| v.sigilless) {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            crate::value::Value::str("Term definition requires an initializer".to_string()),
        );
        let ex = crate::value::Value::make_instance(
            crate::symbol::Symbol::intern("X::Syntax::Term::MissingInitializer"),
            attrs,
        );
        return Err(PError::fatal_with_exception(
            "Term definition requires an initializer".to_string(),
            Box::new(ex),
        ));
    }
    // No assignment
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    let decl = SignatureDecl {
        vars,
        is_state,
        is_our,
        type_constraint,
        group_default: group_default_expr,
        has_nested_group,
        init: None,
    };
    Ok((rest, desugar::signature_decl(decl)))
}

/// Parse the RHS of a destructuring declaration with assignment or binding.
fn parse_destructuring_with_rhs(
    input: &str,
    vars: Vec<DestructureVar>,
    is_state: bool,
    is_our: bool,
    is_binding: bool,
    has_nested_group: bool,
    type_constraint: Option<String>,
) -> PResult<'_, Stmt> {
    let rest = if let Some(stripped) = input.strip_prefix("::=") {
        stripped
    } else if let Some(stripped) = input.strip_prefix(":=") {
        stripped
    } else {
        &input[1..]
    };
    let (rest, _) = ws(rest)?;
    let has_named = vars.iter().any(|v| v.is_named);
    // A loose word-logical (`and`/`or`/...) binds looser than list assignment:
    // `my ($z) = () and $z.defined` is `(my ($z) = ()) and $z.defined`. The
    // positional form stops the RHS before it and re-attaches it below with
    // the assigned LHS list as its left operand.
    let (rest, raw_rhs) = if has_named {
        parse_comma_or_expr(rest)?
    } else {
        parse_comma_or_expr_no_word_logical(rest)?
    };
    // If the RHS is followed (after whitespace) by a `{` block, that block is a
    // separate statement / conditional body — NOT a hash subscript of the
    // declaration. Preserve the whitespace so the expression-context postfix
    // parser (which only subscripts `{` when it has no leading space) leaves it
    // alone: `if my ($a, $b) = f() { ... }` must treat `{ ... }` as the if-body,
    // matching the scalar `if my $x = f() { ... }` path. Only consume the
    // optional trailing `;` when there is no such block.
    let (rest_ws, _) = ws(rest)?;
    let has_following_block = rest_ws.starts_with('{');
    let rest = if has_following_block { rest } else { rest_ws };

    if has_named {
        let rhs = raw_rhs;
        return parse_named_destructuring(rest, vars, rhs, type_constraint, is_state);
    }
    let decl = SignatureDecl {
        vars,
        is_state,
        is_our,
        type_constraint,
        group_default: None,
        has_nested_group,
        init: Some(SignatureInit {
            is_binding,
            rhs: raw_rhs,
        }),
    };
    let (mut stmts, result) = desugar::expand_with_rhs(&decl);
    // Re-attach a trailing loose word-logical with the assigned list as its
    // left operand, so the block's value is `(<assignment>) and ...`.
    let (rest, result, has_following_block, has_tail) = {
        let (r, _) = ws(rest)?;
        if crate::parser::expr::starts_with_loose_word_logical(r) {
            let (r, tail) = crate::parser::expr::word_logical_tail_pub(r, result)?;
            let (r_ws, _) = ws(r)?;
            let block_follows = r_ws.starts_with('{');
            (
                if block_follows { r } else { r_ws },
                tail,
                block_follows,
                true,
            )
        } else {
            (rest, result, has_following_block, false)
        }
    };
    stmts.push(Stmt::Expr(result));
    // The source-form record describes the declaration alone, so a block whose
    // value a trailing word-logical has rewritten carries none.
    if !has_tail {
        stmts.insert(0, desugar::source_form(decl));
    }
    let block = Stmt::SyntheticBlock(stmts);
    if has_following_block {
        // In `if my ($a, $b) = f() { ... }`, the braced block belongs to the
        // surrounding conditional, not to this declaration's modifier parser.
        Ok((rest, block))
    } else {
        parse_statement_modifier(rest, block)
    }
}

/// Return the default expression for a native type, or Nil for non-native types.
fn native_type_default(tc: &Option<String>) -> Expr {
    match tc.as_deref() {
        Some(t) if crate::native_types::is_native_int_type(t) => Expr::Literal(Value::int(0)),
        Some("num" | "num32" | "num64") => Expr::Literal(Value::num(0.0)),
        Some("str") => Expr::Literal(Value::str(String::new())),
        _ => Expr::Literal(Value::NIL),
    }
}
