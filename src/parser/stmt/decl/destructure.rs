use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char, parse_char};
use super::super::{ident, keyword, var_name};
use super::helpers::register_term_symbol_from_decl_name;
use super::parse_decl_type_constraint;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::parse_comma_or_expr;

/// Metadata for each variable in a destructuring declaration.
struct DestructureVar {
    /// Full variable name including sigil prefix for @/% (e.g. "@y", "x", "%h")
    name: String,
    /// Whether this is a slurpy parameter (*@rest)
    is_slurpy: bool,
    /// Whether this is an optional parameter ($x?)
    #[allow(dead_code)]
    is_optional: bool,
    /// Whether this is a named parameter (:@even)
    is_named: bool,
    /// Per-variable default value (e.g. `$x = 5` inside grouped declaration)
    default: Option<Expr>,
    /// Type constraint for this particular variable (e.g. `Foo $d`)
    per_var_type_constraint: Option<String>,
    /// Where constraint (e.g. `$a where 2`)
    where_constraint: Option<Expr>,
    /// Whether this is a sigilless variable (\c)
    sigilless: bool,
    /// Literal match value (e.g. `"foo"`)
    literal_value: Option<Expr>,
}

/// Recursively collect the (flattened) sigilless/sigilled targets of a nested
/// destructure group `(\e, (\f, \g), $h)`, appending one `DestructureVar` per
/// leaf to `vars`. Returns the remaining input past the closing `)`.
fn collect_nested_group_vars<'a>(
    input: &'a str,
    vars: &mut Vec<DestructureVar>,
) -> Result<&'a str, PError> {
    let (mut r, _) = parse_char(input, '(')?;
    let (r2, _) = ws(r)?;
    r = r2;
    loop {
        if r.starts_with(')') {
            break;
        }
        if r.starts_with('(') {
            r = collect_nested_group_vars(r, vars)?;
        } else if let Some(after_backslash) = r.strip_prefix('\\') {
            let (r2, name) = ident(after_backslash)?;
            register_term_symbol_from_decl_name(&name);
            vars.push(DestructureVar {
                name,
                is_slurpy: false,
                is_optional: false,
                is_named: false,
                default: None,
                per_var_type_constraint: None,
                where_constraint: None,
                sigilless: true,
                literal_value: None,
            });
            r = r2;
        } else {
            let sigil = r.as_bytes().first().copied().unwrap_or(0);
            if sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&' {
                let prefix = match sigil {
                    b'@' => "@",
                    b'%' => "%",
                    b'&' => "&",
                    _ => "",
                };
                let (r2, n) = var_name(r)?;
                vars.push(DestructureVar {
                    name: format!("{}{}", prefix, n),
                    is_slurpy: false,
                    is_optional: false,
                    is_named: false,
                    default: None,
                    per_var_type_constraint: None,
                    where_constraint: None,
                    sigilless: false,
                    literal_value: None,
                });
                r = r2;
            } else {
                return Err(PError::expected(
                    "variable sigil ($, @, %, &) or sigilless (\\name) in nested destructure group",
                ));
            }
        }
        let (r2, _) = ws(r)?;
        r = r2;
        if r.starts_with(',') {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            r = r2;
        }
    }
    let (r, _) = parse_char(r, ')')?;
    Ok(r)
}

pub(in crate::parser::stmt) fn parse_destructuring_decl(
    input: &str,
    is_state: bool,
    is_our: bool,
    type_constraint: Option<String>,
) -> PResult<'_, Stmt> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    let mut vars: Vec<DestructureVar> = Vec::new();
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
                        crate::value::Value::Package(crate::symbol::Symbol::intern(outer)),
                    );
                    attrs.insert(
                        "inner".to_string(),
                        crate::value::Value::Package(crate::symbol::Symbol::intern(&tc)),
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
            let (r2, n) = var_name(r)?;
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
    let mut stmts = Vec::new();
    for dvar in &vars {
        let effective_tc = dvar
            .per_var_type_constraint
            .clone()
            .or_else(|| type_constraint.clone());
        let expr = if let Some(ref def_expr) = group_default_expr {
            dvar.default.clone().unwrap_or_else(|| def_expr.clone())
        } else if let Some(ref default) = dvar.default {
            default.clone()
        } else if dvar.name.starts_with('@') {
            Expr::Literal(Value::real_array(Vec::new()))
        } else if dvar.name.starts_with('%') {
            Expr::Hash(Vec::new())
        } else {
            native_type_default(&effective_tc)
        };
        let traits = if let Some(ref def_expr) = group_default_expr {
            vec![("default".to_string(), Some(def_expr.clone()))]
        } else {
            Vec::new()
        };
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr,
            type_constraint: effective_tc,
            is_state,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: traits,
            where_constraint: None,
        });
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Parse the RHS of a destructuring declaration with assignment or binding.
fn parse_destructuring_with_rhs(
    input: &str,
    vars: Vec<DestructureVar>,
    is_state: bool,
    is_our: bool,
    is_binding: bool,
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
    let (rest, raw_rhs) = parse_comma_or_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');

    let has_named = vars.iter().any(|v| v.is_named);

    let rhs = if is_binding && !has_named {
        Expr::MethodCall {
            target: Box::new(raw_rhs),
            name: Symbol::intern("list"),
            args: vec![],
            modifier: None,
            quoted: false,
        }
    } else {
        raw_rhs
    };

    if has_named {
        return parse_named_destructuring(rest, vars, rhs, type_constraint, is_state);
    }

    // Positional destructuring
    let tmp_name = "@__destructure_tmp__".to_string();
    let array_bare = "__destructure_tmp__".to_string();
    let mut stmts = vec![Stmt::VarDecl {
        name: tmp_name,
        expr: rhs,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    }];
    // In Raku positional list assignment, the FIRST `@`/`%` target is greedy:
    // it slurps all remaining RHS values, and every target after it receives
    // nothing (`my ($a, @b, $c) = 1..4` → `@b` is `[2,3,4]`, `$c` is `Any`).
    // `seen_slurpy` tracks whether such a greedy slurp has already consumed the
    // tail so subsequent targets are filled with empties instead of mis-indexed
    // leftover values.
    let mut seen_slurpy = false;
    for (i, dvar) in vars.iter().enumerate() {
        if dvar.literal_value.is_some() {
            continue;
        }

        let is_array = dvar.name.starts_with('@');
        let is_hash = dvar.name.starts_with('%');
        // A plain (non-`*`) array/hash in the target list is implicitly greedy,
        // regardless of what follows it.
        let is_implicit_slurpy = !seen_slurpy && (is_array || is_hash);

        let effective_tc = dvar
            .per_var_type_constraint
            .clone()
            .or_else(|| type_constraint.clone());
        let expr = if seen_slurpy {
            // A target after a greedy slurp gets an empty container / Nil.
            if is_array {
                Expr::ArrayLiteral(Vec::new())
            } else if is_hash {
                Expr::Hash(Vec::new())
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if dvar.is_slurpy || is_implicit_slurpy {
            seen_slurpy = true;
            Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::Int(i as i64))),
                    op: TokenKind::DotDot,
                    right: Box::new(Expr::Whatever),
                }),
                is_positional: true,
            }
        } else {
            let read = Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Literal(Value::Int(i as i64))),
                is_positional: true,
            };
            // A *typed* element whose RHS ran out of values gets the type's
            // DEFAULT, not the `Any` an out-of-range Array read now yields
            // (`my Str ($a) = ()` → `$a` is `Str`, not the un-assignable `Any`).
            // Untyped vars keep the raw `Any`. The `// default` fallback fires
            // only for an undefined (missing) read, so present values pass through.
            if effective_tc.is_some() {
                Expr::Binary {
                    left: Box::new(read),
                    op: TokenKind::SlashSlash,
                    right: Box::new(native_type_default(&effective_tc)),
                }
            } else {
                read
            }
        };
        let effective_where = dvar.where_constraint.clone().map(Box::new);
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr,
            type_constraint: effective_tc,
            is_state,
            is_our,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: effective_where,
        });
        if dvar.sigilless {
            stmts.push(Stmt::MarkSigillessReadonly(dvar.name.clone()));
        }
        if is_binding && dvar.name.starts_with(|c: char| c != '@' && c != '%') {
            stmts.push(Stmt::MarkReadonly(dvar.name.clone()));
        }
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Return the default expression for a native type, or Nil for non-native types.
fn native_type_default(tc: &Option<String>) -> Expr {
    match tc.as_deref() {
        Some(
            "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16" | "uint32"
            | "uint64",
        ) => Expr::Literal(Value::Int(0)),
        Some("num" | "num32" | "num64") => Expr::Literal(Value::Num(0.0)),
        Some("str") => Expr::Literal(Value::str(String::new())),
        _ => Expr::Literal(Value::Nil),
    }
}

/// Parse named destructuring: bind from a hash.
fn parse_named_destructuring(
    rest: &str,
    vars: Vec<DestructureVar>,
    rhs: Expr,
    type_constraint: Option<String>,
    is_state: bool,
) -> PResult<'_, Stmt> {
    let tmp_name = "%__destructure_tmp__".to_string();
    let hash_bare = "__destructure_tmp__".to_string();
    let mut stmts = vec![Stmt::VarDecl {
        name: tmp_name,
        expr: rhs,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    }];
    for dvar in &vars {
        let bare_name = if dvar.name.starts_with('@')
            || dvar.name.starts_with('%')
            || dvar.name.starts_with('&')
        {
            &dvar.name[1..]
        } else {
            &dvar.name
        };
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr: Expr::Index {
                target: Box::new(Expr::HashVar(hash_bare.clone())),
                index: Box::new(Expr::Literal(Value::str(bare_name.to_string()))),
                is_positional: false,
            },
            type_constraint: type_constraint.clone(),
            is_state,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        });
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}
