use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char, parse_char};
use super::super::parse_statement_modifier;
use super::super::{ident, keyword};
use super::helpers::register_term_symbol_from_decl_name;
use super::my_decl_helpers::build_sigilless_bind_stmt;
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
                let (r2, n) = crate::parser::stmt::lexical_var_name(r)?;
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
    // If the RHS is followed (after whitespace) by a `{` block, that block is a
    // separate statement / conditional body — NOT a hash subscript of the
    // declaration. Preserve the whitespace so the expression-context postfix
    // parser (which only subscripts `{` when it has no leading space) leaves it
    // alone: `if my ($a, $b) = f() { ... }` must treat `{ ... }` as the if-body,
    // matching the scalar `if my $x = f() { ... }` path. Only consume the
    // optional trailing `;` when there is no such block.
    let (rest_ws, _) = ws(rest)?;
    let has_following_block = rest_ws.starts_with('{');
    let rhs_ends_with_block = matches!(raw_rhs, Expr::DoBlock { .. } | Expr::DoStmt(_));
    let block_rhs_ends_at_newline =
        rhs_ends_with_block && rest[..rest.len() - rest_ws.len()].contains('\n');
    let rest = if has_following_block { rest } else { rest_ws };

    let has_named = vars.iter().any(|v| v.is_named);

    // A positional sigilless binding must preserve the containers carried by
    // its RHS elements. Staging `($x, $y)` in an Array and then reading its
    // elements binds `\a`/`\b` to copies, so assignments through those terms
    // cannot reach `$x`/`$y`. Desugar directly representable variable
    // references to the same individual bind declarations that
    // `my \a := $x` already uses.
    // Mixed/computed destructuring stays on the general staging path, which
    // retains its existing readonly, default, slurpy, and constraint handling.
    if is_binding
        && !has_named
        && vars.iter().all(is_direct_sigilless_destructure_var)
        && let Some(sources) = direct_sigilless_bind_sources(&raw_rhs, vars.len())
    {
        return parse_direct_sigilless_binding(
            rest,
            vars,
            sources,
            is_state,
            is_our,
            type_constraint,
            has_following_block || block_rhs_ends_at_newline,
        );
    }

    // List-assignment iterates the RHS with one level of decont (Rakudo
    // List.STORE): `my ($a, $b) = $row` where `$row` holds an itemized Array
    // flattens into its elements, while `= $row,` (a comma list) keeps the
    // itemized value whole. `__mutsu_list_assign_rhs` deitemizes exactly the
    // single-itemized-container shape and passes everything else through —
    // unlike `.list`, it leaves a Failure RHS intact (`my ($x) = @e.shift`
    // on an empty array stores the Failure, it does not throw). Binding
    // (`:=`) keeps its historical `.list` wrap; named destructuring keeps
    // the raw value (it subscripts it).
    let rhs = if is_binding && !has_named {
        Expr::MethodCall {
            target: Box::new(raw_rhs),
            name: Symbol::intern("list"),
            args: vec![],
            modifier: None,
            quoted: false,
        }
    } else if !has_named {
        Expr::Call {
            name: Symbol::intern("__mutsu_list_assign_rhs"),
            args: vec![raw_rhs],
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
    // NOTE: this staging temp is NOT a user `Array` -- it IS the RHS list, and
    // every target below reads a VALUE out of it. ADR-0040 slice 2's
    // element-itemization is therefore deliberately suppressed for it; see
    // `Interpreter::is_destructure_staging_temp`.
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
    // List ASSIGNMENT (`=`) and signature BINDING (`:=`) differ here:
    //  - assignment: the FIRST `@`/`%` target is greedy — it slurps all
    //    remaining RHS values, and every target after it receives an empty
    //    container / Nil (`my ($a, @b, $c) = 1..4` → `@b` = `[2,3,4]`, `$c` = Any).
    //  - binding: a plain `@`/`%` binds ONE positional argument; only an
    //    explicit `*@rest` is slurpy (`my ($x, @y, *@r) := (42,[13,17],5,6,7)`
    //    → `@y` = `[13,17]`, `@r` = `[5,6,7]`).
    // So the greedy behaviour applies only in assignment mode. In binding mode a
    // trailing `@x` is NOT slurpy: `my (@a, @b) := (@x, @y)` binds `@b` to `@y`,
    // not to `(@y,)`. (Rakudo type-checks each element as Positional, so the
    // shapes where the distinction is invisible are the ones it rejects outright.)
    let mut seen_slurpy = false;
    for (i, dvar) in vars.iter().enumerate() {
        if let Some(lit) = &dvar.literal_value {
            // A bare literal element (`my ("foo") = ...`) is a postconstraint: the
            // i-th assigned value must smartmatch the literal, else
            // X::TypeCheck::Assignment. Emit a throwaway declaration whose
            // where-constraint IS the literal (identical to `$ where "foo"`, which
            // already enforces this), reading the i-th temp element. (subtypes.t 90)
            let read = Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Literal(Value::int(i as i64))),
                is_positional: true,
            };
            stmts.push(Stmt::VarDecl {
                name: format!("__destructure_lit_{i}"),
                expr: read,
                type_constraint: None,
                is_state,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: Some(Box::new(lit.clone())),
            });
            continue;
        }

        let is_array = dvar.name.starts_with('@');
        let is_hash = dvar.name.starts_with('%');
        let is_implicit_slurpy = !is_binding && !seen_slurpy && (is_array || is_hash);

        let effective_tc = dvar
            .per_var_type_constraint
            .clone()
            .or_else(|| type_constraint.clone());
        let expr = if !is_binding && seen_slurpy {
            // A target after a greedy slurp (assignment mode) gets an empty
            // container / Nil.
            if is_array {
                Expr::ArrayLiteral(Vec::new())
            } else if is_hash {
                Expr::Hash(Vec::new())
            } else {
                Expr::Literal(Value::NIL)
            }
        } else if dvar.is_slurpy || is_implicit_slurpy {
            seen_slurpy = true;
            Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::int(i as i64))),
                    op: TokenKind::DotDot,
                    right: Box::new(Expr::Whatever),
                }),
                is_positional: true,
            }
        } else {
            let read = Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Literal(Value::int(i as i64))),
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
        let decl = Stmt::VarDecl {
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
        };
        // In BINDING mode a non-slurpy `@`/`%` target BINDS the staged element
        // rather than assigning it: `my @x = 1, 2; my (@a,) := (@x,);
        // @a.push(3)` writes through to `@x` in raku, so `@a` must be the
        // element itself and not a copy. `MarkBind` is the same marker the
        // plain `my @a := expr` declaration uses.
        //
        // A slurpy `*@rest` is excluded: its read is a SLICE of the staging
        // temp (a freshly built `List`), and raku gives `@rest` an `Array`
        // there (`my ($x, @y, *@rest) := (42, [13,17], 5, 6, 7)` leaves
        // `@rest.raku` as `[5, 6, 7]`), which is what the assigning form's
        // `coerce_to_array` produces.
        // Pinned by `t/list-bind-trailing-array.t` and
        // `roast/S02-names-vars/signature.t`.
        let decl = if is_binding
            && !dvar.is_slurpy
            && !is_implicit_slurpy
            && dvar.name.starts_with(['@', '%'])
        {
            Stmt::SyntheticBlock(vec![Stmt::MarkBind, decl])
        } else {
            decl
        };
        stmts.push(decl);
        if dvar.sigilless {
            stmts.push(Stmt::MarkSigillessReadonly(dvar.name.clone()));
        }
        if is_binding && dvar.name.starts_with(|c: char| c != '@' && c != '%') {
            stmts.push(Stmt::MarkReadonly(
                dvar.name.clone(),
                crate::ast::ReadonlyKind::Immutable,
            ));
        }
    }
    // Yield the assigned list as the block's value (`(my ($a,$b) = 1,2)` is `(1 2)`,
    // not the last element). This also keeps the per-element check declarations off
    // the block-final position, so a postconstraint (`where`/literal) on the LAST
    // element still enforces in value context — e.g. an EVAL'd `my (\b, "foo") =
    // ...` whose trailing `MarkSigillessReadonly` would otherwise leave a
    // constrained decl block-final and skip its check. (subtypes.t 90)
    stmts.push(Stmt::Expr(Expr::ArrayVar(array_bare)));
    let block = Stmt::SyntheticBlock(stmts);
    if has_following_block || block_rhs_ends_at_newline {
        // In `if my ($a, $b) = f() { ... }`, the braced block belongs to the
        // surrounding conditional, not to this declaration's modifier parser.
        Ok((rest, block))
    } else {
        parse_statement_modifier(rest, block)
    }
}

/// Whether a destructure leaf can use the plain sigilless declaration path.
/// Defaults, slurpy elements, and constraints need the staged destructuring
/// machinery because they change positional binding semantics.
fn is_direct_sigilless_destructure_var(var: &DestructureVar) -> bool {
    var.sigilless
        && !var.is_slurpy
        && !var.is_optional
        && !var.is_named
        && var.default.is_none()
        && var.where_constraint.is_none()
        && var.literal_value.is_none()
}

/// Extract the individual lvalues represented by a positional RHS. An Array
/// literal is the parser's representation of a parenthesized/comma list;
/// Plain variable references retain their source containers so they can be
/// bound rather than copied through a staging array.
fn direct_sigilless_bind_sources(rhs: &Expr, count: usize) -> Option<Vec<Expr>> {
    if count == 0 {
        return Some(Vec::new());
    }
    match rhs {
        Expr::ArrayLiteral(elements) if elements.len() >= count => {
            let sources = elements[..count].to_vec();
            sources
                .iter()
                .all(is_direct_sigilless_bind_source)
                .then_some(sources)
        }
        Expr::Grouped(inner) => direct_sigilless_bind_sources(inner, count),
        _ => None,
    }
}

fn is_direct_sigilless_bind_source(expr: &Expr) -> bool {
    matches!(expr, Expr::Var(_) | Expr::Literal(_))
}

/// Build a positional sigilless binding without copying its RHS through an
/// intermediate Array. Each helper block is flattened so the compiler sees
/// the same `MarkBind` immediately before each declaration as it does for a
/// standalone `my \\name := $source`.
fn parse_direct_sigilless_binding(
    rest: &str,
    vars: Vec<DestructureVar>,
    sources: Vec<Expr>,
    is_state: bool,
    is_our: bool,
    type_constraint: Option<String>,
    preserve_rest: bool,
) -> PResult<'_, Stmt> {
    let mut stmts = Vec::new();
    for (var, source) in vars.iter().zip(sources) {
        let stmt = build_sigilless_bind_stmt(
            var.name.clone(),
            source,
            var.per_var_type_constraint
                .clone()
                .or_else(|| type_constraint.clone()),
            is_state,
            is_our,
        );
        match stmt {
            Stmt::SyntheticBlock(inner) => stmts.extend(inner),
            other => stmts.push(other),
        }
    }
    // A declaration in expression position yields the bound values as a list,
    // matching the staged destructuring block's trailing ArrayVar expression.
    stmts.push(Stmt::Expr(Expr::ArrayLiteral(
        vars.iter()
            .map(|var| Expr::BareWord(var.name.clone()))
            .collect(),
    )));
    let block = Stmt::SyntheticBlock(stmts);
    if preserve_rest {
        Ok((rest, block))
    } else {
        parse_statement_modifier(rest, block)
    }
}

/// Return the default expression for a native type, or Nil for non-native types.
fn native_type_default(tc: &Option<String>) -> Expr {
    match tc.as_deref() {
        Some(t) if crate::runtime::native_types::is_native_int_type(t) => {
            Expr::Literal(Value::int(0))
        }
        Some("num" | "num32" | "num64") => Expr::Literal(Value::num(0.0)),
        Some("str") => Expr::Literal(Value::str(String::new())),
        _ => Expr::Literal(Value::NIL),
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
        let index_expr = Expr::Index {
            target: Box::new(Expr::HashVar(hash_bare.clone())),
            index: Box::new(Expr::Literal(Value::str(bare_name.to_string()))),
            is_positional: false,
        };
        // A named `@`-sigil destructure target binds (`:=`) the hash value, which
        // spreads an itemized array (e.g. a `.classify` bucket `$[2,4]`) into the
        // array. mutsu's destructure lowers to assignment, so de-itemize the value
        // via `.list` for `@`-targets — a no-op for a plain list, but it unwraps a
        // single itemized array so `my (:@even) := classify(...)` yields `[2,4]`.
        //
        // When the key is ABSENT the named-array bind must yield an empty array
        // (Rakudo parameter-binding semantics), not `[Any]`: a bare `%h<absent>`
        // is `Any`, and `Any.list` is `(Any,)`, so guard with `:exists` and fall
        // back to an empty list. `my (:@paths, :@uris) := <a>.classify(...)` must
        // leave the unmatched `@uris` empty, or a downstream `%(... )` init sees a
        // stray `(Any,)` and dies with X::Hash::Store::OddNumber.
        let value_expr = if dvar.name.starts_with('@') {
            Expr::Ternary {
                cond: Box::new(Expr::Exists {
                    target: Box::new(index_expr.clone()),
                    negated: false,
                    delete: false,
                    arg: None,
                    adverb: crate::ast::ExistsAdverb::None,
                }),
                then_expr: Box::new(Expr::MethodCall {
                    target: Box::new(index_expr),
                    name: crate::symbol::Symbol::intern("list"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                }),
                else_expr: Box::new(Expr::ArrayLiteral(Vec::new())),
            }
        } else {
            index_expr
        };
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr: value_expr,
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
