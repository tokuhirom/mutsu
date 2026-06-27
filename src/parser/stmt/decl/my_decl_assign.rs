use super::super::super::expr::expression;
use super::super::super::helpers::ws;
use super::super::super::parse_result::{
    PError, PResult, merge_expected_messages, opt_char, parse_char,
};
use super::super::keyword;
use super::helpers::shaped_array_new_with_data_expr;
use super::my_decl::MyDeclState;
use super::{
    consume_scalar_decl_trailing_comma, default_decl_expr, method_decl_body,
    parse_assign_expr_or_comma, parse_colon_method_arg, parse_comma_chained_decls,
    parse_statement_modifier, rewrite_decl_assignment_or_chain, wrap_with_will_leave,
};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::super::helpers::ws1;

/// Handle the assignment/binding/default portion of a `my`/`our`/`state`
/// variable declaration, after the variable name, type constraint, and
/// traits have been parsed.
pub(super) fn my_decl_assign_or_default(input: &str, s: MyDeclState) -> PResult<'_, Stmt> {
    let rest = input;

    // Feed initialization: my @a <== expr / my @a <<== expr
    if let Some(rest) = rest
        .strip_prefix("<==")
        .or_else(|| rest.strip_prefix("<<=="))
    {
        let (rest, _) = ws(rest)?;
        let (rest, mut expr) = parse_assign_expr_or_comma(rest)?;
        if s.is_array {
            expr = Expr::Call {
                name: Symbol::intern("__mutsu_feed_array_assign"),
                args: vec![expr],
            };
        }
        let expr = if let Some(dims) = s.shape_dims {
            shaped_array_new_with_data_expr(dims, expr)
        } else {
            expr
        };
        let stmt = Stmt::VarDecl {
            name: s.name,
            expr,
            type_constraint: s.type_constraint,
            is_state: s.is_state,
            is_our: s.is_our,
            is_dynamic: s.has_dynamic_trait,
            is_export: s.has_export_trait,
            export_tags: s.export_tags,
            custom_traits: s.custom_traits,
            where_constraint: s.where_constraint,
        };
        if s.apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }

    // Compound assignment
    if let Some((stripped, op)) = super::super::assign::parse_compound_assign_op(rest) {
        return handle_compound_assign(stripped, s, |var_expr, rhs| {
            super::super::assign::compound_assigned_value_expr(var_expr, op, rhs)
        });
    }
    if let Some((stripped, op_name)) = super::super::assign::parse_custom_compound_assign_op(rest) {
        return handle_compound_assign(stripped, s, |var_expr, rhs| Expr::InfixFunc {
            name: op_name,
            left: Box::new(var_expr),
            right: vec![rhs],
            modifier: None,
        });
    }

    // Assignment
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        return handle_simple_assign(&rest[1..], s);
    }

    // Method-call-assign .= in declaration: my Type $var .= method(args)
    if let Some(stripped) = rest.strip_prefix(".=") {
        return handle_method_call_assign(stripped, s);
    }

    // Binding := or ::=
    if let Some(stripped) = rest.strip_prefix("::=").or_else(|| rest.strip_prefix(":=")) {
        return handle_binding(stripped, s);
    }

    // No assignment — default value. Optionally consume a trailing `;` only in
    // statement context. In EXPRESSION context (e.g. a feed sink
    // `(1,2,3) ==> my @o; say @o`) the `;` terminates the *enclosing* statement
    // and must be left for it; eating it here swallows the following statement
    // into the expression (`say` then parses as an infix word on the feed).
    let rest = if s.apply_modifier {
        opt_char(rest, ';').0
    } else {
        rest
    };
    let expr = default_decl_expr(
        s.is_array,
        s.is_hash,
        s.shape_dims.as_deref(),
        s.type_constraint.as_deref(),
    );
    let stmt = Stmt::VarDecl {
        name: s.name.clone(),
        expr,
        type_constraint: s.type_constraint,
        is_state: s.is_state,
        is_our: s.is_our,
        is_dynamic: s.has_dynamic_trait,
        is_export: s.has_export_trait,
        export_tags: s.export_tags,
        custom_traits: s.custom_traits,
        where_constraint: s.where_constraint.clone(),
    };
    let stmt = wrap_with_will_leave(stmt, &s.name, s.will_phasers);
    Ok((rest, stmt))
}

/// Handle compound assignment operators (e.g. `my $x += expr`).
fn handle_compound_assign<'a, F>(
    stripped: &'a str,
    s: MyDeclState,
    make_expr: F,
) -> PResult<'a, Stmt>
where
    F: FnOnce(Expr, Expr) -> Expr,
{
    let (rest, _) = ws(stripped)?;
    let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
        messages: merge_expected_messages(
            "expected right-hand expression after compound assignment",
            &err.messages,
        ),
        remaining_len: err.remaining_len.or(Some(stripped.len())),
        exception: None,
    })?;
    let decl_stmt = Stmt::VarDecl {
        name: s.name.clone(),
        expr: default_decl_expr(
            s.is_array,
            s.is_hash,
            s.shape_dims.as_deref(),
            s.type_constraint.as_deref(),
        ),
        type_constraint: s.type_constraint.clone(),
        is_state: s.is_state,
        is_our: s.is_our,
        is_dynamic: s.has_dynamic_trait,
        is_export: s.has_export_trait,
        export_tags: s.export_tags.clone(),
        custom_traits: s.custom_traits.clone(),
        where_constraint: s.where_constraint.clone(),
    };
    let assign_stmt = Stmt::Assign {
        name: s.name.clone(),
        expr: make_expr(Expr::Var(s.name.clone()), rhs),
        op: AssignOp::Assign,
    };
    let stmt = Stmt::SyntheticBlock(vec![decl_stmt, assign_stmt]);
    if s.apply_modifier {
        return parse_statement_modifier(rest, stmt);
    }
    Ok((rest, stmt))
}

/// Handle simple assignment (`=`).
fn handle_simple_assign(input: &str, s: MyDeclState) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;
    // class-scope routine aliasing form:
    //   our &name = method name(...) { ... }
    if s.is_our && s.is_code {
        if let Some(r) = keyword("method", rest) {
            let (r, _) = ws1(r)?;
            let (r, mut stmt) = method_decl_body(r, false, true)?;
            // Mark as `our &name = method` form so the method stays in the
            // class method table (unlike `our method name()` which does not).
            if let Stmt::MethodDecl {
                ref mut our_variable_form,
                ..
            } = stmt
            {
                *our_variable_form = true;
            }
            if s.apply_modifier {
                return parse_statement_modifier(r, stmt);
            }
            return Ok((r, stmt));
        }
        if let Some(r) = keyword("multi", rest) {
            let (r, _) = ws1(r)?;
            if let Some(r) = keyword("method", r) {
                let (r, _) = ws1(r)?;
                let (r, mut stmt) = method_decl_body(r, true, true)?;
                if let Stmt::MethodDecl {
                    ref mut our_variable_form,
                    ..
                } = stmt
                {
                    *our_variable_form = true;
                }
                if s.apply_modifier {
                    return parse_statement_modifier(r, stmt);
                }
                return Ok((r, stmt));
            }
        }
    }
    // Save flags before `s` is partially moved.
    let is_scalar = !s.is_array && !s.is_hash;
    let var_name_for_leave = s.name.clone();
    // Scalar declarations stop at comma; array/hash consume the full comma list.
    let (rest, expr) = if s.is_array || s.is_hash {
        parse_assign_expr_or_comma(rest)?
    } else {
        expression(rest)?
    };
    // In Raku, andthen/orelse/notandthen have lower precedence than declaration
    // assignment. If the expression is `expr andthen { block }`, split it so
    // the declaration assigns `expr` and the andthen runs as a side effect.
    let (expr, post_andthen) = match expr {
        Expr::Binary {
            left,
            op:
                op @ (crate::token_kind::TokenKind::AndThen
                | crate::token_kind::TokenKind::OrElse
                | crate::token_kind::TokenKind::NotAndThen),
            right,
        } => (*left, Some((op, *right))),
        other => (other, None),
    };
    let expr = match expr {
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } if is_scalar && matches!(meta.as_str(), "X" | "Z") => {
            let stmt = Stmt::Expr(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::DoStmt(Box::new(Stmt::VarDecl {
                    name: s.name.clone(),
                    expr: *left,
                    type_constraint: s.type_constraint.clone(),
                    is_state: s.is_state,
                    is_our: s.is_our,
                    is_dynamic: s.has_dynamic_trait,
                    is_export: s.has_export_trait,
                    export_tags: s.export_tags.clone(),
                    custom_traits: {
                        let mut traits = s.custom_traits.clone();
                        if !traits.iter().any(|(name, _)| name == "__has_initializer") {
                            traits.push(("__has_initializer".to_string(), None));
                        }
                        traits
                    },
                    where_constraint: s.where_constraint.clone(),
                }))),
                right,
            });
            let stmt = wrap_with_will_leave(stmt, &var_name_for_leave, s.will_phasers);
            let (rest, stmt) = parse_comma_chained_decls(rest, stmt)?;
            let (rest, stmt) = if s.apply_modifier {
                consume_scalar_decl_trailing_comma(rest, stmt)?
            } else {
                (rest, stmt)
            };
            if s.apply_modifier {
                return parse_statement_modifier(rest, stmt);
            }
            return Ok((rest, stmt));
        }
        other => other,
    };
    // Feed split: `my @a = SOURCE ==> SINK` — the feed operator is at Sequencer
    // precedence (looser than the declaration's `=`), so it parses as
    // `(my @a = SOURCE) ==> SINK`. Relocate the declaration to wrap the feed's
    // textually-left operand, then run the whole feed as the statement value.
    if matches!(expr, Expr::Feed { .. }) {
        let mut custom_traits = s.custom_traits.clone();
        if !custom_traits
            .iter()
            .any(|(name, _)| name == "__has_initializer")
        {
            custom_traits.push(("__has_initializer".to_string(), None));
        }
        let mut feed = expr;
        {
            let slot = crate::parser::feed_leftmost_operand_mut(&mut feed);
            let mut source = std::mem::replace(slot, Expr::Literal(crate::value::Value::Nil));
            if let Some(dims) = s.shape_dims.clone() {
                source = shaped_array_new_with_data_expr(dims, source);
            }
            *slot = Expr::DoStmt(Box::new(Stmt::VarDecl {
                name: s.name.clone(),
                expr: source,
                type_constraint: s.type_constraint.clone(),
                is_state: s.is_state,
                is_our: s.is_our,
                is_dynamic: s.has_dynamic_trait,
                is_export: s.has_export_trait,
                export_tags: s.export_tags.clone(),
                custom_traits,
                where_constraint: s.where_constraint.clone(),
            }));
        }
        let stmt = wrap_with_will_leave(
            Stmt::Expr(feed),
            &var_name_for_leave,
            s.will_phasers.clone(),
        );
        if s.apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }
    let expr = if let Some(dims) = s.shape_dims {
        shaped_array_new_with_data_expr(dims, expr)
    } else {
        expr
    };
    let mut custom_traits = s.custom_traits.clone();
    if !custom_traits
        .iter()
        .any(|(name, _)| name == "__has_initializer")
    {
        custom_traits.push(("__has_initializer".to_string(), None));
    }
    let base_stmt = Stmt::VarDecl {
        name: s.name.clone(),
        expr: expr.clone(),
        type_constraint: s.type_constraint.clone(),
        is_state: s.is_state,
        is_our: s.is_our,
        is_dynamic: s.has_dynamic_trait,
        is_export: s.has_export_trait,
        export_tags: s.export_tags.clone(),
        custom_traits,
        where_constraint: s.where_constraint.clone(),
    };
    if let Some(stmt) = rewrite_decl_assignment_or_chain(expr.clone(), base_stmt) {
        let stmt = wrap_with_will_leave(stmt, &var_name_for_leave, s.will_phasers.clone());
        let (rest, stmt) = parse_comma_chained_decls(rest, stmt)?;
        // For scalar declarations, consume trailing sink expressions (e.g. `my $c = 1, 2, 3;`)
        // Only in statement context (apply_modifier=true). In expression context (e.g.,
        // `(my $x=42,1)`) the comma should be left for the enclosing expression to form a list.
        let (rest, stmt) = if is_scalar && s.apply_modifier {
            consume_scalar_decl_trailing_comma(rest, stmt)?
        } else {
            (rest, stmt)
        };
        let stmt = if let Some((op, rhs)) = post_andthen.clone() {
            let var_ref = Expr::Var(s.name.clone());
            let andthen_expr = Expr::Binary {
                left: Box::new(var_ref),
                op,
                right: Box::new(rhs),
            };
            Stmt::SyntheticBlock(vec![stmt, Stmt::Expr(andthen_expr)])
        } else {
            stmt
        };
        if s.apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }
    let mut custom_traits = s.custom_traits.clone();
    if !custom_traits
        .iter()
        .any(|(name, _)| name == "__has_initializer")
    {
        custom_traits.push(("__has_initializer".to_string(), None));
    }
    let stmt = Stmt::VarDecl {
        name: s.name.clone(),
        expr,
        type_constraint: s.type_constraint,
        is_state: s.is_state,
        is_our: s.is_our,
        is_dynamic: s.has_dynamic_trait,
        is_export: s.has_export_trait,
        export_tags: s.export_tags.clone(),
        custom_traits,
        where_constraint: s.where_constraint.clone(),
    };
    let stmt = wrap_with_will_leave(stmt, &var_name_for_leave, s.will_phasers.clone());
    let (rest, stmt) = parse_comma_chained_decls(rest, stmt)?;
    // For scalar declarations, consume trailing sink expressions (e.g. `my $c = 1, 2, 3;`)
    // Only in statement context (apply_modifier=true). In expression context (e.g.,
    // `(my $x=42,1)`) the comma should be left for the enclosing expression to form a list.
    let (rest, stmt) = if is_scalar && s.apply_modifier {
        consume_scalar_decl_trailing_comma(rest, stmt)?
    } else {
        (rest, stmt)
    };
    let stmt = if let Some((op, rhs)) = post_andthen {
        let var_ref = Expr::Var(s.name.clone());
        let andthen_expr = Expr::Binary {
            left: Box::new(var_ref),
            op,
            right: Box::new(rhs),
        };
        Stmt::SyntheticBlock(vec![stmt, Stmt::Expr(andthen_expr)])
    } else {
        stmt
    };
    if s.apply_modifier {
        return parse_statement_modifier(rest, stmt);
    }
    Ok((rest, stmt))
}

/// Handle method-call-assign `.=` in declaration: `my Type $var .= method(args)`
fn handle_method_call_assign(input: &str, s: MyDeclState) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;
    // Parse method name
    let (rest, method_name) =
        super::take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let method_name = method_name.to_string();
    // Strip whitespace before checking for args
    let (rest_ws, _) = ws(rest)?;
    // Parse optional args (parenthesized, colon-form, or fake-infix adverbs)
    let (rest, args) = if rest_ws.starts_with('(') {
        let (r, _) = parse_char(rest_ws, '(')?;
        let (r, _) = ws(r)?;
        let (r, args) = super::super::super::primary::parse_call_arg_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        (r, args)
    } else if rest.starts_with(':') && !rest.starts_with("::") {
        // Colon invocant form (no space before ':'): .=method: arg
        parse_colon_args(rest)?
    } else if rest_ws.starts_with(':') && !rest_ws.starts_with("::") {
        // Fake-infix adverb form (space before ':'): .=method :key<val> :key2<val2>
        parse_fake_infix_adverbs(rest_ws)?
    } else {
        (rest_ws, Vec::new())
    };
    // Build: Type.method(args)
    // For typed `@`/`%` declarations, the `.=` target is the *container* type
    // parameterized by the element constraint, e.g. `my Int @x .= new` calls
    // `Array[Int].new`, and `my Array of Bool @x .= new` calls
    // `Array[Array[Bool]].new` — not `Int.new` / `Array[Bool].new`.
    let target_name = match &s.type_constraint {
        Some(c) if s.name.starts_with('@') => {
            if crate::runtime::native_types::is_native_array_element_type(c) {
                format!("array[{c}]")
            } else {
                format!("Array[{c}]")
            }
        }
        Some(c) if s.name.starts_with('%') => format!("Hash[{c}]"),
        Some(c) => c.clone(),
        None => s.name.clone(),
    };
    let expr = Expr::MethodCall {
        target: Box::new(Expr::BareWord(target_name)),
        name: Symbol::intern(&method_name),
        args,
        modifier: None,
        quoted: false,
    };
    let stmt = Stmt::VarDecl {
        name: s.name,
        expr,
        type_constraint: s.type_constraint,
        is_state: s.is_state,
        is_our: s.is_our,
        is_dynamic: s.has_dynamic_trait,
        is_export: s.has_export_trait,
        export_tags: s.export_tags.clone(),
        custom_traits: s.custom_traits.clone(),
        where_constraint: s.where_constraint.clone(),
    };
    // Handle trailing comma list
    let (rest, _) = ws(rest)?;
    if rest.starts_with(',') && !rest.starts_with(",,") {
        let (r, _) = parse_char(rest, ',')?;
        let (r, _) = ws(r)?;
        let mut sink_stmts = vec![stmt];
        if !r.starts_with(';') && !r.is_empty() && !r.starts_with('}') {
            let (mut r_inner, first_sink) = expression(r)?;
            sink_stmts.push(Stmt::Expr(first_sink));
            loop {
                let (r2, _) = ws(r_inner)?;
                if !r2.starts_with(',') || r2.starts_with(",,") {
                    r_inner = r2;
                    break;
                }
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                    r_inner = r2;
                    break;
                }
                let (r2, next_sink) = expression(r2)?;
                sink_stmts.push(Stmt::Expr(next_sink));
                r_inner = r2;
            }
            let combined = Stmt::SyntheticBlock(sink_stmts);
            if s.apply_modifier {
                return parse_statement_modifier(r_inner, combined);
            }
            return Ok((r_inner, combined));
        }
        let combined = Stmt::SyntheticBlock(sink_stmts);
        if s.apply_modifier {
            return parse_statement_modifier(r, combined);
        }
        return Ok((r, combined));
    }
    if s.apply_modifier {
        return parse_statement_modifier(rest, stmt);
    }
    Ok((rest, stmt))
}

/// Parse colon-style method arguments.
pub(super) fn parse_colon_args(input: &str) -> PResult<'_, Vec<Expr>> {
    let r = &input[1..];
    let (r, _) = ws(r)?;
    let (r, first_arg) = parse_colon_method_arg(r)?;
    let mut args = vec![first_arg];
    let mut r_inner = r;
    loop {
        let (r2, _) = ws(r_inner)?;
        // Adjacent colonpairs without comma
        if r2.starts_with(':')
            && !r2.starts_with("::")
            && let Ok((r3, arg)) = crate::parser::primary::misc::colonpair_expr(r2)
        {
            args.push(arg);
            r_inner = r3;
            continue;
        }
        if !r2.starts_with(',') {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        // Handle trailing comma before ';' or '}'
        if r2.starts_with(';') || r2.starts_with('}') || r2.is_empty() {
            r_inner = r2;
            break;
        }
        let (r2, next) = parse_colon_method_arg(r2)?;
        args.push(next);
        r_inner = r2;
    }
    Ok((r_inner, args))
}

/// Parse fake-infix adverb arguments: `:key<val> :key2<val2>`.
pub(super) fn parse_fake_infix_adverbs(input: &str) -> PResult<'_, Vec<Expr>> {
    let mut args = Vec::new();
    let mut r = input;
    while r.starts_with(':') && !r.starts_with("::") {
        if let Ok((r2, arg)) = crate::parser::primary::misc::colonpair_expr(r) {
            args.push(arg);
            let (r3, _) = ws(r2)?;
            r = r3;
        } else {
            break;
        }
    }
    Ok((r, args))
}

/// Handle binding `:=` or `::=`.
fn handle_binding(input: &str, s: MyDeclState) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;
    let (mut rest, mut expr) = parse_assign_expr_or_comma(rest)?;
    let (tail, _) = ws(rest)?;
    if let Expr::BareWord(name) = &expr
        && !tail.is_empty()
        && !tail.starts_with(';')
        && !tail.starts_with('}')
        && let Ok((r_after, arg_expr)) = parse_assign_expr_or_comma(tail)
    {
        expr = Expr::Call {
            name: Symbol::intern(name),
            args: vec![arg_expr],
        };
        rest = r_after;
    }
    let bound_name = s.name.clone();
    // `my %h := %$m` (Slice 2c, hash form): the parser desugars `%$m` (hash-context
    // deref of a scalar `$m` holding a hash by reference) into a `.hash` MethodCall,
    // which materializes a *copy* and so loses the binding's sharing semantics. When
    // the bind target is a hash and the source is a plain `%$var` deref, rewrite it
    // to `HashVar(var)` so it takes the same `:=` cell-sharing path the array form
    // (`@a := @$n`, parsed as `ArrayVar`) already uses (mirrors #3268). Only the
    // plain-scalar case is rewritten; `%$/`, `%$_`, etc. keep their `.hash`
    // coercion since they are not value-aliases of a user container.
    if bound_name.starts_with('%')
        && let Expr::MethodCall {
            target,
            name,
            args,
            modifier: None,
            ..
        } = &expr
        && name.resolve() == "hash"
        && args.is_empty()
        && let Expr::Var(var_name) = target.as_ref()
    {
        expr = Expr::HashVar(var_name.clone());
    }
    let mark_scalar_readonly =
        !s.is_array && !bound_name.starts_with('%') && super::scalar_binding_rhs_is_readonly(&expr);
    let bind_to_var = matches!(expr, Expr::Var(_));
    let bind_to_index = matches!(expr, Expr::Index { .. });
    // A `$` scalar bound (`:=`) to a value is NOT a Scalar container, so
    // `@a = $bound` must flatten a Positional value rather than itemize it.
    // Mark the VarDecl with an internal trait the compiler reads to emit
    // MarkScalarBindContext (instead of wrapping in a SyntheticBlock, which
    // would change the value when the bind is used as an expression).
    let is_scalar_bind =
        !s.is_array && !bound_name.starts_with('%') && !bound_name.starts_with('&');
    // A natively-typed variable (`my int $x`, `my num $n`, `my str $s`) is not a
    // container, so it cannot be bound with `:=` — Raku raises X::Bind::NativeType.
    if is_scalar_bind
        && let Some(tc) = s.type_constraint.as_deref()
        && crate::runtime::native_types::is_native_array_element_type(tc)
    {
        let display = format!("${}", bound_name);
        let msg = format!(
            "Cannot bind to natively typed variable '{}'; use assignment instead",
            display
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::str(display));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Bind::NativeType"), attrs);
        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
    }
    let mut custom_traits = s.custom_traits.clone();
    if is_scalar_bind {
        custom_traits.push(("__scalar_bind".to_string(), None));
    }
    let stmt = Stmt::VarDecl {
        name: s.name,
        expr,
        type_constraint: s.type_constraint,
        is_state: s.is_state,
        is_our: s.is_our,
        is_dynamic: s.has_dynamic_trait,
        is_export: s.has_export_trait,
        export_tags: s.export_tags.clone(),
        custom_traits,
        where_constraint: s.where_constraint.clone(),
    };
    let stmt = if s.is_array || bound_name.starts_with('%') {
        let mut stmts = Vec::new();
        if bound_name.starts_with('%') {
            stmts.push(Stmt::MarkReadonly(bound_name.clone()));
            // Also record a dedicated bound-container marker so a later whole
            // reassignment (`%a = (...)`) is allowed (it propagates to the bound
            // source), while a `constant %M` — also readonly — stays immutable.
            stmts.push(Stmt::MarkBoundContainer(bound_name.clone()));
            stmts.push(Stmt::MarkBind);
        }
        stmts.push(stmt);
        if s.is_array {
            stmts.push(Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_record_bound_array_len"),
                args: vec![Expr::Literal(Value::str(bound_name.clone()))],
            }));
            stmts.push(Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_record_shaped_array_dims"),
                args: vec![Expr::Literal(Value::str(bound_name.clone()))],
            }));
            // Return the bound variable so the expression evaluates to the
            // bound value (important for `+my @a := ...` which expects the
            // list count).
            stmts.push(Stmt::Expr(Expr::Var(bound_name.clone())));
        }
        Stmt::SyntheticBlock(stmts)
    } else if mark_scalar_readonly {
        Stmt::SyntheticBlock(vec![Stmt::MarkReadonly(bound_name), stmt])
    } else if bind_to_var || bind_to_index {
        Stmt::SyntheticBlock(vec![Stmt::MarkBind, stmt])
    } else {
        stmt
    };
    if s.apply_modifier {
        return parse_statement_modifier(rest, stmt);
    }
    Ok((rest, stmt))
}
