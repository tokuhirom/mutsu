use super::*;

pub(in crate::parser) fn assign_stmt(input: &str) -> PResult<'_, Stmt> {
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    let is_sigiled = sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&';

    // Try bare identifier assignment for sigilless variables: a = expr
    if !is_sigiled {
        if let Ok((after_ident, bare_name)) = ident(input) {
            if bare_name == "qx" || bare_name == "qqx" {
                return Err(PError::expected("assignment"));
            }
            let (after_ws, _) = ws(after_ident)?;
            if (after_ws.starts_with('=') && !after_ws.starts_with("=="))
                || after_ws.starts_with("⚛=")
            {
                let is_atomic = after_ws.starts_with("⚛=");
                let rest = if is_atomic {
                    &after_ws["⚛=".len()..]
                } else {
                    &after_ws[1..]
                };
                let (rest, _) = ws(rest)?;
                let (rest, expr) = parse_assign_expr_or_comma(rest)?;
                if is_atomic {
                    let stmt = Stmt::Expr(Expr::Call {
                        name: Symbol::intern("__mutsu_atomic_store_var"),
                        args: vec![Expr::Literal(Value::str(bare_name)), expr],
                    });
                    return parse_statement_modifier(rest, stmt);
                }
                let stmt = Stmt::Assign {
                    name: bare_name,
                    expr,
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(rest, stmt);
            }
        }
        return Err(PError::expected("assignment"));
    }

    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        b'&' => "&",
        _ => "",
    };

    let (rest, var) = var_name(input)?;
    let name = format!("{}{}", prefix, var);
    let (rest, _) = ws(rest)?;
    let var_expr = if sigil == b'@' {
        Expr::ArrayVar(var.clone())
    } else if sigil == b'%' {
        Expr::HashVar(var.clone())
    } else {
        Expr::Var(name.clone())
    };

    // Mutating method compound assignment: $x.m += expr
    if let Some(after_dot) = rest.strip_prefix('.')
        && !after_dot.starts_with('=')
    {
        let (after_name, method_name) = take_while1(after_dot, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let method_name = method_name.to_string();
        let (after_name, _) = ws(after_name)?;
        if let Some((after_op, op)) = parse_compound_assign_op(after_name) {
            let (after_op, _) = ws(after_op)?;
            let (rest, rhs) = parse_assign_expr_or_comma(after_op)?;
            let current_value = Expr::MethodCall {
                target: Box::new(var_expr.clone()),
                name: Symbol::intern(&method_name),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
            let updated_value = Expr::Binary {
                left: Box::new(current_value),
                op: op.token_kind(),
                right: Box::new(rhs),
            };
            let assign_call = Expr::Call {
                name: Symbol::intern("__mutsu_assign_method_lvalue"),
                args: vec![
                    var_expr,
                    Expr::Literal(Value::str(method_name)),
                    Expr::ArrayLiteral(Vec::new()),
                    updated_value,
                    Expr::Literal(Value::str(name.clone())),
                ],
            };
            let stmt = Stmt::Expr(assign_call);
            return parse_statement_modifier(rest, stmt);
        }
    }

    // Meta-op assignment: @a [X+]= @b → @a = @a X+ @b
    // Meta-op assignment: @a [X+]= @b → @a = @a X+ @b
    // Also handles reduction assignment: $x [+]= 6 → $x += 6
    if let Some((after_eq, meta, op)) = parse_bracket_meta_assign_op(rest) {
        let (after_eq, _) = ws(after_eq)?;
        let (rest, rhs) = parse_assign_expr_or_comma(after_eq)?;
        // For "reduce" meta (plain [op]=), reduce on two values is just the base op.
        if meta == "reduce"
            && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
        {
            let expr = compound_assigned_value_expr(var_expr, compound_op, rhs);
            let stmt = Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            };
            return parse_statement_modifier(rest, stmt);
        }
        let expr = Expr::MetaOp {
            meta,
            op,
            left: Box::new(var_expr),
            right: Box::new(rhs),
        };
        let stmt = match rewrite_scalar_assignment_stmt_as_sink(name.clone(), expr.clone()) {
            Some(stmt) => stmt,
            None => Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            },
        };
        return parse_statement_modifier(rest, stmt);
    }

    // Meta-op assignment: @a X*= 10 → @a = @a X* 10
    if let Some((after_eq, meta, op)) = parse_meta_compound_assign_op(rest) {
        let (after_eq, _) = ws(after_eq)?;
        let (rest, rhs) = parse_assign_expr_or_comma(after_eq)?;
        // For scalar variables with Z-meta, fall back to plain compound assignment
        // since scalar Zip on single values is equivalent to the base operator.
        if meta == "Z"
            && sigil == b'$'
            && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
        {
            let expr = compound_assigned_value_expr(var_expr, compound_op, rhs);
            let stmt = Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            };
            return parse_statement_modifier(rest, stmt);
        }
        // The reverse meta-operator assignment `$x R op= $y` assigns to its
        // RIGHT operand: `$y = $y op $x` (= `$x R op $y`). So `$x R~= $y` leaves
        // `$x` unchanged and sets `$y` to `$y ~ $x`, and `$x R op= <literal>`
        // dies with X::Assignment::RO because the literal is not a container.
        if meta == "R" {
            let value = Expr::MetaOp {
                meta,
                op,
                left: Box::new(var_expr),
                right: Box::new(rhs.clone()),
            };
            let assign = crate::parser::expr::precedence::assign_to_target_expr(rhs, value);
            return parse_statement_modifier(rest, Stmt::Expr(assign));
        }
        let expr = Expr::MetaOp {
            meta,
            op,
            left: Box::new(var_expr),
            right: Box::new(rhs),
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    // Set operator compound assignment: $s (|)= 5 → $s = $s (|) 5
    if let Some((stripped, set_tok)) = parse_set_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after set compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let expr = Expr::Binary {
            left: Box::new(var_expr),
            op: set_tok,
            right: Box::new(rhs),
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    if let Some((stripped, op)) = parse_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        // Use the sigil-appropriate lvalue expression so `%h ,= %g` / `@a ,= 3`
        // read the current container as a Hash/Array (not a scalar `Var("%h")`),
        // giving the comma operator the two containers to merge/append.
        let expr = compound_assigned_value_expr(var_expr, op, rhs);
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let stmt = Stmt::Assign {
            name: name.clone(),
            expr: Expr::InfixFunc {
                name: op_name,
                left: Box::new(var_expr),
                right: vec![rhs],
                modifier: None,
            },
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    // Mutating method call: $x.=method or $x .= method(args)
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (stripped, _) = ws(stripped)?;
        let var_expr = if sigil == b'@' {
            Expr::ArrayVar(var.clone())
        } else if sigil == b'%' {
            Expr::HashVar(var.clone())
        } else {
            Expr::Var(var.clone())
        };
        // Check for quoted method name: .="method"() or .='method'()
        if let Some((r_after_quote, qname)) = parse_quoted_method_name(stripped) {
            let (r_after_quote, _) = ws(r_after_quote)?;
            let (r_final, args) = if r_after_quote.starts_with('(') {
                let (r2, _) = parse_char(r_after_quote, '(')?;
                let (r2, _) = ws(r2)?;
                let (r2, a) = parse_call_arg_list(r2)?;
                let (r2, _) = ws(r2)?;
                let (r2, _) = parse_char(r2, ')')?;
                (r2, a)
            } else {
                (r_after_quote, vec![])
            };
            let method_expr = match qname {
                QuotedMethodName::Static(mname) => Expr::MethodCall {
                    target: Box::new(var_expr),
                    name: Symbol::intern(&mname),
                    args,
                    modifier: None,
                    quoted: true,
                },
                QuotedMethodName::Dynamic(name_expr) => Expr::DynamicMethodCall {
                    target: Box::new(var_expr),
                    name_expr: Box::new(name_expr),
                    args,
                    modifier: None,
                },
            };
            let stmt = if name == "_" {
                Stmt::Expr(Expr::Call {
                    name: Symbol::intern("__mutsu_topic_dotassign"),
                    args: vec![method_expr],
                })
            } else {
                Stmt::Assign {
                    name,
                    expr: method_expr,
                    op: AssignOp::Assign,
                }
            };
            return parse_statement_modifier(r_final, stmt);
        }
        let (r, method_name) = take_while1(stripped, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })
        .map_err(|err| PError {
            messages: merge_expected_messages("expected method name after '.='", &err.messages),
            remaining_len: err.remaining_len.or(Some(stripped.len())),
            exception: None,
        })?;
        let method_name = method_name.to_string();
        let r_before_ws = r;
        let (r, _) = ws(r)?;
        let (r, args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, a) = parse_call_arg_list(r).map_err(|err| PError {
                messages: merge_expected_messages("expected method call arguments", &err.messages),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, a)
        } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
            // Colon-arg syntax: .=method: arg, arg2 (no space before colon)
            let r = &r_before_ws[1..];
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
            (r_inner, args)
        } else if r.starts_with(':') && !r.starts_with("::") {
            // Fake-infix adverb form: .=method :key<val> (space before colon)
            let mut args = Vec::new();
            let mut r_inner = r;
            while r_inner.starts_with(':') && !r_inner.starts_with("::") {
                if let Ok((r2, arg)) = crate::parser::primary::misc::colonpair_expr(r_inner) {
                    args.push(arg);
                    let (r3, _) = ws(r2)?;
                    r_inner = r3;
                } else {
                    break;
                }
            }
            (r_inner, args)
        } else {
            (r, Vec::new())
        };
        // A chained `.=` (`$s .= uc .= flip`) must be handled by the expression
        // postfix loop, which threads each step's lvalue back through the next
        // `.=` (`do { $s = $s.uc; $s = $s.flip }`). The statement shortcut here
        // only lowers a single `.= method`, so bail to the expression-statement
        // fallback when another `.=` follows rather than mis-parsing the tail as a
        // bare topic `$_ .= ...`.
        {
            let (r_peek, _) = ws(r)?;
            if r_peek.starts_with(".=") {
                return Err(PError::expected("chained .= (handled as expression)"));
            }
        }
        let expr = Expr::MethodCall {
            target: Box::new(var_expr),
            name: Symbol::intern(&method_name),
            args,
            modifier: None,
            quoted: false,
        };
        // `$_ .= meth`: route the topic metaop through `__mutsu_topic_dotassign`
        // so it can reassign a read-only whole-container topic while a plain
        // `$_ = ...` still throws X::Assignment::RO.
        let stmt = if name == "_" {
            Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_topic_dotassign"),
                args: vec![expr],
            })
        } else {
            Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            }
        };
        return parse_statement_modifier(r, stmt);
    }

    // Detect Perl 5 =~ braino: only when followed by space or m/ (not =~$var which is = ~$var)
    if rest.starts_with("=~") && !rest.starts_with("=~=") && !rest.starts_with("=:=") {
        let after = &rest[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::fatal(
                "X::Obsolete: Unsupported use of =~ to do pattern matching; in Raku please use ~~"
                    .to_string(),
            ));
        }
    }
    // Simple assignment
    if let Some(stripped) = rest.strip_prefix("⚛+=") {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after atomic compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let stmt = Stmt::Expr(Expr::Call {
            name: Symbol::intern("__mutsu_atomic_add_var"),
            args: vec![Expr::Literal(Value::str(name)), rhs],
        });
        return parse_statement_modifier(rest, stmt);
    }

    // $/ = "string" or $/ = 'string' is rejected as a P5-ism
    // ($/ = Nil, $/ = 42, etc. are allowed)
    if name == "/"
        && rest.starts_with('=')
        && !rest.starts_with("==")
        && !rest.starts_with("=>")
        && !rest.starts_with("=:")
    {
        let rhs_rest = rest[1..].trim_start();
        if rhs_rest.starts_with('"') || rhs_rest.starts_with('\'') {
            return Err(PError::fatal(
                "X::Syntax::Perl5Var: Unsupported use of $/ variable; in Raku please use the filehandle's .nl-in attribute".to_string(),
            ));
        }
    }

    if (rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>"))
        || rest.starts_with("⚛=")
    {
        let is_atomic = rest.starts_with("⚛=");
        let rest = if is_atomic {
            &rest["⚛=".len()..]
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;

        // Item assignment (`=`) to a scalar variable binds TIGHTER than the comma
        // operator: `$x = 1, 2` parses as `($x = 1), 2`, so only the first element
        // is assigned and the remaining elements form a (sink-context) list. The
        // parenthesized form `$x = (1, 2)` is a single grouped operand and is
        // assigned whole — and that distinction is *only* available here at parse
        // time, since both collapse to the same `ArrayLiteral` in the AST. List
        // assignment to an `@`/`%` container keeps the comma-absorbing RHS so
        // `@x = 1, 2, 3` assigns the whole list. Atomic `⚛=` is always a scalar
        // store and is handled below.
        if sigil == b'$' && !is_atomic {
            let (after_rhs, rhs) = expression(rest).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected right-hand expression after '='",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(rest.len())),
                exception: None,
            })?;
            let (after_ws, _) = ws(after_rhs)?;
            if after_ws.starts_with(',') && !after_ws.starts_with(",,") {
                // `($x = rhs), <rest...>` — assign the first element, sink the rest.
                let assign_expr = Expr::AssignExpr {
                    name,
                    expr: Box::new(rhs),
                    is_bind: false,
                };
                let mut items = vec![assign_expr];
                let mut r = after_ws;
                while r.starts_with(',') && !r.starts_with(",,") {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.is_empty()
                        || r2.starts_with(';')
                        || r2.starts_with('}')
                        || r2.starts_with(')')
                    {
                        r = r2;
                        break;
                    }
                    let (r2, next) = expression(r2)?;
                    items.push(next);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                let stmt = Stmt::Expr(Expr::ArrayLiteral(items));
                return parse_statement_modifier(r, stmt);
            }
            let stmt = Stmt::Assign {
                name,
                expr: rhs,
                op: AssignOp::Assign,
            };
            return parse_statement_modifier(after_rhs, stmt);
        }

        let (rest, expr) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after '='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        if is_atomic {
            let stmt = Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_atomic_store_var"),
                args: vec![Expr::Literal(Value::str(name)), expr],
            });
            return parse_statement_modifier(rest, stmt);
        }
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }
    // Binding (:= or ::=)
    if let Some(stripped) = rest.strip_prefix("::=").or_else(|| rest.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (rest, expr) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after ':='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Bind,
        };
        return parse_statement_modifier(rest, stmt);
    }

    Err(PError::expected("assignment"))
}
