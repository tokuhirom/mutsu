use super::*;

impl Parser {
    pub(super) fn parse_stmt(&mut self) -> Result<Stmt, RuntimeError> {
        if self.match_kind(TokenKind::LBrace) {
            let body = self.parse_block_body()?;
            return Ok(Stmt::Block(body));
        }
        if self.match_ident("use") {
            // Handle `use v6;` and similar version pragmas - just skip them
            if self
                .advance_if(|k| matches!(k, TokenKind::VersionLiteral { .. }))
                .is_some()
            {
                while !self.check(&TokenKind::Semicolon) && !self.check(&TokenKind::Eof) {
                    self.pos += 1;
                }
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Use {
                    module: "v6".to_string(),
                    arg: None,
                });
            }
            let module = self
                .consume_ident()
                .unwrap_or_else(|_| "unknown".to_string());
            if module == "v6" {
                while !self.check(&TokenKind::Semicolon) && !self.check(&TokenKind::Eof) {
                    self.pos += 1;
                }
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Use {
                    module: "v6".to_string(),
                    arg: None,
                });
            }
            let arg = if self.check(&TokenKind::Semicolon) {
                None
            } else if self.check_angle_start() {
                Some(self.parse_angle_literal())
            } else {
                Some(self.parse_expr()?)
            };
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Use { module, arg });
        }
        if self.match_ident("unit") && self.match_ident("module") {
            let name = self.consume_ident()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Package {
                name,
                body: Vec::new(),
            });
        }
        if self.match_ident("package") {
            let name = self.consume_ident()?;
            let body = self.parse_block()?;
            return Ok(Stmt::Package { name, body });
        }
        if self.match_ident("class") {
            return self.parse_class_decl();
        }
        if self.match_ident("role") {
            return self.parse_role_decl();
        }
        if self.match_ident("grammar") {
            let name = self.consume_ident()?;
            let body = self.parse_block()?;
            return Ok(Stmt::Package { name, body });
        }
        if self.match_ident("subset") {
            let name = self.consume_ident()?;
            let mut base = "Any".to_string();
            if self.match_ident("of") {
                base = self.consume_ident()?;
            }
            if !self.match_ident("where") {
                return Err(RuntimeError::new("Expected where in subset"));
            }
            let predicate = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::SubsetDecl {
                name,
                base,
                predicate,
            });
        }
        if self.match_ident("has") {
            return self.parse_has_decl();
        }
        if self.match_ident("proto") {
            if self.match_ident("token") {
                return self.parse_proto_token_decl();
            }
            if self.match_ident("rule") {
                return self.parse_proto_token_decl();
            }
            self.match_ident("sub");
            return self.parse_proto_sub_decl();
        }
        if self.match_ident("multi") {
            if self.match_ident("token") {
                return self.parse_token_rule_decl(false, true);
            }
            if self.match_ident("rule") {
                return self.parse_token_rule_decl(true, true);
            }
            if self.match_ident("method") {
                return self.parse_method_decl(true);
            }
            if self.match_ident("sub") {
                return self.parse_sub_decl(true);
            }
            if self.peek_ident().is_some() {
                return self.parse_sub_decl(true);
            }
            return Err(RuntimeError::new("Expected method or sub after multi"));
        }
        if self.match_ident("token") {
            return self.parse_token_rule_decl(false, false);
        }
        if self.match_ident("rule") {
            return self.parse_token_rule_decl(true, false);
        }
        if self.match_ident("method") {
            return self.parse_method_decl(false);
        }
        if self.match_ident("sub") {
            return self.parse_sub_decl(false);
        }
        if self.match_ident("return") {
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Return(expr));
        }
        if self.match_ident("subtest") {
            let name = self.parse_or()?;
            if !self.match_kind(TokenKind::FatArrow) {
                return Err(RuntimeError::new("Expected fat arrow after subtest name"));
            }
            if self.match_ident("sub") {
                let body = self.parse_block()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Subtest { name, body });
            }
            if self.match_kind(TokenKind::LBrace) {
                let body = self.parse_block_body()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Subtest { name, body });
            }
            return Err(RuntimeError::new("Expected sub or block for subtest"));
        }
        if self.match_ident("my") || self.match_ident("our") {
            // my enum Foo <...> or my enum Foo (...)
            if self.match_ident("enum") {
                return self.parse_enum_decl();
            }
            // my sub foo(...) { ... } — lexically scoped sub (same as plain sub)
            if matches!(self.peek_ident().as_deref(), Some("sub") | Some("method")) {
                return self.parse_stmt();
            }
            // Destructuring: my ($a, $b, $c) = expr
            if self.check(&TokenKind::LParen) {
                self.pos += 1; // consume (
                let mut names = Vec::new();
                while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                    if self.peek_is_var() {
                        names.push(self.consume_var()?);
                    } else if self.peek_is_codevar() {
                        names.push(format!("&{}", self.consume_codevar()?));
                    } else {
                        return Err(RuntimeError::new(
                            "Expected variable in destructuring declaration",
                        ));
                    }
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.consume_kind(TokenKind::RParen)?;
                if self.match_kind(TokenKind::Eq) || self.match_kind(TokenKind::Bind) {
                    let rhs = self.parse_comma_expr()?;
                    self.match_kind(TokenKind::Semicolon);
                    // Desugar into: { my @tmp = rhs; $a = @tmp[0]; $b = @tmp[1]; ... }
                    let tmp_name = "@__destructure_tmp__".to_string();
                    let array_bare = "__destructure_tmp__".to_string();
                    let mut stmts = vec![Stmt::VarDecl {
                        name: tmp_name,
                        expr: rhs,
                        type_constraint: None,
                    }];
                    for (i, var_name) in names.iter().enumerate() {
                        stmts.push(Stmt::VarDecl {
                            name: var_name.clone(),
                            expr: Expr::Index {
                                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                                index: Box::new(Expr::Literal(Value::Int(i as i64))),
                            },
                            type_constraint: None,
                        });
                    }
                    return Ok(Stmt::Block(stmts));
                }
                // No assignment, just declare all as Nil
                let mut stmts = Vec::new();
                for var_name in &names {
                    stmts.push(Stmt::VarDecl {
                        name: var_name.clone(),
                        expr: Expr::Literal(Value::Nil),
                        type_constraint: None,
                    });
                }
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Block(stmts));
            }
            // Capture optional type annotation (e.g., my Str $a, my Int $b)
            let type_constraint = if let Some(TokenKind::Ident(tc)) =
                self.tokens.get(self.pos).map(|t| &t.kind)
            {
                if let Some(TokenKind::Var(_) | TokenKind::ArrayVar(_) | TokenKind::HashVar(_)) =
                    self.tokens.get(self.pos + 1).map(|t| &t.kind)
                {
                    let tc = tc.clone();
                    self.pos += 1; // skip the type name
                    Some(tc)
                } else {
                    None
                }
            } else {
                None
            };
            let (name, is_array, is_hash) = if let Some(token) =
                self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_)))
            {
                if let TokenKind::ArrayVar(n) = token.kind {
                    (format!("@{}", n), true, false)
                } else {
                    (String::new(), false, false)
                }
            } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::HashVar(_))) {
                if let TokenKind::HashVar(n) = token.kind {
                    (format!("%{}", n), false, true)
                } else {
                    (String::new(), false, false)
                }
            } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::CodeVar(_))) {
                if let TokenKind::CodeVar(n) = token.kind {
                    (format!("&{}", n), false, false)
                } else {
                    (String::new(), false, false)
                }
            } else if let Some(TokenKind::Ident(n)) =
                self.tokens.get(self.pos).map(|t| t.kind.clone())
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Eq | TokenKind::Bind | TokenKind::Semicolon)
                )
            {
                // Sigilless variable: my \name = expr (\ is skipped by lexer)
                self.pos += 1;
                (n, false, false)
            } else {
                (self.consume_var()?, false, false)
            };
            if self.match_kind(TokenKind::Eq) || self.match_kind(TokenKind::Bind) {
                let expr = self.parse_comma_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::VarDecl {
                    name,
                    expr,
                    type_constraint: type_constraint.clone(),
                });
            }
            self.match_kind(TokenKind::Semicolon);
            let expr = if is_array {
                Expr::Literal(Value::Array(Vec::new()))
            } else if is_hash {
                Expr::Hash(Vec::new())
            } else {
                Expr::Literal(Value::Nil)
            };
            return Ok(Stmt::VarDecl {
                name,
                expr,
                type_constraint,
            });
        }
        if self.match_ident("constant") {
            // constant NAME = expr;
            // The name can be a sigil-less identifier or a $var
            let name = if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Var(_))) {
                if let TokenKind::Var(n) = token.kind {
                    format!("${}", n)
                } else {
                    String::new()
                }
            } else if let Some(TokenKind::Ident(n)) =
                self.tokens.get(self.pos).map(|t| t.kind.clone())
            {
                self.pos += 1;
                n
            } else {
                return Err(RuntimeError::new("Expected identifier after 'constant'"));
            };
            if self.match_kind(TokenKind::Eq) || self.match_kind(TokenKind::Bind) {
                let expr = self.parse_comma_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::VarDecl {
                    name,
                    expr,
                    type_constraint: None,
                });
            }
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::VarDecl {
                name,
                expr: Expr::Literal(Value::Nil),
                type_constraint: None,
            });
        }
        if self.match_ident("enum") {
            return self.parse_enum_decl();
        }
        if self.match_ident("say") {
            let exprs = self.parse_expr_list()?;
            let stmt = Stmt::Say(exprs);
            return self.parse_statement_modifier(stmt);
        }
        if self.match_ident("put") {
            let exprs = self.parse_expr_list()?;
            let stmt = Stmt::Say(exprs);
            return self.parse_statement_modifier(stmt);
        }
        if self.match_ident("print") {
            let exprs = self.parse_expr_list()?;
            let stmt = Stmt::Print(exprs);
            return self.parse_statement_modifier(stmt);
        }
        if self.match_ident("note") {
            let exprs = self.parse_expr_list()?;
            let stmt = Stmt::Note(exprs);
            return self.parse_statement_modifier(stmt);
        }
        // push/unshift/append/prepend @arr, val... → @arr.method(val...)
        if let Some(fname) = self.peek_ident()
            && matches!(fname.as_str(), "push" | "unshift" | "append" | "prepend")
        {
            let saved = self.pos;
            self.pos += 1;
            // Check if next token is an array variable (paren form handled in expr parser)
            let is_arr = matches!(
                self.tokens.get(self.pos).map(|t| &t.kind),
                Some(TokenKind::ArrayVar(_) | TokenKind::Var(_))
            );
            if !self.check(&TokenKind::LParen) && is_arr {
                let arr_expr = self.parse_primary()?;
                let mut val_args = Vec::new();
                if self.match_kind(TokenKind::Comma) {
                    val_args = self.parse_expr_list()?;
                }
                let method_call = Expr::MethodCall {
                    target: Box::new(arr_expr),
                    name: fname,
                    args: val_args,
                    modifier: None,
                };
                let stmt = Stmt::Expr(method_call);
                return self.parse_statement_modifier(stmt);
            }
            self.pos = saved;
        }
        if self.match_ident("if") {
            return self.parse_if_chain();
        }
        if self.match_ident("unless") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            // unless does not allow else, elsif, or orwith
            if let Some(kw) = self.peek_ident()
                && matches!(kw.as_str(), "else" | "elsif" | "orwith")
            {
                return Err(RuntimeError::new(format!(
                    "X::Syntax::UnlessElse: \"unless\" does not take \"{}\", please rewrite using \"if\"",
                    kw
                )));
            }
            // unless is if with negated condition
            return Ok(Stmt::If {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                then_branch: body,
                else_branch: Vec::new(),
            });
        }
        if self.match_ident("with") {
            let cond = self.parse_expr()?;
            let then_branch = self.parse_block()?;
            let else_branch = if self.match_ident("orwith") {
                let orwith_cond = self.parse_expr()?;
                let orwith_body = self.parse_block()?;
                let orwith_else = if self.match_ident("else") {
                    self.parse_block()?
                } else {
                    Vec::new()
                };
                vec![Stmt::If {
                    cond: Expr::MethodCall {
                        target: Box::new(orwith_cond),
                        name: "defined".to_string(),
                        args: Vec::new(),
                        modifier: None,
                    },
                    then_branch: orwith_body,
                    else_branch: orwith_else,
                }]
            } else if self.match_ident("else") {
                self.parse_block()?
            } else {
                Vec::new()
            };
            return Ok(Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(cond),
                    name: "defined".to_string(),
                    args: Vec::new(),
                    modifier: None,
                },
                then_branch,
                else_branch,
            });
        }
        if self.match_ident("without") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::If {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::MethodCall {
                        target: Box::new(cond),
                        name: "defined".to_string(),
                        args: Vec::new(),
                        modifier: None,
                    }),
                },
                then_branch: body,
                else_branch: Vec::new(),
            });
        }
        // Labeled loop detection: LABEL: for/while/loop/...
        if let Some(label_name) = self.peek_ident()
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Colon)
            )
            && let Some(TokenKind::Ident(kw)) = self.tokens.get(self.pos + 2).map(|t| &t.kind)
            && matches!(
                kw.as_str(),
                "for" | "while" | "until" | "loop" | "repeat" | "do"
            )
        {
            self.pos += 2; // skip label and colon
            return self.parse_labeled_loop(Some(label_name));
        }
        if self.match_ident("while") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::While {
                cond,
                body,
                label: None,
            });
        }
        if self.match_ident("until") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body,
                label: None,
            });
        }
        if self.match_ident("repeat") {
            // Handle both forms:
            //   repeat { BLOCK } while/until COND
            //   repeat while/until COND { BLOCK }
            let next_is_while_or_until =
                if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                    name == "while" || name == "until"
                } else {
                    false
                };
            if next_is_while_or_until {
                let is_until = self.match_ident("until");
                if !is_until {
                    self.match_ident("while");
                }
                let cond = self.parse_expr()?;
                let body = self.parse_block()?;
                self.match_kind(TokenKind::Semicolon);
                let cond_expr = if is_until {
                    Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(cond),
                    }
                } else {
                    cond
                };
                return Ok(Stmt::Loop {
                    init: None,
                    cond: Some(cond_expr),
                    step: None,
                    body,
                    repeat: true,
                    label: None,
                });
            }
            let body = self.parse_block()?;
            if self.match_ident("while") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Loop {
                    init: None,
                    cond: Some(cond),
                    step: None,
                    body,
                    repeat: true,
                    label: None,
                });
            } else if self.match_ident("until") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Loop {
                    init: None,
                    cond: Some(Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(cond),
                    }),
                    step: None,
                    body,
                    repeat: true,
                    label: None,
                });
            }
            return Err(RuntimeError::new(
                "Expected 'while' or 'until' after repeat block",
            ));
        }
        if self.match_ident("loop") {
            if self.check(&TokenKind::LParen) {
                if !self.match_kind(TokenKind::LParen) {
                    return Err(RuntimeError::new("expected '(' after loop"));
                }
                let init = if self.check(&TokenKind::Semicolon) {
                    self.match_kind(TokenKind::Semicolon);
                    None
                } else {
                    Some(Box::new(self.parse_stmt()?))
                };
                let cond = if self.check(&TokenKind::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                if !self.match_kind(TokenKind::Semicolon) {
                    return Err(RuntimeError::new("expected ';' in loop"));
                }
                let step = if self.check(&TokenKind::RParen) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                if !self.match_kind(TokenKind::RParen) {
                    return Err(RuntimeError::new("expected ')' after loop"));
                }
                let body = self.parse_block()?;
                return Ok(Stmt::Loop {
                    init,
                    cond,
                    step,
                    body,
                    repeat: false,
                    label: None,
                });
            } else {
                let body = self.parse_block()?;
                return Ok(Stmt::Loop {
                    init: None,
                    cond: None,
                    step: None,
                    body,
                    repeat: false,
                    label: None,
                });
            }
        }
        if self.match_ident("last") {
            // Check for label: last LABEL [if/unless cond]
            let label = self.try_consume_loop_label();
            if self.match_ident("if") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: vec![Stmt::Last(label)],
                    else_branch: Vec::new(),
                });
            }
            if self.match_ident("unless") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: Vec::new(),
                    else_branch: vec![Stmt::Last(label)],
                });
            }
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Last(label));
        }
        if self.match_ident("next") {
            let label = self.try_consume_loop_label();
            if self.match_ident("if") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: vec![Stmt::Next(label)],
                    else_branch: Vec::new(),
                });
            }
            if self.match_ident("unless") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: Vec::new(),
                    else_branch: vec![Stmt::Next(label)],
                });
            }
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Next(label));
        }
        if self.match_ident("for") {
            let iterable = self.parse_comma_expr()?;
            let mut param = None;
            let mut params = Vec::new();
            if self.match_kind(TokenKind::Arrow)
                && let Some(first) = self.parse_pointy_param_name()
            {
                if self.match_kind(TokenKind::Comma) {
                    params.push(first);
                    while let Some(name) = self.parse_pointy_param_name() {
                        params.push(name);
                        if !self.match_kind(TokenKind::Comma) {
                            break;
                        }
                    }
                } else {
                    param = Some(first);
                }
            }
            let body = self.parse_block()?;
            return Ok(Stmt::For {
                iterable,
                param,
                params,
                body,
                label: None,
            });
        }
        if self.match_ident("given") {
            let topic = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::Given { topic, body });
        }
        if self.match_ident("when") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::When { cond, body });
        }
        if self.match_ident("default") {
            let body = self.parse_block()?;
            return Ok(Stmt::Default(body));
        }
        if self.match_ident("does") {
            let name = self.consume_ident()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::DoesDecl { name });
        }
        if self.match_ident("die") || self.match_ident("fail") {
            // Handle postfix `die if COND` / `die unless COND` (bare die)
            if self.match_ident("if") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: vec![Stmt::Die(Expr::Literal(Value::Str("Died".to_string())))],
                    else_branch: Vec::new(),
                });
            }
            if self.match_ident("unless") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: Vec::new(),
                    else_branch: vec![Stmt::Die(Expr::Literal(Value::Str("Died".to_string())))],
                });
            }
            let expr = self.parse_expr()?;
            // Check for postfix if/unless after the die expression
            if self.match_ident("if") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: vec![Stmt::Die(expr)],
                    else_branch: Vec::new(),
                });
            }
            if self.match_ident("unless") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: Vec::new(),
                    else_branch: vec![Stmt::Die(expr)],
                });
            }
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Die(expr));
        }
        if self.match_ident("CATCH") {
            let body = self.parse_block()?;
            return Ok(Stmt::Catch(body));
        }
        if self.match_ident("CONTROL") {
            let body = self.parse_block()?;
            return Ok(Stmt::Control(body));
        }
        if self.match_ident("BEGIN") {
            let body = if self.check(&TokenKind::LBrace) {
                self.parse_block()?
            } else {
                vec![self.parse_stmt()?]
            };
            return Ok(Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
            });
        }
        if self.match_ident("CHECK") || self.match_ident("INIT") {
            let body = if self.check(&TokenKind::LBrace) {
                self.parse_block()?
            } else {
                vec![self.parse_stmt()?]
            };
            return Ok(Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
            });
        }
        if self.match_ident("END") {
            let body = if self.check(&TokenKind::LBrace) {
                self.parse_block()?
            } else {
                vec![self.parse_stmt()?]
            };
            return Ok(Stmt::Phaser {
                kind: PhaserKind::End,
                body,
            });
        }
        if self.match_ident("ENTER") {
            let body = self.parse_block()?;
            return Ok(Stmt::Phaser {
                kind: PhaserKind::Enter,
                body,
            });
        }
        if self.match_ident("LEAVE") {
            let body = self.parse_block()?;
            return Ok(Stmt::Phaser {
                kind: PhaserKind::Leave,
                body,
            });
        }
        if self.match_ident("FIRST") {
            let body = self.parse_block()?;
            return Ok(Stmt::Phaser {
                kind: PhaserKind::First,
                body,
            });
        }
        if self.match_ident("NEXT") {
            let body = self.parse_block()?;
            return Ok(Stmt::Phaser {
                kind: PhaserKind::Next,
                body,
            });
        }
        if self.match_ident("LAST") {
            let body = self.parse_block()?;
            return Ok(Stmt::Phaser {
                kind: PhaserKind::Last,
                body,
            });
        }
        if self.match_ident("proceed") {
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Proceed);
        }
        if self.match_ident("succeed") {
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Succeed);
        }
        if self.match_ident("redo") {
            let label = self.try_consume_loop_label();
            if self.match_ident("if") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: vec![Stmt::Redo(label)],
                    else_branch: Vec::new(),
                });
            }
            if self.match_ident("unless") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: Vec::new(),
                    else_branch: vec![Stmt::Redo(label)],
                });
            }
            if self.match_ident("until") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::If {
                    cond,
                    then_branch: Vec::new(),
                    else_branch: vec![Stmt::Redo(label)],
                });
            }
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Redo(label));
        }
        if self.match_ident("react") {
            let body = self.parse_block()?;
            return Ok(Stmt::React { body });
        }
        if self.match_ident("whenever") {
            let supply = self.parse_expr()?;
            let mut param = None;
            if self.match_kind(TokenKind::Arrow) {
                if matches!(
                    self.tokens.get(self.pos).map(|t| &t.kind),
                    Some(TokenKind::Ident(_))
                ) && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Var(_))
                ) {
                    self.pos += 1;
                }
                if self.peek_is_var() {
                    param = Some(self.consume_var()?);
                }
            }
            let body = self.parse_block()?;
            return Ok(Stmt::Whenever {
                supply,
                param,
                body,
            });
        }
        if self.match_ident("take") {
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Take(expr));
        }
        if self.match_ident("warn") {
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            // warn just outputs to stderr/diag; treat as no-op for now
            return Ok(Stmt::Expr(expr));
        }
        if let Some(name) = self.peek_ident()
            && matches!(
                name.as_str(),
                "ok" | "is"
                    | "isnt"
                    | "nok"
                    | "pass"
                    | "flunk"
                    | "cmp-ok"
                    | "like"
                    | "unlike"
                    | "is-deeply"
                    | "is-approx"
                    | "isa-ok"
                    | "lives-ok"
                    | "dies-ok"
                    | "eval-lives-ok"
                    | "eval-dies-ok"
                    | "is_run"
                    | "throws-like"
                    | "force_todo"
                    | "force-todo"
                    | "plan"
                    | "done-testing"
                    | "bail-out"
                    | "skip"
                    | "skip-rest"
                    | "diag"
                    | "todo"
                    | "does-ok"
                    | "can-ok"
                    | "use-ok"
            )
        {
            self.pos += 1;
            let args = if name == "is"
                && (!self.check(&TokenKind::LParen) || self.is_is_grouping_paren())
            {
                self.parse_is_call_args()
            } else {
                self.parse_call_args()?
            };
            let stmt = Stmt::Call { name, args };
            return self.parse_statement_modifier(stmt);
        }
        if let Some(name) = self.peek_ident()
            && name != "temp"
            && name != "do"
            && name != "if"
            && name != "unless"
            && name != "while"
            && name != "until"
            && name != "for"
            && name != "given"
            && name != "when"
            && name != "with"
            && name != "without"
            && name != "try"
            && matches!(
                self.peek_next_kind(),
                Some(
                    TokenKind::Var(_)
                        | TokenKind::ArrayVar(_)
                        | TokenKind::HashVar(_)
                        | TokenKind::CaptureVar(_)
                        | TokenKind::Number(_)
                        | TokenKind::BigNumber(_)
                        | TokenKind::Float(_)
                        | TokenKind::Imaginary(_)
                        | TokenKind::Str(_)
                        | TokenKind::DStr(_)
                        | TokenKind::Regex(_)
                        | TokenKind::Subst { .. }
                )
            )
        {
            self.pos += 1;
            let args = self.parse_call_args()?;
            let stmt = Stmt::Call { name, args };
            return self.parse_statement_modifier(stmt);
        }
        if self.peek_is_var()
            && let Some(next) = self.peek_next_kind()
        {
            if matches!(next, TokenKind::DotEq) {
                let name = self.consume_var()?;
                self.match_kind(TokenKind::DotEq);
                let method_name = self.consume_ident()?;
                let mut method_args = Vec::new();
                if self.match_kind(TokenKind::LParen) {
                    if !self.check(&TokenKind::RParen) {
                        method_args.push(self.parse_method_arg());
                        while self.match_kind(TokenKind::Comma) {
                            method_args.push(self.parse_method_arg());
                        }
                    }
                    self.consume_kind(TokenKind::RParen)?;
                }
                self.match_kind(TokenKind::Semicolon);
                let expr = Expr::MethodCall {
                    target: Box::new(Expr::Var(name.clone())),
                    name: method_name,
                    args: method_args,
                    modifier: None,
                };
                return Ok(Stmt::Assign {
                    name,
                    expr,
                    op: AssignOp::Assign,
                });
            }
            if matches!(
                next,
                TokenKind::PlusEq | TokenKind::MinusEq | TokenKind::TildeEq | TokenKind::StarEq
            ) {
                let name = self.consume_var()?;
                let compound_op = if self.match_kind(TokenKind::PlusEq) {
                    TokenKind::Plus
                } else if self.match_kind(TokenKind::MinusEq) {
                    TokenKind::Minus
                } else if self.match_kind(TokenKind::TildeEq) {
                    TokenKind::Tilde
                } else {
                    self.match_kind(TokenKind::StarEq);
                    TokenKind::Star
                };
                let rhs = self.parse_comma_expr()?;
                let expr = Expr::Binary {
                    left: Box::new(Expr::Var(name.clone())),
                    op: compound_op,
                    right: Box::new(rhs),
                };
                let stmt = Stmt::Assign {
                    name,
                    expr,
                    op: AssignOp::Assign,
                };
                return self.parse_statement_modifier(stmt);
            }
            if matches!(next, TokenKind::LBracket) {
                let name = self.consume_var()?;
                if let Some((meta, op, consumed)) = self.try_bracket_meta_op_at(self.pos) {
                    self.pos += consumed;
                    if self.match_kind(TokenKind::Eq) {
                        let rhs = self.parse_comma_expr()?;
                        self.match_kind(TokenKind::Semicolon);
                        let expr = Expr::MetaOp {
                            meta,
                            op,
                            left: Box::new(Expr::Var(name.clone())),
                            right: Box::new(rhs),
                        };
                        return Ok(Stmt::Assign {
                            name,
                            expr,
                            op: AssignOp::Assign,
                        });
                    }
                }
                self.pos -= 1;
            }
            if matches!(
                next,
                TokenKind::Eq | TokenKind::Bind | TokenKind::MatchAssign
            ) {
                let name = self.consume_var()?;
                let op = if self.match_kind(TokenKind::Eq) {
                    AssignOp::Assign
                } else if self.match_kind(TokenKind::Bind) {
                    AssignOp::Bind
                } else {
                    self.consume_kind(TokenKind::MatchAssign)?;
                    AssignOp::MatchAssign
                };
                let expr = self.parse_comma_expr()?;
                let stmt = Stmt::Assign { name, expr, op };
                return self.parse_statement_modifier(stmt);
            }
        }
        if let Some(TokenKind::ArrayVar(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            let name = name.clone();
            if matches!(self.peek_next_kind(), Some(TokenKind::LBracket)) {
                let name = format!("@{}", name);
                self.pos += 1;
                if let Some((meta, op, consumed)) = self.try_bracket_meta_op_at(self.pos) {
                    self.pos += consumed;
                    if self.match_kind(TokenKind::Eq) {
                        let rhs = self.parse_comma_expr()?;
                        self.match_kind(TokenKind::Semicolon);
                        let expr = Expr::MetaOp {
                            meta,
                            op,
                            left: Box::new(Expr::ArrayVar(name[1..].to_string())),
                            right: Box::new(rhs),
                        };
                        return Ok(Stmt::Assign {
                            name,
                            expr,
                            op: AssignOp::Assign,
                        });
                    }
                }
                self.pos -= 1;
            }
            if matches!(
                self.peek_next_kind(),
                Some(TokenKind::Eq | TokenKind::Bind | TokenKind::MatchAssign)
            ) {
                let name = format!("@{}", name);
                self.pos += 1;
                let op = if self.match_kind(TokenKind::Eq) {
                    AssignOp::Assign
                } else if self.match_kind(TokenKind::Bind) {
                    AssignOp::Bind
                } else {
                    self.consume_kind(TokenKind::MatchAssign)?;
                    AssignOp::MatchAssign
                };
                let expr = self.parse_comma_expr()?;
                let stmt = Stmt::Assign { name, expr, op };
                return self.parse_statement_modifier(stmt);
            }
        }
        // Handle %hash = (...) and %hash .= method()
        if let Some(TokenKind::HashVar(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            let name = name.clone();
            if matches!(
                self.peek_next_kind(),
                Some(TokenKind::Eq | TokenKind::Bind | TokenKind::MatchAssign)
            ) {
                let name = format!("%{}", name);
                self.pos += 1;
                let op = if self.match_kind(TokenKind::Eq) {
                    AssignOp::Assign
                } else if self.match_kind(TokenKind::Bind) {
                    AssignOp::Bind
                } else {
                    self.consume_kind(TokenKind::MatchAssign)?;
                    AssignOp::MatchAssign
                };
                let expr = self.parse_comma_expr()?;
                let stmt = Stmt::Assign { name, expr, op };
                return self.parse_statement_modifier(stmt);
            }
            if matches!(self.peek_next_kind(), Some(TokenKind::DotEq)) {
                let name = format!("%{}", name);
                self.pos += 1;
                self.match_kind(TokenKind::DotEq);
                let method_name = self.consume_ident()?;
                let mut method_args = Vec::new();
                if self.match_kind(TokenKind::LParen) {
                    if !self.check(&TokenKind::RParen) {
                        method_args.push(self.parse_method_arg());
                        while self.match_kind(TokenKind::Comma) {
                            method_args.push(self.parse_method_arg());
                        }
                    }
                    self.consume_kind(TokenKind::RParen)?;
                }
                self.match_kind(TokenKind::Semicolon);
                let expr = Expr::MethodCall {
                    target: Box::new(Expr::HashVar(name[1..].to_string())),
                    name: method_name,
                    args: method_args,
                    modifier: None,
                };
                return Ok(Stmt::Assign {
                    name,
                    expr,
                    op: AssignOp::Assign,
                });
            }
        }
        let expr = self.parse_expr()?;
        // Check for index assignment: %hash{"key"} = val, %hash<key> = val, @arr[0] = val
        if matches!(expr, Expr::Index { .. })
            && matches!(
                self.tokens.get(self.pos).map(|t| &t.kind),
                Some(TokenKind::Eq | TokenKind::Bind)
            )
        {
            let is_bind = self.check(&TokenKind::Bind);
            self.pos += 1; // consume = or :=
            let value = self.parse_comma_expr()?;
            self.match_kind(TokenKind::Semicolon);
            if let Expr::Index { target, index } = expr {
                if is_bind {
                    // For now, treat := like = for hash elements
                    return Ok(Stmt::Expr(Expr::IndexAssign {
                        target,
                        index,
                        value: Box::new(value),
                    }));
                }
                return Ok(Stmt::Expr(Expr::IndexAssign {
                    target,
                    index,
                    value: Box::new(value),
                }));
            }
        }
        // Collect trailing comma-separated expressions into a list
        // (e.g. `(do { ... }), $i;` at statement level)
        let expr = if self.match_kind(TokenKind::Comma) {
            let mut items = vec![expr];
            if !self.check(&TokenKind::Semicolon)
                && !self.check(&TokenKind::Eof)
                && !self.check(&TokenKind::RBrace)
                && !self.check(&TokenKind::RParen)
            {
                items.push(self.parse_expr()?);
                while self.match_kind(TokenKind::Comma) {
                    if self.check(&TokenKind::Semicolon)
                        || self.check(&TokenKind::Eof)
                        || self.check(&TokenKind::RBrace)
                        || self.check(&TokenKind::RParen)
                    {
                        break;
                    }
                    items.push(self.parse_expr()?);
                }
            }
            Expr::ArrayLiteral(items)
        } else {
            expr
        };
        let stmt = Stmt::Expr(expr);
        self.parse_statement_modifier(stmt)
    }
}
