#![allow(clippy::result_large_err)]
use crate::ast::{AssignOp, CallArg, Expr, ParamDef, PhaserKind, Stmt};
use crate::lexer::{Token, TokenKind};
use crate::value::{RuntimeError, Value};

pub(crate) struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub(crate) fn parse_program(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
        crate::trace::trace_log!("parse", "parse_program: {} tokens", self.tokens.len());
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::Eof) {
            let start = self.pos;
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(e)
                    if e.message.contains("X::Obsolete")
                        || e.message.contains("X::Comp")
                        || e.message.contains("X::Syntax") =>
                {
                    return Err(e);
                }
                Err(_) => {
                    self.recover_to_delim();
                    if self.pos == start && !self.check(&TokenKind::Eof) {
                        self.pos += 1;
                    }
                }
            }
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, RuntimeError> {
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
            } else if self.check(&TokenKind::Lt) {
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

    fn parse_enum_decl(&mut self) -> Result<Stmt, RuntimeError> {
        let name = self.consume_ident()?;
        let variants = if self.check(&TokenKind::Lt) {
            self.parse_enum_angle_variants()?
        } else if self.match_kind(TokenKind::LParen) {
            self.parse_enum_paren_variants()?
        } else {
            return Err(RuntimeError::new("Expected < or ( after enum name"));
        };
        self.match_kind(TokenKind::Semicolon);
        Ok(Stmt::EnumDecl { name, variants })
    }

    fn parse_enum_angle_variants(&mut self) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
        self.consume_kind(TokenKind::Lt)?;
        let mut variants = Vec::new();
        while !self.check(&TokenKind::Gt) && !self.check(&TokenKind::Eof) {
            let key = self.consume_ident()?;
            variants.push((key, None));
        }
        self.consume_kind(TokenKind::Gt)?;
        Ok(variants)
    }

    fn parse_enum_paren_variants(&mut self) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
        let mut variants = Vec::new();
        while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
            let key = self.consume_ident()?;
            let value = if self.match_kind(TokenKind::FatArrow) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            variants.push((key, value));
            self.match_kind(TokenKind::Comma);
        }
        self.consume_kind(TokenKind::RParen)?;
        Ok(variants)
    }

    /// Parse an if/elsif/else chain (assumes the `if` keyword has already been consumed).
    fn parse_if_chain(&mut self) -> Result<Stmt, RuntimeError> {
        let cond = self.parse_expr()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.match_ident("elsif") {
            let inner = self.parse_if_chain()?;
            vec![inner]
        } else if self.match_ident("else") {
            self.parse_block()?
        } else {
            Vec::new()
        };
        Ok(Stmt::If {
            cond,
            then_branch,
            else_branch,
        })
    }

    pub(crate) fn parse_block(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
        self.consume_kind(TokenKind::LBrace)?;
        self.parse_block_body()
    }

    fn parse_block_body(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            let start = self.pos;
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(_) => {
                    self.recover_to_delim();
                    if self.pos == start && !self.check(&TokenKind::Eof) {
                        self.pos += 1;
                    }
                }
            }
        }
        self.consume_kind(TokenKind::RBrace)?;
        Ok(stmts)
    }

    fn parse_statement_modifier(&mut self, stmt: Stmt) -> Result<Stmt, RuntimeError> {
        // Check for obsolete do...while/until/for/given
        if matches!(&stmt, Stmt::Expr(Expr::DoBlock { .. }))
            && let Some(kw) = self.peek_ident()
            && matches!(kw.as_str(), "while" | "until" | "for" | "given")
        {
            return Err(RuntimeError::new(format!(
                "Unsupported use of do...{kw} loop; in Raku please use repeat...while instead (X::Obsolete)"
            )));
        }
        if self.match_ident("if") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::If {
                cond,
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            });
        }
        if self.match_ident("unless") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::If {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            });
        }
        if self.match_ident("for") {
            let iterable = self.parse_comma_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::For {
                iterable,
                param: None,
                params: Vec::new(),
                body: vec![stmt],
                label: None,
            });
        }
        if self.match_ident("while") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::While {
                cond,
                body: vec![stmt],
                label: None,
            });
        }
        if self.match_ident("until") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body: vec![stmt],
                label: None,
            });
        }
        if self.match_ident("with") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(cond),
                    name: "defined".to_string(),
                    args: Vec::new(),
                    modifier: None,
                },
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            });
        }
        if self.match_ident("without") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
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
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            });
        }
        if self.match_ident("given") {
            let topic = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Given {
                topic,
                body: vec![stmt],
            });
        }
        self.match_kind(TokenKind::Semicolon);
        Ok(stmt)
    }

    fn parse_call_args(&mut self) -> Result<Vec<CallArg>, RuntimeError> {
        let mut args = Vec::new();
        if self.check(&TokenKind::LParen) && self.is_call_paren() {
            self.match_kind(TokenKind::LParen);
            if !self.check(&TokenKind::RParen) {
                let expr = self.parse_expr_or_true();
                args.push(CallArg::Positional(expr));
                while self.match_kind(TokenKind::Comma) {
                    args.push(self.parse_call_arg()?);
                }
            }
            self.match_kind(TokenKind::RParen);
            if self.match_kind(TokenKind::Comma) {
                args.push(self.parse_call_arg()?);
                while self.match_kind(TokenKind::Comma) {
                    args.push(self.parse_call_arg()?);
                }
            }
            return Ok(args);
        }
        if self.match_kind(TokenKind::Slash) {
            let mut content = String::new();
            while !self.check(&TokenKind::Slash) && !self.check(&TokenKind::Eof) {
                if let Some(token) = self.advance_if(|_| true) {
                    match token.kind {
                        TokenKind::Ident(s) => content.push_str(&s),
                        TokenKind::Number(n) => content.push_str(&n.to_string()),
                        TokenKind::BigNumber(n) => content.push_str(&n.to_string()),
                        TokenKind::Str(s) => content.push_str(&s),
                        _ => {}
                    }
                } else {
                    break;
                }
            }
            self.match_kind(TokenKind::Slash);
            args.push(CallArg::Positional(Expr::Literal(Value::Str(content))));
            while self.match_kind(TokenKind::Comma) {
                args.push(self.parse_call_arg()?);
            }
            return Ok(args);
        }
        if self.check(&TokenKind::Semicolon) || self.check(&TokenKind::Eof) {
            return Ok(args);
        }
        args.push(self.parse_call_arg()?);
        while self.match_kind(TokenKind::Comma) {
            args.push(self.parse_call_arg()?);
        }
        Ok(args)
    }

    fn parse_is_call_args(&mut self) -> Vec<CallArg> {
        let mut args = Vec::new();
        if self.check(&TokenKind::Semicolon) || self.check(&TokenKind::Eof) {
            return args;
        }
        let left = self.parse_expr_until_delim();
        args.push(CallArg::Positional(left));
        if self.match_kind(TokenKind::Comma) {
            let right = self.parse_expr_until_delim();
            args.push(CallArg::Positional(right));
            if self.match_kind(TokenKind::Comma) {
                let desc = self.parse_expr_until_delim();
                args.push(CallArg::Positional(desc));
            }
        }
        args
    }

    fn parse_expr_until_delim(&mut self) -> Expr {
        let start = self.pos;
        let mut depth = 0usize;
        while let Some(token) = self.tokens.get(self.pos) {
            match token.kind {
                TokenKind::Eof => break,
                TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
                TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                    if depth > 0 {
                        depth -= 1;
                    } else {
                        break;
                    }
                }
                TokenKind::Comma | TokenKind::Semicolon if depth == 0 => break,
                // Stop at statement modifier keywords at depth 0
                TokenKind::Ident(ref kw)
                    if depth == 0
                        && matches!(kw.as_str(), "for" | "if" | "unless" | "while" | "until") =>
                {
                    break;
                }
                _ => {}
            }
            self.pos += 1;
        }
        let mut slice = self.tokens[start..self.pos].to_vec();
        slice.push(Token {
            kind: TokenKind::Eof,
            line: 0,
        });
        let mut parser = Parser::new(slice);
        parser
            .parse_expr()
            .unwrap_or(Expr::Literal(Value::Bool(true)))
    }

    fn is_call_paren(&self) -> bool {
        let mut depth = 0usize;
        let mut i = self.pos;
        while let Some(token) = self.tokens.get(i) {
            match token.kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        let next = self.tokens.get(i + 1).map(|t| &t.kind);
                        return matches!(
                            next,
                            Some(
                                TokenKind::Comma
                                    | TokenKind::Semicolon
                                    | TokenKind::Eof
                                    | TokenKind::RParen
                                    | TokenKind::RBrace
                            )
                        );
                    }
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn is_is_grouping_paren(&self) -> bool {
        if !self.check(&TokenKind::LParen) {
            return false;
        }
        let mut depth = 0usize;
        let mut i = self.pos;
        while let Some(token) = self.tokens.get(i) {
            match token.kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        let next = self.tokens.get(i + 1).map(|t| &t.kind);
                        return matches!(next, Some(TokenKind::Comma));
                    }
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn parse_call_arg(&mut self) -> Result<CallArg, RuntimeError> {
        if self.match_kind(TokenKind::Colon) {
            let name = self.consume_ident()?;
            if self.match_kind(TokenKind::LParen) {
                let value = self.parse_expr()?;
                self.consume_kind(TokenKind::RParen)?;
                return Ok(CallArg::Named {
                    name,
                    value: Some(value),
                });
            }
            if self.match_kind(TokenKind::LBracket) {
                let mut items = Vec::new();
                if !self.check(&TokenKind::RBracket) {
                    items.push(self.parse_expr_or_true());
                    while self.match_kind(TokenKind::Comma) {
                        items.push(self.parse_expr_or_true());
                    }
                }
                self.match_kind(TokenKind::RBracket);
                return Ok(CallArg::Named {
                    name,
                    value: Some(Expr::ArrayLiteral(items)),
                });
            }
            return Ok(CallArg::Named { name, value: None });
        }
        if let Some(name) = self.peek_ident()
            && matches!(self.peek_next_kind(), Some(TokenKind::FatArrow))
        {
            self.pos += 1;
            self.consume_kind(TokenKind::FatArrow)?;
            let value = self.parse_expr()?;
            return Ok(CallArg::Named {
                name,
                value: Some(value),
            });
        }
        match self.parse_expr() {
            Ok(expr) => Ok(CallArg::Positional(expr)),
            Err(_) => {
                self.recover_to_delim();
                if self.match_kind(TokenKind::Comma) {
                    // consume delimiter so parsing can continue
                }
                Ok(CallArg::Positional(Expr::Literal(Value::Bool(true))))
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, RuntimeError> {
        // Handle $var = expr as assignment expression
        if let Some(TokenKind::Var(name)) = self.tokens.get(self.pos).map(|t| &t.kind)
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Eq)
            )
        {
            let name = name.clone();
            self.pos += 2;
            let expr = self.parse_expr()?;
            return Ok(Expr::AssignExpr {
                name,
                expr: Box::new(expr),
            });
        }
        // Handle $var += expr, $var -= expr, etc. as compound assignment expression
        if let Some(TokenKind::Var(name)) = self.tokens.get(self.pos).map(|t| &t.kind)
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(
                    TokenKind::PlusEq | TokenKind::MinusEq | TokenKind::TildeEq | TokenKind::StarEq
                )
            )
        {
            let name = name.clone();
            self.pos += 1;
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
            let rhs = self.parse_expr()?;
            return Ok(Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::Var(name)),
                    op: compound_op,
                    right: Box::new(rhs),
                }),
            });
        }
        let mut expr = self.parse_or()?;
        // Handle => (fat arrow / pair constructor)
        if self.match_kind(TokenKind::FatArrow) {
            let value = self.parse_or()?;
            // Auto-quote bareword on LHS of => to a string literal
            let left = match expr {
                Expr::BareWord(ref name) => Expr::Literal(Value::Str(name.clone())),
                _ => expr,
            };
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: TokenKind::FatArrow,
                right: Box::new(value),
            });
        }
        // Wrap WhateverCode expressions (e.g., * > 3, ?*, !*) in a lambda
        if Self::contains_whatever(&expr) {
            let body_expr = Self::replace_whatever(&expr);
            expr = Expr::Lambda {
                param: "_".to_string(),
                body: vec![Stmt::Expr(body_expr)],
            };
        }
        Ok(expr)
    }

    fn parse_comma_expr(&mut self) -> Result<Expr, RuntimeError> {
        // Collect comma-separated items
        let mut exprs = Vec::new();
        exprs.push(self.parse_expr()?);
        while self.match_kind(TokenKind::Comma) {
            // Before parsing next item, check if next token is Z/X list infix or ...
            if self.try_meta_op_zx().is_some() {
                break;
            }
            if self.check(&TokenKind::DotDotDot) || self.check(&TokenKind::DotDotDotCaret) {
                break;
            }
            // Allow trailing comma (before ), ], etc.)
            if self.check(&TokenKind::RParen)
                || self.check(&TokenKind::RBracket)
                || self.check(&TokenKind::Semicolon)
                || self.check(&TokenKind::Eof)
            {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        let left = if exprs.len() == 1 {
            exprs.remove(0)
        } else {
            Expr::ArrayLiteral(exprs)
        };

        // Check for Z/X list infix operators (looser than comma)
        if let Some((meta, op, consumed)) = self.try_meta_op_zx() {
            self.pos += consumed;
            let right = self.parse_comma_expr()?;
            return Ok(Expr::MetaOp {
                meta,
                op,
                left: Box::new(left),
                right: Box::new(right),
            });
        }

        // Check for ... or ...^ (sequence operator, looser than comma)
        if self.match_kind(TokenKind::DotDotDot) {
            let right = self.parse_comma_expr()?;
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotDot,
                right: Box::new(right),
            });
        }
        if self.match_kind(TokenKind::DotDotDotCaret) {
            let right = self.parse_comma_expr()?;
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotDotCaret,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_expr_list(&mut self) -> Result<Vec<Expr>, RuntimeError> {
        let first = self.parse_expr()?;
        let mut exprs = vec![first];
        while self.match_kind(TokenKind::Comma) {
            // Stop if next token looks like a statement modifier keyword
            if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind)
                && matches!(name.as_str(), "if" | "unless" | "for" | "while" | "until")
            {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_method_arg(&mut self) -> Expr {
        // Colonpair as named arg: :name, :name<val>, :name(expr)
        // But :42name and :!name are colonpair expressions (Pair values), not named args
        if self.check(&TokenKind::Colon)
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Ident(_))
            )
            && !matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Number(_) | TokenKind::BigNumber(_))
            )
        {
            self.match_kind(TokenKind::Colon);
            let name = self.consume_ident().unwrap_or_default();
            let value = if self.check(&TokenKind::Lt) {
                self.parse_angle_literal()
            } else if self.match_kind(TokenKind::LParen) {
                let expr = self.parse_expr_or_true();
                self.match_kind(TokenKind::RParen);
                expr
            } else if self.check(&TokenKind::RParen)
                || self.check(&TokenKind::Comma)
                || self.check(&TokenKind::Semicolon)
            {
                // Bare colonpair :name without value → True
                Expr::Literal(Value::Bool(true))
            } else {
                self.parse_expr_or_true()
            };
            return Expr::AssignExpr {
                name,
                expr: Box::new(value),
            };
        }
        // Handle name => value (pair/named arg syntax)
        if let Some(name) = self.peek_ident()
            && matches!(self.peek_next_kind(), Some(TokenKind::FatArrow))
        {
            self.pos += 1; // consume ident
            self.pos += 1; // consume =>
            let value = self.parse_expr_or_true();
            return Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name))),
                op: TokenKind::FatArrow,
                right: Box::new(value),
            };
        }
        self.parse_expr_or_true()
    }

    fn parse_angle_literal(&mut self) -> Expr {
        if !self.match_kind(TokenKind::Lt) {
            return Expr::Literal(Value::Nil);
        }
        let mut words = Vec::new();
        while let Some(token) = self.advance_if(|k| {
            matches!(
                k,
                TokenKind::Ident(_)
                    | TokenKind::Str(_)
                    | TokenKind::Number(_)
                    | TokenKind::BigNumber(_)
                    | TokenKind::Float(_)
                    | TokenKind::Minus
            )
        }) {
            let word = match token.kind {
                TokenKind::Ident(s) => s,
                TokenKind::Str(s) => s,
                TokenKind::Number(n) => n.to_string(),
                TokenKind::BigNumber(n) => n.to_string(),
                TokenKind::Float(f) => f.to_string(),
                TokenKind::Minus => {
                    if let Some(next) = self.advance_if(|k| {
                        matches!(
                            k,
                            TokenKind::Number(_) | TokenKind::BigNumber(_) | TokenKind::Float(_)
                        )
                    }) {
                        match next.kind {
                            TokenKind::Number(n) => format!("-{}", n),
                            TokenKind::BigNumber(n) => format!("-{}", n),
                            TokenKind::Float(f) => format!("-{}", f),
                            _ => "-".to_string(),
                        }
                    } else {
                        "-".to_string()
                    }
                }
                _ => String::new(),
            };
            words.push(word);
        }
        self.match_kind(TokenKind::Gt);
        if words.len() == 1 {
            Expr::Literal(Value::Str(words.into_iter().next().unwrap()))
        } else {
            Expr::ArrayLiteral(
                words
                    .into_iter()
                    .map(|w| Expr::Literal(Value::Str(w)))
                    .collect(),
            )
        }
    }

    fn parse_ternary(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_tight_or()?;
        if self.match_kind(TokenKind::QuestionQuestion) {
            let then_expr = self.parse_ternary()?;
            if !self.match_kind(TokenKind::BangBang) {
                return Err(RuntimeError::new("Expected !! in ternary"));
            }
            let else_expr = self.parse_ternary()?;
            expr = Expr::Ternary {
                cond: Box::new(expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            };
        }
        Ok(expr)
    }

    fn parse_expr_or_true(&mut self) -> Expr {
        match self.parse_expr() {
            Ok(expr) => expr,
            Err(_) => {
                self.recover_to_delim();
                Expr::Literal(Value::Bool(true))
            }
        }
    }

    fn recover_to_delim(&mut self) {
        while !self.check(&TokenKind::Comma)
            && !self.check(&TokenKind::Semicolon)
            && !self.check(&TokenKind::RParen)
            && !self.check(&TokenKind::RBrace)
            && !self.check(&TokenKind::Eof)
        {
            self.pos += 1;
        }
    }

    // Loose `or` / `orelse` — lowest precedence binary ops
    fn parse_or(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_and()?;
        loop {
            if self.match_kind(TokenKind::OrWord) {
                let right = self.parse_and()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::OrWord,
                    right: Box::new(right),
                };
            } else if self.match_kind(TokenKind::OrElse) {
                let right = self.parse_and()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::OrElse,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // Loose `and` / `andthen` — lower precedence than ?? !!
    fn parse_and(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_ternary()?;
        loop {
            if self.match_ident("and") {
                let right = self.parse_ternary()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::AndAnd,
                    right: Box::new(right),
                };
            } else if self.match_kind(TokenKind::AndThen) {
                let right = self.parse_ternary()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::AndThen,
                    right: Box::new(right),
                };
            } else if self.match_kind(TokenKind::NotAndThen) {
                let right = self.parse_ternary()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::NotAndThen,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // Tight `||` / `//` — higher precedence than ?? !!
    fn parse_tight_or(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_tight_and()?;
        loop {
            if self.match_kind(TokenKind::OrOr) {
                let right = self.parse_tight_and()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::OrOr,
                    right: Box::new(right),
                };
            } else if self.match_kind(TokenKind::SlashSlash) {
                let right = self.parse_tight_and()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::SlashSlash,
                    right: Box::new(right),
                };
            } else if self.check(&TokenKind::Bang)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::OrOr)
                )
            {
                self.pos += 2; // consume ! and ||
                let right = self.parse_tight_and()?;
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        op: TokenKind::OrOr,
                        right: Box::new(right),
                    }),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // Tight `&&` — higher precedence than ?? !!
    fn parse_tight_and(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_equality()?;
        loop {
            if self.match_kind(TokenKind::AndAnd) {
                let right = self.parse_equality()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::AndAnd,
                    right: Box::new(right),
                };
            } else if self.check(&TokenKind::Bang)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::AndAnd)
                )
            {
                self.pos += 2; // consume ! and &&
                let right = self.parse_equality()?;
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        op: TokenKind::AndAnd,
                        right: Box::new(right),
                    }),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_junction(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_comparison()?;
        loop {
            let op = if self.match_kind(TokenKind::Ampersand) {
                Some(TokenKind::Ampersand)
            } else if self.match_kind(TokenKind::Pipe) {
                Some(TokenKind::Pipe)
            } else if self.match_kind(TokenKind::Caret) {
                Some(TokenKind::Caret)
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_comparison()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_junction()?;
        loop {
            // Doubled prefix !! is illegal (e.g. !!eq)
            if self.check(&TokenKind::Bang)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Bang)
                )
            {
                return Err(RuntimeError::new(
                    "X::Syntax::Confused: Confused".to_string(),
                ));
            }
            // Negated relational operators: !eq, !==, !===
            if self.check(&TokenKind::Bang)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Ident(n)) if n == "eq"
                )
            {
                self.pos += 2;
                let right = self.parse_junction()?;
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        op: TokenKind::Ident("eq".to_string()),
                        right: Box::new(right),
                    }),
                };
            } else if self.check(&TokenKind::BangEq)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::EqEq)
                )
            {
                self.pos += 2;
                let right = self.parse_junction()?;
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        op: TokenKind::EqEqEq,
                        right: Box::new(right),
                    }),
                };
            } else if self.check(&TokenKind::BangEq)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Eq)
                )
            {
                self.pos += 2;
                let right = self.parse_junction()?;
                expr = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        op: TokenKind::EqEq,
                        right: Box::new(right),
                    }),
                };
            } else if self.match_kind(TokenKind::EqEq) {
                let op = TokenKind::EqEq;
                let right = self.parse_junction()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                };
            } else if self.match_kind(TokenKind::EqEqEq) {
                let op = TokenKind::EqEqEq;
                let right = self.parse_junction()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                };
            } else if self.match_kind(TokenKind::BangEq) {
                let op = TokenKind::BangEq;
                let right = self.parse_junction()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                };
            } else if self.match_ident("eq") {
                let right = self.parse_junction()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::Ident("eq".to_string()),
                    right: Box::new(right),
                };
            } else if self.match_ident("ne") {
                let right = self.parse_junction()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::Ident("ne".to_string()),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_meta_op()?;
        // Track the previous right operand and operator for chained comparisons.
        // e.g., `a eqv b eqv c` should become `(a eqv b) && (b eqv c)`.
        let mut prev_right: Option<Expr> = None;
        let mut prev_op: Option<TokenKind> = None;
        loop {
            let op = if self.match_kind(TokenKind::Lt) {
                Some(TokenKind::Lt)
            } else if self.match_kind(TokenKind::Lte) {
                Some(TokenKind::Lte)
            } else if self.match_kind(TokenKind::Gt) {
                Some(TokenKind::Gt)
            } else if self.match_kind(TokenKind::Gte) {
                Some(TokenKind::Gte)
            } else if self.match_kind(TokenKind::DotDot) {
                Some(TokenKind::DotDot)
            } else if self.match_kind(TokenKind::DotDotCaret) {
                Some(TokenKind::DotDotCaret)
            } else if self.match_kind(TokenKind::CaretDotDot) {
                Some(TokenKind::CaretDotDot)
            } else if self.match_kind(TokenKind::CaretDotDotCaret) {
                Some(TokenKind::CaretDotDotCaret)
            } else if self.match_kind(TokenKind::SmartMatch) {
                Some(TokenKind::SmartMatch)
            } else if self.match_kind(TokenKind::BangTilde) {
                Some(TokenKind::BangTilde)
            } else if self.match_ident("lt") {
                Some(TokenKind::Ident("lt".to_string()))
            } else if self.match_ident("le") {
                Some(TokenKind::Ident("le".to_string()))
            } else if self.match_ident("gt") {
                Some(TokenKind::Ident("gt".to_string()))
            } else if self.match_ident("ge") {
                Some(TokenKind::Ident("ge".to_string()))
            } else if self.match_kind(TokenKind::LtEqGt) {
                Some(TokenKind::LtEqGt)
            } else if self.match_ident("leg") {
                Some(TokenKind::Ident("leg".to_string()))
            } else if self.match_ident("cmp") {
                Some(TokenKind::Ident("cmp".to_string()))
            } else if self.match_ident("eqv") {
                Some(TokenKind::Ident("eqv".to_string()))
            } else if self.match_kind(TokenKind::SetElem) {
                Some(TokenKind::SetElem)
            } else if self.match_kind(TokenKind::SetCont) {
                Some(TokenKind::SetCont)
            } else if self.match_kind(TokenKind::SetSubset) {
                Some(TokenKind::SetSubset)
            } else if self.match_kind(TokenKind::SetSuperset) {
                Some(TokenKind::SetSuperset)
            } else if self.match_kind(TokenKind::SetStrictSubset) {
                Some(TokenKind::SetStrictSubset)
            } else if self.match_kind(TokenKind::SetStrictSuperset) {
                Some(TokenKind::SetStrictSuperset)
            } else if self.match_kind(TokenKind::SetUnion) {
                Some(TokenKind::SetUnion)
            } else if self.match_kind(TokenKind::SetIntersect) {
                Some(TokenKind::SetIntersect)
            } else if self.match_kind(TokenKind::SetDiff) {
                Some(TokenKind::SetDiff)
            } else if self.match_kind(TokenKind::SetSymDiff) {
                Some(TokenKind::SetSymDiff)
            } else if self.match_ident("before") {
                Some(TokenKind::Ident("before".to_string()))
            } else if self.match_ident("after") {
                Some(TokenKind::Ident("after".to_string()))
            } else if self.match_ident("but") {
                Some(TokenKind::Ident("but".to_string()))
            } else {
                None
            };
            // Check for negation meta-operator: ! followed by comparison op
            let (op, negate) = if op.is_some() {
                (op, false)
            } else if self.check(&TokenKind::Bang) {
                let saved = self.pos;
                self.pos += 1; // consume !
                // Doubled prefix !! is illegal
                if self.check(&TokenKind::Bang) {
                    return Err(RuntimeError::new(
                        "X::Syntax::Confused: Confused".to_string(),
                    ));
                }
                let negated_op = if self.match_kind(TokenKind::Lt) {
                    Some(TokenKind::Lt)
                } else if self.match_kind(TokenKind::Lte) {
                    Some(TokenKind::Lte)
                } else if self.match_kind(TokenKind::Gt) {
                    Some(TokenKind::Gt)
                } else if self.match_kind(TokenKind::Gte) {
                    Some(TokenKind::Gte)
                } else if self.match_kind(TokenKind::EqEq) {
                    Some(TokenKind::EqEq)
                } else if self.match_kind(TokenKind::EqEqEq) {
                    Some(TokenKind::EqEqEq)
                } else if self.match_ident("lt") {
                    Some(TokenKind::Ident("lt".to_string()))
                } else if self.match_ident("le") {
                    Some(TokenKind::Ident("le".to_string()))
                } else if self.match_ident("gt") {
                    Some(TokenKind::Ident("gt".to_string()))
                } else if self.match_ident("ge") {
                    Some(TokenKind::Ident("ge".to_string()))
                } else if self.match_ident("eq") {
                    Some(TokenKind::Ident("eq".to_string()))
                } else if self.match_ident("eqv") {
                    Some(TokenKind::Ident("eqv".to_string()))
                } else if self.match_ident("before") {
                    Some(TokenKind::Ident("before".to_string()))
                } else if self.match_ident("after") {
                    Some(TokenKind::Ident("after".to_string()))
                } else {
                    self.pos = saved; // backtrack
                    None
                };
                if let Some(inner) = negated_op {
                    (Some(inner), true)
                } else {
                    (None, false)
                }
            } else {
                (None, false)
            };
            if let Some(op) = op {
                let right = self.parse_meta_op()?;
                // Check if this is a chainable comparison operator
                let is_chainable = Self::is_chainable_comparison(&op);
                if is_chainable && prev_op.is_some() {
                    // Chained comparison: `prev_right op right`
                    let mut new_cmp = Expr::Binary {
                        left: Box::new(prev_right.unwrap()),
                        op: op.clone(),
                        right: Box::new(right.clone()),
                    };
                    if negate {
                        new_cmp = Expr::Unary {
                            op: TokenKind::Bang,
                            expr: Box::new(new_cmp),
                        };
                    }
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: TokenKind::AndAnd,
                        right: Box::new(new_cmp),
                    };
                } else {
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: op.clone(),
                        right: Box::new(right.clone()),
                    };
                    if negate {
                        expr = Expr::Unary {
                            op: TokenKind::Bang,
                            expr: Box::new(expr),
                        };
                    }
                }
                if is_chainable {
                    prev_right = Some(right);
                    prev_op = Some(op);
                } else {
                    prev_right = None;
                    prev_op = None;
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Check if a comparison operator is chainable (e.g., `a eqv b eqv c`).
    fn is_chainable_comparison(op: &TokenKind) -> bool {
        match op {
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Lt
            | TokenKind::Lte
            | TokenKind::Gt
            | TokenKind::Gte
            | TokenKind::EqEqEq => true,
            TokenKind::Ident(name) => matches!(
                name.as_str(),
                "eqv" | "eq" | "ne" | "lt" | "le" | "gt" | "ge" | "before" | "after"
            ),
            _ => false,
        }
    }

    /// Detect R (reverse) meta-operator only. Z/X are handled at list infix level.
    fn try_meta_op(&self) -> Option<(String, String, usize)> {
        if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            if name == "R" {
                let meta = name.clone();
                let op = match self.tokens.get(self.pos + 1).map(|t| &t.kind) {
                    Some(TokenKind::Plus) => "+",
                    Some(TokenKind::Minus) => "-",
                    Some(TokenKind::Star) => "*",
                    Some(TokenKind::StarStar) => "**",
                    Some(TokenKind::Slash) => "/",
                    Some(TokenKind::Percent) => "%",
                    Some(TokenKind::Tilde) => "~",
                    Some(TokenKind::EqEq) => "==",
                    Some(TokenKind::BangEq) => "!=",
                    Some(TokenKind::Lt) => "<",
                    Some(TokenKind::Gt) => ">",
                    Some(TokenKind::Lte) => "<=",
                    Some(TokenKind::Gte) => ">=",
                    Some(TokenKind::BitAnd) => "+&",
                    Some(TokenKind::BitOr) => "+|",
                    Some(TokenKind::BitXor) => "+^",
                    Some(TokenKind::Ident(s))
                        if matches!(
                            s.as_str(),
                            "eq" | "ne"
                                | "lt"
                                | "le"
                                | "gt"
                                | "ge"
                                | "min"
                                | "max"
                                | "x"
                                | "xx"
                                | "cmp"
                                | "leg"
                        ) =>
                    {
                        s.as_str()
                    }
                    _ => return None,
                };
                return Some((meta, op.to_string(), 2));
            }
            if name.len() > 1 && name.is_char_boundary(1) {
                let (meta, op) = name.split_at(1);
                if meta == "R"
                    && matches!(
                        op,
                        "eq" | "ne"
                            | "lt"
                            | "le"
                            | "gt"
                            | "ge"
                            | "min"
                            | "max"
                            | "x"
                            | "xx"
                            | "cmp"
                            | "leg"
                    )
                {
                    return Some((meta.to_string(), op.to_string(), 1));
                }
            }
        }
        None
    }

    /// Detect Z/X meta-operators at list infix precedence (looser than comma).
    /// Returns (meta, op, tokens_consumed). op="" for bare Z/X.
    fn try_meta_op_zx(&self) -> Option<(String, String, usize)> {
        if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            if matches!(name.as_str(), "X" | "Z") {
                let meta = name.clone();
                // Look ahead for operator token
                let op = match self.tokens.get(self.pos + 1).map(|t| &t.kind) {
                    Some(TokenKind::Plus) => "+",
                    Some(TokenKind::Minus) => "-",
                    Some(TokenKind::Star) => "*",
                    Some(TokenKind::StarStar) => "**",
                    Some(TokenKind::Slash) => "/",
                    Some(TokenKind::Percent) => "%",
                    Some(TokenKind::Tilde) => "~",
                    Some(TokenKind::Comma) => ",",
                    Some(TokenKind::EqEq) => "==",
                    Some(TokenKind::BangEq) => "!=",
                    Some(TokenKind::Lt) => {
                        // Disambiguate: Z< (zip-less-than) vs Z <word list>
                        // If < is followed by ident/number tokens ending with >,
                        // treat as bare Z followed by angle bracket word list.
                        let mut j = self.pos + 2;
                        let mut looks_like_qw = false;
                        while j < self.tokens.len() {
                            match &self.tokens[j].kind {
                                TokenKind::Ident(_)
                                | TokenKind::Number(_)
                                | TokenKind::BigNumber(_)
                                | TokenKind::Str(_)
                                | TokenKind::Float(_)
                                | TokenKind::Minus => j += 1,
                                TokenKind::Gt => {
                                    looks_like_qw = true;
                                    break;
                                }
                                _ => break,
                            }
                        }
                        if looks_like_qw {
                            return Some((meta, "".to_string(), 1));
                        }
                        "<"
                    }
                    Some(TokenKind::Gt) => ">",
                    Some(TokenKind::Lte) => "<=",
                    Some(TokenKind::Gte) => ">=",
                    Some(TokenKind::BitAnd) => "+&",
                    Some(TokenKind::BitOr) => "+|",
                    Some(TokenKind::BitXor) => "+^",
                    Some(TokenKind::FatArrow) => "=>",
                    Some(TokenKind::Ident(s))
                        if matches!(
                            s.as_str(),
                            "eq" | "ne"
                                | "lt"
                                | "le"
                                | "gt"
                                | "ge"
                                | "min"
                                | "max"
                                | "x"
                                | "xx"
                                | "cmp"
                                | "leg"
                                | "and"
                                | "or"
                                | "andthen"
                                | "orelse"
                        ) =>
                    {
                        s.as_str()
                    }
                    _ => {
                        // Bare Z/X (no operator suffix)
                        return Some((meta, "".to_string(), 1));
                    }
                };
                return Some((meta, op.to_string(), 2));
            }
            if name.len() > 1 && name.is_char_boundary(1) {
                let (meta, op) = name.split_at(1);
                if matches!(meta, "X" | "Z")
                    && matches!(
                        op,
                        "eq" | "ne"
                            | "lt"
                            | "le"
                            | "gt"
                            | "ge"
                            | "min"
                            | "max"
                            | "x"
                            | "xx"
                            | "cmp"
                            | "leg"
                            | "and"
                            | "or"
                            | "andthen"
                            | "orelse"
                    )
                {
                    return Some((meta.to_string(), op.to_string(), 1));
                }
            }
        }
        None
    }

    fn try_bracket_meta_op_at(&self, pos: usize) -> Option<(String, String, usize)> {
        if !matches!(
            self.tokens.get(pos).map(|t| &t.kind),
            Some(TokenKind::LBracket)
        ) {
            return None;
        }
        let op_from_kind = |kind: &TokenKind| -> Option<String> {
            match kind {
                TokenKind::Plus => Some("+".to_string()),
                TokenKind::Minus => Some("-".to_string()),
                TokenKind::Star => Some("*".to_string()),
                TokenKind::StarStar => Some("**".to_string()),
                TokenKind::Slash => Some("/".to_string()),
                TokenKind::Percent => Some("%".to_string()),
                TokenKind::Tilde => Some("~".to_string()),
                TokenKind::EqEq => Some("==".to_string()),
                TokenKind::BangEq => Some("!=".to_string()),
                TokenKind::Lt => Some("<".to_string()),
                TokenKind::Gt => Some(">".to_string()),
                TokenKind::Lte => Some("<=".to_string()),
                TokenKind::Gte => Some(">=".to_string()),
                TokenKind::BitAnd => Some("+&".to_string()),
                TokenKind::BitOr => Some("+|".to_string()),
                TokenKind::BitXor => Some("+^".to_string()),
                TokenKind::Ident(s)
                    if matches!(
                        s.as_str(),
                        "eq" | "ne"
                            | "lt"
                            | "le"
                            | "gt"
                            | "ge"
                            | "min"
                            | "max"
                            | "x"
                            | "xx"
                            | "cmp"
                            | "leg"
                    ) =>
                {
                    Some(s.clone())
                }
                _ => None,
            }
        };
        if let Some(TokenKind::Ident(name)) = self.tokens.get(pos + 1).map(|t| &t.kind) {
            if matches!(name.as_str(), "R" | "X" | "Z") {
                let meta = name.clone();
                if let Some(kind) = self.tokens.get(pos + 2).map(|t| &t.kind)
                    && let Some(op) = op_from_kind(kind)
                    && matches!(
                        self.tokens.get(pos + 3).map(|t| &t.kind),
                        Some(TokenKind::RBracket)
                    )
                {
                    return Some((meta, op, 4));
                }
            }
            if name.len() > 1 && name.is_char_boundary(1) {
                let (meta, op) = name.split_at(1);
                if matches!(meta, "R" | "X" | "Z")
                    && matches!(
                        op,
                        "eq" | "ne"
                            | "lt"
                            | "le"
                            | "gt"
                            | "ge"
                            | "min"
                            | "max"
                            | "x"
                            | "xx"
                            | "cmp"
                            | "leg"
                    )
                    && matches!(
                        self.tokens.get(pos + 2).map(|t| &t.kind),
                        Some(TokenKind::RBracket)
                    )
                {
                    return Some((meta.to_string(), op.to_string(), 3));
                }
            }
        }
        None
    }

    fn parse_meta_op(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_hyper()?;
        while let Some((meta, op, consumed)) = self.try_meta_op() {
            self.pos += consumed;
            let right = self.parse_hyper()?;
            expr = Expr::MetaOp {
                meta,
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_hyper(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_term()?;
        while self.is_hyper_op() {
            let dwim_left = self.check(&TokenKind::HyperLeft);
            // consume opener (<< or >>)
            self.pos += 1;
            let op = self.consume_hyper_inner_op()?;
            let dwim_right = self.check(&TokenKind::HyperRight);
            // consume closer (>> or <<)
            self.pos += 1;
            let right = self.parse_term()?;
            expr = Expr::HyperOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                dwim_left,
                dwim_right,
            };
        }
        Ok(expr)
    }

    fn is_hyper_op(&self) -> bool {
        let opener = self.tokens.get(self.pos).map(|t| &t.kind);
        if !matches!(
            opener,
            Some(TokenKind::HyperLeft) | Some(TokenKind::HyperRight)
        ) {
            return false;
        }
        // Look ahead for op + closer pattern
        let mut i = self.pos + 1;
        // skip inner op token(s) — some ops are two tokens (e.g. Star Star for **)
        match self.tokens.get(i).map(|t| &t.kind) {
            Some(
                TokenKind::Star
                | TokenKind::StarStar
                | TokenKind::EqEq
                | TokenKind::BangEq
                | TokenKind::Lte
                | TokenKind::Gte
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Slash
                | TokenKind::Tilde
                | TokenKind::Percent
                | TokenKind::BitAnd
                | TokenKind::BitOr
                | TokenKind::BitXor
                | TokenKind::Lt
                | TokenKind::Gt,
            ) => {
                i += 1;
            }
            _ => return false,
        }
        matches!(
            self.tokens.get(i).map(|t| &t.kind),
            Some(TokenKind::HyperLeft) | Some(TokenKind::HyperRight)
        )
    }

    fn consume_hyper_inner_op(&mut self) -> Result<String, RuntimeError> {
        let kind = self.tokens.get(self.pos).map(|t| &t.kind);
        let op = match kind {
            Some(TokenKind::Plus) => {
                self.pos += 1;
                "+".to_string()
            }
            Some(TokenKind::Minus) => {
                self.pos += 1;
                "-".to_string()
            }
            Some(TokenKind::StarStar) => {
                self.pos += 1;
                "**".to_string()
            }
            Some(TokenKind::Star) => {
                self.pos += 1;
                "*".to_string()
            }
            Some(TokenKind::Slash) => {
                self.pos += 1;
                "/".to_string()
            }
            Some(TokenKind::Tilde) => {
                self.pos += 1;
                "~".to_string()
            }
            Some(TokenKind::Percent) => {
                self.pos += 1;
                "%".to_string()
            }
            Some(TokenKind::EqEq) => {
                self.pos += 1;
                "==".to_string()
            }
            Some(TokenKind::BangEq) => {
                self.pos += 1;
                "!=".to_string()
            }
            Some(TokenKind::Lt) => {
                self.pos += 1;
                "<".to_string()
            }
            Some(TokenKind::Gt) => {
                self.pos += 1;
                ">".to_string()
            }
            Some(TokenKind::Lte) => {
                self.pos += 1;
                "<=".to_string()
            }
            Some(TokenKind::Gte) => {
                self.pos += 1;
                ">=".to_string()
            }
            Some(TokenKind::BitAnd) => {
                self.pos += 1;
                "+&".to_string()
            }
            Some(TokenKind::BitOr) => {
                self.pos += 1;
                "+|".to_string()
            }
            Some(TokenKind::BitXor) => {
                self.pos += 1;
                "+^".to_string()
            }
            _ => {
                return Err(RuntimeError::new(
                    "Expected operator inside hyper markers".to_string(),
                ));
            }
        };
        Ok(op)
    }

    // Concatenation (~) - lowest of the arithmetic-like operators
    fn parse_term(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_replication()?;
        loop {
            if let Some(infix) = self.parse_infix_func(expr.clone())? {
                expr = infix;
                continue;
            }
            if self.match_kind(TokenKind::Tilde) {
                let right = self.parse_replication()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::Tilde,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // Replication (x, xx) - between concatenation and additive
    fn parse_replication(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_additive()?;
        loop {
            if self.match_ident("xx") {
                let right = self.parse_additive()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::Ident("xx".to_string()),
                    right: Box::new(right),
                };
                continue;
            }
            if self.match_ident("x") {
                let right = self.parse_additive()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op: TokenKind::Ident("x".to_string()),
                    right: Box::new(right),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    // Additive (+, -) - between replication and multiplicative
    fn parse_additive(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_factor()?;
        loop {
            let op = if self.match_kind(TokenKind::Plus) {
                Some(TokenKind::Plus)
            } else if self.match_kind(TokenKind::Minus) {
                Some(TokenKind::Minus)
            } else if self.match_kind(TokenKind::BitAnd) {
                Some(TokenKind::BitAnd)
            } else if self.match_kind(TokenKind::BitOr) {
                Some(TokenKind::BitOr)
            } else if self.match_kind(TokenKind::BitXor) {
                Some(TokenKind::BitXor)
            } else if self.match_kind(TokenKind::BitShiftLeft) {
                Some(TokenKind::BitShiftLeft)
            } else if self.match_kind(TokenKind::BitShiftRight) {
                Some(TokenKind::BitShiftRight)
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_factor()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_power()?;
        loop {
            let op = if self.match_kind(TokenKind::Star) {
                Some(TokenKind::Star)
            } else if self.match_kind(TokenKind::Slash) {
                Some(TokenKind::Slash)
            } else if self.match_kind(TokenKind::Percent) {
                Some(TokenKind::Percent)
            } else if self.match_kind(TokenKind::PercentPercent) {
                Some(TokenKind::PercentPercent)
            } else if self.match_ident("div") {
                Some(TokenKind::Ident("div".to_string()))
            } else if self.match_ident("mod") {
                Some(TokenKind::Ident("mod".to_string()))
            } else if self.match_ident("gcd") {
                Some(TokenKind::Ident("gcd".to_string()))
            } else if self.match_ident("lcm") {
                Some(TokenKind::Ident("lcm".to_string()))
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_power()?;
                expr = Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    // Exponentiation (**) - right-associative, tighter than multiplicative
    fn parse_power(&mut self) -> Result<Expr, RuntimeError> {
        let base = self.parse_unary()?;
        if self.match_kind(TokenKind::StarStar) {
            let exp = self.parse_power()?; // right-associative
            Ok(Expr::Binary {
                left: Box::new(base),
                op: TokenKind::StarStar,
                right: Box::new(exp),
            })
        } else {
            Ok(base)
        }
    }

    fn parse_infix_func(&mut self, left: Expr) -> Result<Option<Expr>, RuntimeError> {
        let mut modifier = None;
        let start_pos = self.pos;
        if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind)
            && (name == "R" || name == "X")
        {
            modifier = Some(name.clone());
            self.pos += 1;
        }
        if !self.match_kind(TokenKind::LBracket) {
            self.pos = start_pos;
            return Ok(None);
        }
        if !self.match_kind(TokenKind::Ampersand) {
            self.pos = start_pos;
            return Ok(None);
        }
        let name = self.consume_ident()?;
        self.consume_kind(TokenKind::RBracket)?;
        let mut right = Vec::new();
        right.push(self.parse_unary()?);
        if modifier.as_deref() == Some("X") {
            while self.match_kind(TokenKind::Comma) {
                right.push(self.parse_unary()?);
            }
        }
        Ok(Some(Expr::InfixFunc {
            name,
            left: Box::new(left),
            right,
            modifier,
        }))
    }

    fn parse_unary(&mut self) -> Result<Expr, RuntimeError> {
        if self.match_kind(TokenKind::PlusPlus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::PlusPlus,
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::MinusMinus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::MinusMinus,
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::Plus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::Plus,
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::Minus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::Minus,
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::Tilde) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::Tilde,
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::Pipe) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Call {
                name: "slip".to_string(),
                args: vec![expr],
            });
        }
        if self.match_kind(TokenKind::Bang) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            });
        }
        if matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Ident(n)) if n == "not")
            && !matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::FatArrow)
            )
        {
            self.pos += 1;
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            });
        }
        if self.match_ident("so") {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::Ident("so".to_string()),
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::Question) {
            // Check for ?^ (boolean NOT prefix operator)
            if self.match_kind(TokenKind::Caret) {
                let expr = self.parse_unary()?;
                // ?^X means: convert X to Bool, then negate
                return Ok(Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Unary {
                        op: TokenKind::Question,
                        expr: Box::new(expr),
                    }),
                });
            }
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::Caret) {
            // ^N creates a range 0..^N. In Raku, ^ has looser precedence than
            // method calls, so ^10.Seq means (^10).Seq, not ^(10.Seq).
            // Parse only the immediate atom without method postfix.
            let inner = self.parse_caret_operand()?;
            let mut result = Expr::Unary {
                op: TokenKind::Caret,
                expr: Box::new(inner),
            };
            // Handle postfix method calls on the range result
            while self.match_kind(TokenKind::Dot) {
                let method_name = self.consume_ident().unwrap_or_default();
                let mut args = Vec::new();
                if self.match_kind(TokenKind::LParen) {
                    if !self.check(&TokenKind::RParen) {
                        args.push(self.parse_method_arg());
                        while self.match_kind(TokenKind::Comma) {
                            args.push(self.parse_method_arg());
                        }
                    }
                    let _ = self.consume_kind(TokenKind::RParen);
                }
                result = Expr::MethodCall {
                    target: Box::new(result),
                    name: method_name,
                    args,
                    modifier: None,
                };
            }
            return Ok(result);
        }
        self.parse_primary()
    }

    /// Parse the operand for prefix ^ (range constructor).
    /// Only parses simple atoms (numbers, variables, parens) without method postfix.
    fn parse_caret_operand(&mut self) -> Result<Expr, RuntimeError> {
        if let Some(TokenKind::Number(n)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            let n = *n;
            self.pos += 1;
            return Ok(Expr::Literal(Value::Int(n)));
        }
        if let Some(TokenKind::BigNumber(n)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            let n = n.clone();
            self.pos += 1;
            return Ok(Expr::Literal(Value::BigInt(n)));
        }
        if let Some(TokenKind::Var(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            let name = name.clone();
            self.pos += 1;
            return Ok(Expr::Var(name));
        }
        if self.match_kind(TokenKind::LParen) {
            let expr = self.parse_expr()?;
            self.consume_kind(TokenKind::RParen)?;
            return Ok(expr);
        }
        // Fall back to parse_primary for complex operands
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = if self.check(&TokenKind::Percent)
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::LParen)
            ) {
            // %(...) hash constructor: coerce list to hash
            self.pos += 1; // consume %
            self.pos += 1; // consume (
            let inner = if self.check(&TokenKind::RParen) {
                Expr::ArrayLiteral(Vec::new())
            } else {
                self.parse_comma_expr()?
            };
            self.consume_kind(TokenKind::RParen)?;
            Expr::MethodCall {
                target: Box::new(inner),
                name: "hash".to_string(),
                args: Vec::new(),
                modifier: None,
            }
        } else if self.match_kind(TokenKind::RParen) {
            Expr::Literal(Value::Nil)
        } else if self.match_kind(TokenKind::Dot) {
            if self.check(&TokenKind::LParen) {
                // .() - invocation on $_
                self.consume_kind(TokenKind::LParen)?;
                let mut call_args = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    call_args.push(self.parse_method_arg());
                    while self.match_kind(TokenKind::Comma) {
                        call_args.push(self.parse_method_arg());
                    }
                }
                self.consume_kind(TokenKind::RParen)?;
                Expr::CallOn {
                    target: Box::new(Expr::Var("_".to_string())),
                    args: call_args,
                }
            } else {
                let name = self.consume_ident()?;
                Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name,
                    args: Vec::new(),
                    modifier: None,
                }
            }
        } else if self.match_kind(TokenKind::LParen) {
            if self.check(&TokenKind::RParen) {
                // Empty parens: ()
                self.consume_kind(TokenKind::RParen)?;
                Expr::ArrayLiteral(Vec::new())
            } else {
                let expr = self.parse_comma_expr()?;
                self.consume_kind(TokenKind::RParen)?;
                expr
            }
        } else if self.check(&TokenKind::LBracket) && self.is_reduction_op() {
            self.parse_reduction()?
        } else if self.match_kind(TokenKind::LBracket) {
            let mut items = Vec::new();
            if !self.check(&TokenKind::RBracket) {
                items.push(self.parse_expr_or_true());
                while self.match_kind(TokenKind::Comma) {
                    if self.check(&TokenKind::RBracket) {
                        break; // trailing comma
                    }
                    items.push(self.parse_expr_or_true());
                }
            }
            self.consume_kind(TokenKind::RBracket)?;
            Expr::ArrayLiteral(items)
        } else if let Some(token) =
            self.advance_if(|k| matches!(k, TokenKind::VersionLiteral { .. }))
        {
            if let TokenKind::VersionLiteral { parts, plus, minus } = token.kind {
                Expr::Literal(Value::Version { parts, plus, minus })
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) =
            self.advance_if(|k| matches!(k, TokenKind::Number(_) | TokenKind::BigNumber(_)))
        {
            match token.kind {
                TokenKind::Number(value) => Expr::Literal(Value::Int(value)),
                TokenKind::BigNumber(value) => Expr::Literal(Value::BigInt(value)),
                _ => Expr::Literal(Value::Nil),
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Float(_))) {
            if let TokenKind::Float(value) = token.kind {
                Expr::Literal(Value::Num(value))
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Imaginary(_))) {
            if let TokenKind::Imaginary(value) = token.kind {
                Expr::Literal(Value::Complex(0.0, value))
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Str(_))) {
            if let TokenKind::Str(value) = token.kind {
                Expr::Literal(Value::Str(value))
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::DStr(_))) {
            if let TokenKind::DStr(parts) = token.kind {
                use crate::lexer::DStrPart;
                let mut exprs = Vec::new();
                for part in parts {
                    match part {
                        DStrPart::Lit(s) => exprs.push(Expr::Literal(Value::Str(s))),
                        DStrPart::Var(name) => {
                            if let Some(stripped) = name.strip_prefix('@') {
                                exprs.push(Expr::ArrayVar(stripped.to_string()));
                            } else {
                                exprs.push(Expr::Var(name));
                            }
                        }
                        DStrPart::Block(code) => {
                            // Parse block code as an expression
                            let mut lexer = crate::lexer::Lexer::new(&code);
                            let mut tokens = Vec::new();
                            loop {
                                let tok = lexer.next_token();
                                let is_eof = matches!(tok.kind, TokenKind::Eof);
                                tokens.push(tok);
                                if is_eof {
                                    break;
                                }
                            }
                            let mut sub_parser = Parser::new(tokens);
                            if let Ok(expr) = sub_parser.parse_expr() {
                                exprs.push(expr);
                            }
                        }
                    }
                }
                if exprs.len() == 1 {
                    if let Expr::Literal(Value::Str(_)) = &exprs[0] {
                        exprs.remove(0)
                    } else {
                        Expr::StringInterpolation(exprs)
                    }
                } else {
                    Expr::StringInterpolation(exprs)
                }
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Regex(_))) {
            if let TokenKind::Regex(pattern) = token.kind {
                Expr::Literal(Value::Regex(pattern))
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Subst { .. })) {
            if let TokenKind::Subst {
                pattern,
                replacement,
            } = token.kind
            {
                Expr::Subst {
                    pattern,
                    replacement,
                }
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if self.match_kind(TokenKind::True) {
            Expr::Literal(Value::Bool(true))
        } else if self.match_kind(TokenKind::False) {
            Expr::Literal(Value::Bool(false))
        } else if self.match_kind(TokenKind::Nil) {
            Expr::Literal(Value::Nil)
        } else if self.check(&TokenKind::Colon)
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Ident(_) | TokenKind::Number(_) | TokenKind::BigNumber(_))
            )
        {
            // Colonpair: :name, :key<value>, :key(expr), :42name
            self.pos += 1; // consume ':'
            if let Some(TokenKind::Number(n)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                // :42name => Pair("name", 42)
                let num = *n;
                self.pos += 1;
                let name = self.consume_ident().unwrap_or_default();
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::Str(name))),
                    op: TokenKind::FatArrow,
                    right: Box::new(Expr::Literal(Value::Int(num))),
                }
            } else {
                let name = self.consume_ident().unwrap_or_default();
                if self.check(&TokenKind::Lt) {
                    // :key<value>
                    let val = self.parse_angle_literal();
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Str(name))),
                        op: TokenKind::FatArrow,
                        right: Box::new(val),
                    }
                } else if self.match_kind(TokenKind::LParen) {
                    // :key(expr)
                    let val = self.parse_expr()?;
                    self.consume_kind(TokenKind::RParen)?;
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Str(name))),
                        op: TokenKind::FatArrow,
                        right: Box::new(val),
                    }
                } else if self.check(&TokenKind::LBracket) {
                    // :key[...]
                    let val = self.parse_expr()?;
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Str(name))),
                        op: TokenKind::FatArrow,
                        right: Box::new(val),
                    }
                } else if self.match_kind(TokenKind::Bang) {
                    // :!name => Pair("name", False)
                    let neg_name = self.consume_ident().unwrap_or(name);
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Str(neg_name))),
                        op: TokenKind::FatArrow,
                        right: Box::new(Expr::Literal(Value::Bool(false))),
                    }
                } else {
                    // :name => Pair("name", True)
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Str(name))),
                        op: TokenKind::FatArrow,
                        right: Box::new(Expr::Literal(Value::Bool(true))),
                    }
                }
            }
        } else if self.check(&TokenKind::Colon)
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Bang)
            )
            && matches!(
                self.tokens.get(self.pos + 2).map(|t| &t.kind),
                Some(TokenKind::Ident(_))
            )
        {
            // :!name => Pair("name", False)
            self.pos += 2; // consume ':' and '!'
            let name = self.consume_ident().unwrap_or_default();
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name))),
                op: TokenKind::FatArrow,
                right: Box::new(Expr::Literal(Value::Bool(false))),
            }
        } else if self.check(&TokenKind::Colon)
            && matches!(
                self.tokens.get(self.pos + 1).map(|t| &t.kind),
                Some(TokenKind::Var(_))
            )
        {
            // :$name => Pair("name", $name)
            self.pos += 1; // consume ':'
            let var = self.consume_var()?;
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(var.clone()))),
                op: TokenKind::FatArrow,
                right: Box::new(Expr::Var(var)),
            }
        } else if let Some(name) = self.peek_ident() {
            if name != "sub" && matches!(self.peek_next_kind(), Some(TokenKind::LParen)) {
                self.pos += 1;
                self.consume_kind(TokenKind::LParen)?;
                let mut args = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    args.push(self.parse_expr_or_true());
                    while self.match_kind(TokenKind::Comma) {
                        args.push(self.parse_expr_or_true());
                    }
                }
                self.consume_kind(TokenKind::RParen)?;
                Expr::Call { name, args }
            } else if matches!(name.as_str(), "map" | "grep")
                && matches!(
                    self.peek_next_kind(),
                    Some(TokenKind::LBrace | TokenKind::Arrow)
                )
            {
                // map { BLOCK }, @list  or  map -> $a, $b { BLOCK }, @list
                self.pos += 1; // consume function name
                let block = self.parse_expr()?; // parse the block
                let mut args = vec![block];
                while self.match_kind(TokenKind::Comma) {
                    if self.check(&TokenKind::Semicolon) || self.check(&TokenKind::Eof) {
                        break;
                    }
                    args.push(self.parse_expr()?);
                }
                Expr::Call { name, args }
            } else if matches!(
                name.as_str(),
                "keys" | "values" | "pairs" | "kv" | "elems" | "flat" | "item"
            ) && matches!(
                self.peek_next_kind(),
                Some(TokenKind::HashVar(_) | TokenKind::ArrayVar(_) | TokenKind::Var(_))
            ) {
                self.pos += 1; // consume function name
                let arg = self.parse_expr()?;
                Expr::Call {
                    name,
                    args: vec![arg],
                }
            } else if !matches!(
                name.as_str(),
                "if" | "unless"
                    | "while"
                    | "until"
                    | "for"
                    | "given"
                    | "when"
                    | "with"
                    | "without"
                    | "loop"
                    | "my"
                    | "our"
                    | "has"
                    | "sub"
                    | "do"
                    | "so"
                    | "not"
                    | "and"
                    | "or"
                    | "True"
                    | "False"
                    | "Nil"
                    | "try"
                    | "gather"
                    | "quietly"
                    | "eager"
                    | "lazy"
                    | "rand"
                    | "self"
                    | "return"
                    | "EVAL"
                    | "BEGIN"
                    | "END"
                    | "die"
                    | "class"
                    | "role"
                    | "enum"
                    | "use"
            ) && matches!(
                self.peek_next_kind(),
                Some(TokenKind::HashVar(_) | TokenKind::ArrayVar(_))
            ) {
                // Bare function call with sigiled variable argument: test2 %h, foo @arr
                self.pos += 1; // consume function name
                let arg = self.parse_expr()?;
                Expr::Call {
                    name,
                    args: vec![arg],
                }
            } else if name == "my"
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Ident(_))
                )
                && matches!(
                    self.tokens.get(self.pos + 2).map(|t| &t.kind),
                    Some(TokenKind::Var(_) | TokenKind::ArrayVar(_) | TokenKind::HashVar(_))
                )
            {
                // Expression-level: my TYPE $var = expr
                self.pos += 2; // skip 'my' and type name
                let var_name = if let Some(TokenKind::ArrayVar(n)) =
                    self.tokens.get(self.pos).map(|t| &t.kind)
                {
                    let n = format!("@{}", n);
                    self.pos += 1;
                    n
                } else if let Some(TokenKind::HashVar(n)) =
                    self.tokens.get(self.pos).map(|t| &t.kind)
                {
                    let n = format!("%{}", n);
                    self.pos += 1;
                    n
                } else {
                    self.consume_var()?
                };
                if self.match_kind(TokenKind::Eq) || self.match_kind(TokenKind::Bind) {
                    let expr = self.parse_comma_expr()?;
                    Expr::AssignExpr {
                        name: var_name,
                        expr: Box::new(expr),
                    }
                } else {
                    Expr::Literal(Value::Nil)
                }
            } else if name == "my"
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Var(_) | TokenKind::ArrayVar(_) | TokenKind::HashVar(_))
                )
            {
                self.pos += 1;
                let var_name = if let Some(TokenKind::ArrayVar(n)) =
                    self.tokens.get(self.pos).map(|t| &t.kind)
                {
                    let n = format!("@{}", n);
                    self.pos += 1;
                    n
                } else if let Some(TokenKind::HashVar(n)) =
                    self.tokens.get(self.pos).map(|t| &t.kind)
                {
                    let n = format!("%{}", n);
                    self.pos += 1;
                    n
                } else {
                    self.consume_var()?
                };
                if self.match_kind(TokenKind::Eq) || self.match_kind(TokenKind::Bind) {
                    let expr = self.parse_comma_expr()?;
                    Expr::AssignExpr {
                        name: var_name,
                        expr: Box::new(expr),
                    }
                } else {
                    Expr::Literal(Value::Nil)
                }
            } else if name == "EVAL" && matches!(self.peek_next_kind(), Some(TokenKind::Str(_))) {
                let name = self.consume_ident()?;
                let arg = self.parse_expr()?;
                Expr::Call {
                    name,
                    args: vec![arg],
                }
            } else if name == "sub"
                && matches!(
                    self.peek_next_kind(),
                    Some(TokenKind::LBrace | TokenKind::LParen)
                )
            {
                self.pos += 1; // consume "sub"
                let mut params = Vec::new();
                if self.match_kind(TokenKind::LParen) {
                    while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                        if self.match_kind(TokenKind::Star) {
                            // skip slurpy marker
                        }
                        if self.match_kind(TokenKind::Colon) {
                            // skip named marker
                        }
                        // Skip type constraint
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
                            let var = self.consume_var()?;
                            // Skip default values
                            if self.match_kind(TokenKind::Eq) {
                                let _ = self.parse_expr()?;
                            }
                            self.match_kind(TokenKind::Question);
                            self.match_kind(TokenKind::Bang);
                            params.push(var);
                        } else if self
                            .advance_if(|k| matches!(k, TokenKind::ArrayVar(_)))
                            .is_some()
                        {
                            // skip array params for now
                        } else if self
                            .advance_if(|k| matches!(k, TokenKind::HashVar(_)))
                            .is_some()
                        {
                            // skip hash params for now
                        } else {
                            break;
                        }
                        if !self.match_kind(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.match_kind(TokenKind::RParen);
                }
                let body = self.parse_block()?;
                if params.is_empty() {
                    Expr::AnonSub(body)
                } else {
                    Expr::AnonSubParams { params, body }
                }
            } else {
                let name = self.consume_ident()?;
                if name == "last" || name == "next" || name == "redo" {
                    let kind = match name.as_str() {
                        "last" => crate::ast::ControlFlowKind::Last,
                        "next" => crate::ast::ControlFlowKind::Next,
                        "redo" => crate::ast::ControlFlowKind::Redo,
                        _ => unreachable!(),
                    };
                    let label = self.try_consume_loop_label();
                    Expr::ControlFlow { kind, label }
                } else if name == "start" && self.check(&TokenKind::LBrace) {
                    let body = self.parse_block()?;
                    Expr::Call {
                        name: "start".to_string(),
                        args: vec![Expr::AnonSub(body)],
                    }
                } else if name == "do" {
                    if self.check(&TokenKind::LBrace) {
                        let body = self.parse_block()?;
                        Expr::DoBlock { body, label: None }
                    } else if self.match_ident("if") {
                        let stmt = self.parse_if_chain()?;
                        Expr::DoStmt(Box::new(stmt))
                    } else if self.match_ident("unless") {
                        let cond = self.parse_expr()?;
                        let body = self.parse_block()?;
                        Expr::DoStmt(Box::new(Stmt::If {
                            cond: Expr::Unary {
                                op: TokenKind::Bang,
                                expr: Box::new(cond),
                            },
                            then_branch: body,
                            else_branch: Vec::new(),
                        }))
                    } else if self.match_ident("given") {
                        let topic = self.parse_expr()?;
                        let body = self.parse_block()?;
                        Expr::DoStmt(Box::new(Stmt::Given { topic, body }))
                    } else {
                        // do EXPR - parse expression and return it
                        self.parse_expr()?
                    }
                } else if name == "so" {
                    let inner = self.parse_expr()?;
                    Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(Expr::Unary {
                            op: TokenKind::Bang,
                            expr: Box::new(inner),
                        }),
                    }
                } else if name == "quietly" || name == "eager" || name == "lazy" {
                    self.parse_expr()?
                } else if name == "gather" && self.check(&TokenKind::LBrace) {
                    let body = self.parse_block()?;
                    Expr::Gather(body)
                } else if name == "gather" {
                    // gather for ... { take ... }
                    let inner = self.parse_stmt()?;
                    Expr::Gather(vec![inner])
                } else if name == "try" && self.check(&TokenKind::LBrace) {
                    let body = self.parse_block()?;
                    Expr::Try { body, catch: None }
                } else if name == "rand" {
                    Expr::Call {
                        name: "rand".to_string(),
                        args: vec![],
                    }
                } else if name == "Bool::False" {
                    Expr::Literal(Value::Bool(false))
                } else if name == "Bool::True" {
                    Expr::Literal(Value::Bool(true))
                } else if name == "self" {
                    Expr::Var("self".to_string())
                } else if matches!(
                    name.as_str(),
                    "ok" | "nok" | "is" | "isnt" | "isa-ok" | "diag" | "pass" | "flunk" | "slip"
                ) && !self.check(&TokenKind::Semicolon)
                    && !self.check(&TokenKind::Eof)
                    && !self.check(&TokenKind::RBrace)
                    && !self.check(&TokenKind::RParen)
                {
                    let call_args = self.parse_call_args()?;
                    let args = call_args
                        .into_iter()
                        .filter_map(|a| match a {
                            CallArg::Positional(e) => Some(e),
                            _ => None,
                        })
                        .collect();
                    Expr::Call { name, args }
                } else if name == "die" || name == "fail" {
                    // die/fail in expression context: parse the argument and emit as a Call
                    if self.check(&TokenKind::Semicolon)
                        || self.check(&TokenKind::Eof)
                        || self.check(&TokenKind::RBrace)
                        || self.check(&TokenKind::RParen)
                    {
                        Expr::Call { name, args: vec![] }
                    } else {
                        let arg = self.parse_expr()?;
                        Expr::Call {
                            name,
                            args: vec![arg],
                        }
                    }
                } else if name == "class" && self.check(&TokenKind::LBrace) {
                    // Anonymous class expression: class { ... }
                    static ANON_CTR: std::sync::atomic::AtomicUsize =
                        std::sync::atomic::AtomicUsize::new(0);
                    let anon_name = format!(
                        "__ANON_CLASS_{}__",
                        ANON_CTR.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
                    );
                    self.consume_kind(TokenKind::LBrace)?;
                    let mut body = Vec::new();
                    while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
                        body.push(self.parse_stmt()?);
                    }
                    self.consume_kind(TokenKind::RBrace)?;
                    Expr::DoBlock {
                        body: vec![
                            Stmt::ClassDecl {
                                name: anon_name.clone(),
                                parents: Vec::new(),
                                body,
                            },
                            Stmt::Expr(Expr::Literal(Value::Package(anon_name))),
                        ],
                        label: None,
                    }
                } else if name == "class"
                    && matches!(
                        self.tokens.get(self.pos).map(|t| &t.kind),
                        Some(TokenKind::Colon)
                    )
                    && matches!(
                        self.tokens.get(self.pos + 1).map(|t| &t.kind),
                        Some(TokenKind::Colon)
                    )
                {
                    // Anonymous class with parent: class :: is Cool { ... }
                    self.pos += 2; // skip ::
                    static ANON_CTR2: std::sync::atomic::AtomicUsize =
                        std::sync::atomic::AtomicUsize::new(0);
                    let anon_name = format!(
                        "__ANON_CLASS_{}__",
                        ANON_CTR2.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
                    );
                    let mut parents = Vec::new();
                    while self.match_ident("is") {
                        parents.push(self.consume_ident()?);
                    }
                    self.consume_kind(TokenKind::LBrace)?;
                    let mut body = Vec::new();
                    while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
                        body.push(self.parse_stmt()?);
                    }
                    self.consume_kind(TokenKind::RBrace)?;
                    Expr::DoBlock {
                        body: vec![
                            Stmt::ClassDecl {
                                name: anon_name.clone(),
                                parents,
                                body,
                            },
                            Stmt::Expr(Expr::Literal(Value::Package(anon_name))),
                        ],
                        label: None,
                    }
                } else if name == "infix" && self.check(&TokenKind::Colon) {
                    if let Some(op_name) = self.try_parse_angled_op_name() {
                        let full_name = format!("infix:<{}>", op_name);
                        if self.match_kind(TokenKind::LParen) {
                            let mut args = Vec::new();
                            if !self.check(&TokenKind::RParen) {
                                args.push(self.parse_expr()?);
                                while self.match_kind(TokenKind::Comma) {
                                    if self.check(&TokenKind::RParen) {
                                        break;
                                    }
                                    args.push(self.parse_expr()?);
                                }
                            }
                            self.consume_kind(TokenKind::RParen)?;
                            Expr::Call {
                                name: full_name,
                                args,
                            }
                        } else {
                            Expr::BareWord(full_name)
                        }
                    } else {
                        Expr::BareWord(name)
                    }
                } else {
                    Expr::BareWord(name)
                }
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::HashVar(_))) {
            if let TokenKind::HashVar(name) = token.kind {
                Expr::HashVar(name)
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::CaptureVar(_))) {
            if let TokenKind::CaptureVar(name) = token.kind {
                Expr::CaptureVar(name)
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_))) {
            if let TokenKind::ArrayVar(name) = token.kind {
                Expr::ArrayVar(name)
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::CodeVar(_))) {
            if let TokenKind::CodeVar(name) = token.kind {
                if name == "infix" && self.check(&TokenKind::Colon) {
                    if let Some(op_name) = self.try_parse_angled_op_name() {
                        let full_name = format!("infix:<{}>", op_name);
                        if self.match_kind(TokenKind::LParen) {
                            let mut args = Vec::new();
                            if !self.check(&TokenKind::RParen) {
                                args.push(self.parse_expr()?);
                                while self.match_kind(TokenKind::Comma) {
                                    if self.check(&TokenKind::RParen) {
                                        break;
                                    }
                                    args.push(self.parse_expr()?);
                                }
                            }
                            self.consume_kind(TokenKind::RParen)?;
                            Expr::Call {
                                name: full_name,
                                args,
                            }
                        } else {
                            Expr::CodeVar(full_name)
                        }
                    } else {
                        Expr::CodeVar(name)
                    }
                } else {
                    Expr::CodeVar(name)
                }
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if self.match_kind(TokenKind::RoutineMagic) {
            Expr::RoutineMagic
        } else if self.match_kind(TokenKind::BlockMagic) {
            Expr::BlockMagic
        } else if self.match_kind(TokenKind::Arrow) {
            // Skip optional return type annotation (e.g., --> Int)
            let skip_type_annotation = |s: &mut Self| {
                if matches!(
                    s.tokens.get(s.pos).map(|t| &t.kind),
                    Some(TokenKind::Ident(_))
                ) && matches!(
                    s.tokens.get(s.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Var(_))
                ) {
                    s.pos += 1;
                }
            };
            skip_type_annotation(self);
            let mut params = Vec::new();
            if self.peek_is_var() {
                params.push(self.consume_var()?);
                while self.match_kind(TokenKind::Comma) {
                    // Skip optional type annotation before next param
                    skip_type_annotation(self);
                    if self.peek_is_var() {
                        params.push(self.consume_var()?);
                    } else if let Some(name) = self.peek_ident() {
                        // Sigilless parameter (e.g., \x)
                        if !matches!(name.as_str(), "if" | "unless" | "while" | "until" | "for") {
                            self.pos += 1;
                            params.push(name);
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            } else if let Some(name) = self.peek_ident() {
                // Sigilless parameter (e.g., -> \t where \ is skipped by lexer)
                if !matches!(name.as_str(), "if" | "unless" | "while" | "until" | "for") {
                    self.pos += 1;
                    params.push(name);
                    while self.match_kind(TokenKind::Comma) {
                        skip_type_annotation(self);
                        if self.peek_is_var() {
                            params.push(self.consume_var()?);
                        } else if let Some(name2) = self.peek_ident() {
                            if !matches!(
                                name2.as_str(),
                                "if" | "unless" | "while" | "until" | "for"
                            ) {
                                self.pos += 1;
                                params.push(name2);
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
            // Skip --> ReturnType annotation before block
            if matches!(
                self.tokens.get(self.pos).map(|t| &t.kind),
                Some(TokenKind::Ident(n)) if n == "-->"
            ) {
                self.pos += 1; // skip -->
                if self.peek_ident().is_some() {
                    self.pos += 1; // skip type name
                }
            }
            let body = self.parse_block()?;
            if params.len() == 1 {
                Expr::Lambda {
                    param: params.into_iter().next().unwrap_or_default(),
                    body,
                }
            } else {
                Expr::AnonSubParams { params, body }
            }
        } else if self.peek_is_var() {
            let var_line = self.tokens.get(self.pos).map(|t| t.line).unwrap_or(0);
            let name = self.consume_var()?;
            if name.is_empty() && self.match_kind(TokenKind::LParen) {
                // $(...) item context operator: parse contents and return as-is
                if self.check(&TokenKind::RParen) {
                    self.consume_kind(TokenKind::RParen)?;
                    Expr::ArrayLiteral(Vec::new())
                } else {
                    let expr = self.parse_comma_expr()?;
                    self.consume_kind(TokenKind::RParen)?;
                    expr
                }
            } else if name == "?LINE" {
                Expr::Literal(Value::Int(var_line as i64))
            } else {
                Expr::Var(name)
            }
        } else if self.match_kind(TokenKind::LBrace) {
            if self.check(&TokenKind::RBrace) {
                // Empty {} is a Hash, not a Block
                self.pos += 1; // consume '}'
                Expr::Hash(Vec::new())
            } else if self.is_hash_literal_start() {
                let pairs = self.parse_hash_literal()?;
                Expr::Hash(pairs)
            } else {
                let body = self.parse_block_body()?;
                // Blocks in expression context are closures, not immediate execution.
                // Immediate execution (do { ... }) uses Expr::Block instead.
                Expr::AnonSub(body)
            }
        } else if self.check(&TokenKind::Lt) {
            self.parse_angle_literal()
        } else if self.match_kind(TokenKind::Star) {
            // Whatever star (*) — check if it's a WhateverCode (*.method, * op expr)
            if self.check(&TokenKind::Dot) {
                // WhateverCode: *.method — parse as lambda { $_.method }
                // Use $_ as placeholder, then wrap in lambda after postcircumfix
                Expr::Var("__WHATEVER__".to_string())
            } else {
                Expr::Literal(Value::Num(f64::INFINITY))
            }
        } else {
            return Err(RuntimeError::new(format!(
                "Unexpected token in expression at {:?}",
                self.tokens.get(self.pos).map(|t| &t.kind)
            )));
        };

        loop {
            // Postfix `i` for imaginary numbers: (expr)i, $x.i => Complex(0, value)
            if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind)
                && name == "i"
                && !matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Eq | TokenKind::LParen | TokenKind::Dot)
                )
            {
                self.pos += 1;
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name: "Complex-i".to_string(),
                    args: Vec::new(),
                    modifier: None,
                };
                continue;
            }
            if self.match_kind(TokenKind::PlusPlus) {
                expr = Expr::PostfixOp {
                    op: TokenKind::PlusPlus,
                    expr: Box::new(expr),
                };
                continue;
            }
            if self.match_kind(TokenKind::MinusMinus) {
                expr = Expr::PostfixOp {
                    op: TokenKind::MinusMinus,
                    expr: Box::new(expr),
                };
                continue;
            }
            if self.check(&TokenKind::Dot)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::LBracket)
                )
            {
                // .[] subscript syntax — treat as index access
                self.pos += 2; // skip . and [
                let index = self.parse_expr_or_true();
                self.consume_kind(TokenKind::RBracket)?;
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                };
                continue;
            }
            if self.match_kind(TokenKind::Dot) {
                // Handle dispatch modifiers: .?method, .+method, .*method
                let modifier = if self.match_kind(TokenKind::Question) {
                    Some('?')
                } else if self.match_kind(TokenKind::Plus) {
                    Some('+')
                } else if self.match_kind(TokenKind::Star) {
                    Some('*')
                } else {
                    None
                };
                // .() - invocation on the target as a callable
                if self.check(&TokenKind::LParen) {
                    self.consume_kind(TokenKind::LParen)?;
                    let mut call_args = Vec::new();
                    if !self.check(&TokenKind::RParen) {
                        call_args.push(self.parse_method_arg());
                        while self.match_kind(TokenKind::Comma) {
                            call_args.push(self.parse_method_arg());
                        }
                    }
                    self.consume_kind(TokenKind::RParen)?;
                    expr = Expr::CallOn {
                        target: Box::new(expr),
                        args: call_args,
                    };
                    continue;
                }
                // Handle .^name (meta-method call) - treat ^ as prefix to method name
                let is_meta = self.match_kind(TokenKind::Caret);
                let name = self.consume_ident()?;
                let full_name = if is_meta { format!("^{}", name) } else { name };
                let mut args = Vec::new();
                if self.match_kind(TokenKind::LParen) {
                    if !self.check(&TokenKind::RParen) {
                        args.push(self.parse_method_arg());
                        while self.match_kind(TokenKind::Comma) {
                            args.push(self.parse_method_arg());
                        }
                    }
                    self.consume_kind(TokenKind::RParen)?;
                } else if self.check(&TokenKind::Colon)
                    && !matches!(
                        self.tokens.get(self.pos + 1).map(|t| &t.kind),
                        Some(TokenKind::Semicolon | TokenKind::Eof)
                    )
                {
                    // Colon method call syntax: .method: arg1, arg2
                    self.pos += 1; // consume ':'
                    args.push(self.parse_method_arg());
                    while self.match_kind(TokenKind::Comma) {
                        args.push(self.parse_method_arg());
                    }
                }
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name: full_name,
                    args,
                    modifier,
                };
                continue;
            }
            if self.match_kind(TokenKind::LBracket) {
                let index = self.parse_expr_or_true();
                self.consume_kind(TokenKind::RBracket)?;
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                };
                continue;
            }
            if matches!(expr, Expr::HashVar(_) | Expr::Index { .. })
                && self.check(&TokenKind::LBrace)
            {
                self.match_kind(TokenKind::LBrace);
                // Zen slice: %h{} returns the hash itself
                let index = if self.check(&TokenKind::RBrace) {
                    Expr::Literal(Value::Nil)
                } else {
                    self.parse_comma_expr()?
                };
                self.consume_kind(TokenKind::RBrace)?;
                expr = Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                };
                continue;
            }
            if self.check(&TokenKind::Lt) {
                let is_postcircumfix = if matches!(expr, Expr::HashVar(_)) {
                    true
                } else if matches!(expr, Expr::Var(_)) {
                    // Only treat <...> as postcircumfix subscript on $var if it looks like $var<ident>
                    matches!(
                        self.tokens.get(self.pos + 1).map(|t| &t.kind),
                        Some(TokenKind::Ident(_) | TokenKind::Str(_))
                    ) && matches!(
                        self.tokens.get(self.pos + 2).map(|t| &t.kind),
                        Some(TokenKind::Gt)
                    )
                } else if matches!(expr, Expr::Index { .. }) {
                    // For chained subscripts like %h<foo><bar>
                    matches!(
                        self.tokens.get(self.pos + 1).map(|t| &t.kind),
                        Some(TokenKind::Ident(_) | TokenKind::Str(_) | TokenKind::Gt)
                    )
                } else {
                    false
                };
                if is_postcircumfix {
                    self.match_kind(TokenKind::Lt);
                    // Collect all words before >
                    let mut keys = Vec::new();
                    while let Some(token) = self.advance_if(|k| {
                        matches!(
                            k,
                            TokenKind::Ident(_)
                                | TokenKind::Str(_)
                                | TokenKind::Number(_)
                                | TokenKind::BigNumber(_)
                        )
                    }) {
                        let key = match token.kind {
                            TokenKind::Ident(s) => s,
                            TokenKind::Str(s) => s,
                            TokenKind::Number(n) => n.to_string(),
                            TokenKind::BigNumber(n) => n.to_string(),
                            _ => String::new(),
                        };
                        keys.push(key);
                    }
                    self.consume_kind(TokenKind::Gt)?;
                    if keys.is_empty() {
                        // %hash<> — empty angle brackets, return all pairs
                        expr = Expr::MethodCall {
                            target: Box::new(expr),
                            name: "pairs".to_string(),
                            args: Vec::new(),
                            modifier: None,
                        };
                    } else if keys.len() == 1 {
                        let key = keys.remove(0);
                        if let Expr::HashVar(name) = expr {
                            if name == "*ENV" {
                                expr = Expr::EnvIndex(key);
                            } else {
                                expr = Expr::Index {
                                    target: Box::new(Expr::HashVar(name)),
                                    index: Box::new(Expr::Literal(Value::Str(key))),
                                };
                            }
                        } else {
                            expr = Expr::Index {
                                target: Box::new(expr),
                                index: Box::new(Expr::Literal(Value::Str(key))),
                            };
                        }
                    } else {
                        // Multi-word: %hash<three one> => slice
                        let key_exprs: Vec<Expr> = keys
                            .into_iter()
                            .map(|k| Expr::Literal(Value::Str(k)))
                            .collect();
                        expr = Expr::Index {
                            target: Box::new(expr),
                            index: Box::new(Expr::ArrayLiteral(key_exprs)),
                        };
                    }
                    continue;
                }
            }
            if self.check(&TokenKind::Colon)
                && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Ident(name)) if name == "exists"
                )
            {
                self.match_kind(TokenKind::Colon);
                let _ = self.consume_ident()?;
                expr = Expr::Exists(Box::new(expr));
                continue;
            }
            if self.match_kind(TokenKind::LParen) {
                let mut args = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    args.push(self.parse_expr_or_true());
                    while self.match_kind(TokenKind::Comma) {
                        args.push(self.parse_expr_or_true());
                    }
                }
                self.consume_kind(TokenKind::RParen)?;
                expr = Expr::CallOn {
                    target: Box::new(expr),
                    args,
                };
                continue;
            }
            break;
        }
        // Wrap WhateverCode expressions in a lambda
        if Self::contains_whatever(&expr) {
            let body_expr = Self::replace_whatever(&expr);
            expr = Expr::Lambda {
                param: "_".to_string(),
                body: vec![Stmt::Expr(body_expr)],
            };
        }
        Ok(expr)
    }

    fn is_whatever_star(expr: &Expr) -> bool {
        matches!(expr, Expr::Literal(Value::Num(f)) if f.is_infinite() && f.is_sign_positive())
    }

    fn contains_whatever(expr: &Expr) -> bool {
        match expr {
            Expr::Var(name) if name == "__WHATEVER__" => true,
            Expr::Literal(Value::Num(f)) if f.is_infinite() && f.is_sign_positive() => false, // standalone * is not WhateverCode
            Expr::MethodCall { target, .. } => Self::contains_whatever(target),
            Expr::Binary {
                left, right, op, ..
            } => {
                // Range operators: * as endpoint means Infinity, not WhateverCode
                match op {
                    TokenKind::DotDot
                    | TokenKind::DotDotCaret
                    | TokenKind::CaretDotDot
                    | TokenKind::CaretDotDotCaret
                    | TokenKind::DotDotDot
                    | TokenKind::DotDotDotCaret => false,
                    _ => {
                        (Self::contains_whatever(left) || Self::is_whatever_star(left))
                            || (Self::contains_whatever(right) || Self::is_whatever_star(right))
                    }
                }
            }
            Expr::Unary { expr, .. } => {
                Self::contains_whatever(expr) || Self::is_whatever_star(expr)
            }
            Expr::Index { target, .. } => Self::contains_whatever(target),
            _ => false,
        }
    }

    fn replace_whatever(expr: &Expr) -> Expr {
        match expr {
            Expr::Var(name) if name == "__WHATEVER__" => Expr::Var("_".to_string()),
            Expr::Literal(Value::Num(f)) if f.is_infinite() && f.is_sign_positive() => {
                Expr::Var("_".to_string())
            }
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
            } => Expr::MethodCall {
                target: Box::new(Self::replace_whatever(target)),
                name: name.clone(),
                args: args.clone(),
                modifier: *modifier,
            },
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(Self::replace_whatever(left)),
                op: op.clone(),
                right: Box::new(Self::replace_whatever(right)),
            },
            Expr::Unary { op, expr } => Expr::Unary {
                op: op.clone(),
                expr: Box::new(Self::replace_whatever(expr)),
            },
            Expr::Index { target, index } => Expr::Index {
                target: Box::new(Self::replace_whatever(target)),
                index: index.clone(),
            },
            other => other.clone(),
        }
    }

    fn parse_hash_pair(&mut self) -> Result<(String, Option<Expr>), RuntimeError> {
        if let Some(token) =
            self.advance_if(|k| matches!(k, TokenKind::Ident(_) | TokenKind::Str(_)))
        {
            let name = match token.kind {
                TokenKind::Ident(s) => s,
                TokenKind::Str(s) => s,
                _ => String::new(),
            };
            if self.match_kind(TokenKind::FatArrow) {
                let value = self.parse_expr()?;
                return Ok((name, Some(value)));
            } else {
                return Err(RuntimeError::new("Expected fat arrow in hash pair"));
            }
        }
        self.consume_kind(TokenKind::Colon)?;
        // :$var => Pair(var_name_without_sigil, $var)
        if let Some(TokenKind::Var(_)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            let var = self.consume_var()?;
            return Ok((var.clone(), Some(Expr::Var(var))));
        }
        if let Some(token) =
            self.advance_if(|k| matches!(k, TokenKind::Number(_) | TokenKind::BigNumber(_)))
        {
            let literal = match token.kind {
                TokenKind::Number(value) => Value::Int(value),
                TokenKind::BigNumber(value) => Value::BigInt(value),
                _ => Value::Int(0),
            };
            let name = self.consume_ident()?;
            return Ok((name, Some(Expr::Literal(literal))));
        }
        let name = self.consume_ident()?;
        if self.check(&TokenKind::Lt) {
            // :key<value>
            let val = self.parse_angle_literal();
            return Ok((name, Some(val)));
        }
        if self.match_kind(TokenKind::LParen) {
            let value = self.parse_expr()?;
            self.consume_kind(TokenKind::RParen)?;
            return Ok((name, Some(value)));
        }
        // :key{block} → Pair with Block/Lambda value
        if self.check(&TokenKind::LBrace) {
            let value = self.parse_expr()?;
            return Ok((name, Some(value)));
        }
        Ok((name, None))
    }

    fn parse_hash_literal(&mut self) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
        let mut pairs = Vec::new();
        let mut failed = false;
        if !self.check(&TokenKind::RBrace) {
            if self.check(&TokenKind::Lt) {
                self.parse_angle_into_hash_pairs(&mut pairs);
            } else {
                match self.parse_hash_pair() {
                    Ok(pair) => pairs.push(pair),
                    Err(_) => failed = true,
                }
            }
            while !failed && !self.check(&TokenKind::RBrace) {
                // Allow comma or space-separated colonpairs
                self.match_kind(TokenKind::Comma);
                if self.check(&TokenKind::RBrace) {
                    break;
                }
                if self.check(&TokenKind::Lt) {
                    self.parse_angle_into_hash_pairs(&mut pairs);
                } else {
                    match self.parse_hash_pair() {
                        Ok(pair) => pairs.push(pair),
                        Err(_) => {
                            failed = true;
                            break;
                        }
                    }
                }
            }
        }
        if failed {
            while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
                self.pos += 1;
            }
        }
        self.consume_kind(TokenKind::RBrace)?;
        Ok(pairs)
    }

    /// Parse `<a b c d>` and expand into hash pairs: a => "b", c => "d"
    fn parse_angle_into_hash_pairs(&mut self, pairs: &mut Vec<(String, Option<Expr>)>) {
        let expr = self.parse_angle_literal();
        let words: Vec<String> = match &expr {
            Expr::Literal(Value::Str(s)) => vec![s.clone()],
            Expr::ArrayLiteral(items) => items
                .iter()
                .filter_map(|e| {
                    if let Expr::Literal(Value::Str(s)) = e {
                        Some(s.clone())
                    } else {
                        None
                    }
                })
                .collect(),
            _ => Vec::new(),
        };
        let mut iter = words.into_iter();
        while let Some(key) = iter.next() {
            let value = iter.next().map(|v| Expr::Literal(Value::Str(v)));
            pairs.push((key, value));
        }
    }

    fn is_hash_literal_start(&self) -> bool {
        if matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::Colon)
        ) {
            return true;
        }
        matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::Ident(_)) | Some(TokenKind::Str(_))
        ) && matches!(
            self.tokens.get(self.pos + 1).map(|t| &t.kind),
            Some(TokenKind::FatArrow)
        )
    }

    fn consume_var(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Var(_)))
            && let TokenKind::Var(name) = token.kind
        {
            return Ok(name);
        }
        Err(RuntimeError::new("Expected variable"))
    }

    fn consume_codevar(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::CodeVar(_)))
            && let TokenKind::CodeVar(name) = token.kind
        {
            return Ok(name);
        }
        Err(RuntimeError::new("Expected code variable"))
    }

    fn consume_ident(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_)))
            && let TokenKind::Ident(name) = token.kind
        {
            return Ok(name);
        }
        Err(RuntimeError::new("Expected identifier"))
    }

    fn match_ident(&mut self, ident: &str) -> bool {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_)))
            && let TokenKind::Ident(name) = token.kind
        {
            if name == ident {
                return true;
            }
            self.pos -= 1;
        }
        false
    }

    fn match_kind(&mut self, kind: TokenKind) -> bool {
        if self.check(&kind) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn consume_kind(&mut self, kind: TokenKind) -> Result<(), RuntimeError> {
        if self.check(&kind) {
            self.pos += 1;
            Ok(())
        } else {
            Err(RuntimeError::new("Unexpected token"))
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        match self.tokens.get(self.pos) {
            Some(t) => &t.kind == kind,
            None => *kind == TokenKind::Eof,
        }
    }

    fn advance_if<F>(&mut self, predicate: F) -> Option<Token>
    where
        F: Fn(&TokenKind) -> bool,
    {
        if let Some(token) = self.tokens.get(self.pos)
            && predicate(&token.kind)
        {
            self.pos += 1;
            return Some(token.clone());
        }
        None
    }

    fn peek_is_var(&self) -> bool {
        matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::Var(_))
        )
    }

    fn peek_is_codevar(&self) -> bool {
        matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::CodeVar(_))
        )
    }

    fn parse_pointy_param_name(&mut self) -> Option<String> {
        if let Some(TokenKind::Ident(_)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            let mut next = self.pos + 1;
            if matches!(
                self.tokens.get(next).map(|t| &t.kind),
                Some(TokenKind::Colon)
            ) && matches!(
                self.tokens.get(next + 1).map(|t| &t.kind),
                Some(TokenKind::Ident(_))
            ) {
                next += 2;
            }
            match self.tokens.get(next).map(|t| &t.kind) {
                Some(TokenKind::Var(name)) => {
                    self.pos = next + 1;
                    return Some(name.clone());
                }
                Some(TokenKind::CodeVar(name)) => {
                    self.pos = next + 1;
                    return Some(name.clone());
                }
                Some(TokenKind::Ident(name))
                    if !matches!(name.as_str(), "if" | "unless" | "while" | "until" | "for") =>
                {
                    self.pos = next + 1;
                    return Some(name.clone());
                }
                _ => {}
            }
        }
        if self.peek_is_var() {
            return self.consume_var().ok();
        }
        if self.peek_is_codevar() {
            return self.consume_codevar().ok();
        }
        if let Some(name) = self.peek_ident()
            && !matches!(name.as_str(), "if" | "unless" | "while" | "until" | "for")
        {
            self.pos += 1;
            return Some(name);
        }
        None
    }

    fn peek_ident(&self) -> Option<String> {
        match self.tokens.get(self.pos).map(|t| &t.kind) {
            Some(TokenKind::Ident(name)) => Some(name.clone()),
            _ => None,
        }
    }

    fn peek_next_kind(&self) -> Option<TokenKind> {
        self.tokens.get(self.pos + 1).map(|t| t.kind.clone())
    }

    fn try_consume_loop_label(&mut self) -> Option<String> {
        // Check if next token is an identifier that's NOT a keyword
        if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind)
            && !matches!(
                name.as_str(),
                "if" | "unless" | "while" | "until" | "for" | "given" | "when" | "with" | "without"
            )
        {
            let label = name.clone();
            self.pos += 1;
            return Some(label);
        }
        None
    }

    fn parse_labeled_loop(&mut self, label: Option<String>) -> Result<Stmt, RuntimeError> {
        if self.match_ident("for") {
            let iterable = self.parse_comma_expr()?;
            let mut param = None;
            let mut params = Vec::new();
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
                if self.peek_is_var() || self.peek_is_codevar() {
                    let first = if self.peek_is_var() {
                        self.consume_var()?
                    } else {
                        self.consume_codevar()?
                    };
                    if self.match_kind(TokenKind::Comma) {
                        params.push(first);
                        while self.peek_is_var() || self.peek_is_codevar() {
                            if self.peek_is_var() {
                                params.push(self.consume_var()?);
                            } else {
                                params.push(self.consume_codevar()?);
                            }
                            if !self.match_kind(TokenKind::Comma) {
                                break;
                            }
                        }
                    } else {
                        param = Some(first);
                    }
                }
            }
            let body = self.parse_block()?;
            return Ok(Stmt::For {
                iterable,
                param,
                params,
                body,
                label,
            });
        }
        if self.match_ident("while") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::While { cond, body, label });
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
                label,
            });
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
                    label,
                });
            } else {
                let body = self.parse_block()?;
                return Ok(Stmt::Loop {
                    init: None,
                    cond: None,
                    step: None,
                    body,
                    repeat: false,
                    label,
                });
            }
        }
        if self.match_ident("repeat") {
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
                    label,
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
                    label,
                });
            }
            return Err(RuntimeError::new(
                "Expected 'while' or 'until' after repeat block",
            ));
        }
        if self.match_ident("do") {
            if self.check(&TokenKind::LBrace) {
                let body = self.parse_block()?;
                return Ok(Stmt::Expr(Expr::DoBlock { body, label }));
            }
            return Err(RuntimeError::new("Expected block after labeled do"));
        }
        Err(RuntimeError::new("Expected loop keyword after label"))
    }

    fn is_reduction_op(&self) -> bool {
        if !matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::LBracket)
        ) {
            return false;
        }
        if !matches!(
            self.tokens.get(self.pos + 2).map(|t| &t.kind),
            Some(TokenKind::RBracket)
        ) {
            return false;
        }
        match self.tokens.get(self.pos + 1).map(|t| &t.kind) {
            Some(
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::StarStar
                | TokenKind::Tilde
                | TokenKind::AndAnd
                | TokenKind::OrOr
                | TokenKind::SlashSlash
                | TokenKind::Comma
                | TokenKind::LtEqGt
                | TokenKind::EqEq
                | TokenKind::BangEq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Lte
                | TokenKind::Gte
                | TokenKind::BitAnd
                | TokenKind::BitOr
                | TokenKind::BitXor
                | TokenKind::OrElse
                | TokenKind::AndThen
                | TokenKind::NotAndThen,
            ) => true,
            Some(TokenKind::Ident(name)) => matches!(
                name.as_str(),
                "min"
                    | "max"
                    | "leg"
                    | "cmp"
                    | "eq"
                    | "ne"
                    | "lt"
                    | "gt"
                    | "le"
                    | "ge"
                    | "lcm"
                    | "gcd"
                    | "and"
                    | "or"
                    | "x"
            ),
            _ => false,
        }
    }

    fn parse_reduction(&mut self) -> Result<Expr, RuntimeError> {
        self.consume_kind(TokenKind::LBracket)?;
        let op_name = match self.tokens.get(self.pos).map(|t| &t.kind) {
            Some(TokenKind::Plus) => "+".to_string(),
            Some(TokenKind::Minus) => "-".to_string(),
            Some(TokenKind::Star) => "*".to_string(),
            Some(TokenKind::Slash) => "/".to_string(),
            Some(TokenKind::StarStar) => "**".to_string(),
            Some(TokenKind::Tilde) => "~".to_string(),
            Some(TokenKind::AndAnd) => "&&".to_string(),
            Some(TokenKind::OrOr) => "||".to_string(),
            Some(TokenKind::SlashSlash) => "//".to_string(),
            Some(TokenKind::Comma) => ",".to_string(),
            Some(TokenKind::LtEqGt) => "<=>".to_string(),
            Some(TokenKind::EqEq) => "==".to_string(),
            Some(TokenKind::BangEq) => "!=".to_string(),
            Some(TokenKind::Lt) => "<".to_string(),
            Some(TokenKind::Gt) => ">".to_string(),
            Some(TokenKind::Lte) => "<=".to_string(),
            Some(TokenKind::Gte) => ">=".to_string(),
            Some(TokenKind::BitAnd) => "+&".to_string(),
            Some(TokenKind::BitOr) => "+|".to_string(),
            Some(TokenKind::BitXor) => "+^".to_string(),
            Some(TokenKind::Ident(name)) => name.clone(),
            _ => return Err(RuntimeError::new("Expected operator in reduction")),
        };
        self.pos += 1;
        self.consume_kind(TokenKind::RBracket)?;
        let expr = self.parse_comma_expr()?;
        Ok(Expr::Reduction {
            op: op_name,
            expr: Box::new(expr),
        })
    }

    /// Try to parse `:<op>` (angle-bracketed operator name) at the current position.
    fn try_parse_angled_op_name(&mut self) -> Option<String> {
        let start = self.pos;
        if !self.match_kind(TokenKind::Colon) {
            self.pos = start;
            return None;
        }
        // Case 1: Lte (<=) starts the sequence
        if self.check(&TokenKind::Lte) {
            self.pos += 1;
            let mut op_str = String::from("=");
            loop {
                match self.tokens.get(self.pos).map(|t| &t.kind) {
                    Some(TokenKind::Gt) => {
                        self.pos += 1;
                        break;
                    }
                    Some(TokenKind::LParen | TokenKind::RParen) => {
                        self.pos += 1;
                        continue;
                    }
                    Some(TokenKind::FatArrow) => {
                        op_str.push('=');
                        self.pos += 1;
                        break;
                    }
                    Some(TokenKind::Ident(name)) => {
                        op_str.push_str(name);
                        self.pos += 1;
                    }
                    Some(tok) => {
                        if let Some(s) = Self::token_to_op_str(tok) {
                            op_str.push_str(s);
                            self.pos += 1;
                        } else {
                            self.pos = start;
                            return None;
                        }
                    }
                    None => {
                        self.pos = start;
                        return None;
                    }
                }
            }
            return Some(op_str);
        }
        // Case 2: LtEqGt (<=>)
        if self.check(&TokenKind::LtEqGt) {
            self.pos += 1;
            return Some("=".to_string());
        }
        // Case 3: Lt (<) starts the sequence
        if !self.match_kind(TokenKind::Lt) {
            self.pos = start;
            return None;
        }
        let mut op_str = String::new();
        loop {
            match self.tokens.get(self.pos).map(|t| &t.kind) {
                Some(TokenKind::Gt) => {
                    self.pos += 1;
                    break;
                }
                Some(TokenKind::LParen | TokenKind::RParen) => {
                    self.pos += 1;
                    continue;
                }
                Some(TokenKind::FatArrow) => {
                    op_str.push('=');
                    self.pos += 1;
                    break;
                }
                Some(TokenKind::Ident(name)) => {
                    op_str.push_str(name);
                    self.pos += 1;
                }
                Some(TokenKind::Str(value)) => {
                    op_str.push_str(value);
                    self.pos += 1;
                }
                Some(TokenKind::Gte) => {
                    op_str.push('=');
                    self.pos += 1;
                    break;
                }
                Some(tok) => {
                    if let Some(s) = Self::token_to_op_str(tok) {
                        op_str.push_str(s);
                        self.pos += 1;
                    } else {
                        self.pos = start;
                        return None;
                    }
                }
                None => {
                    self.pos = start;
                    return None;
                }
            }
        }
        if op_str.is_empty() {
            self.pos = start;
            return None;
        }
        Some(op_str)
    }

    fn parse_routine_name(&mut self) -> Result<String, RuntimeError> {
        let name = self.consume_ident()?;
        if matches!(
            name.as_str(),
            "prefix" | "infix" | "postfix" | "circumfix" | "postcircumfix"
        ) && self.check(&TokenKind::Colon)
            && let Some(op_name) = self.try_parse_angled_op_name()
        {
            return Ok(format!("{name}:<{op_name}>"));
        }
        Ok(name)
    }

    fn skip_balanced_parens(&mut self) {
        let mut depth = 1usize;
        while depth > 0 && !self.check(&TokenKind::Eof) {
            if self.match_kind(TokenKind::LParen) {
                depth += 1;
                continue;
            }
            if self.match_kind(TokenKind::RParen) {
                depth -= 1;
                continue;
            }
            self.pos += 1;
        }
    }

    fn skip_balanced_angles(&mut self) {
        let mut depth = 1usize;
        while depth > 0 && !self.check(&TokenKind::Eof) {
            if self.match_kind(TokenKind::Lt) {
                depth += 1;
                continue;
            }
            if self.match_kind(TokenKind::Gt) {
                depth -= 1;
                continue;
            }
            self.pos += 1;
        }
    }

    fn token_to_op_str(tok: &TokenKind) -> Option<&'static str> {
        match tok {
            TokenKind::Plus => Some("+"),
            TokenKind::Minus => Some("-"),
            TokenKind::Star => Some("*"),
            TokenKind::Slash => Some("/"),
            TokenKind::StarStar => Some("**"),
            TokenKind::Tilde => Some("~"),
            TokenKind::AndAnd => Some("&&"),
            TokenKind::OrOr => Some("||"),
            TokenKind::SlashSlash => Some("//"),
            TokenKind::Comma => Some(","),
            TokenKind::LtEqGt => Some("<=>"),
            TokenKind::EqEq => Some("=="),
            TokenKind::EqEqEq => Some("==="),
            TokenKind::BangEq => Some("!="),
            TokenKind::Lt => Some("<"),
            TokenKind::Gt => Some(">"),
            TokenKind::Lte => Some("<="),
            TokenKind::Gte => Some(">="),
            TokenKind::SmartMatch => Some("~~"),
            TokenKind::BangTilde => Some("!~~"),
            TokenKind::Eq => Some("="),
            TokenKind::BitAnd => Some("+&"),
            TokenKind::BitOr => Some("+|"),
            TokenKind::BitXor => Some("+^"),
            TokenKind::Ident(name) => match name.as_str() {
                "eq" | "ne" | "lt" | "le" | "gt" | "ge" | "leg" | "cmp" | "eqv" => {
                    // SAFETY: these string literals have static lifetime
                    match name.as_str() {
                        "eq" => Some("eq"),
                        "ne" => Some("ne"),
                        "lt" => Some("lt"),
                        "le" => Some("le"),
                        "gt" => Some("gt"),
                        "ge" => Some("ge"),
                        "leg" => Some("leg"),
                        "cmp" => Some("cmp"),
                        "eqv" => Some("eqv"),
                        _ => None,
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_class_decl(&mut self) -> Result<Stmt, RuntimeError> {
        let name = self.consume_ident()?;
        let mut parents = Vec::new();
        while self.match_ident("is") {
            parents.push(self.consume_ident()?);
        }
        self.consume_kind(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            body.push(self.parse_stmt()?);
        }
        self.consume_kind(TokenKind::RBrace)?;
        self.match_kind(TokenKind::Semicolon);
        Ok(Stmt::ClassDecl {
            name,
            parents,
            body,
        })
    }

    fn parse_role_decl(&mut self) -> Result<Stmt, RuntimeError> {
        let name = self.consume_ident()?;
        self.consume_kind(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            body.push(self.parse_stmt()?);
        }
        self.consume_kind(TokenKind::RBrace)?;
        self.match_kind(TokenKind::Semicolon);
        Ok(Stmt::RoleDecl { name, body })
    }

    fn parse_has_decl(&mut self) -> Result<Stmt, RuntimeError> {
        // Skip optional type annotation
        if matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::Ident(_))
        ) && matches!(
            self.tokens.get(self.pos + 1).map(|t| &t.kind),
            Some(TokenKind::Var(_))
        ) {
            self.pos += 1;
        }
        let var = self.consume_var()?;
        // var is like ".name" (public) or "!name" (private)
        let (is_public, attr_name) = if let Some(rest) = var.strip_prefix('.') {
            (true, rest.to_string())
        } else if let Some(rest) = var.strip_prefix('!') {
            (false, rest.to_string())
        } else {
            (false, var.clone())
        };
        let default = if self.match_kind(TokenKind::Eq) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.match_kind(TokenKind::Semicolon);
        Ok(Stmt::HasDecl {
            name: attr_name,
            is_public,
            default,
        })
    }

    fn parse_method_decl(&mut self, multi: bool) -> Result<Stmt, RuntimeError> {
        let name = self.consume_ident()?;
        let mut params = Vec::new();
        let mut param_defs = Vec::new();
        if self.match_kind(TokenKind::LParen) {
            while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                let mut is_named = false;
                let mut is_slurpy = false;
                if self.match_kind(TokenKind::Star) {
                    is_slurpy = true;
                }
                if self.match_kind(TokenKind::Colon) {
                    is_named = true;
                }
                let mut type_constraint = None;
                if matches!(
                    self.tokens.get(self.pos).map(|t| &t.kind),
                    Some(TokenKind::Ident(_))
                ) && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Var(_))
                ) {
                    if let Some(TokenKind::Ident(ty)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                        type_constraint = Some(ty.clone());
                    }
                    self.pos += 1;
                }
                if self.peek_is_var() {
                    let var = self.consume_var()?;
                    let default = if self.match_kind(TokenKind::Eq) {
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    params.push(var.clone());
                    param_defs.push(ParamDef {
                        name: var,
                        default,
                        named: is_named,
                        slurpy: is_slurpy,
                        type_constraint,
                        literal_value: None,
                    });
                } else {
                    self.pos += 1;
                }
                self.match_kind(TokenKind::Comma);
            }
            self.match_kind(TokenKind::RParen);
        }
        if self.match_kind(TokenKind::LBrace) {
            let body = self.parse_block_body()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::MethodDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            });
        }
        Err(RuntimeError::new("Expected block for method"))
    }

    fn parse_token_rule_decl(&mut self, is_rule: bool, multi: bool) -> Result<Stmt, RuntimeError> {
        let mut name = self.consume_ident()?;
        if self.match_kind(TokenKind::Colon)
            && let Some(variant) = self.consume_ident().ok()
            && variant == "sym"
            && self.match_kind(TokenKind::Lt)
        {
            let mut sym = String::new();
            while !self.check(&TokenKind::Gt) && !self.check(&TokenKind::Eof) {
                if let Some(tok) = self.tokens.get(self.pos).map(|t| &t.kind) {
                    if let Some(op) = Self::token_to_op_str(tok) {
                        sym.push_str(op);
                    } else if let TokenKind::Ident(s) | TokenKind::Str(s) = tok {
                        sym.push_str(s);
                    }
                }
                self.pos += 1;
            }
            self.match_kind(TokenKind::Gt);
            if !sym.is_empty() {
                name = format!("{name}:sym<{sym}>");
            }
        }
        let mut params = Vec::new();
        let mut param_defs = Vec::new();
        if self.match_kind(TokenKind::LParen) {
            while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                let mut is_named = false;
                let mut is_slurpy = false;
                if self.match_kind(TokenKind::Star) {
                    is_slurpy = true;
                }
                if self.match_kind(TokenKind::Colon) {
                    is_named = true;
                }
                let mut type_constraint = None;
                if matches!(
                    self.tokens.get(self.pos).map(|t| &t.kind),
                    Some(TokenKind::Ident(_))
                ) && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Var(_))
                ) {
                    if let Some(TokenKind::Ident(ty)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                        type_constraint = Some(ty.clone());
                    }
                    self.pos += 1;
                }
                if self.peek_is_var() {
                    let var = self.consume_var()?;
                    let default = if self.match_kind(TokenKind::Eq) {
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    self.match_kind(TokenKind::Question);
                    self.match_kind(TokenKind::Bang);
                    params.push(var.clone());
                    param_defs.push(ParamDef {
                        name: var,
                        default,
                        named: is_named,
                        slurpy: is_slurpy,
                        type_constraint,
                        literal_value: None,
                    });
                } else {
                    self.pos += 1;
                }
                self.match_kind(TokenKind::Comma);
            }
            self.match_kind(TokenKind::RParen);
        }
        if self.match_kind(TokenKind::LBrace) {
            let body = self.parse_block_body()?;
            self.match_kind(TokenKind::Semicolon);
            if is_rule {
                return Ok(Stmt::RuleDecl {
                    name,
                    params,
                    param_defs,
                    body,
                    multi,
                });
            }
            return Ok(Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            });
        }
        Err(RuntimeError::new("Expected block for token/rule"))
    }

    fn parse_proto_token_decl(&mut self) -> Result<Stmt, RuntimeError> {
        let name = self.consume_ident()?;
        if self.match_kind(TokenKind::LParen) {
            while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                if self.match_kind(TokenKind::Star) || self.match_kind(TokenKind::Colon) {
                    // ignore modifiers
                }
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
                    let _ = self.consume_var()?;
                    if self.match_kind(TokenKind::Eq) {
                        let _ = self.parse_expr()?;
                    }
                    self.match_kind(TokenKind::Question);
                    self.match_kind(TokenKind::Bang);
                } else {
                    self.pos += 1;
                }
                self.match_kind(TokenKind::Comma);
            }
            self.match_kind(TokenKind::RParen);
        }
        if self.match_kind(TokenKind::LBrace) {
            let mut depth = 1usize;
            while depth > 0 && !self.check(&TokenKind::Eof) {
                if self.match_kind(TokenKind::LBrace) {
                    depth += 1;
                    continue;
                }
                if self.match_kind(TokenKind::RBrace) {
                    depth -= 1;
                    continue;
                }
                self.pos += 1;
            }
        } else {
            self.match_kind(TokenKind::Semicolon);
        }
        Ok(Stmt::ProtoToken { name })
    }

    fn parse_proto_sub_decl(&mut self) -> Result<Stmt, RuntimeError> {
        let name = self.parse_routine_name()?;
        let mut params = Vec::new();
        let mut param_defs = Vec::new();
        if self.match_kind(TokenKind::LParen) {
            while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                let mut is_named = false;
                let mut is_slurpy = false;
                if self.match_kind(TokenKind::Star) {
                    is_slurpy = true;
                }
                if self.match_kind(TokenKind::Colon) {
                    is_named = true;
                }
                let mut type_constraint = None;
                if matches!(
                    self.tokens.get(self.pos).map(|t| &t.kind),
                    Some(TokenKind::Ident(_))
                ) && matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Var(_))
                ) {
                    if let Some(TokenKind::Ident(ty)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                        type_constraint = Some(ty.clone());
                    }
                    self.pos += 1;
                }
                if self.peek_is_var() {
                    let var = self.consume_var()?;
                    let default = if self.match_kind(TokenKind::Eq) {
                        Some(self.parse_expr()?)
                    } else {
                        self.match_kind(TokenKind::QuestionQuestion);
                        None
                    };
                    self.match_kind(TokenKind::Question);
                    self.match_kind(TokenKind::Bang);
                    params.push(var.clone());
                    param_defs.push(ParamDef {
                        name: var,
                        default,
                        named: is_named,
                        slurpy: is_slurpy,
                        type_constraint,
                        literal_value: None,
                    });
                } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_)))
                {
                    if let TokenKind::ArrayVar(n) = token.kind {
                        let var = format!("@{}", n);
                        params.push(var.clone());
                        param_defs.push(ParamDef {
                            name: var,
                            default: None,
                            named: false,
                            slurpy: is_slurpy,
                            type_constraint: None,
                            literal_value: None,
                        });
                    }
                } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::HashVar(_)))
                    && let TokenKind::HashVar(n) = token.kind
                {
                    let var = format!("%{}", n);
                    params.push(var.clone());
                    param_defs.push(ParamDef {
                        name: var,
                        default: None,
                        named: is_named || is_slurpy,
                        slurpy: is_slurpy,
                        type_constraint: None,
                        literal_value: None,
                    });
                }
                if !self.match_kind(TokenKind::Comma) {
                    break;
                }
            }
            self.consume_kind(TokenKind::RParen)?;
        }
        if self.check(&TokenKind::LBrace) {
            let _ = self.parse_block()?;
        } else {
            self.match_kind(TokenKind::Semicolon);
        }
        Ok(Stmt::ProtoDecl {
            name,
            params,
            param_defs,
        })
    }

    fn parse_sub_decl(&mut self, is_multi: bool) -> Result<Stmt, RuntimeError> {
        let name = self.parse_routine_name()?;
        let mut params = Vec::new();
        let mut param_defs = Vec::new();
        if self.match_kind(TokenKind::LParen) {
            while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                let mut is_named = false;
                let mut is_slurpy = false;
                if self.match_kind(TokenKind::Star) {
                    is_slurpy = true;
                }
                if self.match_kind(TokenKind::Colon) {
                    is_named = true;
                }
                let mut type_constraint = None;
                let mut type_only_constraint: Option<String> = None;
                if let Some(TokenKind::Ident(ty)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                    let mut next = self.pos + 1;
                    if matches!(
                        self.tokens.get(next).map(|t| &t.kind),
                        Some(TokenKind::Colon)
                    ) {
                        if matches!(
                            self.tokens.get(next + 1).map(|t| &t.kind),
                            Some(TokenKind::Ident(_))
                        ) {
                            next += 2;
                        } else if matches!(
                            self.tokens.get(next + 1).map(|t| &t.kind),
                            Some(
                                TokenKind::Var(_)
                                    | TokenKind::CaptureVar(_)
                                    | TokenKind::ArrayVar(_)
                                    | TokenKind::HashVar(_)
                            )
                        ) {
                            is_named = true;
                            next += 1;
                        }
                    }
                    if matches!(
                        self.tokens.get(next).map(|t| &t.kind),
                        Some(
                            TokenKind::Var(_)
                                | TokenKind::CaptureVar(_)
                                | TokenKind::ArrayVar(_)
                                | TokenKind::HashVar(_)
                                | TokenKind::Ident(_)
                        )
                    ) {
                        type_constraint = Some(ty.clone());
                        self.pos = next;
                    } else if matches!(
                        self.tokens.get(next).map(|t| &t.kind),
                        Some(TokenKind::Comma | TokenKind::RParen)
                    ) {
                        type_only_constraint = Some(ty.clone());
                        self.pos = next;
                    }
                }
                let mut param_added = false;
                if self.peek_is_var() {
                    let var = self.consume_var()?;
                    let default = if self.match_kind(TokenKind::Eq) {
                        Some(self.parse_expr()?)
                    } else {
                        self.match_kind(TokenKind::QuestionQuestion);
                        None
                    };
                    self.match_kind(TokenKind::Question);
                    self.match_kind(TokenKind::Bang);
                    params.push(var.clone());
                    param_defs.push(ParamDef {
                        name: var,
                        default,
                        named: is_named,
                        slurpy: is_slurpy,
                        type_constraint,
                        literal_value: None,
                    });
                    param_added = true;
                } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_)))
                {
                    if let TokenKind::ArrayVar(n) = token.kind {
                        let var = format!("@{}", n);
                        params.push(var.clone());
                        param_defs.push(ParamDef {
                            name: var,
                            default: None,
                            named: false,
                            slurpy: is_slurpy,
                            type_constraint: None,
                            literal_value: None,
                        });
                        param_added = true;
                    }
                } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::HashVar(_)))
                {
                    if let TokenKind::HashVar(n) = token.kind {
                        let var = format!("%{}", n);
                        params.push(var.clone());
                        param_defs.push(ParamDef {
                            name: var,
                            default: None,
                            named: is_named || is_slurpy,
                            slurpy: is_slurpy,
                            type_constraint: None,
                            literal_value: None,
                        });
                        param_added = true;
                    }
                } else if let Some(token) =
                    self.advance_if(|k| matches!(k, TokenKind::CaptureVar(_)))
                {
                    if let TokenKind::CaptureVar(n) = token.kind {
                        params.push(n.clone());
                        param_defs.push(ParamDef {
                            name: n,
                            default: None,
                            named: is_named,
                            slurpy: is_slurpy,
                            type_constraint,
                            literal_value: None,
                        });
                        param_added = true;
                    }
                } else if let Some(TokenKind::Ident(n)) = self.tokens.get(self.pos).map(|t| &t.kind)
                {
                    let name = n.clone();
                    self.pos += 1;
                    params.push(name.clone());
                    param_defs.push(ParamDef {
                        name,
                        default: None,
                        named: is_named,
                        slurpy: is_slurpy,
                        type_constraint,
                        literal_value: None,
                    });
                    param_added = true;
                } else if let Some(literal_value) = self.parse_literal_param_value() {
                    params.push(String::new());
                    param_defs.push(ParamDef {
                        name: String::new(),
                        default: None,
                        named: false,
                        slurpy: false,
                        type_constraint: None,
                        literal_value: Some(literal_value),
                    });
                    param_added = true;
                } else if let Some(constraint) = type_only_constraint {
                    let name = format!("__param{}", param_defs.len());
                    params.push(name.clone());
                    param_defs.push(ParamDef {
                        name,
                        default: None,
                        named: is_named,
                        slurpy: is_slurpy,
                        type_constraint: Some(constraint),
                        literal_value: None,
                    });
                    param_added = true;
                }
                if !param_added {
                    break;
                }
                while self.match_ident("is") {
                    let _ = self.consume_ident();
                    while self.match_kind(TokenKind::Minus) {
                        let _ = self.consume_ident();
                    }
                    if self.match_kind(TokenKind::LParen) {
                        self.skip_balanced_parens();
                    } else if self.check(&TokenKind::Colon) {
                        let _ = self.try_parse_angled_op_name();
                    } else if self.match_kind(TokenKind::Lt) {
                        self.skip_balanced_angles();
                    }
                }
                if !self.match_kind(TokenKind::Comma) {
                    break;
                }
            }
            self.match_kind(TokenKind::RParen);
            if self.match_kind(TokenKind::Colon) {
                let _ = self.consume_ident();
                let _ = self.consume_ident();
            }
        }
        // Skip `is <trait>` annotations (e.g., `is test-assertion`, `is export`, `is rw`)
        while self.match_ident("is") {
            // Consume trait name (may contain hyphens)
            let _ = self.consume_ident();
            while self.match_kind(TokenKind::Minus) {
                let _ = self.consume_ident();
            }
            if self.match_kind(TokenKind::LParen) {
                self.skip_balanced_parens();
            } else if self.check(&TokenKind::Colon) {
                let _ = self.try_parse_angled_op_name();
            } else if self.match_kind(TokenKind::Lt) {
                self.skip_balanced_angles();
            }
        }
        if self.match_kind(TokenKind::LBrace) {
            let body = self.parse_block_body()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                multi: is_multi,
            });
        }
        Err(RuntimeError::new("Expected block for sub"))
    }

    fn parse_literal_param_value(&mut self) -> Option<Value> {
        if let Some(token) =
            self.advance_if(|k| matches!(k, TokenKind::Number(_) | TokenKind::BigNumber(_)))
        {
            match token.kind {
                TokenKind::Number(value) => return Some(Value::Int(value)),
                TokenKind::BigNumber(value) => return Some(Value::BigInt(value)),
                _ => {}
            }
        }
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Float(_)))
            && let TokenKind::Float(value) = token.kind
        {
            return Some(Value::Num(value));
        }
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Str(_)))
            && let TokenKind::Str(value) = token.kind
        {
            return Some(Value::Str(value));
        }
        if self.match_kind(TokenKind::True) {
            return Some(Value::Bool(true));
        }
        if self.match_kind(TokenKind::False) {
            return Some(Value::Bool(false));
        }
        if self.match_kind(TokenKind::Nil) {
            return Some(Value::Nil);
        }
        None
    }
}
