use crate::ast::{AssignOp, CallArg, Expr, ParamDef, Stmt};
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
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::Eof) {
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
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, RuntimeError> {
        if self.match_kind(TokenKind::LBrace) {
            let body = self.parse_block_body()?;
            return Ok(Stmt::Block(body));
        }
        if self.match_ident("use") {
            let module = self.consume_ident().unwrap_or_else(|_| "unknown".to_string());
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
        if self.match_ident("unit") {
            if self.match_ident("module") {
                let name = self.consume_ident()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Package { name, body: Vec::new() });
            }
        }
        if self.match_ident("package") {
            let name = self.consume_ident()?;
            let body = self.parse_block()?;
            return Ok(Stmt::Package { name, body });
        }
        let is_multi = self.match_ident("multi");
        if is_multi {
            self.match_ident("sub"); // optional 'sub' after 'multi'
        }
        if is_multi || self.match_ident("sub") {
            let name = self.consume_ident()?;
            let mut params = Vec::new();
            let mut param_defs = Vec::new();
            if self.match_kind(TokenKind::LParen) {
                while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                    let mut is_named = false;
                    let mut is_slurpy = false;
                    // Check for slurpy *@args or *%opts
                    if self.match_kind(TokenKind::Star) {
                        is_slurpy = true;
                    }
                    // Check for named parameter :$name
                    if self.match_kind(TokenKind::Colon) {
                        is_named = true;
                    }
                    // Parse optional type annotation (e.g., Int $x)
                    let mut type_constraint = None;
                    if matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Ident(_)))
                        && matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Var(_)))
                    {
                        if let Some(TokenKind::Ident(ty)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                            type_constraint = Some(ty.clone());
                        }
                        self.pos += 1;
                    }
                    if self.peek_is_var() {
                        let var = self.consume_var()?;
                        // Check for default value
                        let default = if self.match_kind(TokenKind::Eq) {
                            Some(self.parse_expr()?)
                        } else if self.match_kind(TokenKind::QuestionQuestion) {
                            // Handle ?? !! ternary in default - skip for now
                            None
                        } else {
                            None
                        };
                        // Check for ? (optional) or ! (required) suffix
                        self.match_kind(TokenKind::Question);
                        self.match_kind(TokenKind::Bang);
                        params.push(var.clone());
                        param_defs.push(ParamDef { name: var, default, named: is_named, slurpy: is_slurpy, type_constraint });
                    } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_))) {
                        if let TokenKind::ArrayVar(n) = token.kind {
                            let var = format!("@{}", n);
                            params.push(var.clone());
                            param_defs.push(ParamDef { name: var, default: None, named: false, slurpy: is_slurpy, type_constraint: None });
                        }
                    } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::HashVar(_))) {
                        if let TokenKind::HashVar(n) = token.kind {
                            let var = format!("%{}", n);
                            params.push(var.clone());
                            param_defs.push(ParamDef { name: var, default: None, named: is_named || is_slurpy, slurpy: is_slurpy, type_constraint: None });
                        }
                    } else {
                        self.pos += 1;
                    }
                    self.match_kind(TokenKind::Comma);
                }
                self.match_kind(TokenKind::RParen);
            }
            while self.match_ident("is") {
                if self.match_kind(TokenKind::Colon) {
                    let _ = self.consume_ident();
                } else {
                    let _ = self.consume_ident();
                }
            }
            if self.match_kind(TokenKind::LBrace) {
                let body = self.parse_block_body()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::SubDecl { name, params, param_defs, body, multi: is_multi });
            }
            return Err(RuntimeError::new("Expected block for sub"));
        }
        if self.match_ident("return") {
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Return(expr));
        }
        if self.match_ident("subtest") {
            let name = self.parse_expr()?;
            if !self.match_kind(TokenKind::FatArrow) {
                return Err(RuntimeError::new("Expected fat arrow after subtest name"));
            }
            if self.match_ident("sub") {
                let body = self.parse_block()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Subtest { name, body, is_sub: true });
            }
            if self.match_kind(TokenKind::LBrace) {
                let body = self.parse_block_body()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Subtest { name, body, is_sub: false });
            }
            return Err(RuntimeError::new("Expected sub or block for subtest"));
        }
        if self.match_ident("my") {
            // Skip optional type annotation (e.g., my Str $a, my Int $b)
            if let Some(TokenKind::Ident(_)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                if let Some(TokenKind::Var(_) | TokenKind::ArrayVar(_) | TokenKind::HashVar(_)) = self.tokens.get(self.pos + 1).map(|t| &t.kind) {
                    self.pos += 1; // skip the type name
                }
            }
            let (name, is_array, is_hash) = if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_))) {
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
            } else {
                (self.consume_var()?, false, false)
            };
            if self.match_kind(TokenKind::Eq) || self.match_kind(TokenKind::Bind) {
                let expr = self.parse_comma_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::VarDecl { name, expr });
            }
            self.match_kind(TokenKind::Semicolon);
            let expr = if is_array {
                Expr::Literal(Value::Array(Vec::new()))
            } else if is_hash {
                Expr::Hash(Vec::new())
            } else {
                Expr::Literal(Value::Nil)
            };
            return Ok(Stmt::VarDecl { name, expr });
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
        if self.match_ident("if") {
            let cond = self.parse_expr()?;
            let then_branch = self.parse_block()?;
            let else_branch = if self.match_ident("elsif") {
                // Desugar elsif into nested if
                let elsif = self.parse_expr()?;
                let elsif_branch = self.parse_block()?;
                let inner_else = if self.match_ident("else") {
                    self.parse_block()?
                } else {
                    Vec::new()
                };
                vec![Stmt::If { cond: elsif, then_branch: elsif_branch, else_branch: inner_else }]
            } else if self.match_ident("else") {
                self.parse_block()?
            } else {
                Vec::new()
            };
            return Ok(Stmt::If { cond, then_branch, else_branch });
        }
        if self.match_ident("unless") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            // unless is if with negated condition
            return Ok(Stmt::If {
                cond: Expr::Unary { op: TokenKind::Bang, expr: Box::new(cond) },
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
                    }),
                },
                then_branch: body,
                else_branch: Vec::new(),
            });
        }
        // Labeled loop detection: LABEL: for/while/loop/...
        if let Some(label_name) = self.peek_ident() {
            if matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Colon)) {
                if let Some(TokenKind::Ident(kw)) = self.tokens.get(self.pos + 2).map(|t| &t.kind) {
                    if matches!(kw.as_str(), "for" | "while" | "until" | "loop" | "repeat") {
                        self.pos += 2; // skip label and colon
                        return self.parse_labeled_loop(Some(label_name));
                    }
                }
            }
        }
        if self.match_ident("while") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::While { cond, body, label: None });
        }
        if self.match_ident("until") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::While {
                cond: Expr::Unary { op: TokenKind::Bang, expr: Box::new(cond) },
                body,
                label: None,
            });
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
                    label: None,
                });
            } else if self.match_ident("until") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Loop {
                    init: None,
                    cond: Some(Expr::Unary { op: TokenKind::Bang, expr: Box::new(cond) }),
                    step: None,
                    body,
                    repeat: true,
                    label: None,
                });
            }
            return Err(RuntimeError::new("Expected 'while' or 'until' after repeat block"));
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
                return Ok(Stmt::Loop { init, cond, step, body, repeat: false, label: None });
            } else {
                let body = self.parse_block()?;
                return Ok(Stmt::Loop { init: None, cond: None, step: None, body, repeat: false, label: None });
            }
        }
        if self.match_ident("last") {
            // Check for label: last LABEL [if cond]
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
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Next(label));
        }
        if self.match_ident("for") {
            let iterable = self.parse_expr()?;
            let mut param = None;
            if self.match_kind(TokenKind::Arrow) {
                // Skip optional type annotation
                if matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Ident(_)))
                    && matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Var(_)))
                {
                    self.pos += 1;
                }
                if self.peek_is_var() {
                    param = Some(self.consume_var()?);
                }
            }
            let body = self.parse_block()?;
            return Ok(Stmt::For { iterable, param, body, label: None });
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
        if self.match_ident("die") || self.match_ident("fail") {
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Die(expr));
        }
        if self.match_ident("CATCH") {
            let body = self.parse_block()?;
            return Ok(Stmt::Catch(body));
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
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Redo);
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
        if let Some(name) = self.peek_ident() {
            if matches!(name.as_str(), "ok" | "is" | "isnt" | "nok" | "pass" | "flunk" | "cmp-ok" | "like" | "unlike" | "is-deeply" | "isa-ok" | "lives-ok" | "dies-ok" | "eval-lives-ok" | "is_run" | "throws-like" | "force_todo" | "force-todo" | "plan" | "done-testing" | "bail-out" | "skip" | "skip-rest" | "diag" | "todo" | "does-ok" | "can-ok") {
                self.pos += 1;
                let args = if name == "is"
                    && (!self.check(&TokenKind::LParen)
                        || self.is_is_grouping_paren())
                {
                    self.parse_is_call_args()
                } else if name == "ok"
                    && matches!(
                        self.tokens.get(self.pos).map(|t| &t.kind),
                        Some(TokenKind::LParen | TokenKind::Bang)
                    )
                {
                    self.parse_call_args_loose(&name)
                } else {
                    self.parse_call_args()?
                };
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Call { name, args });
            }
        }
        if self.peek_is_var() {
            if let Some(next) = self.peek_next_kind() {
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
                    };
                    return Ok(Stmt::Assign { name, expr, op: AssignOp::Assign });
                }
                if matches!(next, TokenKind::PlusEq | TokenKind::MinusEq | TokenKind::TildeEq | TokenKind::StarEq) {
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
                    self.match_kind(TokenKind::Semicolon);
                    let expr = Expr::Binary {
                        left: Box::new(Expr::Var(name.clone())),
                        op: compound_op,
                        right: Box::new(rhs),
                    };
                    return Ok(Stmt::Assign { name, expr, op: AssignOp::Assign });
                }
                if matches!(next, TokenKind::Eq | TokenKind::Bind | TokenKind::MatchAssign) {
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
                    self.match_kind(TokenKind::Semicolon);
                    return Ok(Stmt::Assign { name, expr, op });
                }
            }
        }
        if let Some(TokenKind::ArrayVar(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            if matches!(self.peek_next_kind(), Some(TokenKind::Eq | TokenKind::Bind | TokenKind::MatchAssign)) {
                let name = format!("@{}", name.clone());
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
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Assign { name, expr, op });
            }
        }
        let expr = self.parse_expr()?;
        let stmt = Stmt::Expr(expr);
        self.parse_statement_modifier(stmt)
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
        if self.match_ident("if") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::If { cond, then_branch: vec![stmt], else_branch: Vec::new() });
        }
        if self.match_ident("unless") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::If {
                cond: Expr::Unary { op: TokenKind::Bang, expr: Box::new(cond) },
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            });
        }
        if self.match_ident("for") {
            let iterable = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::For { iterable, param: None, body: vec![stmt], label: None });
        }
        if self.match_ident("while") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::While { cond, body: vec![stmt], label: None });
        }
        if self.match_ident("until") {
            let cond = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::While {
                cond: Expr::Unary { op: TokenKind::Bang, expr: Box::new(cond) },
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
                    }),
                },
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            });
        }
        if self.match_ident("given") {
            let topic = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Given { topic, body: vec![stmt] });
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

    fn parse_call_args_loose(&mut self, name: &str) -> Vec<CallArg> {
        let mut depth = 0usize;
        let mut last_str = None;
        while let Some(token) = self.tokens.get(self.pos) {
            match token.kind {
                TokenKind::LParen | TokenKind::LBrace => depth += 1,
                TokenKind::RParen | TokenKind::RBrace => {
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                TokenKind::Semicolon => {
                    if depth == 0 {
                        break;
                    }
                }
                TokenKind::Str(ref s) => last_str = Some(s.clone()),
                _ => {}
            }
            self.pos += 1;
        }
        let mut args = Vec::new();
        if matches!(name, "is" | "isnt") {
            args.push(CallArg::Positional(Expr::Literal(Value::Bool(true))));
            args.push(CallArg::Positional(Expr::Literal(Value::Bool(true))));
        } else {
            args.push(CallArg::Positional(Expr::Literal(Value::Bool(true))));
        }
        if let Some(desc) = last_str {
            args.push(CallArg::Positional(Expr::Literal(Value::Str(desc))));
        }
        args
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
                TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
                TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                    if depth > 0 {
                        depth -= 1;
                    } else {
                        break;
                    }
                }
                TokenKind::Comma | TokenKind::Semicolon if depth == 0 => break,
                _ => {}
            }
            self.pos += 1;
        }
        let mut slice = self.tokens[start..self.pos].to_vec();
        slice.push(Token { kind: TokenKind::Eof });
        let mut parser = Parser::new(slice);
        parser.parse_expr().unwrap_or(Expr::Literal(Value::Bool(true)))
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
                            Some(TokenKind::Comma | TokenKind::Semicolon | TokenKind::Eof | TokenKind::RParen)
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
                return Ok(CallArg::Named { name, value: Some(value) });
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
                return Ok(CallArg::Named { name, value: Some(Expr::ArrayLiteral(items)) });
            }
            return Ok(CallArg::Named { name, value: None });
        }
        if let Some(name) = self.peek_ident() {
            if matches!(self.peek_next_kind(), Some(TokenKind::FatArrow)) {
                self.pos += 1;
                self.consume_kind(TokenKind::FatArrow)?;
                let value = self.parse_expr()?;
                return Ok(CallArg::Named { name, value: Some(value) });
            }
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
        if let Some(TokenKind::Var(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            if matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Eq)) {
                let name = name.clone();
                self.pos += 2;
                let expr = self.parse_expr()?;
                return Ok(Expr::AssignExpr { name, expr: Box::new(expr) });
            }
        }
        self.parse_ternary()
    }

    fn parse_comma_expr(&mut self) -> Result<Expr, RuntimeError> {
        let mut exprs = Vec::new();
        exprs.push(self.parse_expr()?);
        if self.match_kind(TokenKind::Comma) {
            exprs.push(self.parse_expr()?);
            while self.match_kind(TokenKind::Comma) {
                exprs.push(self.parse_expr()?);
            }
            return Ok(Expr::ArrayLiteral(exprs));
        }
        Ok(exprs.remove(0))
    }

    fn parse_expr_list(&mut self) -> Result<Vec<Expr>, RuntimeError> {
        let first = self.parse_expr()?;
        let mut exprs = vec![first];
        while self.match_kind(TokenKind::Comma) {
            // Stop if next token looks like a statement modifier keyword
            if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
                if matches!(name.as_str(), "if" | "unless" | "for" | "while" | "until") {
                    break;
                }
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_method_arg(&mut self) -> Expr {
        if self.match_kind(TokenKind::Colon) {
            let name = self.consume_ident().unwrap_or_default();
            let value = if self.check(&TokenKind::Lt) {
                self.parse_angle_literal()
            } else if self.match_kind(TokenKind::LParen) {
                let expr = self.parse_expr_or_true();
                self.match_kind(TokenKind::RParen);
                expr
            } else {
                self.parse_expr_or_true()
            };
            return Expr::AssignExpr { name, expr: Box::new(value) };
        }
        self.parse_expr_or_true()
    }

    fn parse_angle_literal(&mut self) -> Expr {
        if !self.match_kind(TokenKind::Lt) {
            return Expr::Literal(Value::Nil);
        }
        let value = if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_) | TokenKind::Str(_))) {
            match token.kind {
                TokenKind::Ident(s) => s,
                TokenKind::Str(s) => s,
                _ => String::new(),
            }
        } else {
            String::new()
        };
        self.match_kind(TokenKind::Gt);
        Expr::Literal(Value::Str(value))
    }

    fn parse_ternary(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_or()?;
        if self.match_kind(TokenKind::QuestionQuestion) {
            let then_expr = self.parse_or()?;
            if !self.match_kind(TokenKind::BangBang) {
                return Err(RuntimeError::new("Expected !! in ternary"));
            }
            let else_expr = self.parse_or()?;
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
            && !self.check(&TokenKind::Eof)
        {
            self.pos += 1;
        }
    }

    fn parse_or(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_and()?;
        loop {
            if self.match_kind(TokenKind::OrOr) {
                let right = self.parse_and()?;
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::OrOr, right: Box::new(right) };
            } else if self.match_kind(TokenKind::OrWord) {
                let right = self.parse_and()?;
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::OrWord, right: Box::new(right) };
            } else if self.match_kind(TokenKind::SlashSlash) {
                let right = self.parse_and()?;
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::SlashSlash, right: Box::new(right) };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_equality()?;
        loop {
            if self.match_kind(TokenKind::AndAnd) || self.match_ident("and") {
                let right = self.parse_equality()?;
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::AndAnd, right: Box::new(right) };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.match_kind(TokenKind::EqEq) {
                let op = TokenKind::EqEq;
                let right = self.parse_comparison()?;
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
            } else if self.match_kind(TokenKind::EqEqEq) {
                let op = TokenKind::EqEqEq;
                let right = self.parse_comparison()?;
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
            } else if self.match_kind(TokenKind::BangEq) {
                let op = TokenKind::BangEq;
                let right = self.parse_comparison()?;
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
            } else if self.match_ident("eq") {
                let right = self.parse_comparison()?;
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::Ident("eq".to_string()), right: Box::new(right) };
            } else if self.match_ident("ne") {
                let right = self.parse_comparison()?;
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::Ident("ne".to_string()), right: Box::new(right) };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_term()?;
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
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_term()?;
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
            } else {
                break;
            }
        }
        Ok(expr)
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
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::Tilde, right: Box::new(right) };
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
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::Ident("xx".to_string()), right: Box::new(right) };
                continue;
            }
            if self.match_ident("x") {
                let right = self.parse_additive()?;
                expr = Expr::Binary { left: Box::new(expr), op: TokenKind::Ident("x".to_string()), right: Box::new(right) };
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
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
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
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_power()?;
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
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
            Ok(Expr::Binary { left: Box::new(base), op: TokenKind::StarStar, right: Box::new(exp) })
        } else {
            Ok(base)
        }
    }

    fn parse_infix_func(&mut self, left: Expr) -> Result<Option<Expr>, RuntimeError> {
        let mut modifier = None;
        let start_pos = self.pos;
        if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            if name == "R" || name == "X" {
                modifier = Some(name.clone());
                self.pos += 1;
            }
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
            return Ok(Expr::Unary { op: TokenKind::PlusPlus, expr: Box::new(expr) });
        }
        if self.match_kind(TokenKind::MinusMinus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::MinusMinus, expr: Box::new(expr) });
        }
        if self.match_kind(TokenKind::Plus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Plus, expr: Box::new(expr) });
        }
        if self.match_kind(TokenKind::Minus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Minus, expr: Box::new(expr) });
        }
        if self.match_kind(TokenKind::Bang) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Bang, expr: Box::new(expr) });
        }
        if self.match_ident("not") {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Bang, expr: Box::new(expr) });
        }
        if self.match_ident("so") {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Ident("so".to_string()), expr: Box::new(expr) });
        }
        if self.match_kind(TokenKind::Question) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Question, expr: Box::new(expr) });
        }
        if self.match_kind(TokenKind::Caret) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Caret, expr: Box::new(expr) });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = if self.match_kind(TokenKind::RParen) {
            Expr::Literal(Value::Nil)
        } else if self.match_kind(TokenKind::Dot) {
            let name = self.consume_ident()?;
            Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name,
                args: Vec::new(),
            }
        } else if self.match_kind(TokenKind::LParen) {
            let expr = self.parse_expr()?;
            self.consume_kind(TokenKind::RParen)?;
            expr
        } else if self.check(&TokenKind::LBracket) && self.is_reduction_op() {
            self.parse_reduction()?
        } else if self.match_kind(TokenKind::LBracket) {
            let mut items = Vec::new();
            if !self.check(&TokenKind::RBracket) {
                items.push(self.parse_expr_or_true());
                while self.match_kind(TokenKind::Comma) {
                    items.push(self.parse_expr_or_true());
                }
            }
            self.consume_kind(TokenKind::RBracket)?;
            Expr::ArrayLiteral(items)
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Number(_))) {
            if let TokenKind::Number(value) = token.kind {
                Expr::Literal(Value::Int(value))
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Float(_))) {
            if let TokenKind::Float(value) = token.kind {
                Expr::Literal(Value::Num(value))
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
                            if name.starts_with('@') {
                                exprs.push(Expr::ArrayVar(name[1..].to_string()));
                            } else {
                                exprs.push(Expr::Var(name));
                            }
                        }
                        DStrPart::Block(code) => {
                            exprs.push(Expr::Literal(Value::Str(code)));
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
        } else if self.match_kind(TokenKind::True) {
            Expr::Literal(Value::Bool(true))
        } else if self.match_kind(TokenKind::False) {
            Expr::Literal(Value::Bool(false))
        } else if self.match_kind(TokenKind::Nil) {
            Expr::Literal(Value::Nil)
        } else if let Some(name) = self.peek_ident() {
            if matches!(self.peek_next_kind(), Some(TokenKind::LParen)) {
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
            } else if name == "my"
                && matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Var(_)))
            {
                self.pos += 1;
                let var_name = self.consume_var()?;
                if self.match_kind(TokenKind::Eq) || self.match_kind(TokenKind::Bind) {
                    let expr = self.parse_comma_expr()?;
                    Expr::AssignExpr { name: var_name, expr: Box::new(expr) }
                } else {
                    Expr::Literal(Value::Nil)
                }
            } else if name == "class" && matches!(self.peek_next_kind(), Some(TokenKind::LBrace)) {
                self.pos += 1;
                self.consume_kind(TokenKind::LBrace)?;
                self.skip_brace_block();
                Expr::Literal(Value::Nil)
            } else if name == "EVAL" && matches!(self.peek_next_kind(), Some(TokenKind::Str(_))) {
                let name = self.consume_ident()?;
                let arg = self.parse_expr()?;
                Expr::Call { name, args: vec![arg] }
            } else if name == "sub" && matches!(self.peek_next_kind(), Some(TokenKind::LBrace)) {
                self.pos += 1;
                let body = self.parse_block()?;
                Expr::AnonSub(body)
            } else {
                let name = self.consume_ident()?;
                if name == "do" && self.check(&TokenKind::LBrace) {
                    let body = self.parse_block()?;
                    Expr::Block(body)
                } else if name == "so" {
                    let inner = self.parse_expr()?;
                    Expr::Unary { op: TokenKind::Bang, expr: Box::new(
                        Expr::Unary { op: TokenKind::Bang, expr: Box::new(inner) }
                    )}
                } else if name == "quietly" {
                    self.parse_expr()?
                } else if name == "eager" {
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
                    Expr::Literal(Value::Int(0))
                } else if name == "Bool::False" {
                    Expr::Literal(Value::Bool(false))
                } else if name == "Bool::True" {
                    Expr::Literal(Value::Bool(true))
                } else {
                    Expr::Literal(Value::Str(name))
                }
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::HashVar(_))) {
            if let TokenKind::HashVar(name) = token.kind {
                Expr::HashVar(name)
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_))) {
            if let TokenKind::ArrayVar(name) = token.kind {
                Expr::ArrayVar(name)
            } else {
                Expr::Literal(Value::Nil)
            }
        } else if self.match_kind(TokenKind::RoutineMagic) {
            Expr::RoutineMagic
        } else if self.match_kind(TokenKind::BlockMagic) {
            Expr::BlockMagic
        } else if self.match_kind(TokenKind::Arrow) {
            if matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Ident(_)))
                && matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Var(_)))
            {
                self.pos += 1;
            }
            let param = if self.peek_is_var() {
                self.consume_var()?
            } else {
                String::new()
            };
            let body = self.parse_block()?;
            Expr::Lambda { param, body }
        } else if self.peek_is_var() {
            let name = self.consume_var()?;
            Expr::Var(name)
        } else if self.match_kind(TokenKind::LBrace) {
            if self.is_hash_literal_start() {
                let pairs = self.parse_hash_literal()?;
                Expr::Hash(pairs)
            } else {
                let body = self.parse_block_body()?;
                Expr::Block(body)
            }
        } else {
            return Err(RuntimeError::new(format!(
                "Unexpected token in expression at {:?}",
                self.tokens.get(self.pos).map(|t| &t.kind)
            )));
        };

        loop {
            if self.match_kind(TokenKind::PlusPlus) {
                expr = Expr::PostfixOp { op: TokenKind::PlusPlus, expr: Box::new(expr) };
                continue;
            }
            if self.match_kind(TokenKind::MinusMinus) {
                expr = Expr::PostfixOp { op: TokenKind::MinusMinus, expr: Box::new(expr) };
                continue;
            }
            if self.match_kind(TokenKind::Dot) {
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
                }
                expr = Expr::MethodCall { target: Box::new(expr), name: full_name, args };
                continue;
            }
            if self.match_kind(TokenKind::LBracket) {
                let index = self.parse_expr_or_true();
                self.consume_kind(TokenKind::RBracket)?;
                expr = Expr::Index { target: Box::new(expr), index: Box::new(index) };
                continue;
            }
            if matches!(expr, Expr::HashVar(_)) && self.check(&TokenKind::LBrace) {
                self.match_kind(TokenKind::LBrace);
                let index = self.parse_expr()?;
                self.consume_kind(TokenKind::RBrace)?;
                expr = Expr::Index { target: Box::new(expr), index: Box::new(index) };
                continue;
            }
            if self.check(&TokenKind::Lt) {
                if matches!(expr, Expr::HashVar(_)) {
                    self.match_kind(TokenKind::Lt);
                    let key = if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_) | TokenKind::Str(_))) {
                        match token.kind {
                            TokenKind::Ident(s) => s,
                            TokenKind::Str(s) => s,
                            _ => String::new(),
                        }
                    } else {
                        String::new()
                    };
                    self.consume_kind(TokenKind::Gt)?;
                    if let Expr::HashVar(name) = expr {
                        if name == "*ENV" {
                            expr = Expr::EnvIndex(key);
                        } else {
                            expr = Expr::Index {
                                target: Box::new(Expr::HashVar(name)),
                                index: Box::new(Expr::Literal(Value::Str(key))),
                            };
                        }
                    }
                    continue;
                }
            }
            if self.check(&TokenKind::Colon) {
                if matches!(
                    self.tokens.get(self.pos + 1).map(|t| &t.kind),
                    Some(TokenKind::Ident(name)) if name == "exists"
                ) {
                    self.match_kind(TokenKind::Colon);
                    let _ = self.consume_ident()?;
                    expr = Expr::Exists(Box::new(expr));
                    continue;
                }
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
                expr = Expr::CallOn { target: Box::new(expr), args };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_hash_pair(&mut self) -> Result<(String, Option<Expr>), RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_) | TokenKind::Str(_))) {
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
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Number(_))) {
            let number = if let TokenKind::Number(value) = token.kind { value } else { 0 };
            let name = self.consume_ident()?;
            return Ok((name, Some(Expr::Literal(Value::Int(number)))));
        }
        let name = self.consume_ident()?;
        if self.match_kind(TokenKind::LParen) {
            let value = self.parse_expr()?;
            self.consume_kind(TokenKind::RParen)?;
            return Ok((name, Some(value)));
        }
        Ok((name, None))
    }

    fn parse_hash_literal(&mut self) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
        let mut pairs = Vec::new();
        let mut failed = false;
        if !self.check(&TokenKind::RBrace) {
            match self.parse_hash_pair() {
                Ok(pair) => pairs.push(pair),
                Err(_) => failed = true,
            }
            while !failed && self.match_kind(TokenKind::Comma) {
                match self.parse_hash_pair() {
                    Ok(pair) => pairs.push(pair),
                    Err(_) => {
                        failed = true;
                        break;
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

    fn is_hash_literal_start(&self) -> bool {
        if matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Colon)) {
            return true;
        }
        matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::Ident(_)) | Some(TokenKind::Str(_))
        ) && matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::FatArrow))
    }

    fn skip_brace_block(&mut self) {
        let mut depth = 1usize;
        while let Some(token) = self.tokens.get(self.pos) {
            match token.kind {
                TokenKind::LBrace => depth += 1,
                TokenKind::RBrace => {
                    depth -= 1;
                    if depth == 0 {
                        self.pos += 1;
                        break;
                    }
                }
                _ => {}
            }
            self.pos += 1;
        }
    }

    fn consume_var(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Var(_))) {
            if let TokenKind::Var(name) = token.kind {
                return Ok(name);
            }
        }
        Err(RuntimeError::new("Expected variable"))
    }

    fn consume_ident(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_))) {
            if let TokenKind::Ident(name) = token.kind {
                return Ok(name);
            }
        }
        Err(RuntimeError::new("Expected identifier"))
    }

    fn match_ident(&mut self, ident: &str) -> bool {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_))) {
            if let TokenKind::Ident(name) = token.kind {
                if name == ident {
                    return true;
                }
                self.pos -= 1;
            }
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
        self.tokens.get(self.pos).map(|t| &t.kind) == Some(kind)
    }

    fn advance_if<F>(&mut self, predicate: F) -> Option<Token>
    where
        F: Fn(&TokenKind) -> bool,
    {
        if let Some(token) = self.tokens.get(self.pos) {
            if predicate(&token.kind) {
                self.pos += 1;
                return Some(token.clone());
            }
        }
        None
    }

    fn peek_is_var(&self) -> bool {
        matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Var(_)))
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
        if let Some(TokenKind::Ident(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            if !matches!(name.as_str(), "if" | "unless" | "while" | "until" | "for" | "given" | "when" | "with" | "without") {
                let label = name.clone();
                self.pos += 1;
                return Some(label);
            }
        }
        None
    }

    fn parse_labeled_loop(&mut self, label: Option<String>) -> Result<Stmt, RuntimeError> {
        if self.match_ident("for") {
            let iterable = self.parse_expr()?;
            let mut param = None;
            if self.match_kind(TokenKind::Arrow) {
                if matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Ident(_)))
                    && matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Var(_)))
                {
                    self.pos += 1;
                }
                if self.peek_is_var() {
                    param = Some(self.consume_var()?);
                }
            }
            let body = self.parse_block()?;
            return Ok(Stmt::For { iterable, param, body, label });
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
                cond: Expr::Unary { op: TokenKind::Bang, expr: Box::new(cond) },
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
                return Ok(Stmt::Loop { init, cond, step, body, repeat: false, label });
            } else {
                let body = self.parse_block()?;
                return Ok(Stmt::Loop { init: None, cond: None, step: None, body, repeat: false, label });
            }
        }
        if self.match_ident("repeat") {
            let body = self.parse_block()?;
            if self.match_ident("while") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Loop { init: None, cond: Some(cond), step: None, body, repeat: true, label });
            } else if self.match_ident("until") {
                let cond = self.parse_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Loop {
                    init: None,
                    cond: Some(Expr::Unary { op: TokenKind::Bang, expr: Box::new(cond) }),
                    step: None,
                    body,
                    repeat: true,
                    label,
                });
            }
            return Err(RuntimeError::new("Expected 'while' or 'until' after repeat block"));
        }
        Err(RuntimeError::new("Expected loop keyword after label"))
    }

    fn is_reduction_op(&self) -> bool {
        if !matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::LBracket)) {
            return false;
        }
        if !matches!(self.tokens.get(self.pos + 2).map(|t| &t.kind), Some(TokenKind::RBracket)) {
            return false;
        }
        match self.tokens.get(self.pos + 1).map(|t| &t.kind) {
            Some(TokenKind::Plus | TokenKind::Minus | TokenKind::Star |
                 TokenKind::Slash | TokenKind::StarStar | TokenKind::Tilde |
                 TokenKind::AndAnd | TokenKind::OrOr | TokenKind::SlashSlash |
                 TokenKind::Comma | TokenKind::LtEqGt | TokenKind::EqEq |
                 TokenKind::BangEq | TokenKind::Lt | TokenKind::Gt |
                 TokenKind::Lte | TokenKind::Gte |
                 TokenKind::BitAnd | TokenKind::BitOr | TokenKind::BitXor) => true,
            Some(TokenKind::Ident(name)) => matches!(name.as_str(),
                "min" | "max" | "leg" | "cmp" | "eq" | "ne" | "lt" | "gt" |
                "le" | "ge" | "lcm" | "gcd" | "and" | "or" | "x"
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
        Ok(Expr::Reduction { op: op_name, expr: Box::new(expr) })
    }
}
