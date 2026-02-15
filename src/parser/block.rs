use super::*;

impl Parser {
    pub(super) fn parse_enum_decl(&mut self) -> Result<Stmt, RuntimeError> {
        let name = self.consume_ident()?;
        let variants = if self.check_angle_start() {
            self.parse_enum_angle_variants()?
        } else if self.match_kind(TokenKind::LParen) {
            self.parse_enum_paren_variants()?
        } else {
            return Err(RuntimeError::new("Expected < or ( after enum name"));
        };
        self.match_kind(TokenKind::Semicolon);
        Ok(Stmt::EnumDecl { name, variants })
    }

    pub(super) fn parse_enum_angle_variants(
        &mut self,
    ) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
        // Handle QWords token (lexer-level word list)
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::QWords(_))) {
            let words = match token.kind {
                TokenKind::QWords(w) => w,
                _ => unreachable!(),
            };
            return Ok(words.into_iter().map(|w| (w, None)).collect());
        }
        self.consume_kind(TokenKind::Lt)?;
        let mut variants = Vec::new();
        while !self.check(&TokenKind::Gt) && !self.check(&TokenKind::Eof) {
            let key = self.consume_ident()?;
            variants.push((key, None));
        }
        self.consume_kind(TokenKind::Gt)?;
        Ok(variants)
    }

    pub(super) fn parse_enum_paren_variants(
        &mut self,
    ) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
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
    pub(super) fn parse_if_chain(&mut self) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_block_body(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
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

    pub(super) fn parse_statement_modifier(&mut self, stmt: Stmt) -> Result<Stmt, RuntimeError> {
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
}
