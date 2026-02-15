use super::*;

impl Parser {
    pub(super) fn parse_class_decl(&mut self) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_role_decl(&mut self) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_has_decl(&mut self) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_method_decl(&mut self, multi: bool) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_token_rule_decl(
        &mut self,
        is_rule: bool,
        multi: bool,
    ) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_proto_token_decl(&mut self) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_proto_sub_decl(&mut self) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_sub_decl(&mut self, is_multi: bool) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn parse_literal_param_value(&mut self) -> Option<Value> {
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
