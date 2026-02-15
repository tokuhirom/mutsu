use super::*;

impl Parser {
    pub(super) fn parse_call_args(&mut self) -> Result<Vec<CallArg>, RuntimeError> {
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

    pub(super) fn parse_is_call_args(&mut self) -> Vec<CallArg> {
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

    pub(super) fn parse_expr_until_delim(&mut self) -> Expr {
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

    pub(super) fn is_call_paren(&self) -> bool {
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

    pub(super) fn is_is_grouping_paren(&self) -> bool {
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

    pub(super) fn parse_call_arg(&mut self) -> Result<CallArg, RuntimeError> {
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
}
