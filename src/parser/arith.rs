use super::*;

impl Parser {
    pub(super) fn parse_term(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn parse_replication(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn parse_additive(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_factor(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn parse_power(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_infix_func(&mut self, left: Expr) -> Result<Option<Expr>, RuntimeError> {
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

    pub(super) fn parse_unary(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn parse_caret_operand(&mut self) -> Result<Expr, RuntimeError> {
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
}
