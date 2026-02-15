use super::*;

impl Parser {
    pub(super) fn parse_expr(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_comma_expr(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_expr_list(&mut self) -> Result<Vec<Expr>, RuntimeError> {
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

    pub(super) fn parse_method_arg(&mut self) -> Expr {
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
            let value = if self.check_angle_start() {
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

    pub(super) fn parse_angle_literal(&mut self) -> Expr {
        // New path: lexer produces QWords token directly
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::QWords(_))) {
            let words = match token.kind {
                TokenKind::QWords(w) => w,
                _ => unreachable!(),
            };
            return if words.len() == 1 {
                Expr::Literal(Value::Str(words.into_iter().next().unwrap()))
            } else {
                Expr::ArrayLiteral(
                    words
                        .into_iter()
                        .map(|w| Expr::Literal(Value::Str(w)))
                        .collect(),
                )
            };
        }
        // Fallback: Lt token (comparison context, shouldn't normally reach here for word lists)
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
                TokenKind::Minus => "-".to_string(),
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

    pub(super) fn parse_ternary(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_expr_or_true(&mut self) -> Expr {
        match self.parse_expr() {
            Ok(expr) => expr,
            Err(_) => {
                self.recover_to_delim();
                Expr::Literal(Value::Bool(true))
            }
        }
    }

    pub(super) fn recover_to_delim(&mut self) {
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
    pub(super) fn parse_or(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn parse_and(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn parse_tight_or(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn parse_tight_and(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_junction(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_equality(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_comparison(&mut self) -> Result<Expr, RuntimeError> {
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
    pub(super) fn is_chainable_comparison(op: &TokenKind) -> bool {
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
}
