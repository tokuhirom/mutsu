use super::*;

impl Parser {
    pub(super) fn parse_hash_pair(&mut self) -> Result<(String, Option<Expr>), RuntimeError> {
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
        if self.check_angle_start() {
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

    pub(super) fn parse_hash_literal(
        &mut self,
    ) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
        let mut pairs = Vec::new();
        let mut failed = false;
        if !self.check(&TokenKind::RBrace) {
            if self.check_angle_start() {
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
                if self.check_angle_start() {
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
    pub(super) fn parse_angle_into_hash_pairs(&mut self, pairs: &mut Vec<(String, Option<Expr>)>) {
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

    pub(super) fn is_hash_literal_start(&self) -> bool {
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

    pub(super) fn consume_var(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Var(_)))
            && let TokenKind::Var(name) = token.kind
        {
            return Ok(name);
        }
        Err(RuntimeError::new("Expected variable"))
    }

    pub(super) fn consume_codevar(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::CodeVar(_)))
            && let TokenKind::CodeVar(name) = token.kind
        {
            return Ok(name);
        }
        Err(RuntimeError::new("Expected code variable"))
    }

    pub(super) fn consume_ident(&mut self) -> Result<String, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_)))
            && let TokenKind::Ident(name) = token.kind
        {
            return Ok(name);
        }
        Err(RuntimeError::new("Expected identifier"))
    }

    pub(super) fn match_ident(&mut self, ident: &str) -> bool {
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

    pub(super) fn match_kind(&mut self, kind: TokenKind) -> bool {
        if self.check(&kind) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    pub(super) fn consume_kind(&mut self, kind: TokenKind) -> Result<(), RuntimeError> {
        if self.check(&kind) {
            self.pos += 1;
            Ok(())
        } else {
            Err(RuntimeError::new("Unexpected token"))
        }
    }

    pub(super) fn check(&self, kind: &TokenKind) -> bool {
        match self.tokens.get(self.pos) {
            Some(t) => &t.kind == kind,
            None => *kind == TokenKind::Eof,
        }
    }

    /// Check if the current token starts an angle-bracket word list (QWords or Lt)
    pub(super) fn check_angle_start(&self) -> bool {
        matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::QWords(_)) | Some(TokenKind::Lt)
        )
    }

    pub(super) fn advance_if<F>(&mut self, predicate: F) -> Option<Token>
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

    pub(super) fn peek_is_var(&self) -> bool {
        matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::Var(_))
        )
    }

    pub(super) fn peek_is_codevar(&self) -> bool {
        matches!(
            self.tokens.get(self.pos).map(|t| &t.kind),
            Some(TokenKind::CodeVar(_))
        )
    }

    pub(super) fn parse_pointy_param_name(&mut self) -> Option<String> {
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

    pub(super) fn peek_ident(&self) -> Option<String> {
        match self.tokens.get(self.pos).map(|t| &t.kind) {
            Some(TokenKind::Ident(name)) => Some(name.clone()),
            _ => None,
        }
    }

    pub(super) fn peek_next_kind(&self) -> Option<TokenKind> {
        self.tokens.get(self.pos + 1).map(|t| t.kind.clone())
    }

    pub(super) fn try_consume_loop_label(&mut self) -> Option<String> {
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

    pub(super) fn parse_labeled_loop(
        &mut self,
        label: Option<String>,
    ) -> Result<Stmt, RuntimeError> {
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

    pub(super) fn is_reduction_op(&self) -> bool {
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
                | TokenKind::BangPercentPercent
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

    pub(super) fn parse_reduction(&mut self) -> Result<Expr, RuntimeError> {
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
            Some(TokenKind::BangPercentPercent) => "!%%".to_string(),
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
    pub(super) fn try_parse_angled_op_name(&mut self) -> Option<String> {
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

    pub(super) fn parse_routine_name(&mut self) -> Result<String, RuntimeError> {
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

    pub(super) fn skip_balanced_parens(&mut self) {
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

    pub(super) fn skip_balanced_angles(&mut self) {
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

    pub(super) fn token_to_op_str(tok: &TokenKind) -> Option<&'static str> {
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
            TokenKind::BangPercentPercent => Some("!%%"),
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
}
