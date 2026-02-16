use super::*;

impl Parser {
    pub(super) fn try_meta_op(&self) -> Option<(String, String, usize)> {
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
    pub(super) fn try_meta_op_zx(&self) -> Option<(String, String, usize)> {
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

    pub(super) fn try_bracket_meta_op_at(&self, pos: usize) -> Option<(String, String, usize)> {
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

    pub(super) fn parse_meta_op(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn parse_hyper(&mut self) -> Result<Expr, RuntimeError> {
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

    pub(super) fn is_hyper_op(&self) -> bool {
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
                | TokenKind::BangPercentPercent
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

    pub(super) fn consume_hyper_inner_op(&mut self) -> Result<String, RuntimeError> {
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
}
