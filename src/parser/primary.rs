use super::*;

impl Parser {
    pub(super) fn parse_primary(&mut self) -> Result<Expr, RuntimeError> {
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
                if self.check_angle_start() {
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
        } else if self.check_angle_start() {
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
            if self.check_angle_start() {
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

    pub(super) fn is_whatever_star(expr: &Expr) -> bool {
        matches!(expr, Expr::Literal(Value::Num(f)) if f.is_infinite() && f.is_sign_positive())
    }

    pub(super) fn contains_whatever(expr: &Expr) -> bool {
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

    pub(super) fn replace_whatever(expr: &Expr) -> Expr {
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
}
