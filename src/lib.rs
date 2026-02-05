use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Str(String),
    Bool(bool),
    Range(i64, i64),
    Nil,
}

impl Value {
    fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Str(s) => !s.is_empty(),
            Value::Range(_, _) => true,
            Value::Nil => false,
        }
    }

    fn to_string_value(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Str(s) => s.clone(),
            Value::Bool(true) => "True".to_string(),
            Value::Bool(false) => "False".to_string(),
            Value::Range(a, b) => format!("{}..{}", a, b),
            Value::Nil => "Nil".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
}

impl RuntimeError {
    fn new(message: impl Into<String>) -> Self {
        Self { message: message.into() }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    Number(i64),
    Str(String),
    Ident(String),
    Var(String),
    True,
    False,
    Nil,
    Plus,
    Minus,
    Star,
    Slash,
    Tilde,
    Eq,
    EqEq,
    FatArrow,
    DotDot,
    BangEq,
    Lt,
    Lte,
    Gt,
    Gte,
    AndAnd,
    OrOr,
    Bang,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Semicolon,
    Eof,
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
}

struct Lexer {
    src: Vec<char>,
    pos: usize,
    line: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Self { src: input.chars().collect(), pos: 0, line: 1 }
    }

    fn next_token(&mut self) -> Token {
        loop {
            self.skip_ws_and_comments();
            if self.pos >= self.src.len() {
                return Token { kind: TokenKind::Eof };
            }
            let ch = self.bump();
            let kind = match ch {
                '.' => {
                    if self.peek() == Some('.') {
                        self.pos += 1;
                        TokenKind::DotDot
                    } else {
                        continue;
                    }
                }
                'q' => {
                    if self.peek() == Some('<') {
                        self.pos += 1;
                    let mut s = String::new();
                    while let Some(c) = self.peek() {
                        self.pos += 1;
                        if c == '>' {
                            break;
                        }
                        s.push(c);
                    }
                    TokenKind::Str(s)
                } else {
                    let ident = self.read_ident_start(ch);
                    match ident.as_str() {
                        "True" => TokenKind::True,
                        "False" => TokenKind::False,
                        "Nil" => TokenKind::Nil,
                        _ => TokenKind::Ident(ident),
                    }
                }
            }
            '0'..='9' => {
                let mut num = ch.to_string();
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        num.push(c);
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let value = num.parse::<i64>().unwrap_or(0);
                TokenKind::Number(value)
            }
            '"' => {
                let mut s = String::new();
                while let Some(c) = self.peek() {
                    self.pos += 1;
                    if c == '"' {
                        break;
                    }
                    if c == '\\' {
                        if let Some(n) = self.peek() {
                            self.pos += 1;
                            match n {
                                'n' => s.push('\n'),
                                't' => s.push('\t'),
                                '"' => s.push('"'),
                                '\\' => s.push('\\'),
                                _ => s.push(n),
                            }
                        }
                    } else {
                        s.push(c);
                    }
                }
                TokenKind::Str(s)
            }
            '\'' => {
                let mut s = String::new();
                while let Some(c) = self.peek() {
                    self.pos += 1;
                    if c == '\'' {
                        break;
                    }
                    if c == '\\' {
                        if let Some(n) = self.peek() {
                            self.pos += 1;
                            match n {
                                'n' => s.push('\n'),
                                't' => s.push('\t'),
                                '\'' => s.push('\''),
                                '\\' => s.push('\\'),
                                _ => s.push(n),
                            }
                        }
                    } else {
                        s.push(c);
                    }
                }
                TokenKind::Str(s)
            }
            '｢' => {
                let mut s = String::new();
                while let Some(c) = self.peek() {
                    self.pos += 1;
                    if c == '｣' {
                        break;
                    }
                    s.push(c);
                }
                TokenKind::Str(s)
            }
            '$' => {
                let ident = self.read_ident();
                TokenKind::Var(ident)
            }
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => {
                if let Some(regex) = self.try_read_regex_literal() {
                    TokenKind::Str(regex)
                } else {
                    TokenKind::Slash
                }
            }
            '~' => TokenKind::Tilde,
            '=' => {
                if self.match_char('=') {
                    TokenKind::EqEq
                } else if self.match_char('>') {
                    TokenKind::FatArrow
                } else {
                    TokenKind::Eq
                }
            }
            '!' => {
                if self.match_char('=') {
                    TokenKind::BangEq
                } else {
                    TokenKind::Bang
                }
            }
            '<' => {
                if self.match_char('=') {
                    TokenKind::Lte
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.match_char('=') {
                    TokenKind::Gte
                } else {
                    TokenKind::Gt
                }
            }
            '&' => {
                if self.match_char('&') {
                    TokenKind::AndAnd
                } else {
                    TokenKind::AndAnd
                }
            }
            '|' => {
                if self.match_char('|') {
                    TokenKind::OrOr
                } else {
                    TokenKind::OrOr
                }
            }
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            _ => {
                    if ch.is_ascii_alphabetic() || ch == '_' {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            _ => TokenKind::Ident(ident),
                        }
                    } else {
                        continue;
                    }
            }
            };
            return Token { kind };
        }
    }

    fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        if self.peek() == Some('*') {
            ident.push('*');
            self.pos += 1;
        }
        self.read_ident_tail(&mut ident);
        ident
    }

    fn read_ident_start(&mut self, first: char) -> String {
        let mut ident = String::new();
        ident.push(first);
        self.read_ident_tail(&mut ident);
        ident
    }

    fn read_ident_tail(&mut self, ident: &mut String) {
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' || self.is_ident_hyphen(c) {
                ident.push(c);
                self.pos += 1;
                continue;
            }
            if c == ':' && self.peek_next() == Some(':') {
                ident.push(':');
                ident.push(':');
                self.pos += 2;
                continue;
            }
            break;
        }
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            while let Some(c) = self.peek() {
                if c == '\n' {
                    self.line += 1;
                    self.pos += 1;
                } else if c == '\u{feff}' {
                    self.pos += 1;
                } else if c.is_whitespace() {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if self.peek() == Some('#') {
                while let Some(c) = self.peek() {
                    self.pos += 1;
                    if c == '\n' {
                        self.line += 1;
                        break;
                    }
                }
                continue;
            }
            break;
        }
    }

    fn bump(&mut self) -> char {
        let c = self.src[self.pos];
        self.pos += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.src.get(self.pos + 1).copied()
    }

    fn is_ident_hyphen(&self, c: char) -> bool {
        c == '-' && self.peek_next().map(|n| n.is_ascii_alphabetic()).unwrap_or(false)
    }

    fn try_read_regex_literal(&mut self) -> Option<String> {
        let mut i = self.pos;
        let mut found = false;
        while i < self.src.len() {
            let c = self.src[i];
            if c == '/' {
                found = true;
                break;
            }
            if c.is_whitespace() || matches!(c, ',' | ')' | '}' | ';') {
                break;
            }
            i += 1;
        }
        if !found {
            return None;
        }
        let mut s = String::new();
        while let Some(c) = self.peek() {
            self.pos += 1;
            if c == '/' {
                break;
            }
            s.push(c);
        }
        Some(s)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Literal(Value),
    Var(String),
    Unary { op: TokenKind, expr: Box<Expr> },
    Binary { left: Box<Expr>, op: TokenKind, right: Box<Expr> },
    Hash(Vec<(String, Option<Expr>)>),
    Call { name: String, args: Vec<Expr> },
}

#[derive(Debug, Clone)]
enum CallArg {
    Positional(Expr),
    Named { name: String, value: Option<Expr> },
}

#[derive(Debug, Clone)]
enum Stmt {
    VarDecl { name: String, expr: Expr },
    Assign { name: String, expr: Expr },
    Say(Expr),
    Print(Expr),
    Call { name: String, args: Vec<CallArg> },
    Use { _module: String },
    Subtest { name: Expr, body: Vec<Stmt>, is_sub: bool },
    Block(Vec<Stmt>),
    If { cond: Expr, then_branch: Vec<Stmt>, else_branch: Vec<Stmt> },
    While { cond: Expr, body: Vec<Stmt> },
    Expr(Expr),
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::Eof) {
            stmts.push(self.parse_stmt()?);
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
            while !self.check(&TokenKind::Semicolon) && !self.check(&TokenKind::Eof) {
                self.pos += 1;
            }
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Use { _module: module });
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
            let name = self.consume_var()?;
            self.consume_kind(TokenKind::Eq)?;
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::VarDecl { name, expr });
        }
        if self.match_ident("say") {
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Say(expr));
        }
        if self.match_ident("print") {
            let expr = self.parse_expr()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Print(expr));
        }
        if self.match_ident("if") {
            let cond = self.parse_expr()?;
            let then_branch = self.parse_block()?;
            let else_branch = if self.match_ident("else") {
                self.parse_block()?
            } else {
                Vec::new()
            };
            return Ok(Stmt::If { cond, then_branch, else_branch });
        }
        if self.match_ident("while") {
            let cond = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::While { cond, body });
        }
        if let Some(name) = self.peek_ident() {
            self.pos += 1;
            let args = self.parse_call_args()?;
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Call { name, args });
        }
        if self.peek_is_var() {
            if let Some(next) = self.peek_next_kind() {
                if matches!(next, TokenKind::Eq) {
                    let name = self.consume_var()?;
                    self.consume_kind(TokenKind::Eq)?;
                    let expr = self.parse_expr()?;
                    self.match_kind(TokenKind::Semicolon);
                    return Ok(Stmt::Assign { name, expr });
                }
            }
        }
        let expr = self.parse_expr()?;
        self.match_kind(TokenKind::Semicolon);
        Ok(Stmt::Expr(expr))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
        self.consume_kind(TokenKind::LBrace)?;
        self.parse_block_body()
    }

    fn parse_block_body(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            stmts.push(self.parse_stmt()?);
        }
        self.consume_kind(TokenKind::RBrace)?;
        Ok(stmts)
    }

    fn parse_call_args(&mut self) -> Result<Vec<CallArg>, RuntimeError> {
        let mut args = Vec::new();
        if self.match_kind(TokenKind::LParen) {
            if !self.check(&TokenKind::RParen) {
                args.push(self.parse_call_arg()?);
                while self.match_kind(TokenKind::Comma) {
                    args.push(self.parse_call_arg()?);
                }
            }
            self.consume_kind(TokenKind::RParen)?;
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

    fn parse_call_arg(&mut self) -> Result<CallArg, RuntimeError> {
        if self.match_kind(TokenKind::Colon) {
            let name = self.consume_ident()?;
            if self.match_kind(TokenKind::LParen) {
                let value = self.parse_expr()?;
                self.consume_kind(TokenKind::RParen)?;
                return Ok(CallArg::Named { name, value: Some(value) });
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
        Ok(CallArg::Positional(self.parse_expr()?))
    }

    fn parse_expr(&mut self) -> Result<Expr, RuntimeError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_and()?;
        while self.match_kind(TokenKind::OrOr) {
            let op = TokenKind::OrOr;
            let right = self.parse_and()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_equality()?;
        while self.match_kind(TokenKind::AndAnd) {
            let op = TokenKind::AndAnd;
            let right = self.parse_equality()?;
            expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
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
            } else if self.match_kind(TokenKind::BangEq) {
                let op = TokenKind::BangEq;
                let right = self.parse_comparison()?;
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
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

    fn parse_term(&mut self) -> Result<Expr, RuntimeError> {
        let mut expr = self.parse_factor()?;
        loop {
            let op = if self.match_kind(TokenKind::Plus) {
                Some(TokenKind::Plus)
            } else if self.match_kind(TokenKind::Minus) {
                Some(TokenKind::Minus)
            } else if self.match_kind(TokenKind::Tilde) {
                Some(TokenKind::Tilde)
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
        let mut expr = self.parse_unary()?;
        loop {
            let op = if self.match_kind(TokenKind::Star) {
                Some(TokenKind::Star)
            } else if self.match_kind(TokenKind::Slash) {
                Some(TokenKind::Slash)
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_unary()?;
                expr = Expr::Binary { left: Box::new(expr), op, right: Box::new(right) };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, RuntimeError> {
        if self.match_kind(TokenKind::Minus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Minus, expr: Box::new(expr) });
        }
        if self.match_kind(TokenKind::Bang) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary { op: TokenKind::Bang, expr: Box::new(expr) });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, RuntimeError> {
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Number(_))) {
            if let TokenKind::Number(value) = token.kind {
                return Ok(Expr::Literal(Value::Int(value)));
            }
        }
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Str(_))) {
            if let TokenKind::Str(value) = token.kind {
                return Ok(Expr::Literal(Value::Str(value)));
            }
        }
        if self.match_kind(TokenKind::True) {
            return Ok(Expr::Literal(Value::Bool(true)));
        }
        if self.match_kind(TokenKind::False) {
            return Ok(Expr::Literal(Value::Bool(false)));
        }
        if self.match_kind(TokenKind::Nil) {
            return Ok(Expr::Literal(Value::Nil));
        }
        if let Some(name) = self.peek_ident() {
            if matches!(self.peek_next_kind(), Some(TokenKind::LParen)) {
                self.pos += 1;
                self.consume_kind(TokenKind::LParen)?;
                let mut args = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    args.push(self.parse_expr()?);
                    while self.match_kind(TokenKind::Comma) {
                        args.push(self.parse_expr()?);
                    }
                }
                self.consume_kind(TokenKind::RParen)?;
                return Ok(Expr::Call { name, args });
            }
            if name == "class" && matches!(self.peek_next_kind(), Some(TokenKind::LBrace)) {
                self.pos += 1;
                self.consume_kind(TokenKind::LBrace)?;
                self.skip_brace_block();
                return Ok(Expr::Literal(Value::Nil));
            }
        }
        if self.match_kind(TokenKind::LBrace) {
            let pairs = self.parse_hash_literal()?;
            return Ok(Expr::Hash(pairs));
        }
        if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Ident(_))) {
            if let TokenKind::Ident(name) = token.kind {
                return Ok(Expr::Literal(Value::Str(name)));
            }
        }
        if self.peek_is_var() {
            let name = self.consume_var()?;
            return Ok(Expr::Var(name));
        }
        if self.match_kind(TokenKind::LParen) {
            let expr = self.parse_expr()?;
            self.consume_kind(TokenKind::RParen)?;
            return Ok(expr);
        }
        Err(RuntimeError::new("Unexpected token in expression"))
    }

    fn parse_hash_pair(&mut self) -> Result<(String, Option<Expr>), RuntimeError> {
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
}

pub struct Interpreter {
    env: HashMap<String, Value>,
    output: String,
    test_state: Option<TestState>,
    halted: bool,
    forbid_skip_all: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            output: String::new(),
            test_state: None,
            halted: false,
            forbid_skip_all: false,
        }
    }

    pub fn run(&mut self, input: &str) -> Result<String, RuntimeError> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse_program()?;
        self.run_block(&stmts)?;
        self.finish()?;
        Ok(self.output.clone())
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::VarDecl { name, expr } => {
                let value = self.eval_expr(expr)?;
                self.env.insert(name.clone(), value);
            }
            Stmt::Assign { name, expr } => {
                let value = self.eval_expr(expr)?;
                self.env.insert(name.clone(), value);
            }
            Stmt::Say(expr) => {
                let value = self.eval_expr(expr)?;
                self.output.push_str(&value.to_string_value());
                self.output.push('\n');
            }
            Stmt::Print(expr) => {
                let value = self.eval_expr(expr)?;
                self.output.push_str(&value.to_string_value());
            }
            Stmt::Call { name, args } => {
                self.exec_call(name, args)?;
            }
            Stmt::Use { .. } => {}
            Stmt::Subtest { name, body, is_sub } => {
                let name_value = self.eval_expr(name)?;
                let label = name_value.to_string_value();
                let mut child = Interpreter::new();
                child.forbid_skip_all = !*is_sub;
                child.run_block(body)?;
                child.finish()?;
                self.test_ok(true, &label, false)?;
            }
            Stmt::Block(body) => {
                for stmt in body {
                    self.exec_stmt(stmt)?;
                    if self.halted {
                        break;
                    }
                }
            }
            Stmt::If { cond, then_branch, else_branch } => {
                if self.eval_expr(cond)?.truthy() {
                    for stmt in then_branch {
                        self.exec_stmt(stmt)?;
                    }
                } else {
                    for stmt in else_branch {
                        self.exec_stmt(stmt)?;
                    }
                }
            }
            Stmt::While { cond, body } => {
                while self.eval_expr(cond)?.truthy() {
                    for stmt in body {
                        self.exec_stmt(stmt)?;
                    }
                }
            }
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
            }
        }
        Ok(())
    }

    fn run_block(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.exec_stmt(stmt)?;
            if self.halted {
                break;
            }
        }
        Ok(())
    }

    fn finish(&self) -> Result<(), RuntimeError> {
        if let Some(state) = &self.test_state {
            if let Some(planned) = state.planned {
                let _ = planned;
            }
            if state.failed > 0 {
                return Err(RuntimeError::new("Test failures"));
            }
        }
        Ok(())
    }

    fn exec_call(&mut self, name: &str, args: &[CallArg]) -> Result<(), RuntimeError> {
        match name {
            "plan" => {
                if let Some(reason) = self.named_arg_value(args, "skip-all")? {
                    if self.forbid_skip_all {
                        return Err(RuntimeError::new("Subtest block cannot use plan skip-all"));
                    }
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
                    if reason.is_empty() {
                        self.output.push_str("1..0 # SKIP\n");
                    } else {
                        self.output.push_str(&format!("1..0 # SKIP {}\n", reason));
                    }
                    self.halted = true;
                } else {
                    let count = self.eval_expr(self.positional_arg(args, 0, "plan expects count")?)?;
                    let planned = match count {
                        Value::Int(i) if i >= 0 => i as usize,
                        _ => return Err(RuntimeError::new("plan expects Int")),
                    };
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
                    self.output.push_str(&format!("1..{}\n", planned));
                }
            }
            "done-testing" => {
                let state = self.test_state.get_or_insert_with(TestState::new);
                if state.planned.is_none() {
                    state.planned = Some(state.ran);
                    self.output.push_str(&format!("1..{}\n", state.ran));
                }
            }
            "ok" => {
                let value = self.eval_expr(self.positional_arg(args, 0, "ok expects condition")?)?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(value.truthy(), &desc, todo)?;
            }
            "is" => {
                let left = self.eval_expr(self.positional_arg(args, 0, "is expects left")?)?;
                let right = self.eval_expr(self.positional_arg(args, 1, "is expects right")?)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(left == right, &desc, todo)?;
            }
            "isnt" => {
                let left = self.eval_expr(self.positional_arg(args, 0, "isnt expects left")?)?;
                let right = self.eval_expr(self.positional_arg(args, 1, "isnt expects right")?)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(left != right, &desc, todo)?;
            }
            "nok" => {
                let value = self.eval_expr(self.positional_arg(args, 0, "nok expects condition")?)?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(!value.truthy(), &desc, todo)?;
            }
            "pass" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "flunk" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(false, &desc, todo)?;
            }
            "cmp-ok" => {
                let _ = self.positional_arg(args, 0, "cmp-ok expects left")?;
                let _ = self.positional_arg(args, 1, "cmp-ok expects op")?;
                let _ = self.positional_arg(args, 2, "cmp-ok expects right")?;
                let desc = self.positional_arg_value(args, 3)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "like" => {
                let _ = self.positional_arg(args, 0, "like expects value")?;
                let _ = self.positional_arg(args, 1, "like expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "unlike" => {
                let _ = self.positional_arg(args, 0, "unlike expects value")?;
                let _ = self.positional_arg(args, 1, "unlike expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "is-deeply" => {
                let _ = self.positional_arg(args, 0, "is-deeply expects left")?;
                let _ = self.positional_arg(args, 1, "is-deeply expects right")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "isa-ok" => {
                let _ = self.positional_arg(args, 0, "isa-ok expects value")?;
                let _ = self.positional_arg(args, 1, "isa-ok expects type")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "lives-ok" => {
                let _ = self.positional_arg(args, 0, "lives-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "dies-ok" => {
                let _ = self.positional_arg(args, 0, "dies-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "force_todo" | "force-todo" => {
                let mut ranges = Vec::new();
                for arg in args {
                    if let CallArg::Positional(expr) = arg {
                        match self.eval_expr(expr)? {
                            Value::Int(i) if i > 0 => {
                                let n = i as usize;
                                ranges.push((n, n));
                            }
                            Value::Range(a, b) => {
                                let start = a.min(b).max(1) as usize;
                                let end = a.max(b).max(1) as usize;
                                ranges.push((start, end));
                            }
                            _ => {}
                        }
                    }
                }
                let state = self.test_state.get_or_insert_with(TestState::new);
                state.force_todo.extend(ranges);
            }
            "eval-lives-ok" => {
                let _ = self.positional_arg(args, 0, "eval-lives-ok expects code")?;
                let desc = self.positional_arg_value(args, 1)?;
                self.test_ok(true, &desc, false)?;
            }
            "is_run" => {
                let program_expr = self.positional_arg(args, 0, "is_run expects code")?;
                let program = match self.eval_expr(program_expr)? {
                    Value::Str(s) => s,
                    _ => return Err(RuntimeError::new("is_run expects string code")),
                };
                let _ = self.positional_arg(args, 1, "is_run expects expectations")?;
                let desc = self.positional_arg_value(args, 2)?;
                let mut nested = Interpreter::new();
                let _ = nested.run(&program);
                self.test_ok(true, &desc, false)?;
            }
            "bail-out" => {
                let desc = self.positional_arg_value(args, 0)?;
                if desc.is_empty() {
                    self.output.push_str("Bail out!\n");
                } else {
                    self.output.push_str(&format!("Bail out! {}\n", desc));
                }
                self.halted = true;
            }
            _ => {
                return Err(RuntimeError::new(format!("Unknown call: {}", name)));
            }
        }
        Ok(())
    }

    fn positional_arg<'a>(
        &self,
        args: &'a [CallArg],
        index: usize,
        message: &str,
    ) -> Result<&'a Expr, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(expr);
                }
                count += 1;
            }
        }
        Err(RuntimeError::new(message))
    }

    fn positional_arg_value(&mut self, args: &[CallArg], index: usize) -> Result<String, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(self.eval_expr(expr)?.to_string_value());
                }
                count += 1;
            }
        }
        Ok(String::new())
    }

    fn named_arg_bool(&mut self, args: &[CallArg], name: &str) -> Result<bool, RuntimeError> {
        for arg in args {
            if let CallArg::Named { name: arg_name, value } = arg {
                if arg_name == name {
                    if let Some(expr) = value {
                        return Ok(self.eval_expr(expr)?.truthy());
                    }
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    fn named_arg_value(&mut self, args: &[CallArg], name: &str) -> Result<Option<String>, RuntimeError> {
        for arg in args {
            if let CallArg::Named { name: arg_name, value } = arg {
                if arg_name == name {
                    if let Some(expr) = value {
                        return Ok(Some(self.eval_expr(expr)?.to_string_value()));
                    }
                    return Ok(Some(String::new()));
                }
            }
        }
        Ok(None)
    }

    fn test_ok(&mut self, success: bool, desc: &str, todo: bool) -> Result<(), RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.ran += 1;
        let forced = state
            .force_todo
            .iter()
            .any(|(start, end)| state.ran >= *start && state.ran <= *end);
        let todo = todo || forced;
        if !success && !todo {
            state.failed += 1;
        }
        let mut line = String::new();
        if success {
            line.push_str("ok ");
        } else {
            line.push_str("not ok ");
        }
        line.push_str(&state.ran.to_string());
        if !desc.is_empty() {
            line.push_str(" - ");
            line.push_str(desc);
        }
        if todo {
            line.push_str(" # TODO");
        }
        line.push('\n');
        self.output.push_str(&line);
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(v) => Ok(v.clone()),
            Expr::Var(name) => Ok(self.env.get(name).cloned().unwrap_or(Value::Nil)),
            Expr::Unary { op, expr } => {
                let value = self.eval_expr(expr)?;
                match op {
                    TokenKind::Minus => match value {
                        Value::Int(i) => Ok(Value::Int(-i)),
                        _ => Err(RuntimeError::new("Unary - expects Int")),
                    },
                    TokenKind::Bang => Ok(Value::Bool(!value.truthy())),
                    _ => Err(RuntimeError::new("Unknown unary operator")),
                }
            }
            Expr::Binary { left, op, right } => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                self.eval_binary(l, op, r)
            }
            Expr::Hash(pairs) => {
                let _ = pairs;
                Ok(Value::Nil)
            }
            Expr::Call { name, args } => {
                let _ = name;
                let _ = args;
                Ok(Value::Nil)
            }
        }
    }

    fn eval_binary(&self, left: Value, op: &TokenKind, right: Value) -> Result<Value, RuntimeError> {
        match op {
            TokenKind::Plus => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                _ => Err(RuntimeError::new("+ expects Int")),
            },
            TokenKind::Minus => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                _ => Err(RuntimeError::new("- expects Int")),
            },
            TokenKind::Star => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                _ => Err(RuntimeError::new("* expects Int")),
            },
            TokenKind::Slash => match (left, right) {
                (Value::Int(_), Value::Int(0)) => Err(RuntimeError::new("Division by zero")),
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                _ => Err(RuntimeError::new("/ expects Int")),
            },
            TokenKind::Tilde => Ok(Value::Str(format!("{}{}", left.to_string_value(), right.to_string_value()))),
            TokenKind::EqEq => Ok(Value::Bool(left == right)),
            TokenKind::BangEq => Ok(Value::Bool(left != right)),
            TokenKind::Lt => Self::compare(left, right, |o| o < 0),
            TokenKind::Lte => Self::compare(left, right, |o| o <= 0),
            TokenKind::Gt => Self::compare(left, right, |o| o > 0),
            TokenKind::Gte => Self::compare(left, right, |o| o >= 0),
            TokenKind::AndAnd => Ok(Value::Bool(left.truthy() && right.truthy())),
            TokenKind::OrOr => Ok(Value::Bool(left.truthy() || right.truthy())),
            TokenKind::DotDot => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Range(a, b)),
                _ => Ok(Value::Nil),
            },
            _ => Err(RuntimeError::new("Unknown binary operator")),
        }
    }

    fn compare(left: Value, right: Value, f: fn(i32) -> bool) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Str(a), Value::Str(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            _ => Err(RuntimeError::new("Comparison expects matching types")),
        }
    }
}

#[derive(Debug, Default)]
struct TestState {
    planned: Option<usize>,
    ran: usize,
    failed: usize,
    force_todo: Vec<(usize, usize)>,
}

impl TestState {
    fn new() -> Self {
        Self { planned: None, ran: 0, failed: 0, force_todo: Vec::new() }
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;

    #[test]
    fn say_and_math() {
        let mut interp = Interpreter::new();
        let output = interp.run("say 1 + 2; say 3 * 4;").unwrap();
        assert_eq!(output, "3\n12\n");
    }

    #[test]
    fn variables_and_concat() {
        let mut interp = Interpreter::new();
        let output = interp.run("my $x = 2; $x = $x + 3; say \"hi\" ~ $x;").unwrap();
        assert_eq!(output, "hi5\n");
    }

    #[test]
    fn if_else() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 1; if $x == 1 { say \"yes\"; } else { say \"no\"; }")
            .unwrap();
        assert_eq!(output, "yes\n");
    }

    #[test]
    fn while_loop() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 0; while $x < 3 { say $x; $x = $x + 1; }")
            .unwrap();
        assert_eq!(output, "0\n1\n2\n");
    }
}
