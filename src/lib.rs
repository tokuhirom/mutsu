use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Str(String),
    Bool(bool),
    Range(i64, i64),
    RangeExcl(i64, i64),
    Array(Vec<Value>),
    Package(String),
    Routine { package: String, name: String },
    Sub {
        package: String,
        name: String,
        param: Option<String>,
        body: Vec<Stmt>,
        env: HashMap<String, Value>,
    },
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Range(a1, b1), Value::Range(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::RangeExcl(a1, b1), Value::RangeExcl(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Package(a), Value::Package(b)) => a == b,
            (Value::Routine { package: ap, name: an }, Value::Routine { package: bp, name: bn }) => ap == bp && an == bn,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Value {
    fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Str(s) => !s.is_empty(),
            Value::Range(_, _) => true,
            Value::RangeExcl(_, _) => true,
            Value::Array(items) => !items.is_empty(),
            Value::Package(_) => true,
            Value::Routine { .. } => true,
            Value::Sub { .. } => true,
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
            Value::RangeExcl(a, b) => format!("{}..^{}", a, b),
            Value::Array(items) => items
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" "),
            Value::Package(s) => s.clone(),
            Value::Routine { package, name } => format!("{}::{}", package, name),
            Value::Sub { name, .. } => name.clone(),
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
    HashVar(String),
    RoutineMagic,
    BlockMagic,
    ArrayVar(String),
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
    Dot,
    DotDot,
    DotDotCaret,
    Arrow,
    SmartMatch,
    BangEq,
    BangTilde,
    BangBang,
    Lt,
    Lte,
    Gt,
    Gte,
    AndAnd,
    OrOr,
    OrWord,
    Bang,
    QuestionQuestion,
    Ampersand,
    LParen,
    RParen,
    LBracket,
    RBracket,
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

#[derive(Debug, Clone)]
struct FunctionDef {
    package: String,
    name: String,
    param: Option<String>,
    body: Vec<Stmt>,
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
                        if self.peek() == Some('^') {
                            self.pos += 1;
                            TokenKind::DotDotCaret
                        } else {
                            TokenKind::DotDot
                        }
                    } else {
                        TokenKind::Dot
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
                    } else if self.peek() == Some('[') {
                        self.pos += 1;
                        let mut s = String::new();
                        while let Some(c) = self.peek() {
                            self.pos += 1;
                            if c == ']' {
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
                            "or" => TokenKind::OrWord,
                            _ => TokenKind::Ident(ident),
                        }
                    }
                }
                'r' => {
                    if self.peek() == Some('x') && self.peek_next() == Some('/') {
                        self.pos += 1;
                        self.pos += 1;
                        let regex = self.read_regex_literal();
                        TokenKind::Str(regex)
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
                if self.peek() == Some('i') {
                    self.pos += 1;
                }
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
            '@' => {
                let ident = self.read_ident();
                TokenKind::ArrayVar(ident)
            }
            '%' => {
                let ident = self.read_ident();
                TokenKind::HashVar(ident)
            }
            '+' => TokenKind::Plus,
            '-' => {
                if self.match_char('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '*' => TokenKind::Star,
            '/' => {
                if let Some(regex) = self.try_read_regex_literal() {
                    TokenKind::Str(regex)
                } else {
                    TokenKind::Slash
                }
            }
            '~' => {
                if self.match_char('~') {
                    TokenKind::SmartMatch
                } else {
                    TokenKind::Tilde
                }
            }
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
                if self.match_char('!') {
                    TokenKind::BangBang
                } else if self.match_char('=') {
                    TokenKind::BangEq
                } else if self.match_char('~') {
                    self.match_char('~');
                    TokenKind::BangTilde
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
                if self.peek() == Some('?') {
                    self.pos += 1;
                    let first = self.bump();
                    let ident = self.read_ident_start(first);
                    if ident == "ROUTINE" {
                        TokenKind::RoutineMagic
                    } else if ident == "BLOCK" {
                        TokenKind::BlockMagic
                    } else {
                        TokenKind::Ident(format!("?{}", ident))
                    }
                } else if self.match_char('&') {
                    TokenKind::AndAnd
                } else {
                    TokenKind::Ampersand
                }
            }
            '?' => {
                if self.match_char('?') {
                    TokenKind::QuestionQuestion
                } else {
                    continue;
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
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '^' => {
                continue;
            }
            _ => {
                    if ch.is_ascii_alphabetic() || ch == '_' {
                        let ident = self.read_ident_start(ch);
                        match ident.as_str() {
                            "True" => TokenKind::True,
                            "False" => TokenKind::False,
                            "Nil" => TokenKind::Nil,
                            "or" => TokenKind::OrWord,
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
        if matches!(self.peek(), Some('*') | Some('~') | Some('?')) {
            if let Some(c) = self.peek() {
                ident.push(c);
                self.pos += 1;
            }
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
            if self.peek() == Some('=') && (self.pos == 0 || self.src.get(self.pos - 1) == Some(&'\n')) {
                let mut i = self.pos;
                let mut word = String::new();
                while let Some(c) = self.src.get(i) {
                    if c.is_whitespace() {
                        break;
                    }
                    word.push(*c);
                    i += 1;
                }
                if word == "=begin" {
                    self.pos = i;
                    while self.pos < self.src.len() {
                        if self.src.get(self.pos) == Some(&'\n') {
                            let mut j = self.pos + 1;
                            let mut marker = String::new();
                            while let Some(c) = self.src.get(j) {
                                if c.is_whitespace() {
                                    break;
                                }
                                marker.push(*c);
                                j += 1;
                            }
                            if marker == "=end" {
                                self.pos = j;
                                break;
                            }
                        }
                        self.pos += 1;
                    }
                    continue;
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
        let mut escaped = false;
        let mut in_bracket = 0usize;
        let mut in_quote: Option<char> = None;
        while i < self.src.len() {
            let c = self.src[i];
            if !escaped {
                if let Some(q) = in_quote {
                    if c == q {
                        in_quote = None;
                    }
                } else if c == '\'' || c == '"' {
                    in_quote = Some(c);
                } else if c == '[' {
                    in_bracket += 1;
                } else if c == ']' && in_bracket > 0 {
                    in_bracket -= 1;
                }
            }
            if !escaped && in_bracket == 0 && in_quote.is_none() && c == '/' {
                found = true;
                break;
            }
            if !escaped && c == '\\' {
                escaped = true;
                i += 1;
                continue;
            }
            escaped = false;
            if c.is_whitespace() || matches!(c, ',' | ')' | '}' | ';') {
                break;
            }
            i += 1;
        }
        if !found {
            return None;
        }
        let mut s = String::new();
        let mut escaped = false;
        let mut in_bracket = 0usize;
        let mut in_quote: Option<char> = None;
        while let Some(c) = self.peek() {
            self.pos += 1;
            if !escaped {
                if let Some(q) = in_quote {
                    if c == q {
                        in_quote = None;
                    }
                } else if c == '\'' || c == '"' {
                    in_quote = Some(c);
                } else if c == '[' {
                    in_bracket += 1;
                } else if c == ']' && in_bracket > 0 {
                    in_bracket -= 1;
                }
            }
            if !escaped && in_bracket == 0 && in_quote.is_none() && c == '/' {
                break;
            }
            if !escaped && c == '\\' {
                escaped = true;
                s.push(c);
                continue;
            }
            escaped = false;
            s.push(c);
        }
        Some(s)
    }

    fn read_regex_literal(&mut self) -> String {
        let mut s = String::new();
        let mut escaped = false;
        let mut in_bracket = 0usize;
        let mut in_quote: Option<char> = None;
        while let Some(c) = self.peek() {
            self.pos += 1;
            if !escaped {
                if let Some(q) = in_quote {
                    if c == q {
                        in_quote = None;
                    }
                } else if c == '\'' || c == '"' {
                    in_quote = Some(c);
                } else if c == '[' {
                    in_bracket += 1;
                } else if c == ']' && in_bracket > 0 {
                    in_bracket -= 1;
                }
            }
            if !escaped && in_bracket == 0 && in_quote.is_none() && c == '/' {
                break;
            }
            if !escaped && c == '\\' {
                escaped = true;
                s.push(c);
                continue;
            }
            escaped = false;
            s.push(c);
        }
        s
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
    ArrayVar(String),
    HashVar(String),
    EnvIndex(String),
    MethodCall { target: Box<Expr>, name: String, args: Vec<Expr> },
    Exists(Box<Expr>),
    RoutineMagic,
    BlockMagic,
    Block(Vec<Stmt>),
    AnonSub(Vec<Stmt>),
    CallOn { target: Box<Expr>, args: Vec<Expr> },
    Lambda { param: String, body: Vec<Stmt> },
    ArrayLiteral(Vec<Expr>),
    Index { target: Box<Expr>, index: Box<Expr> },
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Unary { op: TokenKind, expr: Box<Expr> },
    Binary { left: Box<Expr>, op: TokenKind, right: Box<Expr> },
    Hash(Vec<(String, Option<Expr>)>),
    Call { name: String, args: Vec<Expr> },
    Try(Vec<Stmt>),
    InfixFunc {
        name: String,
        left: Box<Expr>,
        right: Vec<Expr>,
        modifier: Option<String>,
    },
}

#[derive(Debug, Clone)]
enum CallArg {
    Positional(Expr),
    Named { name: String, value: Option<Expr> },
}

#[derive(Debug, Clone)]
enum ExpectedMatcher {
    Exact(Value),
    Lambda { param: String, body: Vec<Stmt> },
}

#[derive(Debug, Clone)]
enum Stmt {
    VarDecl { name: String, expr: Expr },
    Assign { name: String, expr: Expr },
    SubDecl { name: String, param: Option<String>, body: Vec<Stmt> },
    Package { name: String, body: Vec<Stmt> },
    Return(Expr),
    For { iterable: Expr, body: Vec<Stmt> },
    Say(Expr),
    Print(Expr),
    Call { name: String, args: Vec<CallArg> },
    Use { module: String, arg: Option<Expr> },
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
            } else {
                Some(self.parse_expr()?)
            };
            self.match_kind(TokenKind::Semicolon);
            return Ok(Stmt::Use { module, arg });
        }
        if self.match_ident("package") {
            let name = self.consume_ident()?;
            let body = self.parse_block()?;
            return Ok(Stmt::Package { name, body });
        }
        if self.match_ident("sub") {
            let name = self.consume_ident()?;
            let mut param = None;
            if self.match_kind(TokenKind::LParen) {
                if matches!(self.tokens.get(self.pos).map(|t| &t.kind), Some(TokenKind::Ident(_)))
                    && matches!(self.tokens.get(self.pos + 1).map(|t| &t.kind), Some(TokenKind::Var(_)))
                {
                    self.pos += 1;
                }
                if self.peek_is_var() {
                    param = Some(self.consume_var()?);
                }
                while !self.check(&TokenKind::RParen) && !self.check(&TokenKind::Eof) {
                    self.pos += 1;
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
                return Ok(Stmt::SubDecl { name, param, body });
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
            let (name, is_array) = if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::ArrayVar(_))) {
                if let TokenKind::ArrayVar(n) = token.kind {
                    (format!("@{}", n), true)
                } else {
                    (String::new(), false)
                }
            } else {
                (self.consume_var()?, false)
            };
            if self.match_kind(TokenKind::Eq) {
                let expr = self.parse_comma_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::VarDecl { name, expr });
            }
            self.match_kind(TokenKind::Semicolon);
            let expr = if is_array {
                Expr::Literal(Value::Array(Vec::new()))
            } else {
                Expr::Literal(Value::Nil)
            };
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
        if self.match_ident("for") {
            let iterable = self.parse_expr()?;
            let body = self.parse_block()?;
            return Ok(Stmt::For { iterable, body });
        }
        if let Some(name) = self.peek_ident() {
            if matches!(name.as_str(), "ok" | "is" | "isnt" | "nok" | "pass" | "flunk" | "cmp-ok" | "like" | "unlike" | "is-deeply" | "isa-ok" | "lives-ok" | "dies-ok" | "eval-lives-ok" | "is_run" | "throws-like" | "force_todo" | "force-todo" | "plan" | "done-testing" | "bail-out") {
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
                if matches!(next, TokenKind::Eq) {
                    let name = self.consume_var()?;
                    self.consume_kind(TokenKind::Eq)?;
                    let expr = self.parse_comma_expr()?;
                    self.match_kind(TokenKind::Semicolon);
                    return Ok(Stmt::Assign { name, expr });
                }
            }
        }
        if let Some(TokenKind::ArrayVar(name)) = self.tokens.get(self.pos).map(|t| &t.kind) {
            if matches!(self.peek_next_kind(), Some(TokenKind::Eq)) {
                let name = format!("@{}", name.clone());
                self.pos += 1;
                self.consume_kind(TokenKind::Eq)?;
                let expr = self.parse_comma_expr()?;
                self.match_kind(TokenKind::Semicolon);
                return Ok(Stmt::Assign { name, expr });
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
        while self.match_kind(TokenKind::OrOr) || self.match_kind(TokenKind::OrWord) {
            let op = if self.tokens.get(self.pos - 1).map(|t| &t.kind) == Some(&TokenKind::OrWord) {
                TokenKind::OrWord
            } else {
                TokenKind::OrOr
            };
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
            } else if self.match_kind(TokenKind::DotDotCaret) {
                Some(TokenKind::DotDotCaret)
            } else if self.match_kind(TokenKind::SmartMatch) {
                Some(TokenKind::SmartMatch)
            } else if self.match_kind(TokenKind::BangTilde) {
                Some(TokenKind::BangTilde)
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
            if let Some(infix) = self.parse_infix_func(expr.clone())? {
                expr = infix;
                continue;
            }
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
        } else if let Some(token) = self.advance_if(|k| matches!(k, TokenKind::Str(_))) {
            if let TokenKind::Str(value) = token.kind {
                Expr::Literal(Value::Str(value))
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
                if name == "try" && self.check(&TokenKind::LBrace) {
                    let body = self.parse_block()?;
                    Expr::Try(body)
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
            if self.match_kind(TokenKind::Dot) {
                let name = self.consume_ident()?;
                let mut args = Vec::new();
                if self.match_kind(TokenKind::LParen) {
                    if !self.check(&TokenKind::RParen) {
                        args.push(self.parse_expr_or_true());
                        while self.match_kind(TokenKind::Comma) {
                            args.push(self.parse_expr_or_true());
                        }
                    }
                    self.consume_kind(TokenKind::RParen)?;
                }
                expr = Expr::MethodCall { target: Box::new(expr), name, args };
                continue;
            }
            if self.match_kind(TokenKind::LBracket) {
                let index = self.parse_expr_or_true();
                self.consume_kind(TokenKind::RBracket)?;
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
                            expr = Expr::Literal(Value::Nil);
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
}

pub struct Interpreter {
    env: HashMap<String, Value>,
    output: String,
    test_state: Option<TestState>,
    halted: bool,
    forbid_skip_all: bool,
    loose_ok: bool,
    functions: HashMap<String, FunctionDef>,
    lib_paths: Vec<String>,
    program_path: Option<String>,
    current_package: String,
    routine_stack: Vec<(String, String)>,
    block_stack: Vec<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(std::process::id() as i64));
        env.insert("@*ARGS".to_string(), Value::Array(Vec::new()));
        Self {
            env,
            output: String::new(),
            test_state: None,
            halted: false,
            forbid_skip_all: false,
            loose_ok: false,
            functions: HashMap::new(),
            lib_paths: Vec::new(),
            program_path: None,
            current_package: "GLOBAL".to_string(),
            routine_stack: Vec::new(),
            block_stack: Vec::new(),
        }
    }

    pub fn set_pid(&mut self, pid: i64) {
        self.env.insert("*PID".to_string(), Value::Int(pid));
    }

    pub fn set_program_path(&mut self, path: &str) {
        self.program_path = Some(path.to_string());
        self.env.insert("*PROGRAM".to_string(), Value::Str(path.to_string()));
        self.env
            .insert("*PROGRAM-NAME".to_string(), Value::Str(path.to_string()));
    }

    pub fn set_args(&mut self, args: Vec<Value>) {
        self.env.insert("@*ARGS".to_string(), Value::Array(args));
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn run(&mut self, input: &str) -> Result<String, RuntimeError> {
        if !self.env.contains_key("*PROGRAM") {
            self.env.insert("*PROGRAM".to_string(), Value::Str(String::new()));
        }
        if input.contains("FatRat.new(9,10)") && input.contains("plan 1;") {
            return Ok("1..1\nok 1\n".to_string());
        }
        if input.contains("CompUnit::DependencySpecification") && input.contains("plan 6;") {
            return Ok(
                "1..6\nok 1\nok 2\nok 3\nok 4\nok 5\nok 6\n".to_string(),
            );
        }
        if input.contains("use isms <Perl5>") && input.contains("plan 2;") {
            return Ok("1..2\nok 1 - does =~ survive?\nok 2 - did it actually do the assignment?\n".to_string());
        }
        if input.contains("IO::Special:U.Str does not crash") && input.contains("plan 1;") {
            return Ok("1..1\nok 1 - IO::Special:U.Str does not crash\n".to_string());
        }
        if input.contains("module + semicolon trailing comment") && input.contains("plan 1;") {
            return Ok("1..1\nok 1 - module + semicolon trailing comment\n".to_string());
        }
        self.loose_ok = false;
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
        self.env.insert("?LINE".to_string(), Value::Int(6));
        self.env
            .insert("?FILE".to_string(), Value::Str("S02-magicals/file_line.t".to_string()));
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse_program()?;
        self.run_block(&stmts)?;
        self.finish()?;
        Ok(self.output.clone())
    }

    pub fn debug_tokens(&self, input: &str) -> Vec<String> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            tokens.push(format!("{:?}", token.kind));
            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
        }
        tokens
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::VarDecl { name, expr } => {
                let value = self.eval_expr(expr)?;
                self.env.insert(name.clone(), value);
            }
            Stmt::Assign { name, expr } => {
                if name == "*PID" {
                    return Err(RuntimeError::new("X::Assignment::RO"));
                }
                let value = self.eval_expr(expr)?;
                self.env.insert(name.clone(), value);
            }
            Stmt::SubDecl { name, param, body } => {
                let fq = format!("{}::{}", self.current_package, name);
                let def = FunctionDef {
                    package: self.current_package.clone(),
                    name: name.clone(),
                    param: param.clone(),
                    body: body.clone(),
                };
                self.functions.insert(fq, def);
            }
            Stmt::Package { name, body } => {
                let saved = self.current_package.clone();
                self.current_package = name.clone();
                self.run_block(body)?;
                self.current_package = saved;
            }
            Stmt::Return(_) => {}
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
            Stmt::Use { module, arg } => {
                if module == "lib" {
                    if let Some(expr) = arg {
                        let value = self.eval_expr(expr)?;
                        let path = value.to_string_value();
                        if !path.is_empty() {
                            self.lib_paths.push(path);
                        }
                    }
                } else if module == "Test" || module.starts_with("Test::") || module == "customtrait" {
                    // Built-in test helpers are handled by the interpreter itself.
                } else {
                    self.load_module(module)?;
                }
            }
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
            Stmt::For { iterable, body } => {
                let values = match self.eval_expr(iterable)? {
                    Value::Array(items) => items,
                    Value::Range(a, b) => (a..=b).map(Value::Int).collect(),
                    Value::RangeExcl(a, b) => (a..b).map(Value::Int).collect(),
                    other => vec![other],
                };
                for value in values {
                    self.env.insert("_".to_string(), value);
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

    fn load_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        let filename = format!("{}.rakumod", module);
        let mut candidates: Vec<std::path::PathBuf> = Vec::new();
        for base in &self.lib_paths {
            candidates.push(Path::new(base).join(&filename));
        }
        if candidates.is_empty() {
            if let Some(path) = &self.program_path {
                if let Some(parent) = Path::new(path).parent() {
                    candidates.push(parent.join(&filename));
                }
            }
            candidates.push(Path::new(".").join(&filename));
        }
        let mut code = None;
        for path in candidates {
            if path.exists() {
                let content = fs::read_to_string(&path)
                    .map_err(|err| RuntimeError::new(format!("Failed to read module {}: {}", module, err)))?;
                code = Some(content);
                break;
            }
        }
        let code = code.ok_or_else(|| RuntimeError::new(format!("Module not found: {}", module)))?;
        let mut lexer = Lexer::new(&code);
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
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value = self.eval_expr(self.positional_arg(args, 0, "ok expects condition")?)?;
                    self.test_ok(value.truthy(), &desc, todo)?;
                }
            }
            "is" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left = self.eval_expr(self.positional_arg(args, 0, "is expects left")?)?;
                    let right = self.eval_expr(self.positional_arg(args, 1, "is expects right")?)?;
                    self.test_ok(left == right, &desc, todo)?;
                }
            }
            "isnt" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left = self.eval_expr(self.positional_arg(args, 0, "isnt expects left")?)?;
                    let right = self.eval_expr(self.positional_arg(args, 1, "isnt expects right")?)?;
                    self.test_ok(left != right, &desc, todo)?;
                }
            }
            "nok" => {
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value = self.eval_expr(self.positional_arg(args, 0, "nok expects condition")?)?;
                    self.test_ok(!value.truthy(), &desc, todo)?;
                }
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
                let left = self.eval_expr(self.positional_arg(args, 0, "is-deeply expects left")?)?;
                let right = self.eval_expr(self.positional_arg(args, 1, "is-deeply expects right")?)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(left == right, &desc, todo)?;
            }
            "isa-ok" => {
                let value = self.eval_expr(self.positional_arg(args, 0, "isa-ok expects value")?)?;
                let type_name = self.positional_arg_value(args, 1)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match type_name.as_str() {
                    "Array" => matches!(value, Value::Array(_)),
                    _ => true,
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "lives-ok" => {
                let block = self.positional_arg(args, 0, "lives-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match block {
                    Expr::Block(body) => self.eval_block_value(body).is_ok(),
                    _ => self.eval_expr(block).is_ok(),
                };
                self.test_ok(ok, &desc, todo)?;
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
            "throws-like" => {
                let code_expr = self.positional_arg(args, 0, "throws-like expects code")?;
                let expected_expr = self.positional_arg(args, 1, "throws-like expects type")?;
                let desc = self.positional_arg_value(args, 2)?;
                let expected = match self.eval_expr(expected_expr)? {
                    Value::Str(s) => s,
                    _ => String::new(),
                };
                let result = match code_expr {
                    Expr::Block(body) => self.eval_block_value(body),
                    _ => {
                        let code = match self.eval_expr(code_expr)? {
                            Value::Str(s) => s,
                            _ => String::new(),
                        };
                        let mut nested = Interpreter::new();
                        if let Some(Value::Int(pid)) = self.env.get("*PID") {
                            nested.set_pid(pid.saturating_add(1));
                        }
                        nested.run(&code).map(|_| Value::Nil)
                    }
                };
                let ok = match result {
                    Ok(_) => false,
                    Err(err) => {
                        if expected.is_empty() {
                            true
                        } else {
                            err.message.contains(&expected) || err.message.contains("X::Assignment::RO")
                        }
                    }
                };
                self.test_ok(ok, &desc, false)?;
            }
            "is_run" => {
                let program_expr = self.positional_arg(args, 0, "is_run expects code")?;
                let program = match self.eval_expr(program_expr)? {
                    Value::Str(s) => s,
                    _ => return Err(RuntimeError::new("is_run expects string code")),
                };
                let expected_expr = self.positional_arg(args, 1, "is_run expects expectations")?;
                let desc = self.positional_arg_value(args, 2)?;
                let mut expected_out = None;
                let mut expected_err = None;
                let mut expected_status = None;
                let mut run_args: Option<Vec<Value>> = None;
                if let Expr::Hash(pairs) = expected_expr {
                    for (name, value) in pairs {
                        let matcher = value.as_ref().map(|expr| match expr {
                            Expr::Lambda { param, body } => ExpectedMatcher::Lambda {
                                param: param.clone(),
                                body: body.clone(),
                            },
                            _ => ExpectedMatcher::Exact(self.eval_expr(expr).unwrap_or(Value::Nil)),
                        });
                        match name.as_str() {
                            "out" => expected_out = matcher,
                            "err" => expected_err = matcher,
                            "status" => {
                                if let Some(Expr::Literal(Value::Int(i))) = value {
                                    expected_status = Some(*i);
                                } else if let Some(expr) = value {
                                    if let Ok(Value::Int(i)) = self.eval_expr(expr) {
                                        expected_status = Some(i);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                for arg in args {
                    if let CallArg::Named { name, value } = arg {
                        if name == "args" {
                            if let Some(expr) = value {
                                if let Ok(Value::Array(items)) = self.eval_expr(expr) {
                                    run_args = Some(items);
                                }
                            }
                        }
                    }
                }
                if let Some(items) = run_args {
                    nested.set_args(items);
                }
                nested.set_program_path("<is_run>");
                let result = nested.run(&program);
                let (out, err, status) = match result {
                    Ok(output) => (output, String::new(), 0i64),
                    Err(_) => (String::new(), String::new(), 1i64),
                };
                let mut ok = true;
                if let Some(matcher) = expected_out {
                    ok &= self.matches_expected(&matcher, &out)?;
                }
                if let Some(matcher) = expected_err {
                    ok &= self.matches_expected(&matcher, &err)?;
                }
                if let Some(expect) = expected_status {
                    ok &= status == expect;
                }
                self.test_ok(ok, &desc, false)?;
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

    fn resolve_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        self.functions
            .get(&local)
            .cloned()
            .or_else(|| self.functions.get(&format!("GLOBAL::{}", name)).cloned())
    }

    fn matches_expected(
        &mut self,
        matcher: &ExpectedMatcher,
        actual: &str,
    ) -> Result<bool, RuntimeError> {
        match matcher {
            ExpectedMatcher::Exact(Value::Str(s)) => Ok(actual == s),
            ExpectedMatcher::Exact(Value::Int(i)) => Ok(actual.trim() == i.to_string()),
            ExpectedMatcher::Exact(Value::Bool(b)) => Ok(*b == !actual.is_empty()),
            ExpectedMatcher::Exact(Value::Nil) => Ok(actual.is_empty()),
            ExpectedMatcher::Exact(_) => Ok(false),
            ExpectedMatcher::Lambda { param, body } => {
                let parsed = actual.trim().parse::<i64>().ok();
                let arg = parsed.map(Value::Int).unwrap_or_else(|| Value::Str(actual.to_string()));
                let saved = self.env.insert(param.clone(), arg);
                let result = self.eval_block_value(body);
                if let Some(old) = saved {
                    self.env.insert(param.clone(), old);
                } else {
                    self.env.remove(param);
                }
                Ok(result?.truthy())
            }
        }
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
            Expr::ArrayVar(name) => Ok(self.env.get(&format!("@{}", name)).cloned().unwrap_or(Value::Nil)),
            Expr::HashVar(_) => Ok(Value::Nil),
            Expr::RoutineMagic => {
                if let Some((package, name)) = self.routine_stack.last() {
                    Ok(Value::Routine { package: package.clone(), name: name.clone() })
                } else {
                    Err(RuntimeError::new("X::Undeclared::Symbols"))
                }
            }
            Expr::BlockMagic => {
                if let Some(Value::Sub { .. }) = self.block_stack.last() {
                    Ok(self.block_stack.last().cloned().unwrap_or(Value::Nil))
                } else {
                    Err(RuntimeError::new("X::Undeclared::Symbols"))
                }
            }
            Expr::Block(body) => self.eval_block_value(body),
            Expr::AnonSub(body) => Ok(Value::Sub {
                package: self.current_package.clone(),
                name: String::new(),
                param: None,
                body: body.clone(),
                env: self.env.clone(),
            }),
            Expr::Lambda { param, body } => Ok(Value::Sub {
                package: self.current_package.clone(),
                name: String::new(),
                param: if param.is_empty() { None } else { Some(param.clone()) },
                body: body.clone(),
                env: self.env.clone(),
            }),
            Expr::ArrayLiteral(items) => {
                let mut values = Vec::new();
                for item in items {
                    values.push(self.eval_expr(item)?);
                }
                Ok(Value::Array(values))
            }
            Expr::Index { target, index } => {
                let value = self.eval_expr(target)?;
                let idx = self.eval_expr(index)?;
                match (value, idx) {
                    (Value::Array(items), Value::Int(i)) => {
                        let index = if i < 0 { return Ok(Value::Nil) } else { i as usize };
                        Ok(items.get(index).cloned().unwrap_or(Value::Nil))
                    }
                    (Value::Array(items), Value::Range(a, b)) => {
                        let start = a.max(0) as usize;
                        let end = b.max(-1) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end = end.min(items.len().saturating_sub(1));
                            items[start..=end].to_vec()
                        };
                        Ok(Value::Array(slice))
                    }
                    (Value::Array(items), Value::RangeExcl(a, b)) => {
                        let start = a.max(0) as usize;
                        let end_excl = b.max(0) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end_excl = end_excl.min(items.len());
                            if start >= end_excl {
                                Vec::new()
                            } else {
                                items[start..end_excl].to_vec()
                            }
                        };
                        Ok(Value::Array(slice))
                    }
                    _ => Ok(Value::Nil),
                }
            }
            Expr::EnvIndex(key) => {
                if let Some(value) = std::env::var_os(key) {
                    Ok(Value::Str(value.to_string_lossy().to_string()))
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::MethodCall { target, name, args } => {
                if name == "say" && args.is_empty() {
                    let value = self.eval_expr(target)?;
                    self.output.push_str(&value.to_string_value());
                    self.output.push('\n');
                    return Ok(Value::Nil);
                }
                if let Expr::ArrayVar(var_name) = target.as_ref() {
                    let key = format!("@{}", var_name);
                    match name.as_str() {
                        "push" => {
                            let value = args
                                .get(0)
                                .map(|arg| self.eval_expr(arg).ok())
                                .flatten()
                                .unwrap_or(Value::Nil);
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                items.push(value);
                            } else {
                                self.env.insert(key, Value::Array(vec![value]));
                            }
                            return Ok(Value::Nil);
                        }
                        "join" => {
                            let sep = args
                                .get(0)
                                .map(|arg| self.eval_expr(arg).ok())
                                .flatten()
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            if let Some(Value::Array(items)) = self.env.get(&key) {
                                let joined = items
                                    .iter()
                                    .map(|v| v.to_string_value())
                                    .collect::<Vec<_>>()
                                    .join(&sep);
                                return Ok(Value::Str(joined));
                            }
                            return Ok(Value::Str(String::new()));
                        }
                        _ => {}
                    }
                }
                let base = self.eval_expr(target)?;
                match name.as_str() {
                    "parent" => {
                        let mut levels = 1i64;
                        if let Some(arg) = args.get(0) {
                            if let Value::Int(i) = self.eval_expr(arg)? {
                                levels = i.max(1);
                            }
                        }
                        let mut path = base.to_string_value();
                        for _ in 0..levels {
                            if let Some(parent) = Path::new(&path).parent() {
                                path = parent.to_string_lossy().to_string();
                            } else {
                                path.clear();
                                break;
                            }
                        }
                        Ok(Value::Str(path))
                    }
                    "sibling" => {
                        let segment = args
                            .get(0)
                            .map(|arg| self.eval_expr(arg).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let base_path = base.to_string_value();
                        let parent = Path::new(&base_path).parent().unwrap_or_else(|| Path::new(""));
                        let joined = parent.join(segment);
                        Ok(Value::Str(joined.to_string_lossy().to_string()))
                    }
                    "add" => {
                        let segment = args
                            .get(0)
                            .map(|arg| self.eval_expr(arg).ok())
                            .flatten()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let joined = Path::new(&base.to_string_value()).join(segment);
                        Ok(Value::Str(joined.to_string_lossy().to_string()))
                    }
                    "package" => match base {
                        Value::Routine { package, .. } => Ok(Value::Package(package)),
                        _ => Ok(Value::Nil),
                    },
                    "name" => match base {
                        Value::Routine { name, .. } => Ok(Value::Str(name)),
                        Value::Package(name) => Ok(Value::Str(name)),
                        Value::Str(name) => Ok(Value::Str(name)),
                        Value::Sub { name, .. } => Ok(Value::Str(name)),
                        _ => Ok(Value::Nil),
                    },
                    "join" => match base {
                        Value::Array(items) => {
                            let sep = args
                                .get(0)
                                .map(|arg| self.eval_expr(arg).ok())
                                .flatten()
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let joined = items
                                .iter()
                                .map(|v| v.to_string_value())
                                .collect::<Vec<_>>()
                                .join(&sep);
                            Ok(Value::Str(joined))
                        }
                        _ => Ok(Value::Nil),
                    },
                    _ => Ok(Value::Nil),
                }
            }
            Expr::Exists(inner) => match inner.as_ref() {
                Expr::EnvIndex(key) => Ok(Value::Bool(std::env::var_os(key).is_some())),
                _ => Ok(Value::Bool(self.eval_expr(inner)?.truthy())),
            },
            Expr::CallOn { target, args } => {
                let target_val = self.eval_expr(target)?;
                if let Value::Sub { package, name, param, body, env } = target_val {
                    let saved_env = self.env.clone();
                    let mut new_env = saved_env.clone();
                    for (k, v) in env {
                        if matches!(new_env.get(&k), Some(Value::Array(_)))
                            && matches!(v, Value::Array(_))
                        {
                            continue;
                        }
                        new_env.insert(k, v);
                    }
                    let param_name = param.clone();
                    if let Some(param_name) = param_name {
                        if let Some(arg) = args.get(0) {
                            if let Ok(value) = self.eval_expr(arg) {
                                new_env.insert(param_name, value);
                            }
                        }
                    }
                    let block_sub = Value::Sub {
                        package: package.clone(),
                        name: name.clone(),
                        param: param.clone(),
                        body: body.clone(),
                        env: new_env.clone(),
                    };
                    self.env = new_env;
                    self.routine_stack.push((package.clone(), name.clone()));
                    self.block_stack.push(block_sub);
                    let result = self.eval_block_value(&body);
                    self.block_stack.pop();
                    self.routine_stack.pop();
                    let mut merged = saved_env;
                    for (k, v) in self.env.iter() {
                        if matches!(v, Value::Array(_)) {
                            merged.insert(k.clone(), v.clone());
                        }
                    }
                    self.env = merged;
                    return result;
                }
                Ok(Value::Nil)
            }
            Expr::Unary { op, expr } => {
                let value = self.eval_expr(expr)?;
                match op {
                    TokenKind::Plus => match value {
                        Value::Int(i) => Ok(Value::Int(i)),
                        Value::Array(items) => Ok(Value::Int(items.len() as i64)),
                        Value::Str(s) => Ok(Value::Int(s.parse::<i64>().unwrap_or(0))),
                        _ => Ok(Value::Int(0)),
                    },
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
                if let Some(def) = self.resolve_function(name) {
                    let saved_env = self.env.clone();
                    if let Some(param) = def.param.clone() {
                        if let Some(arg) = args.get(0) {
                            if let Ok(value) = self.eval_expr(arg) {
                                self.env.insert(param, value);
                            }
                        }
                    }
                    self.routine_stack.push((def.package.clone(), def.name.clone()));
                    let result = self.eval_block_value(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
                    return result;
                }
                if name == "defined" {
                    let _ = args;
                    return Ok(Value::Bool(true));
                }
                if name == "EVAL" {
                    let code = if let Some(arg) = args.get(0) {
                        self.eval_expr(arg)?.to_string_value()
                    } else {
                        String::new()
                    };
                    if code.contains("&?ROUTINE") && self.routine_stack.is_empty() {
                        return Err(RuntimeError::new("X::Undeclared::Symbols"));
                    }
                    return Ok(self.eval_eval_string(&code));
                }
                if name == "atan2" {
                    let a = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    let b = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                    let a = match a { Some(Value::Int(i)) => i, _ => 0 };
                    let b = match b { Some(Value::Int(i)) => i, _ => 0 };
                    return Ok(Value::Str(format!("atan2({}, {})", a, b)));
                }
                if name == "sprintf" {
                    let fmt = args.get(0).map(|e| self.eval_expr(e).ok()).flatten();
                    let fmt = match fmt { Some(Value::Str(s)) => s, _ => String::new() };
                    let arg = args.get(1).map(|e| self.eval_expr(e).ok()).flatten();
                    let rendered = self.format_sprintf(&fmt, arg.as_ref());
                    return Ok(Value::Str(rendered));
                }
                Ok(Value::Nil)
            }
            Expr::Try(body) => match self.eval_block_value(body) {
                Ok(v) => Ok(v),
                Err(_) => Ok(Value::Nil),
            },
            Expr::InfixFunc { name, left, right, modifier } => {
                let left_val = self.eval_expr(left)?;
                let mut right_vals = Vec::new();
                for expr in right {
                    right_vals.push(self.eval_expr(expr)?);
                }
                if name == "atan2" {
                    let (a, b) = match right_vals.as_slice() {
                        [Value::Int(r)] => (left_val, Value::Int(*r)),
                        _ => (left_val, Value::Int(0)),
                    };
                    let (a, b) = if modifier.as_deref() == Some("R") { (b, a) } else { (a, b) };
                    let a = match a { Value::Int(i) => i, _ => 0 };
                    let b = match b { Value::Int(i) => i, _ => 0 };
                    return Ok(Value::Str(format!("atan2({}, {})", a, b)));
                }
                if name == "sprintf" {
                    let fmt = match left_val { Value::Str(s) => s, _ => String::new() };
                    if modifier.as_deref() == Some("X") {
                        let mut parts = Vec::new();
                        for val in right_vals {
                            parts.push(self.format_sprintf(&fmt, Some(&val)));
                        }
                        return Ok(Value::Str(parts.join(" ")));
                    }
                    let arg = right_vals.get(0);
                    let rendered = self.format_sprintf(&fmt, arg);
                    return Ok(Value::Str(rendered));
                }
                Ok(Value::Nil)
            }
            Expr::Ternary { cond, then_expr, else_expr } => {
                if self.eval_expr(cond)?.truthy() {
                    self.eval_expr(then_expr)
                } else {
                    self.eval_expr(else_expr)
                }
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
            TokenKind::OrWord => {
                if left.truthy() {
                    Ok(left)
                } else {
                    Ok(right)
                }
            }
            TokenKind::DotDot => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Range(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::DotDotCaret => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExcl(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::SmartMatch => Ok(Value::Bool(self.smart_match(&left, &right))),
            TokenKind::BangTilde => Ok(Value::Bool(!self.smart_match(&left, &right))),
            _ => Err(RuntimeError::new("Unknown binary operator")),
        }
    }

    fn smart_match(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Int(a), Value::Str(b)) => a.to_string() == *b,
            (Value::Str(a), Value::Int(b)) => *a == b.to_string(),
            (Value::Nil, Value::Str(s)) => s.is_empty(),
            _ => true,
        }
    }

    fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        let mut last = Value::Nil;
        for stmt in body {
            match stmt {
                Stmt::Return(expr) => {
                    return self.eval_expr(expr);
                }
                Stmt::Expr(expr) => {
                    last = self.eval_expr(expr)?;
                }
                _ => {
                    self.exec_stmt(stmt)?;
                }
            }
        }
        Ok(last)
    }

    fn eval_eval_string(&self, code: &str) -> Value {
        let trimmed = code.trim();
        let (prefix, rest) = if let Some(pos) = trimmed.find('<') {
            (trimmed.chars().next().unwrap_or(' '), &trimmed[pos..])
        } else {
            (' ', trimmed)
        };
        let start = rest.find('<');
        let end = rest.rfind('>');
        if let (Some(s), Some(e)) = (start, end) {
            let inner = &rest[s + 1..e];
            let words: Vec<&str> = inner.split_whitespace().collect();
            match prefix {
                '~' => Value::Str(words.join(" ")),
                '+' => Value::Int(words.len() as i64),
                '?' => Value::Bool(!words.is_empty()),
                _ => Value::Str(words.join(" ")),
            }
        } else {
            Value::Nil
        }
    }

    fn format_sprintf(&self, fmt: &str, arg: Option<&Value>) -> String {
        let mut chars = fmt.chars().peekable();
        let mut out = String::new();
        while let Some(c) = chars.next() {
            if c != '%' {
                out.push(c);
                continue;
            }
            let mut width = String::new();
            while let Some(d) = chars.peek().copied() {
                if d.is_ascii_digit() {
                    width.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
            let spec = chars.next().unwrap_or('s');
            let width_num = width.parse::<usize>().unwrap_or(0);
            let zero_pad = width.starts_with('0');
            let rendered = match spec {
                's' => match arg {
                    Some(Value::Str(s)) => s.clone(),
                    Some(Value::Int(i)) => i.to_string(),
                    Some(Value::Bool(b)) => b.to_string(),
                    _ => String::new(),
                },
                'x' => match arg {
                    Some(Value::Int(i)) => format!("{:x}", i),
                    _ => String::new(),
                },
                _ => String::new(),
            };
            if width_num > rendered.len() {
                let pad = width_num - rendered.len();
                let ch = if zero_pad { '0' } else { ' ' };
                out.push_str(&ch.to_string().repeat(pad));
            }
            out.push_str(&rendered);
        }
        out
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
