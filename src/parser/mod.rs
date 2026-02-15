#![allow(clippy::result_large_err)]

mod arith;
mod block;
mod calls;
mod declarations;
mod expr;
mod helpers;
mod operators;
mod primary;
mod stmt;

use crate::ast::{AssignOp, CallArg, Expr, ParamDef, PhaserKind, Stmt};
use crate::lexer::{Token, TokenKind};
use crate::value::{RuntimeError, Value};

pub(crate) struct Parser {
    pub(super) tokens: Vec<Token>,
    pub(super) pos: usize,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub(crate) fn parse_program(&mut self) -> Result<Vec<Stmt>, RuntimeError> {
        crate::trace::trace_log!("parse", "parse_program: {} tokens", self.tokens.len());
        let mut stmts = Vec::new();
        while !self.check(&TokenKind::Eof) {
            let start = self.pos;
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(e)
                    if e.message.contains("X::Obsolete")
                        || e.message.contains("X::Comp")
                        || e.message.contains("X::Syntax") =>
                {
                    return Err(e);
                }
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
}
