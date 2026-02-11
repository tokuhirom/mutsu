use crate::ast::Stmt;
use crate::opcode::{CompiledCode, OpCode};

pub(crate) struct Compiler {
    code: CompiledCode,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            code: CompiledCode::new(),
        }
    }

    pub(crate) fn compile(mut self, stmts: &[Stmt]) -> CompiledCode {
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
        self.code
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        // Phase 1: fallback everything to the interpreter
        let idx = self.code.add_stmt(stmt.clone());
        self.code.emit(OpCode::InterpretStmt(idx));
    }
}
