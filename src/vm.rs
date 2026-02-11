use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, OpCode};
use crate::value::RuntimeError;

pub(crate) struct VM {
    interpreter: Interpreter,
}

impl VM {
    pub(crate) fn new(interpreter: Interpreter) -> Self {
        Self { interpreter }
    }

    /// Run the compiled bytecode. Always returns the interpreter back
    /// (even on error) so the caller can restore it.
    pub(crate) fn run(mut self, code: &CompiledCode) -> (Interpreter, Result<(), RuntimeError>) {
        let mut ip = 0;
        while ip < code.ops.len() {
            match &code.ops[ip] {
                OpCode::InterpretStmt(idx) => {
                    let stmt = &code.stmt_pool[*idx as usize];
                    if let Err(e) = self.interpreter.exec_stmt(stmt) {
                        return (self.interpreter, Err(e));
                    }
                    if self.interpreter.is_halted() {
                        break;
                    }
                }
                OpCode::InterpretExpr(idx) => {
                    let expr = &code.expr_pool[*idx as usize];
                    if let Err(e) = self.interpreter.eval_expr(expr) {
                        return (self.interpreter, Err(e));
                    }
                }
                _ => {
                    unreachable!("OpCode {:?} not yet implemented", &code.ops[ip]);
                }
            }
            ip += 1;
        }
        (self.interpreter, Ok(()))
    }
}
