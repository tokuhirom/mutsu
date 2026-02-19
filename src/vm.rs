#![allow(clippy::result_large_err)]
use std::collections::HashMap;

use crate::ast::Stmt;
use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::runtime;
use crate::value::{JunctionKind, LazyList, RuntimeError, Value, make_rat, next_instance_id};
use num_traits::{Signed, Zero};

mod vm_call_ops;
mod vm_control_ops;
mod vm_data_ops;
mod vm_exec;
mod vm_exec_ops;
mod vm_helpers;
mod vm_var_ops;
pub(crate) struct VM {
    interpreter: Interpreter,
    stack: Vec<Value>,
    locals: Vec<Value>,
}

impl VM {
    pub(crate) fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            stack: Vec::new(),
            locals: Vec::new(),
        }
    }

    /// Run the compiled bytecode. Always returns the interpreter back
    /// (even on error) so the caller can restore it.
    pub(crate) fn run(
        mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> (Interpreter, Result<(), RuntimeError>) {
        // Initialize local variable slots
        self.locals = vec![Value::Nil; code.locals.len()];
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                return (self.interpreter, Err(e));
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        (self.interpreter, Ok(()))
    }

    /// Run compiled bytecode without consuming self.
    /// Used by map/grep to avoid VM creation/destruction per iteration.
    pub(crate) fn run_reuse(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        self.stack.clear();
        // Initialize local variable slots
        self.locals.resize(code.locals.len(), Value::Nil);
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            } else {
                self.locals[i] = Value::Nil;
            }
        }
        let mut ip = 0;
        while ip < code.ops.len() {
            self.exec_one(code, &mut ip, compiled_fns)?;
            if self.interpreter.is_halted() {
                break;
            }
        }
        Ok(())
    }

    /// Get a reference to the interpreter (for reading env values).
    pub(crate) fn interpreter(&self) -> &Interpreter {
        &self.interpreter
    }

    /// Get a mutable reference to the interpreter (for setting env values).
    pub(crate) fn interpreter_mut(&mut self) -> &mut Interpreter {
        &mut self.interpreter
    }

    /// Consume the VM and return the interpreter.
    pub(crate) fn into_interpreter(self) -> Interpreter {
        self.interpreter
    }

    /// Execute opcodes in [start..end), used by loop compound opcodes.
    fn run_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mut ip = start;
        while ip < end {
            self.exec_one(code, &mut ip, compiled_fns)?;
            if self.interpreter.is_halted() {
                break;
            }
        }
        Ok(())
    }
}
