use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{AssignOp, CallArg, Expr, PhaserKind, Stmt, make_anon_sub};
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::token_kind::TokenKind;
use crate::value::Value;

static STATE_COUNTER: AtomicUsize = AtomicUsize::new(0);
mod expr;
mod helpers;
mod stmt;

pub(crate) struct Compiler {
    code: CompiledCode,
    local_map: HashMap<String, u32>,
    compiled_functions: HashMap<String, CompiledFunction>,
    current_package: String,
    tmp_counter: usize,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            code: CompiledCode::new(),
            local_map: HashMap::new(),
            compiled_functions: HashMap::new(),
            current_package: "GLOBAL".to_string(),
            tmp_counter: 0,
        }
    }

    fn alloc_local(&mut self, name: &str) -> u32 {
        if let Some(&slot) = self.local_map.get(name) {
            return slot;
        }
        let slot = self.code.locals.len() as u32;
        self.code.locals.push(name.to_string());
        self.local_map.insert(name.to_string(), slot);
        slot
    }

    fn emit_set_named_var(&mut self, name: &str) {
        if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::SetLocal(slot));
        } else {
            let idx = self.code.add_constant(Value::Str(name.to_string()));
            self.code.emit(OpCode::SetGlobal(idx));
        }
    }

    fn compile_exprs(&mut self, exprs: &[Expr]) {
        for expr in exprs {
            self.compile_expr(expr);
        }
    }

    fn build_for_bind_stmts(
        param: &Option<String>,
        param_idx: Option<u32>,
        params: &[String],
    ) -> Vec<Stmt> {
        let mut bind_stmts = Vec::new();
        if let Some(single_param) = param
            && param_idx.is_none()
        {
            bind_stmts.push(Stmt::Assign {
                name: single_param.clone(),
                expr: Expr::Var("_".to_string()),
                op: AssignOp::Assign,
            });
        }
        for (i, p) in params.iter().enumerate() {
            bind_stmts.push(Stmt::Assign {
                name: p.clone(),
                expr: Expr::Index {
                    target: Box::new(Expr::Var("_".to_string())),
                    index: Box::new(Expr::Literal(Value::Int(i as i64))),
                },
                op: AssignOp::Assign,
            });
        }
        bind_stmts
    }

    pub(crate) fn compile(
        mut self,
        stmts: &[Stmt],
    ) -> (CompiledCode, HashMap<String, CompiledFunction>) {
        self.hoist_sub_decls(stmts);
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last && let Stmt::Expr(expr) = stmt {
                self.compile_expr(expr);
                self.code.emit(OpCode::SetTopic);
                continue;
            }
            self.compile_stmt(stmt);
        }
        (self.code, self.compiled_functions)
    }
}
