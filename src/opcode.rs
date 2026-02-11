use crate::ast::{Expr, Stmt};
use crate::value::Value;

/// Bytecode operations for the VM.
#[derive(Debug, Clone)]
pub(crate) enum OpCode {
    // -- Constants --
    LoadConst(u32),
    LoadNil,
    LoadTrue,
    LoadFalse,

    // -- Variables --
    GetLocal(u32),
    SetLocal(u32),
    GetGlobal(u32),
    SetGlobal(u32),

    // -- Arithmetic --
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Negate,

    // -- Logic / coercion --
    Not,
    BoolCoerce,

    // -- String --
    Concat,

    // -- Numeric comparison --
    NumEq,
    NumNe,
    NumLt,
    NumLe,
    NumGt,
    NumGe,

    // -- String comparison --
    StrEq,
    StrNe,

    // -- Nil check (for defined-or //) --
    IsNil,

    // -- Control flow --
    Jump(i32),
    JumpIfFalse(i32),
    JumpIfTrue(i32),
    /// Jump if top of stack is nil (without popping)
    JumpIfNil(i32),
    /// Jump if top of stack is not nil (without popping)
    JumpIfNotNil(i32),

    // -- Stack manipulation --
    Dup,
    Pop,

    // -- Composite --
    MakeArray(u32),

    // -- I/O --
    Say(u32),
    Print(u32),

    // -- Functions --
    Return,

    // -- Fallback to tree-walker --
    InterpretExpr(u32),
    InterpretStmt(u32),
}

/// A compiled chunk of bytecode.
#[derive(Debug)]
pub(crate) struct CompiledCode {
    pub(crate) ops: Vec<OpCode>,
    pub(crate) constants: Vec<Value>,
    pub(crate) expr_pool: Vec<Expr>,
    pub(crate) stmt_pool: Vec<Stmt>,
    pub(crate) locals: Vec<String>,
}

impl CompiledCode {
    pub(crate) fn new() -> Self {
        Self {
            ops: Vec::new(),
            constants: Vec::new(),
            expr_pool: Vec::new(),
            stmt_pool: Vec::new(),
            locals: Vec::new(),
        }
    }

    pub(crate) fn emit(&mut self, op: OpCode) -> usize {
        let idx = self.ops.len();
        self.ops.push(op);
        idx
    }

    /// Patch a jump instruction at `idx` to point to the current position.
    pub(crate) fn patch_jump(&mut self, idx: usize) {
        let target = self.ops.len() as i32;
        match &mut self.ops[idx] {
            OpCode::Jump(offset)
            | OpCode::JumpIfFalse(offset)
            | OpCode::JumpIfTrue(offset)
            | OpCode::JumpIfNil(offset)
            | OpCode::JumpIfNotNil(offset) => {
                *offset = target;
            }
            _ => panic!("patch_jump on non-jump opcode"),
        }
    }

    pub(crate) fn current_pos(&self) -> usize {
        self.ops.len()
    }

    pub(crate) fn add_constant(&mut self, value: Value) -> u32 {
        let idx = self.constants.len() as u32;
        self.constants.push(value);
        idx
    }

    pub(crate) fn add_expr(&mut self, expr: Expr) -> u32 {
        let idx = self.expr_pool.len() as u32;
        self.expr_pool.push(expr);
        idx
    }

    pub(crate) fn add_stmt(&mut self, stmt: Stmt) -> u32 {
        let idx = self.stmt_pool.len() as u32;
        self.stmt_pool.push(stmt);
        idx
    }
}
