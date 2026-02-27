use crate::ast::{ParamDef, Stmt};
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
    SetVarType {
        name_idx: u32,
        tc_idx: u32,
    },
    SetTopic,
    GetArrayVar(u32),
    GetHashVar(u32),
    GetBareWord(u32),
    GetPseudoStash(u32),

    // -- Arithmetic --
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Negate,
    IntBitNeg,  // +^ prefix: integer bitwise negation
    BoolBitNeg, // ?^ prefix: boolean bitwise negation
    MakeSlip,   // | prefix: convert array/list to Slip for flattening

    // -- Logic / coercion --
    Not,
    BoolCoerce,
    WrapVarRef(u32),

    // -- String --
    Concat,

    // -- Numeric comparison --
    NumEq,
    NumNe,
    NumLt,
    NumLe,
    NumGt,
    NumGe,
    ApproxEq,
    ContainerEq,

    // -- String comparison --
    StrEq,
    StrNe,
    StrLt,
    StrGt,
    StrLe,
    StrGe,

    // -- Generic ordering (cmp-based) --
    Before,
    After,

    // -- Three-way comparison --
    Spaceship,
    Cmp,
    Leg,

    // -- Identity/value equality --
    StrictEq,
    StrictNe,
    Eqv,
    /// Smart match with compiled RHS expression at [ip+1..rhs_end).
    SmartMatchExpr {
        rhs_end: u32,
        negate: bool,
        /// Variable name for LHS (to write back after s/// substitution)
        lhs_var: Option<String>,
    },

    // -- Divisibility --
    DivisibleBy,
    NotDivisibleBy,

    // -- Keyword math --
    IntDiv,
    IntMod,
    Gcd,
    Lcm,
    InfixMin,
    InfixMax,

    // -- Repetition --
    StringRepeat,
    ListRepeat,
    FunctionCompose,

    // -- Mixin --
    ButMixin,
    // -- Type check --
    Isa,
    Does,
    DoesVar(u32),

    // -- Pair --
    MakePair,
    /// Convert Pair(k,v) → ValuePair(Str(k),v) so it's treated as positional arg
    ContainerizePair,

    // -- Bitwise --
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    BoolBitOr,
    BoolBitAnd,
    BoolBitXor,

    // -- Set operations --
    SetElem,
    SetCont,
    SetUnion,
    SetIntersect,
    SetDiff,
    SetSymDiff,
    SetSubset,
    SetSuperset,
    SetStrictSubset,
    SetStrictSuperset,
    JunctionAny,
    JunctionAll,
    JunctionOne,

    // -- Sequence --
    Sequence {
        exclude_end: bool,
    },

    // -- Nil check (for defined-or //) --
    #[allow(dead_code)]
    IsNil,

    // -- Control flow --
    Jump(i32),
    JumpIfFalse(i32),
    JumpIfTrue(i32),
    /// Jump if top of stack is nil/undefined (without popping)
    #[allow(dead_code)]
    JumpIfNil(i32),
    /// Jump if top of stack is not nil/defined (without popping)
    JumpIfNotNil(i32),
    /// Call .defined on top of stack, replace with Bool result
    CallDefined,

    // -- Logical --
    /// Logical XOR: pops two values, returns truthy one if exactly one is truthy, else Nil/falsy
    XorXor,

    // -- Stack manipulation --
    Dup,
    Pop,

    // -- Range creation --
    MakeRange,
    MakeRangeExcl,
    MakeRangeExclStart,
    MakeRangeExclBoth,

    // -- Composite --
    MakeArray(u32),
    /// Like MakeArray but creates a true Array (from [...] literals) instead of a List.
    MakeRealArray(u32),
    MakeHash(u32),
    /// Create a Capture from `count` items on stack. Pair values become named args,
    /// non-Pair values become positional args. Slip values are flattened.
    MakeCapture(u32),

    // -- I/O --
    Say(u32),
    Print(u32),
    Note(u32),

    // -- Calls (args compiled to bytecode, dispatch delegated to interpreter) --
    /// Expression-level function call: pop `arity` args, call name, push result.
    CallFunc {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Expression-level function call with capture slip: pop 1 slip + `regular_arity` args,
    /// flatten the slip into the argument list, call name, push result.
    CallFuncSlip {
        name_idx: u32,
        regular_arity: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Method call: pop `arity` args + target, call method, push result.
    CallMethod {
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        /// When true, the method name was quoted (e.g. `."DEFINITE"()`),
        /// bypassing pseudo-method macros.
        quoted: bool,
    },
    /// Method call with writeback: target is a variable that may be mutated.
    CallMethodMut {
        name_idx: u32,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        /// When true, the method name was quoted (e.g. `."DEFINITE"()`),
        /// bypassing pseudo-method macros.
        quoted: bool,
    },
    /// Dynamic method call: method name is evaluated at runtime.
    /// Stack layout: [target, name_str, arg0, arg1, ...]
    CallMethodDynamic {
        arity: u32,
    },
    /// Statement-level call: pop `arity` args, call name (no push).
    ExecCall {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    /// Statement-level call with positional/named values encoded as Value::Pair.
    ExecCallPairs {
        name_idx: u32,
        arity: u32,
    },
    /// Call with capture slip: `regular_arity` normal args + 1 slip arg on stack.
    /// The slip arg (top of stack) is an Array whose elements are flattened into
    /// the argument list before the call.
    ExecCallSlip {
        name_idx: u32,
        regular_arity: u32,
        arg_sources_idx: Option<u32>,
    },
    BlockScope {
        enter_end: u32,
        body_end: u32,
        end: u32,
    },
    DoBlockExpr {
        body_end: u32,
        label: Option<String>,
    },
    DoGivenExpr {
        body_end: u32,
    },
    MakeGather(u32),
    CallOnValue {
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    CallOnCodeVar {
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    },
    MakeAnonSub(u32),
    MakeAnonSubParams(u32),
    MakeLambda(u32),
    MakeBlockClosure(u32),
    // -- Indexing --
    Index,
    DeleteIndexNamed(u32),
    DeleteIndexExpr,
    /// Hash hyperslice: recursively iterate hash with given adverb mode.
    /// Stack: [target] → [result list]
    HyperSlice(u8),
    /// Hash hyperindex: drill into nested hash by key path.
    /// Stack: [target, keys] → [value]
    HyperIndex,

    // -- String interpolation --
    StringConcat(u32),

    // -- Loop control --
    Last(Option<String>),
    Next(Option<String>),
    Redo(Option<String>),

    // -- Given/When control --
    Proceed,
    Succeed,
    /// Tag the current value as coming from a named container (for Scalar binding)
    TagContainerRef(u32),

    // -- Unary coercion --
    NumCoerce,
    StrCoerce,
    UptoRange,

    // -- Prefix increment/decrement (returns NEW value) --
    PreIncrement(u32),
    PreDecrement(u32),
    PreIncrementIndex(u32),
    PreDecrementIndex(u32),

    // -- Variable access --
    GetCaptureVar(u32),
    GetCodeVar(u32),

    // -- Postfix operators --
    PostIncrement(u32),
    PostDecrement(u32),
    PostIncrementIndex(u32),
    PostDecrementIndex(u32),
    IndexAssignExprNamed(u32),

    // -- Assignment as expression --
    AssignExpr(u32),
    /// Assignment as expression for local variable (indexed slot)
    AssignExprLocal(u32),
    IndexAssignExprNested(u32),
    /// Generic index assignment on a stack-computed target.
    /// Stack: [target, index, value] → assigns value to target[index].
    /// Supports callframe .my hash writeback for dynamic variables.
    IndexAssignGeneric,
    AssignReadOnly,
    /// Check if a variable is readonly; throw if so (for assignment to readonly params).
    CheckReadOnly(u32),

    // -- Loops (compound opcodes) --
    /// While loop. Condition opcodes follow at [ip+1..cond_end).
    /// Body opcodes at [cond_end..body_end). VM loops internally.
    WhileLoop {
        cond_end: u32,
        body_end: u32,
        label: Option<String>,
        collect: bool,
    },
    /// For loop. Iterable value must be on stack.
    /// Body opcodes at [ip+1..body_end). VM iterates internally.
    ForLoop {
        param_idx: Option<u32>,
        param_local: Option<u32>,
        body_end: u32,
        label: Option<String>,
        arity: u32,
        collect: bool,
    },
    /// C-style loop: [cond opcodes][body opcodes][step opcodes].
    /// Layout after CStyleLoop: cond at [ip+1..cond_end), body at [cond_end..step_start),
    /// step at [step_start..body_end).
    CStyleLoop {
        cond_end: u32,
        step_start: u32,
        body_end: u32,
        label: Option<String>,
        collect: bool,
    },

    // -- Given/When/Default (compound opcodes) --
    Given {
        body_end: u32,
    },
    When {
        body_end: u32,
    },
    Default {
        body_end: u32,
    },

    // -- Repeat loop (compound opcode) --
    RepeatLoop {
        cond_end: u32,
        body_end: u32,
        label: Option<String>,
    },

    // -- Environment variable access --
    GetEnvIndex(u32),

    // -- Exists check --
    ExistsEnvIndex(u32),
    ExistsExpr,
    /// Rich :exists adverb with flags.
    /// Stack: [target, index] or [target, index, arg] or [target] (zen).
    /// Flags: bit0=negated, bit1=has_arg, bit2=is_zen,
    ///        bits 4-7=adverb (0=None,1=Kv,2=NotKv,3=P,4=NotP,5=NotV,
    ///                         6=InvalidK,7=InvalidNotK,8=InvalidV)
    ExistsIndexAdv(u32),

    // -- Reduction ([+] @arr) --
    Reduction(u32),

    // -- Magic variables --
    RoutineMagic,
    BlockMagic,

    // -- Substitution (s///) --
    Subst {
        pattern_idx: u32,
        replacement_idx: u32,
        samemark: bool,
    },

    // -- Non-destructive substitution (S///) --
    NonDestructiveSubst {
        pattern_idx: u32,
        replacement_idx: u32,
        samemark: bool,
    },

    // -- Transliteration (tr///) --
    Transliterate {
        from_idx: u32,
        to_idx: u32,
        delete: bool,
        complement: bool,
        squash: bool,
    },

    // -- Take (gather/take) --
    Take,

    // -- Package scope --
    PackageScope {
        name_idx: u32,
        body_end: u32,
    },
    /// Register a package name so it's accessible as a Package value.
    RegisterPackage {
        name_idx: u32,
    },

    // -- Phaser --
    PhaserEnd(u32),

    // -- HyperMethodCall (».method) --
    HyperMethodCall {
        name_idx: u32,
        arity: u32,
    },

    // -- HyperOp (>>op<<) --
    HyperOp {
        op_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
    },

    // -- MetaOp (Rop, Xop, Zop) --
    MetaOp {
        meta_idx: u32,
        op_idx: u32,
    },

    // -- InfixFunc (atan2, sprintf) --
    InfixFunc {
        name_idx: u32,
        right_arity: u32,
        modifier_idx: Option<u32>,
    },

    // -- Exception handling --
    /// Try block layout:
    /// body at [ip+1..catch_start),
    /// catch at [catch_start..control_start),
    /// control at [control_start..body_end).
    TryCatch {
        catch_start: u32,
        control_start: u32,
        body_end: u32,
        /// True when CATCH { } is explicitly present — unhandled exceptions
        /// (no `when`/`default` match) must be re-thrown.
        explicit_catch: bool,
    },

    // -- Error handling --
    Die,
    Fail,

    // -- Functions --
    Return,
    RegisterSub(u32),
    RegisterToken(u32),
    RegisterProtoSub(u32),
    RegisterProtoToken(u32),
    RegisterEnum(u32),
    RegisterClass(u32),
    RegisterRole(u32),
    RegisterSubset(u32),
    SubtestScope {
        body_end: u32,
    },
    ReactScope {
        body_end: u32,
    },
    WheneverScope {
        body_idx: u32,
        param_idx: Option<u32>,
        target_var_idx: Option<u32>,
    },
    UseModule(u32),
    ImportModule {
        name_idx: u32,
        tags_idx: Option<u32>,
    },
    NoModule(u32),
    /// `need Module;` — load module without importing exports.
    NeedModule(u32),
    UseLibPath,
    /// Save current function/class registries for lexical import scoping.
    PushImportScope,
    /// Restore function/class registries to the last saved snapshot.
    PopImportScope,

    // -- Type checking --
    /// Check that the value on top of stack matches the given type constraint.
    /// The u32 is a constant index for the type name string.
    TypeCheck(u32),

    // -- Fallback to interpreter --
    /// Evaluate an AST expression via the interpreter (for operators not yet compiled).
    /// The u32 indexes into stmt_pool.
    EvalAstExpr(u32),

    /// State variable initialization.
    /// slot = local slot index, key_idx = constant index for unique state key.
    /// Pops init value from stack.
    /// If state_vars has key: set locals[slot] = stored value (discard init).
    /// If not: set locals[slot] = init value, store in state_vars.
    StateVarInit(u32, u32),
    /// Mark whether a declared variable should report `.VAR.dynamic` true.
    SetVarDynamic {
        name_idx: u32,
        dynamic: bool,
    },
    RegisterVarExport {
        name_idx: u32,
        tags_idx: Option<u32>,
    },

    /// Get a variable from the caller's scope ($CALLER::varname).
    /// name_idx = constant index for the bare variable name (without CALLER:: prefix).
    /// depth = number of CALLER:: levels (1 for $CALLER::x, 2 for $CALLER::CALLER::x).
    GetCallerVar {
        name_idx: u32,
        depth: u32,
    },

    /// Set a variable in the caller's scope ($CALLER::varname = value).
    SetCallerVar {
        name_idx: u32,
        depth: u32,
    },

    /// Bind a variable in the caller's scope to a local variable ($CALLER::target := $source).
    /// This creates an alias so that changes to source are reflected in target.
    BindCallerVar {
        target_idx: u32,
        source_idx: u32,
        depth: u32,
    },

    /// Get a variable by searching the dynamic call stack ($DYNAMIC::varname).
    GetDynamicVar(u32),

    /// Indirect type lookup: pop string from stack, resolve to Package value.
    IndirectTypeLookup,

    /// Indirect code lookup: pop package string from stack, resolve &name in that package context.
    IndirectCodeLookup(u32),

    /// Save current variable value for `let` scope management.
    /// Pops the array index (if index_mode is true) from the stack.
    LetSave {
        name_idx: u32,
        index_mode: bool,
    },

    /// Block with `let` scope management. Executes body, then checks
    /// the topic ($_) to decide whether to restore or discard let saves.
    LetBlock {
        body_end: u32,
    },
}

/// A compiled chunk of bytecode.
#[derive(Debug)]
pub(crate) struct CompiledCode {
    pub(crate) ops: Vec<OpCode>,
    pub(crate) constants: Vec<Value>,
    pub(crate) stmt_pool: Vec<Stmt>,
    pub(crate) locals: Vec<String>,
    /// Maps local slot indices to persistent state keys for `state` variables.
    pub(crate) state_locals: Vec<(usize, String)>,
}

impl CompiledCode {
    pub(crate) fn new() -> Self {
        Self {
            ops: Vec::new(),
            constants: Vec::new(),
            stmt_pool: Vec::new(),
            locals: Vec::new(),
            state_locals: Vec::new(),
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

    #[allow(dead_code)]
    pub(crate) fn current_pos(&self) -> usize {
        self.ops.len()
    }

    /// Patch the body_end field of a loop opcode.
    pub(crate) fn patch_loop_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::WhileLoop { body_end, .. } => *body_end = target,
            OpCode::ForLoop { body_end, .. } => *body_end = target,
            OpCode::CStyleLoop { body_end, .. } => *body_end = target,
            OpCode::RepeatLoop { body_end, .. } => *body_end = target,
            OpCode::BlockScope { end, .. } => *end = target,
            _ => panic!("patch_loop_end on non-loop opcode"),
        }
    }

    pub(crate) fn patch_block_enter_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { enter_end, .. } => *enter_end = target,
            _ => panic!("patch_block_enter_end on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_block_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::BlockScope { body_end, .. } => *body_end = target,
            _ => panic!("patch_block_body_end on non-BlockScope opcode"),
        }
    }

    pub(crate) fn patch_repeat_cond_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::RepeatLoop { cond_end, .. } => *cond_end = target,
            _ => panic!("patch_repeat_cond_end on non-RepeatLoop opcode"),
        }
    }

    pub(crate) fn patch_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::Given { body_end, .. } => *body_end = target,
            OpCode::When { body_end, .. } => *body_end = target,
            OpCode::Default { body_end, .. } => *body_end = target,
            OpCode::PackageScope { body_end, .. } => *body_end = target,
            OpCode::DoBlockExpr { body_end, .. } => *body_end = target,
            OpCode::DoGivenExpr { body_end, .. } => *body_end = target,
            OpCode::SubtestScope { body_end, .. } => *body_end = target,
            OpCode::ReactScope { body_end, .. } => *body_end = target,
            _ => panic!("patch_body_end on opcode without body_end"),
        }
    }

    pub(crate) fn patch_smart_match_rhs_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::SmartMatchExpr { rhs_end, .. } => *rhs_end = target,
            _ => panic!("patch_smart_match_rhs_end on non-SmartMatchExpr opcode"),
        }
    }

    pub(crate) fn patch_let_block_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::LetBlock { body_end, .. } => *body_end = target,
            _ => panic!("patch_let_block_end on non-LetBlock opcode"),
        }
    }

    pub(crate) fn patch_try_catch_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::TryCatch { catch_start, .. } => *catch_start = target,
            _ => panic!("patch_try_catch_start on non-TryCatch opcode"),
        }
    }

    pub(crate) fn patch_try_body_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::TryCatch { body_end, .. } => *body_end = target,
            _ => panic!("patch_try_body_end on non-TryCatch opcode"),
        }
    }

    pub(crate) fn patch_try_control_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::TryCatch { control_start, .. } => *control_start = target,
            _ => panic!("patch_try_control_start on non-TryCatch opcode"),
        }
    }

    pub(crate) fn patch_while_cond_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::WhileLoop { cond_end, .. } => *cond_end = target,
            _ => panic!("patch_while_cond_end on non-WhileLoop opcode"),
        }
    }

    pub(crate) fn patch_cstyle_cond_end(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::CStyleLoop { cond_end, .. } => *cond_end = target,
            _ => panic!("patch_cstyle_cond_end on non-CStyleLoop opcode"),
        }
    }

    pub(crate) fn patch_cstyle_step_start(&mut self, idx: usize) {
        let target = self.ops.len() as u32;
        match &mut self.ops[idx] {
            OpCode::CStyleLoop { step_start, .. } => *step_start = target,
            _ => panic!("patch_cstyle_step_start on non-CStyleLoop opcode"),
        }
    }

    pub(crate) fn add_constant(&mut self, value: Value) -> u32 {
        let idx = self.constants.len() as u32;
        self.constants.push(value);
        idx
    }

    pub(crate) fn add_stmt(&mut self, stmt: Stmt) -> u32 {
        let idx = self.stmt_pool.len() as u32;
        self.stmt_pool.push(stmt);
        idx
    }
}

/// A compiled function body (SubDecl compiled to bytecode).
#[derive(Debug)]
pub(crate) struct CompiledFunction {
    pub(crate) code: CompiledCode,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) return_type: Option<String>,
    pub(crate) fingerprint: u64,
    /// When true, this sub has an explicit empty signature `()` and should reject any arguments.
    pub(crate) empty_sig: bool,
}
