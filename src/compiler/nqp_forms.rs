use super::*;

/// `nqp::const::*` names that compile to integer literals. The values follow
/// MoarVM: the low 2 bits of a binary read/write flag are the endianness
/// (0 native, 1 little, 2 big — matching Raku's `Endian` enum, with which
/// these are `nqp::bitor_i`ed), and the size flag occupies the bits above
/// (`1 << (flags >> 2)` bytes).
pub(crate) fn nqp_const_value(name: &str) -> Option<i64> {
    let konst = name.strip_prefix("nqp::const::")?;
    // Cost: O(1) at run time (every constant folds to an integer literal at compile time).
    Some(match konst {
        "BINARY_ENDIAN_NATIVE" => 0,
        "BINARY_ENDIAN_LITTLE" => 1,
        "BINARY_ENDIAN_BIG" => 2,
        "BINARY_SIZE_8_BIT" => 0,
        "BINARY_SIZE_16_BIT" => 4,
        "BINARY_SIZE_32_BIT" => 8,
        "BINARY_SIZE_64_BIT" => 12,
        // MoarVM's character-class bits, as `nqp::iscclass` / `findcclass` /
        // `findnotcclass` take them (runtime/nqp_ops_text.rs implements the
        // membership rules). These are a bit SET: `CCLASS_ANY` is every bit.
        "CCLASS_UPPERCASE" => 1,
        "CCLASS_LOWERCASE" => 2,
        "CCLASS_ALPHABETIC" => 4,
        "CCLASS_NUMERIC" => 8,
        "CCLASS_HEXADECIMAL" => 16,
        "CCLASS_WHITESPACE" => 32,
        "CCLASS_PRINTING" => 64,
        "CCLASS_BLANK" => 256,
        "CCLASS_CONTROL" => 512,
        "CCLASS_PUNCTUATION" => 1024,
        "CCLASS_ALPHANUMERIC" => 2048,
        "CCLASS_NEWLINE" => 4096,
        "CCLASS_WORD" => 8192,
        "CCLASS_ANY" => 65535,
        // Normalization forms for `nqp::strtocodes`.
        "NORMALIZE_NONE" => 0,
        "NORMALIZE_NFC" => 1,
        "NORMALIZE_NFD" => 2,
        "NORMALIZE_NFKC" => 3,
        "NORMALIZE_NFKD" => 4,
        // Filesystem stat selectors used by nqp::stat. These values follow
        // NQP's STAT_* constants (and the order used by MoarVM).
        "STAT_EXISTS" => 0,
        "STAT_FILESIZE" => 1,
        "STAT_ISDIR" => 2,
        "STAT_ISREG" => 3,
        _ => return None,
    })
}

impl Compiler {
    /// Compile the `nqp::` CONTROL-FLOW ops, which are special forms, not
    /// calls: their operands are evaluated lazily (`nqp::if` branches) or
    /// repeatedly (`nqp::while` condition/body), so compiling them as an
    /// eager-argument `Call` would both mis-evaluate side effects and lose
    /// the loop. CBOR::Simple's encoder is written entirely in this style.
    ///
    /// Returns true when `name` was such a form and has been fully compiled
    /// (one value left on the stack); false to fall through to the normal
    /// call path (the VALUE ops — `nqp::add_i` etc. — stay ordinary calls,
    /// dispatched in `runtime/nqp_ops.rs`).
    pub(super) fn try_compile_nqp_form(&mut self, name: &str, args: &[Expr]) -> bool {
        match name {
            // nqp::stmts(a, b, ..., z) — evaluate in order, yield the last.
            // Cost: O(1) (compiles to sequenced code + Pop; no runtime op).
            "nqp::stmts" => {
                if args.is_empty() {
                    let nil_idx = self.code.add_constant(Value::NIL);
                    self.code.emit(OpCode::LoadConst(nil_idx));
                    return true;
                }
                for (i, arg) in args.iter().enumerate() {
                    self.compile_expr(arg);
                    if i + 1 < args.len() {
                        self.code.emit(OpCode::Pop);
                    }
                }
                true
            }
            // nqp::if(c, t) / nqp::if(c, t, e) — lazy, value-yielding.
            // Cost: O(1) (compiles to jumps; no runtime op).
            "nqp::if" | "nqp::unless" if args.len() == 2 || args.len() == 3 => {
                self.compile_expr(&args[0]);
                let jump_else = if name == "nqp::if" {
                    self.code.emit(OpCode::JumpIfFalse(0))
                } else {
                    self.code.emit(OpCode::JumpIfTrue(0))
                };
                self.compile_expr(&args[1]);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_else);
                match args.get(2) {
                    Some(e) => self.compile_expr(e),
                    None => {
                        let nil_idx = self.code.add_constant(Value::NIL);
                        self.code.emit(OpCode::LoadConst(nil_idx));
                    }
                }
                self.code.patch_jump(jump_end);
                true
            }
            // nqp::where(obj) — the object's identity integer. It is `.WHERE`
            // (rakudo's `Mu.WHERE` is `nqp::where(self)`), so it compiles to
            // that method rather than keeping a second identity scheme (#9346).
            // Cost: O(1) (the `.WHERE` method call it compiles to).
            "nqp::where" if args.len() == 1 => {
                let call = Expr::MethodCall {
                    target: Box::new(args[0].clone()),
                    name: crate::symbol::Symbol::intern("WHERE"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                };
                self.compile_expr(&call);
                true
            }
            // nqp::iscont(obj) — 1 when the operand is a container (a Scalar,
            // an element container, a Proxy) rather than a bare value. The
            // `nqp::` layer sees only decontainerized values, so the operand is
            // compiled as `obj.VAR` -- the one place mutsu already answers
            // "which container is this" for a variable, an element or a
            // parameter -- and the runtime op classifies what that yields
            // (#9346).
            // Cost: O(1) (a `.VAR` plus one classification op).
            "nqp::iscont" if args.len() == 1 => {
                let var = Expr::MethodCall {
                    target: Box::new(args[0].clone()),
                    name: crate::symbol::Symbol::intern("VAR"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                };
                self.try_compile_nqp_value_op(name, std::slice::from_ref(&var))
            }
            // nqp::ifnull(a, b) — yield `a` unless it is null, in which case
            // yield `b`. A special form because `b` must not be evaluated when
            // `a` is there: rakudo's idiom is
            // `nqp::ifnull(nqp::getattr(...), nqp::bindattr(..., fresh))`,
            // which would install a fresh empty store over a live one if both
            // arms ran. mutsu has no VM-level null distinct from an undefined
            // Raku value, so "null" is tested as undefined — which also makes a
            // type object take the `b` arm.
            // Cost: O(1) (compiles to jumps; no runtime op).
            "nqp::ifnull" if args.len() == 2 => {
                self.compile_expr(&args[0]);
                // `JumpIfNotNil` peeks, so the defined value is already the
                // result on the taken path.
                let jump_keep = self.code.emit(OpCode::JumpIfNotNil(0));
                self.code.emit(OpCode::Pop);
                self.compile_expr(&args[1]);
                self.code.patch_jump(jump_keep);
                true
            }
            // nqp::handle(body, 'CATCH', handler) — evaluate the body and run
            // the handler only when it throws. NQP uses this around filesystem
            // operations such as opendir, where the handler can return an
            // empty iterator after a missing directory.
            // Cost: O(1) (compiles to a try/CATCH region; no runtime op).
            "nqp::handle" if args.len() == 3 => {
                let body = vec![Stmt::Expr(args[0].clone())];
                let catch = Some(vec![Stmt::Default(vec![Stmt::Expr(args[2].clone())])]);
                self.compile_try_with_catch_value(&body, &catch);
                true
            }
            // nqp::while(c, body) / nqp::until(c, body) — re-evaluate the
            // condition each iteration; yields Nil.
            // Cost: O(1) per iteration (compiles to jumps; no runtime op).
            "nqp::while" | "nqp::until" if args.len() == 2 => {
                let loop_start = self.code.ops.len();
                self.compile_expr(&args[0]);
                let jump_end = if name == "nqp::while" {
                    self.code.emit(OpCode::JumpIfFalse(0))
                } else {
                    self.code.emit(OpCode::JumpIfTrue(0))
                };
                self.compile_expr(&args[1]);
                self.code.emit(OpCode::Pop);
                self.code.emit(OpCode::Jump(loop_start as i32));
                self.code.patch_jump(jump_end);
                let nil_idx = self.code.add_constant(Value::NIL);
                self.code.emit(OpCode::LoadConst(nil_idx));
                true
            }
            // nqp::repeat_while(c, body) / nqp::repeat_until(c, body) — the
            // post-test loops: the body runs once before the condition is
            // first evaluated. Yields Nil, like `nqp::while`.
            // TODO: in value context rakudo yields a Seq of the body values
            // (for all four loop forms); mutsu yields Nil.
            // Cost: O(1) per iteration (compiles to jumps; no runtime op).
            "nqp::repeat_while" | "nqp::repeat_until" if args.len() == 2 => {
                let loop_start = self.code.ops.len();
                self.compile_expr(&args[1]);
                self.code.emit(OpCode::Pop);
                self.compile_expr(&args[0]);
                if name == "nqp::repeat_until" {
                    // `JumpIfFalse` pops the condition on both paths.
                    self.code.emit(OpCode::JumpIfFalse(loop_start as i32));
                } else {
                    let jump_end = self.code.emit(OpCode::JumpIfFalse(0));
                    self.code.emit(OpCode::Jump(loop_start as i32));
                    self.code.patch_jump(jump_end);
                }
                let nil_idx = self.code.add_constant(Value::NIL);
                self.code.emit(OpCode::LoadConst(nil_idx));
                true
            }
            _ => false,
        }
    }

    /// Compile an `nqp::` VALUE op (`nqp::add_i`, `nqp::ordat`, ...) to the
    /// dedicated [`OpCode::NqpOp`], resolving WHICH op it is here rather than
    /// once per execution.
    ///
    /// `nqp::` is a reserved namespace of compiler-known primitives — no user
    /// routine can be declared there, and the ops bind no parameters — so a
    /// call site's op is a compile-time constant and its operands are plain
    /// values. The `CallFunc` this replaces re-established both per execution:
    /// it rebuilt the operand list through the generic call protocol
    /// (`|EXPR` spreading, `VarRef` unwrapping, callsite-line sanitizing,
    /// `Proxy` auto-FETCH), then stripped `nqp::` off the callee string and
    /// walked up to six chained `match op` tables to find the implementation.
    ///
    /// Returns true when the call was compiled here. Everything else falls
    /// through to the ordinary call path, which is what keeps the two shapes
    /// this deliberately does not take working, and keeps an unknown op name
    /// failing loudly:
    ///
    /// * a **named argument or a `|EXPR` spread** — no nqp op takes either,
    ///   but the operand count then is not a compile-time fact, so the general
    ///   path (which alone can spread) keeps them;
    /// * an **op name the registry does not know**, including every
    ///   `nqp::`-namespaced name that is not an op at all: `CallFunc` reaches
    ///   the same dispatch chain and raises the same `Unsupported nqp:: op`
    ///   error (`runtime/nqp_ops.rs`'s module doc explains why that guard
    ///   matters — `nqp::index` answers -1 where Raku's `index` answers Nil,
    ///   and nqp code branches on exactly that).
    ///
    /// Operands compile as ORDINARY EXPRESSIONS (`compile_expr`), not through
    /// `compile_call_arg`: that helper's job is to hand a callee's `is rw` /
    /// `\raw` parameter a container to bind, which it does by wrapping the
    /// argument in a `VarRef` — and the nqp path then unwrapped every one of
    /// them again before dispatch, because the `nqp::` layer has no notion of
    /// a Raku container (see the decontainerizing preamble of `call_nqp_op`).
    /// Building those wrappers to discard them was pure per-operand waste.
    pub(super) fn try_compile_nqp_value_op(&mut self, name: &str, args: &[Expr]) -> bool {
        let Some(op) = name.strip_prefix(crate::symbol::NQP_OP_PREFIX) else {
            return false;
        };
        if args.len() > u8::MAX as usize || args.iter().any(Self::is_named_arg_expr) {
            return false;
        }
        let Some(id) = crate::runtime::nqp_op_ids::nqp_op_id(op) else {
            return false;
        };
        for arg in args {
            self.compile_expr(arg);
        }
        self.code.emit(OpCode::NqpOp {
            id,
            arity: args.len() as u8,
        });
        true
    }
}
