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
        // Field indices of the array `nqp::getrusage` fills (MoarVM's
        // `MVM_RUSAGE_*`, the order `builtins::process_rusage` produces).
        "RUSAGE_UTIME_SEC" => 0,
        "RUSAGE_UTIME_MSEC" => 1,
        "RUSAGE_STIME_SEC" => 2,
        "RUSAGE_STIME_MSEC" => 3,
        "RUSAGE_MAXRSS" => 4,
        "RUSAGE_IXRSS" => 5,
        "RUSAGE_IDRSS" => 6,
        "RUSAGE_ISRSS" => 7,
        "RUSAGE_MINFLT" => 8,
        "RUSAGE_MAJFLT" => 9,
        "RUSAGE_NSWAP" => 10,
        "RUSAGE_INBLOCK" => 11,
        "RUSAGE_OUBLOCK" => 12,
        "RUSAGE_MSGSND" => 13,
        "RUSAGE_MSGRCV" => 14,
        "RUSAGE_NSIGNALS" => 15,
        "RUSAGE_NVCSW" => 16,
        "RUSAGE_NIVCSW" => 17,
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
        // Whether this form's value is discarded: it is a statement root (or a
        // block's tail), or a sunk operand of an enclosing form. Only the loop
        // forms' lowering depends on it; `nqp::if` passes it on to its
        // branches. See `Compiler::expr_depth`.
        let sunk = self.expr_depth <= 1;
        match name {
            // nqp::stmts(a, b, ..., z) — evaluate in order, yield the last.
            // Cost: O(1) (compiles to sequenced code + Pop; no runtime op).
            "nqp::stmts" => {
                if args.is_empty() {
                    let nil_idx = self.code.add_constant(Value::NIL);
                    self.code.emit(OpCode::LoadConst(nil_idx));
                    return true;
                }
                // Every operand is in sink position as far as a loop form is
                // concerned, the last one included: rakudo compiles a loop
                // there as a void loop (`nqp::stmts(nqp::while(...))` runs
                // eagerly and yields null), which JSON::Fast relies on to
                // `return` from inside the loop of its parse-obj.
                for (i, arg) in args.iter().enumerate() {
                    self.compile_nqp_operand(arg, true);
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
                self.compile_nqp_operand(&args[1], sunk);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_else);
                match args.get(2) {
                    Some(e) => self.compile_nqp_operand(e, sunk),
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
            // The four loop forms. Sunk (a statement, a block's tail, an
            // `nqp::stmts` operand, the body of a sunk loop), each is a plain
            // jump loop that discards the body values and yields Nil -- the
            // hot path of nqp-style ecosystem code. Anywhere else the loop's
            // value is used, and rakudo yields a lazy Seq of the body values
            // (`nqp::while(c, $i++)` in an argument is `(0, 1, 2).Seq`, and
            // binding it runs nothing until it is pulled); that is exactly
            // the `gather`-backed lowering of a `(while ...)` / `do repeat`
            // expression, so it is built here as that AST.
            // Cost: O(1) per iteration sunk (compiles to jumps; no runtime
            // op); O(1) per pulled element in value position (a gather).
            "nqp::while" | "nqp::until" | "nqp::repeat_while" | "nqp::repeat_until"
                if args.len() == 2 =>
            {
                let is_until = name.ends_with("until");
                let repeat = name.starts_with("nqp::repeat_");
                if sunk {
                    self.compile_nqp_sunk_loop(&args[0], &args[1], is_until, repeat);
                } else {
                    self.compile_nqp_value_loop(&args[0], &args[1], is_until, repeat);
                }
                true
            }
            _ => false,
        }
    }

    /// Run `f` as the compilation of a statement-root expression: a
    /// `Stmt::Expr` compiled as a statement, or as the tail of a block or
    /// routine (whose value rakudo takes from the loop forms as `Nil`, as it
    /// does for a statement). See `Compiler::expr_depth`.
    pub(super) fn with_stmt_root<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let saved = std::mem::replace(&mut self.expr_depth, 0);
        let r = f(self);
        self.expr_depth = saved;
        r
    }

    /// Compile an operand of an `nqp::` control form, in sink position when
    /// `sunk` (so a loop form there stays a plain jump loop).
    fn compile_nqp_operand(&mut self, e: &Expr, sunk: bool) {
        if sunk {
            self.with_stmt_root(|c| c.compile_expr(e));
        } else {
            self.compile_expr(e);
        }
    }

    /// A sunk `nqp::` loop: jumps, each body value popped, Nil at the exit.
    fn compile_nqp_sunk_loop(&mut self, cond: &Expr, body: &Expr, is_until: bool, repeat: bool) {
        let loop_start = self.code.ops.len();
        if repeat {
            // The post-test loops: the body runs once before the condition
            // is first evaluated.
            self.compile_nqp_operand(body, true);
            self.code.emit(OpCode::Pop);
            self.compile_expr(cond);
            if is_until {
                // `JumpIfFalse` pops the condition on both paths.
                self.code.emit(OpCode::JumpIfFalse(loop_start as i32));
            } else {
                let jump_end = self.code.emit(OpCode::JumpIfFalse(0));
                self.code.emit(OpCode::Jump(loop_start as i32));
                self.code.patch_jump(jump_end);
            }
        } else {
            self.compile_expr(cond);
            let jump_end = if is_until {
                self.code.emit(OpCode::JumpIfTrue(0))
            } else {
                self.code.emit(OpCode::JumpIfFalse(0))
            };
            self.compile_nqp_operand(body, true);
            self.code.emit(OpCode::Pop);
            self.code.emit(OpCode::Jump(loop_start as i32));
            self.code.patch_jump(jump_end);
        }
        let nil_idx = self.code.add_constant(Value::NIL);
        self.code.emit(OpCode::LoadConst(nil_idx));
    }

    /// An `nqp::` loop whose value is used: `gather { LOOP { take BODY } }`,
    /// the same lazy Seq a `(while ...)` expression compiles to.
    fn compile_nqp_value_loop(&mut self, cond: &Expr, body: &Expr, is_until: bool, repeat: bool) {
        let body = vec![Stmt::Take(body.clone(), false)];
        // `is_until` on the loop statements is only a marker: the parser has
        // already negated an `until` condition, so the negation is built here.
        let cond = if is_until {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(cond.clone()),
            }
        } else {
            cond.clone()
        };
        let inner = if repeat {
            Stmt::Loop {
                init: None,
                cond: Some(cond),
                step: None,
                body,
                repeat: true,
                label: None,
                is_until,
            }
        } else {
            Stmt::While {
                cond,
                body,
                label: None,
                is_statement_modifier: false,
                is_until,
            }
        };
        self.compile_expr(&Expr::Gather(vec![inner]));
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
