use super::*;

/// `nqp::const::*` names that compile to integer literals. The values follow
/// MoarVM: the low 2 bits of a binary read/write flag are the endianness
/// (0 native, 1 little, 2 big — matching Raku's `Endian` enum, with which
/// these are `nqp::bitor_i`ed), and the size flag occupies the bits above
/// (`1 << (flags >> 2)` bytes).
pub(crate) fn nqp_const_value(name: &str) -> Option<i64> {
    let konst = name.strip_prefix("nqp::const::")?;
    Some(match konst {
        "BINARY_ENDIAN_NATIVE" => 0,
        "BINARY_ENDIAN_LITTLE" => 1,
        "BINARY_ENDIAN_BIG" => 2,
        "BINARY_SIZE_8_BIT" => 0,
        "BINARY_SIZE_16_BIT" => 4,
        "BINARY_SIZE_32_BIT" => 8,
        "BINARY_SIZE_64_BIT" => 12,
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
            // nqp::while(c, body) / nqp::until(c, body) — re-evaluate the
            // condition each iteration; yields Nil.
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
            _ => false,
        }
    }
}
