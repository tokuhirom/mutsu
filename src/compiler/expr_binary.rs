use super::*;

impl Compiler {
    pub(super) fn compile_expr_binary(
        &mut self,
        expr: &Expr,
        left: &Expr,
        op: &TokenKind,
        right: &Expr,
    ) {
        // Iteratively flatten left-recursive Binary chains for simple opcodes
        // to avoid stack overflow on deeply nested expressions like
        // "a" ~ "b" ~ "c" ~ ... (300+ operands).
        if let Some(opcode) = Self::binary_opcode(op) {
            // Check if this is a left-recursive chain of the same operator
            // that can be compiled with a simple opcode (no special handling).
            if let Expr::Binary { op: left_op, .. } = left
                && left_op == op
                && !matches!(op, TokenKind::Ident(name) if name == "does")
            {
                // Collect all operands from the left-recursive chain
                let mut operands = Vec::new();
                let mut current: &Expr = expr;
                while let Expr::Binary {
                    left: inner_left,
                    op: inner_op,
                    right: inner_right,
                } = current
                {
                    if inner_op != op {
                        break;
                    }
                    operands.push(inner_right.as_ref());
                    current = inner_left.as_ref();
                }
                // For junction operators (|, &, ^), emit all operands then a
                // single multi-operand opcode so user-defined overrides with
                // slurpy params get called once (list-associative).
                if let Some(multi_op) = Self::junction_multi_opcode(op) {
                    let count = operands.len() + 1; // +1 for leftmost
                    self.compile_expr(current);
                    for operand in operands.into_iter().rev() {
                        self.compile_expr(operand);
                    }
                    self.code.emit(multi_op(count as u32));
                    return;
                }
                // `current` is the leftmost non-matching operand
                self.compile_expr(current);
                // Operands were collected right-to-left, reverse to emit
                // left-to-right
                for operand in operands.into_iter().rev() {
                    self.compile_expr(operand);
                    self.code.emit(opcode.clone());
                }
                return;
            }
        }
        if matches!(op, TokenKind::Ident(name) if name == "xx")
            && let Expr::Literal(Value::Int(n)) = right
            && let Expr::Call { name, .. } = left
            && name == "start"
        {
            let count = (*n).max(0) as usize;
            for _ in 0..count {
                self.compile_expr(left);
            }
            self.code.emit(OpCode::MakeArray(count as u32));
            return;
        }
        if matches!(op, TokenKind::Ident(name) if name == "xx") {
            // Raku's list repeat reevaluates call-like lhs expressions on each
            // repetition (e.g. rand/pick). Keep literal/list lhs values as-is.
            if Self::xx_lhs_needs_reeval(left) {
                let reevaluated_lhs = if let Expr::MethodCall {
                    target,
                    name,
                    args,
                    modifier,
                    quoted,
                } = left
                {
                    if matches!(target.as_ref(), Expr::Var(v) if v == "_") {
                        let temp_name = format!(
                            "__mutsu_xx_target_{}",
                            STATE_COUNTER.fetch_add(1, Ordering::Relaxed)
                        );
                        self.compile_expr(target);
                        self.code.emit(OpCode::Dup);
                        self.emit_set_named_var(&temp_name);
                        Expr::MethodCall {
                            target: Box::new(Expr::Var(temp_name)),
                            name: *name,
                            args: args.clone(),
                            modifier: *modifier,
                            quoted: *quoted,
                        }
                    } else {
                        left.clone()
                    }
                } else {
                    left.clone()
                };
                let thunk = Expr::AnonSubParams {
                    params: Vec::new(),
                    param_defs: Vec::new(),
                    return_type: None,
                    body: vec![Stmt::Expr(reevaluated_lhs)],
                    is_rw: false,
                    is_whatever_code: false,
                };
                self.compile_expr(&thunk);
            } else {
                self.compile_expr(left);
            }
            self.compile_expr(right);
            self.code.emit(OpCode::ListRepeat);
            return;
        }
        // Detect `funcname |capture` pattern (listop call with capture slip)
        // Exclude names starting with uppercase -- they are likely type
        // names, and `TypeName|OtherType` should compile as a junction.
        if *op == TokenKind::Pipe
            && matches!(right, Expr::BareWord(_))
            && let Expr::BareWord(name) = left
            && !name.starts_with(char::is_uppercase)
        {
            // Compile the slip arg (the capture variable)
            self.compile_expr(right);
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            self.code.emit(OpCode::CallFuncSlip {
                name_idx,
                regular_arity: 0,
                arg_sources_idx: None,
                slip_pos: None,
            });
            return;
        }
        // Short-circuit operators
        match op {
            TokenKind::AndAnd => {
                self.compile_expr(left);
                self.code.emit(OpCode::Dup);
                let jump_end = self.code.emit(OpCode::JumpIfFalse(0));
                self.code.emit(OpCode::Pop);
                self.compile_expr(right);
                self.code.patch_jump(jump_end);
                return;
            }
            TokenKind::OrOr | TokenKind::OrWord => {
                self.compile_expr(left);
                self.code.emit(OpCode::Dup);
                let jump_cleanup = self.code.emit(OpCode::JumpIfTrue(0));
                self.code.emit(OpCode::Pop);
                self.code.emit(OpCode::Pop);
                self.compile_expr(right);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_cleanup);
                self.code.emit(OpCode::Pop);
                self.code.patch_jump(jump_end);
                return;
            }
            TokenKind::XorXor => {
                self.compile_xor_chain(left, right);
                return;
            }
            TokenKind::SlashSlash => {
                self.compile_expr(left);
                self.code.emit(OpCode::Dup);
                let jump_cleanup = self.code.emit(OpCode::JumpIfNotNil(0));
                self.code.emit(OpCode::Pop);
                self.code.emit(OpCode::Pop);
                self.compile_expr(right);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_cleanup);
                self.code.emit(OpCode::Pop);
                self.code.patch_jump(jump_end);
                return;
            }
            TokenKind::OrElse => {
                self.compile_expr(left);
                self.code.emit(OpCode::Dup);
                self.code.emit(OpCode::CallDefined);
                let jump_undef = self.code.emit(OpCode::JumpIfFalse(0));
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_undef);
                self.code.emit(OpCode::Dup);
                self.code.emit(OpCode::SetTopic);
                self.compile_expr(right);
                let finalize_name_idx = self
                    .code
                    .add_constant(Value::str_from("__mutsu_andthen_finalize"));
                self.code.emit(OpCode::CallFunc {
                    name_idx: finalize_name_idx,
                    arity: 2,
                    arg_sources_idx: None,
                });
                self.code.patch_jump(jump_end);
                return;
            }
            TokenKind::AndThen => {
                self.compile_expr(left);
                self.code.emit(OpCode::Dup);
                self.code.emit(OpCode::CallDefined);
                let jump_undef = self.code.emit(OpCode::JumpIfFalse(0));
                self.code.emit(OpCode::Dup);
                self.code.emit(OpCode::SetTopic);
                self.compile_expr(right);
                let finalize_name_idx = self
                    .code
                    .add_constant(Value::str_from("__mutsu_andthen_finalize"));
                self.code.emit(OpCode::CallFunc {
                    name_idx: finalize_name_idx,
                    arity: 2,
                    arg_sources_idx: None,
                });
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_undef);
                self.code.emit(OpCode::Pop);
                let empty_idx = self.code.add_constant(Value::slip(vec![]));
                self.code.emit(OpCode::LoadConst(empty_idx));
                self.code.patch_jump(jump_end);
                return;
            }
            TokenKind::NotAndThen => {
                self.compile_expr(left);
                self.code.emit(OpCode::Dup);
                self.code.emit(OpCode::CallDefined);
                let jump_undef = self.code.emit(OpCode::JumpIfFalse(0));
                self.code.emit(OpCode::Pop);
                self.code.emit(OpCode::LoadNil);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_undef);
                self.code.emit(OpCode::Pop);
                self.compile_expr(right);
                self.code.patch_jump(jump_end);
                return;
            }
            TokenKind::SmartMatch | TokenKind::BangTilde => {
                let lhs_var = match left {
                    Expr::Var(name) => Some(name.clone()),
                    _ => None,
                };
                let rhs_is_match_regex = matches!(right, Expr::MatchRegex(_));
                self.compile_expr(left);
                let sm_idx = self.code.emit(OpCode::SmartMatchExpr {
                    rhs_end: 0,
                    negate: matches!(op, TokenKind::BangTilde),
                    lhs_var,
                    rhs_is_match_regex,
                });
                // When RHS is m/regex/, unwrap to the regex value since
                // SmartMatchExpr already handles the matching against LHS
                match right {
                    Expr::MatchRegex(v) => {
                        let idx = self.code.add_constant(v.clone());
                        self.code.emit(OpCode::LoadConst(idx));
                    }
                    _ => self.compile_expr(right),
                }
                self.code.patch_smart_match_rhs_end(sm_idx);
                return;
            }
            // Sequential metaoperators: S&, S|, S^
            TokenKind::Ident(op) if op == "S&" => {
                self.compile_expr(left);
                self.compile_expr(right);
                self.code.emit(OpCode::JunctionAll);
                return;
            }
            TokenKind::Ident(op) if op == "S|" => {
                self.compile_expr(left);
                self.compile_expr(right);
                self.code.emit(OpCode::JunctionAny);
                return;
            }
            TokenKind::Ident(op) if op == "S^" => {
                self.compile_expr(left);
                self.compile_expr(right);
                self.code.emit(OpCode::JunctionOne);
                return;
            }
            _ => {}
        }

        // Special-case: =:= (container identity)
        if matches!(op, TokenKind::Ident(name) if name == "=:=") {
            if let (Expr::Var(left_name), Expr::Var(right_name)) = (left, right) {
                self.compile_expr(left);
                self.compile_expr(right);
                let left_idx = self.code.add_constant(Value::str(left_name.clone()));
                let right_idx = self.code.add_constant(Value::str(right_name.clone()));
                self.code.emit(OpCode::ContainerEqNamed {
                    left_name_idx: left_idx,
                    right_name_idx: right_idx,
                });
                return;
            }
            let left_fresh = Self::expr_is_fresh_container(left);
            let right_fresh = Self::expr_is_fresh_container(right);
            let flags =
                (if left_fresh { 1u8 } else { 0u8 }) | (if right_fresh { 2u8 } else { 0u8 });
            self.compile_expr(left);
            self.compile_expr(right);
            self.code.emit(OpCode::ContainerEq(flags));
            return;
        }

        if let Some(opcode) = Self::binary_opcode(op) {
            if matches!(op, TokenKind::Ident(name) if name == "does") {
                let var_name = match left {
                    Expr::Var(name) => Some(name.clone()),
                    Expr::BareWord(name) => Some(name.clone()),
                    _ => None,
                };
                if let Some(name) = var_name {
                    self.compile_expr(left);
                    // Set does-context flag so role calls with args return Pairs
                    // instead of throwing X::Coerce::Impossible.
                    self.code.emit(OpCode::SetDoesContext(true));
                    self.compile_expr(right);
                    self.code.emit(OpCode::SetDoesContext(false));
                    let name_idx = self.code.add_constant(Value::str(name));
                    self.code.emit(OpCode::DoesVar(name_idx));
                    return;
                }
            }
            self.compile_expr(left);
            self.compile_expr(right);
            self.code.emit(opcode);
        } else if let TokenKind::Ident(name) = op
            && matches!(name.as_str(), "~&" | "~|" | "~^")
        {
            self.compile_expr(left);
            self.compile_expr(right);
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            self.code.emit(OpCode::InfixFunc {
                name_idx,
                right_arity: 1,
                modifier_idx: None,
            });
        } else {
            self.compile_expr(left);
            self.compile_expr(right);
            let op_name = super::helpers::token_kind_to_op_name(op);
            let name_idx = self.code.add_constant(Value::str(op_name));
            self.code.emit(OpCode::InfixFunc {
                name_idx,
                right_arity: 1,
                modifier_idx: None,
            });
        }
    }
}
