use super::*;

impl Compiler {
    /// Whether a smartmatch RHS is a plain `Value::Regex` literal (a bare `/…/`
    /// or adverb-less `m/…/`). This is the *compile-time* half of the Slice 6.3
    /// step 2 gate that lets the smartmatch op skip its conservative post-match
    /// `env_dirty` re-sync: a plain regex match writes only `$/`/captures, never
    /// an arbitrary caller variable — UNLESS the pattern runs an embedded `{ }`
    /// code block (which the *runtime* half catches precisely via
    /// `Interpreter::pending_local_updates`). Excludes `RegexWithAdverbs`
    /// (`:pos`/`:g`/… carry match state across calls), named/Sub regexes,
    /// substitution, transliteration, and non-regex smartmatch — all of which
    /// keep the conservative mark.
    pub(super) fn rhs_is_plain_regex_literal(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Literal(Value::Regex(_)) | Expr::MatchRegex(Value::Regex(_))
        )
    }

    pub(super) fn compile_expr_binary(
        &mut self,
        expr: &Expr,
        left: &Expr,
        op: &TokenKind,
        right: &Expr,
    ) {
        // Special handling for `but` with a literal tuple RHS:
        // `True but (1, "x")` compiles as multiple ButMixinTupleElem operations
        // (one per element), generating per-element type methods with conflict
        // checking.
        if matches!(op, TokenKind::Ident(name) if name == "but")
            && let Expr::ArrayLiteral(elems) = right
            && elems.len() > 1
        {
            self.compile_expr(left);
            for elem in elems {
                self.compile_expr(elem);
                self.code.emit(OpCode::ButMixinTupleElem);
            }
            return;
        }

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
                self.code.emit(OpCode::SaveTopic);
                self.code.emit(OpCode::Dup);
                self.code.emit(OpCode::CallDefined);
                let jump_undef = self.code.emit(OpCode::JumpIfFalse(0));
                self.code.emit(OpCode::RestoreTopic);
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
                self.code.emit(OpCode::RestoreTopic);
                self.code.patch_jump(jump_end);
                return;
            }
            TokenKind::AndThen => {
                self.compile_expr(left);
                self.code.emit(OpCode::SaveTopic);
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
                self.code.emit(OpCode::RestoreTopic);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_undef);
                self.code.emit(OpCode::Pop);
                let empty_idx = self.code.add_constant(Value::slip(vec![]));
                self.code.emit(OpCode::LoadConst(empty_idx));
                self.code.emit(OpCode::RestoreTopic);
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
                // Only a *destructive* `s///` / `tr///` against a literal LHS is an
                // X::Assignment::RO. Non-destructive `S///` / `TR///` return a copy
                // and never write back, so `1 ~~ TR/\#//` must not throw.
                let rhs_is_destructive = matches!(right, Expr::Subst { .. })
                    || matches!(
                        right,
                        Expr::Transliterate {
                            non_destructive: false,
                            ..
                        }
                    );
                let lhs_is_literal = rhs_is_destructive && matches!(left, Expr::Literal(_));
                let rhs_pure_regex = Self::rhs_is_plain_regex_literal(right);
                self.compile_expr(left);
                let sm_idx = self.code.emit(OpCode::SmartMatchExpr {
                    rhs_end: 0,
                    negate: matches!(op, TokenKind::BangTilde),
                    lhs_var,
                    rhs_is_match_regex,
                    lhs_is_literal,
                    rhs_pure_regex,
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
            // Resolve variable names from both sides, including through
            // list-indexing patterns like ($foo, "x")[0] which preserves
            // the container of $foo.
            let left_name = Self::resolve_container_var_name(left);
            let right_name = Self::resolve_container_var_name(right);
            if let (Some(ln), Some(rn)) = (&left_name, &right_name) {
                self.compile_expr(left);
                self.compile_expr(right);
                let left_idx = self.code.add_constant(Value::str(ln.clone()));
                let right_idx = self.code.add_constant(Value::str(rn.clone()));
                self.code.emit(OpCode::ContainerEqNamed {
                    left_name_idx: left_idx,
                    right_name_idx: right_idx,
                });
                return;
            }
            // When both sides are index expressions, use ContainerEqIndexed
            // to check binding metadata on array/hash elements.
            if let (Some(left_encoded), Some(right_encoded)) = (
                Self::encode_index_source(left),
                Self::encode_index_source(right),
            ) {
                self.compile_expr(left);
                self.compile_expr(right);
                let left_idx = self.code.add_constant(Value::str(left_encoded));
                let right_idx = self.code.add_constant(Value::str(right_encoded));
                self.code.emit(OpCode::ContainerEqIndexed {
                    left_name_idx: left_idx,
                    right_name_idx: right_idx,
                });
                return;
            }
            // Mixed case: one side is a named variable (with a local slot holding
            // a DeferredHashAccess from `:=` binding), the other is an Index expression.
            // Use GetLocalRaw + IndexAutovivifyLazy + ContainerEqRaw.
            if let Some(slot) = self.container_eq_var_slot(left)
                && matches!(right, Expr::Index { .. })
            {
                self.code.emit(OpCode::GetLocalRaw(slot));
                self.scalar_bind_autovivify = true;
                self.compile_expr(right);
                self.scalar_bind_autovivify = false;
                self.code.emit(OpCode::ContainerEqRaw);
                return;
            }
            if let Some(slot) = self.container_eq_var_slot(right)
                && matches!(left, Expr::Index { .. })
            {
                self.scalar_bind_autovivify = true;
                self.compile_expr(left);
                self.scalar_bind_autovivify = false;
                self.code.emit(OpCode::GetLocalRaw(slot));
                self.code.emit(OpCode::ContainerEqRaw);
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
                    Expr::HashVar(name) => Some(format!("%{}", name)),
                    Expr::ArrayVar(name) => Some(format!("@{}", name)),
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
            // `but`/`does` with a role-applied RHS (`X but R(v)`, `99 does R(v)`)
            // must let the role call return a role-application Pair instead of
            // coercing. The `does`+variable path above already does this; cover
            // the remaining cases (`but`, and `does` on a non-variable LHS).
            let does_context =
                matches!(op, TokenKind::Ident(name) if name == "but" || name == "does");
            if does_context {
                self.code.emit(OpCode::SetDoesContext(true));
            }
            self.compile_expr(right);
            if does_context {
                self.code.emit(OpCode::SetDoesContext(false));
            }
            // For `!=` between native int typed variables, emit a native-aware
            // opcode that replicates Rakudo's MoarVM behaviour: cross-signed
            // comparisons where the signed operand is negative return False.
            if matches!(opcode, OpCode::NumNe) {
                if let Some(native_flags) = self.native_ne_flags(left, right) {
                    self.code.emit(OpCode::NumNeNative(native_flags));
                } else {
                    self.code.emit(opcode);
                }
            } else {
                self.code.emit(opcode);
            }
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

    /// Check if both operands of `!=` are native-int-typed variables with
    /// different signedness.  Returns `Some(flags)` when at least one is
    /// unsigned and one is signed (bit 0 = left unsigned, bit 1 = right
    /// unsigned).  Returns `None` when native-aware comparison is not needed.
    fn native_ne_flags(&self, left: &Expr, right: &Expr) -> Option<u8> {
        let left_type = self.expr_native_int_type(left)?;
        let right_type = self.expr_native_int_type(right)?;
        let left_unsigned = left_type.starts_with('u') || left_type == "byte";
        let right_unsigned = right_type.starts_with('u') || right_type == "byte";
        // Only emit native-aware opcode when signedness differs
        if left_unsigned == right_unsigned {
            return None;
        }
        let mut flags = 0u8;
        if left_unsigned {
            flags |= 1;
        }
        if right_unsigned {
            flags |= 2;
        }
        Some(flags)
    }

    /// If `expr` is a variable reference with a native integer type
    /// constraint, return the type name.
    fn expr_native_int_type(&self, expr: &Expr) -> Option<String> {
        let var_name = match expr {
            Expr::Var(name) => name,
            _ => return None,
        };
        let constraint = self.local_types.get(var_name)?;
        let base = constraint
            .strip_suffix(":D")
            .or_else(|| constraint.strip_suffix(":U"))
            .unwrap_or(constraint);
        if crate::runtime::native_types::is_native_int_type(base) {
            Some(base.to_string())
        } else {
            None
        }
    }
}
