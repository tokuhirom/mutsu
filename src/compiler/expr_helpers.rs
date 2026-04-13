use super::*;

impl Compiler {
    fn topic_subst_pattern_from_index_with_groups(expr: &Expr, top_level: bool) -> Option<String> {
        match expr {
            Expr::CaptureLiteral(items) => {
                if top_level
                    && items.len() == 1
                    && matches!(items.first(), Some(Expr::CaptureLiteral(_)))
                {
                    return Self::topic_subst_pattern_from_index_with_groups(&items[0], true);
                }
                let mut out = String::new();
                for item in items {
                    out.push_str(&Self::topic_subst_pattern_from_index_with_groups(
                        item, false,
                    )?);
                }
                Some(format!("({out})"))
            }
            Expr::BareWord(name) if name.len() == 1 => Some(format!("\\{name}")),
            Expr::BareWord(name) => Some(name.clone()),
            Expr::Literal(Value::Str(s)) => Some(s.as_ref().clone()),
            _ => None,
        }
    }

    pub(super) fn topic_subst_pattern_from_index(expr: &Expr) -> Option<String> {
        Self::topic_subst_pattern_from_index_with_groups(expr, true)
    }

    pub(super) fn atomic_target_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::CodeVar(name) => Some(format!("&{}", name)),
            Expr::Index { target, index, .. }
                if matches!(target.as_ref(), Expr::PseudoStash(_))
                    && matches!(index.as_ref(), Expr::Literal(Value::Str(_))) =>
            {
                let Expr::Literal(Value::Str(raw)) = index.as_ref() else {
                    return None;
                };
                let mut name = raw.as_ref().clone();
                if let Some(first) = name.chars().next()
                    && matches!(first, '$' | '@' | '%' | '&')
                {
                    name = name.chars().skip(1).collect();
                }
                if name.is_empty() { None } else { Some(name) }
            }
            _ => None,
        }
    }

    pub(super) fn binary_opcode(op: &TokenKind) -> Option<OpCode> {
        match op {
            TokenKind::Plus => Some(OpCode::Add),
            TokenKind::Minus => Some(OpCode::Sub),
            TokenKind::Star => Some(OpCode::Mul),
            TokenKind::Slash => Some(OpCode::Div),
            TokenKind::Percent => Some(OpCode::Mod),
            TokenKind::StarStar => Some(OpCode::Pow),
            TokenKind::Tilde => Some(OpCode::Concat),
            TokenKind::EqEq => Some(OpCode::NumEq),
            TokenKind::BangEq => Some(OpCode::NumNe),
            TokenKind::Lt => Some(OpCode::NumLt),
            TokenKind::Lte => Some(OpCode::NumLe),
            TokenKind::Gt => Some(OpCode::NumGt),
            TokenKind::Gte => Some(OpCode::NumGe),
            TokenKind::Ident(name) if name == "eq" => Some(OpCode::StrEq),
            TokenKind::Ident(name) if name == "ne" => Some(OpCode::StrNe),
            TokenKind::Ident(name) if name == "lt" => Some(OpCode::StrLt),
            TokenKind::Ident(name) if name == "gt" => Some(OpCode::StrGt),
            TokenKind::Ident(name) if name == "before" => Some(OpCode::Before),
            TokenKind::Ident(name) if name == "after" => Some(OpCode::After),
            TokenKind::Ident(name) if name == "le" => Some(OpCode::StrLe),
            TokenKind::Ident(name) if name == "ge" => Some(OpCode::StrGe),
            TokenKind::DotDot => Some(OpCode::MakeRange),
            TokenKind::DotDotCaret => Some(OpCode::MakeRangeExcl),
            TokenKind::CaretDotDot => Some(OpCode::MakeRangeExclStart),
            TokenKind::CaretDotDotCaret => Some(OpCode::MakeRangeExclBoth),
            // Three-way comparison
            TokenKind::LtEqGt => Some(OpCode::Spaceship),
            TokenKind::Ident(name) if name == "cmp" => Some(OpCode::Cmp),
            TokenKind::Ident(name) if name == "coll" => Some(OpCode::Coll),
            TokenKind::Ident(name) if name == "leg" => Some(OpCode::Leg),
            // Identity/value equality
            TokenKind::EqEqEq => Some(OpCode::StrictEq),
            TokenKind::BangEqEqEq => Some(OpCode::StrictNe),
            TokenKind::Ident(name) if name == "eqv" => Some(OpCode::Eqv),
            TokenKind::Ident(name) if name == "=~=" => Some(OpCode::ApproxEq),
            // =:= is handled as a special case in compile_expr (needs containerisation flags)
            TokenKind::Ident(name) if name == "=:=" => None,
            // Divisibility
            TokenKind::PercentPercent => Some(OpCode::DivisibleBy),
            TokenKind::BangPercentPercent => Some(OpCode::NotDivisibleBy),
            // Keyword math
            TokenKind::Ident(name) if name == "div" => Some(OpCode::IntDiv),
            TokenKind::Ident(name) if name == "mod" => Some(OpCode::IntMod),
            TokenKind::Ident(name) if name == "gcd" => Some(OpCode::Gcd),
            TokenKind::Ident(name) if name == "lcm" => Some(OpCode::Lcm),
            TokenKind::Ident(name) if name == "min" => Some(OpCode::InfixMin),
            TokenKind::Ident(name) if name == "max" => Some(OpCode::InfixMax),
            // Repetition
            TokenKind::Ident(name) if name == "x" => Some(OpCode::StringRepeat),
            TokenKind::Ident(name) if name == "but" => Some(OpCode::ButMixin),
            TokenKind::Ident(name) if name == "isa" => Some(OpCode::Isa),
            TokenKind::Ident(name) if name == "does" => Some(OpCode::Does),
            TokenKind::Ident(name) if name == "xx" => Some(OpCode::ListRepeat),
            TokenKind::Ident(name) if name == "o" => Some(OpCode::FunctionCompose),
            // Pair
            TokenKind::FatArrow => Some(OpCode::MakePair),
            // Bitwise
            TokenKind::BitAnd => Some(OpCode::BitAnd),
            TokenKind::BitOr => Some(OpCode::BitOr),
            TokenKind::Ident(name) if name == "?|" => Some(OpCode::BoolBitOr),
            TokenKind::Ident(name) if name == "?&" => Some(OpCode::BoolBitAnd),
            TokenKind::Ident(name) if name == "?^" => Some(OpCode::BoolBitXor),
            TokenKind::BoolBitOr => Some(OpCode::BoolBitOr),
            TokenKind::BoolBitAnd => Some(OpCode::BoolBitAnd),
            TokenKind::BoolBitXor => Some(OpCode::BoolBitXor),
            TokenKind::BitXor => Some(OpCode::BitXor),
            TokenKind::BitShiftLeft => Some(OpCode::BitShiftLeft),
            TokenKind::BitShiftRight => Some(OpCode::BitShiftRight),
            TokenKind::StrBitAnd => Some(OpCode::StrBitAnd),
            TokenKind::StrBitOr => Some(OpCode::StrBitOr),
            TokenKind::StrBitXor => Some(OpCode::StrBitXor),
            TokenKind::StrShiftLeft => Some(OpCode::StrShiftLeft),
            TokenKind::StrShiftRight => Some(OpCode::StrShiftRight),
            // Set operations
            TokenKind::SetElem => Some(OpCode::SetElem),
            TokenKind::SetCont => Some(OpCode::SetCont),
            TokenKind::SetUnion => Some(OpCode::SetUnion),
            TokenKind::SetAddition => Some(OpCode::SetAddition),
            TokenKind::SetIntersect => Some(OpCode::SetIntersect),
            TokenKind::SetMultiply => Some(OpCode::SetMultiply),
            TokenKind::SetDiff => Some(OpCode::SetDiff),
            TokenKind::SetSymDiff => Some(OpCode::SetSymDiff),
            TokenKind::SetSubset => Some(OpCode::SetSubset),
            TokenKind::SetSuperset => Some(OpCode::SetSuperset),
            TokenKind::SetStrictSubset => Some(OpCode::SetStrictSubset),
            TokenKind::SetStrictSuperset => Some(OpCode::SetStrictSuperset),
            TokenKind::Pipe => Some(OpCode::JunctionAny),
            TokenKind::Ampersand => Some(OpCode::JunctionAll),
            TokenKind::Caret => Some(OpCode::JunctionOne),
            TokenKind::DotDotDot => Some(OpCode::Sequence { exclude_end: false }),
            TokenKind::DotDotDotCaret => Some(OpCode::Sequence { exclude_end: true }),
            _ => None,
        }
    }

    /// Returns a constructor for multi-operand junction opcodes when the
    /// token is a junction operator (|, &, ^). Used to emit a single opcode
    /// for flattened chains so that user-defined overrides are called once
    /// with all operands (list-associative behavior).
    pub(super) fn junction_multi_opcode(op: &TokenKind) -> Option<fn(u32) -> OpCode> {
        match op {
            TokenKind::Pipe => Some(OpCode::JunctionAnyN),
            TokenKind::Ampersand => Some(OpCode::JunctionAllN),
            TokenKind::Caret => Some(OpCode::JunctionOneN),
            _ => None,
        }
    }

    /// True when the expression is a `$`-sigiled scalar variable access.
    /// `Expr::Var` represents `$` variables (the sigil is stripped).
    /// Sigilless variables appear as `Expr::BareWord` instead.
    pub(super) fn expr_is_scalar_var(expr: &Expr) -> bool {
        matches!(expr, Expr::Var(_))
    }

    /// Returns `true` when `expr` provably produces a fresh container
    /// -- i.e. the result is a copy that cannot share identity with any
    /// existing container.
    pub(super) fn expr_is_fresh_container(expr: &Expr) -> bool {
        match expr {
            // Indexing into an array/hash element produces a value that
            // was copied into the array, hence a distinct container.
            Expr::Index { .. } => true,
            _ => false,
        }
    }

    pub(super) fn xx_lhs_needs_reeval(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Call { name, .. } if name == "rand" || name == "pick" || name == "roll" || name == "start" || name == "getc" || name == "get"
        ) || matches!(
            expr,
            Expr::MethodCall { name, target, .. }
                if name == "rand"
                    || name == "pick"
                    || name == "roll"
                    || name == "take"
                    || name == "readchars"
                    || name == "receive"
                    || name == "getc"
                    || name == "get"
                    || (name == "new" && matches!(target.as_ref(), Expr::BareWord(n) if n == "Promise"))
                    || Self::xx_lhs_needs_reeval(target)
        ) || matches!(
            expr,
            Expr::DynamicMethodCall { target, .. }
                | Expr::HyperMethodCall { target, .. }
                | Expr::HyperMethodCallDynamic { target, .. }
                | Expr::CallOn { target, .. } if Self::xx_lhs_needs_reeval(target)
        ) || matches!(
            expr,
            Expr::Block(_)
                | Expr::DoBlock { .. }
                | Expr::AnonSub { .. }
                | Expr::AnonSubParams { .. }
                | Expr::Lambda { .. }
        )
    }

    pub(super) fn flatten_xor_terms<'a>(expr: &'a Expr, out: &mut Vec<&'a Expr>) {
        if let Expr::Binary { left, op, right } = expr
            && *op == TokenKind::XorXor
        {
            Self::flatten_xor_terms(left, out);
            Self::flatten_xor_terms(right, out);
            return;
        }
        out.push(expr);
    }

    pub(super) fn compile_xor_chain(&mut self, left: &Expr, right: &Expr) {
        let mut terms = Vec::new();
        Self::flatten_xor_terms(left, &mut terms);
        Self::flatten_xor_terms(right, &mut terms);
        if terms.len() == 2 {
            self.compile_expr(terms[0]);
            self.compile_expr(terms[1]);
            self.code.emit(OpCode::XorXor);
            return;
        }

        self.tmp_counter += 1;
        let acc_slot = self.alloc_local(&format!("__mutsu_xor_acc_{}", self.tmp_counter));
        self.tmp_counter += 1;
        let seen_slot = self.alloc_local(&format!("__mutsu_xor_seen_{}", self.tmp_counter));

        self.compile_expr(terms[0]);
        self.code.emit(OpCode::Dup);
        self.code.emit(OpCode::SetLocal(acc_slot));
        let first_truthy = self.code.emit(OpCode::JumpIfTrue(0));
        self.code.emit(OpCode::LoadFalse);
        let first_done = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(first_truthy);
        self.code.emit(OpCode::LoadTrue);
        self.code.patch_jump(first_done);
        self.code.emit(OpCode::SetLocal(seen_slot));
        self.code.emit(OpCode::Pop);

        let mut early_end_jumps = Vec::new();
        for term in terms.iter().skip(1) {
            self.compile_expr(term);

            self.code.emit(OpCode::GetLocal(seen_slot));
            let seen_false = self.code.emit(OpCode::JumpIfFalse(0));

            self.code.emit(OpCode::Dup);
            let second_truthy = self.code.emit(OpCode::JumpIfTrue(0));
            self.code.emit(OpCode::Pop);
            self.code.emit(OpCode::Pop);
            let next_after_seen_true = self.code.emit(OpCode::Jump(0));

            self.code.patch_jump(second_truthy);
            self.code.emit(OpCode::Pop);
            self.code.emit(OpCode::Pop);
            self.code.emit(OpCode::LoadNil);
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetLocal(acc_slot));
            self.code.emit(OpCode::LoadTrue);
            self.code.emit(OpCode::SetLocal(seen_slot));
            early_end_jumps.push(self.code.emit(OpCode::Jump(0)));

            self.code.patch_jump(seen_false);

            self.code.emit(OpCode::Dup);
            let became_truthy = self.code.emit(OpCode::JumpIfTrue(0));
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetLocal(acc_slot));
            self.code.emit(OpCode::Pop);
            let next_after_seen_false = self.code.emit(OpCode::Jump(0));

            self.code.patch_jump(became_truthy);
            self.code.emit(OpCode::SetLocal(acc_slot));
            self.code.emit(OpCode::LoadTrue);
            self.code.emit(OpCode::SetLocal(seen_slot));
            self.code.emit(OpCode::Pop);

            self.code.patch_jump(next_after_seen_true);
            self.code.patch_jump(next_after_seen_false);
        }

        for jump in early_end_jumps {
            self.code.patch_jump(jump);
        }
        self.code.emit(OpCode::GetLocal(acc_slot));
    }

    /// Compile Expr::Var -- variable access with all special cases.
    pub(super) fn compile_expr_var(&mut self, name: &str) {
        // $.attr (public twigil) — compile as self.attr() method call.
        // In Raku, $.attr is syntactic sugar for self.attr(), not a variable lookup.
        if let Some(attr_name) = name.strip_prefix('.')
            && !attr_name.is_empty()
        {
            // Load self
            let self_name = self.qualify_variable_name("self");
            let self_idx = self.code.add_constant(Value::str(self_name));
            self.code.emit(OpCode::GetGlobal(self_idx));
            // Call method
            let method_idx = self.code.add_constant(Value::str(attr_name.to_string()));
            self.code.emit(OpCode::CallMethod {
                name_idx: method_idx,
                arity: 0,
                modifier_idx: None,
                quoted: false,
                arg_sources_idx: None,
            });
            return;
        }
        // $!attr (private twigil) — direct attribute access.
        // In Raku, $!attr reads the attribute directly from the instance,
        // bypassing any accessor method. The runtime sets `!attr_name` in the
        // env when a method body executes on an instance.
        if let Some(attr_name) = name.strip_prefix('!')
            && !attr_name.is_empty()
        {
            // Look up `!attr_name` in the environment
            let name_idx = self
                .code
                .add_constant(Value::str(self.qualify_variable_name(name)));
            self.code.emit(OpCode::GetGlobal(name_idx));
            return;
        }
        // $CALLER:: / $CALLER::CALLER:: variable access
        if let Some((bare_name, depth)) = Self::parse_caller_prefix(name) {
            let name_idx = self.code.add_constant(Value::str(bare_name));
            self.code.emit(OpCode::GetCallerVar {
                name_idx,
                depth: depth as u32,
            });
            return;
        }
        // $DYNAMIC:: variable access
        if let Some(bare_name) = name.strip_prefix("DYNAMIC::") {
            let name_idx = self.code.add_constant(Value::str(bare_name.to_string()));
            self.code.emit(OpCode::GetDynamicVar(name_idx));
            return;
        }
        // X::Dynamic::Package: dynamic variables cannot have package-like names
        if Self::is_dynamic_package_var(name) {
            self.emit_dynamic_package_error(name);
            return;
        }
        // Track dynamic variable access for postdeclaration check
        if name.starts_with('*') && !self.local_map.contains_key(name) {
            self.accessed_dynamic_vars.insert(name.to_string());
        }
        // Slang variables ($~MAIN, $~Quote, $~Regex, $~P5Regex)
        if let Some(slang_name) = name.strip_prefix('~') {
            let idx = self.code.add_constant(Value::str(slang_name.to_string()));
            self.code.emit(OpCode::LoadConst(idx));
        }
        // Compile-time package/module variables
        else if name == "?PACKAGE" || name == "?MODULE" {
            let pkg = self
                .enclosing_package
                .as_deref()
                .unwrap_or(&self.current_package);
            let idx = self
                .code
                .add_constant(Value::Package(crate::symbol::Symbol::intern(pkg)));
            self.code.emit(OpCode::LoadConst(idx));
        } else if name == "?CLASS" || name == "?ROLE" {
            let name_idx = self.code.add_constant(Value::str(name.to_string()));
            self.code.emit(OpCode::GetGlobal(name_idx));
        } else if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::GetLocal(slot));
        } else {
            let name_idx = self
                .code
                .add_constant(Value::str(self.qualify_variable_name(name)));
            self.code.emit(OpCode::GetGlobal(name_idx));
        }
    }

    /// Compile a regex value as `$_ ~~ /regex/`, so it matches against $_
    /// and sets $/ with the match result.
    pub(super) fn compile_match_regex(&mut self, v: &Value) {
        let lhs_var = Some("_".to_string());
        // Load $_ as the LHS
        let name_idx = self
            .code
            .add_constant(Value::str(self.qualify_variable_name("_")));
        self.code.emit(OpCode::GetGlobal(name_idx));
        // SmartMatchExpr will set $_ to LHS, run RHS, then smartmatch
        let sm_idx = self.code.emit(OpCode::SmartMatchExpr {
            rhs_end: 0,
            negate: false,
            lhs_var,
            // m// standalone (not in ~~ context) behaves like m// for result purposes
            rhs_is_match_regex: true,
        });
        // RHS: load the regex constant
        let idx = self.code.add_constant(v.clone());
        self.code.emit(OpCode::LoadConst(idx));
        self.code.patch_smart_match_rhs_end(sm_idx);
    }
}
