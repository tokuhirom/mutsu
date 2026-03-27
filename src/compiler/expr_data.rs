use super::*;

impl Compiler {
    /// Compile AssignExpr: assignment as expression.
    pub(super) fn compile_expr_assign(&mut self, name: &str, expr: &Expr) {
        // $.attr = expr — In Raku, this first resolves self.attr (method call),
        // then assigns to the resulting lvalue container. If the accessor doesn't
        // exist, the method call throws. Compile as: first call self.attr() to
        // check it exists (pop result), then do the normal assignment.
        if let Some(attr_name) = name.strip_prefix('.')
            && !attr_name.is_empty()
        {
            // Load self and call the accessor method to validate it exists
            let self_name = self.qualify_variable_name("self");
            let self_idx = self.code.add_constant(Value::str(self_name));
            self.code.emit(OpCode::GetGlobal(self_idx));
            let method_idx = self.code.add_constant(Value::str(attr_name.to_string()));
            self.code.emit(OpCode::CallMethod {
                name_idx: method_idx,
                arity: 0,
                modifier_idx: None,
                quoted: false,
                arg_sources_idx: None,
            });
            self.code.emit(OpCode::Pop); // discard the accessor result
        }
        self.compile_expr(expr);
        if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::AssignExprLocal(slot));
        } else {
            let name_idx = self.code.add_constant(Value::str(name.to_string()));
            self.code.emit(OpCode::AssignExpr(name_idx));
        }
        // Preserve lvalue container identity for expression-context consumers
        // (e.g. collected postfix `for` results).
        let name_idx = self.code.add_constant(Value::str(name.to_string()));
        self.code.emit(OpCode::TagContainerRef(name_idx));
    }

    /// Compile CaptureVar ($0, $1, etc.).
    pub(super) fn compile_expr_capture_var(&mut self, name: &str) {
        let keys: Vec<&str> = name.split_whitespace().collect();
        if keys.len() > 1 {
            // Multi-key subscript: $<w1 w2 w3> -> list of individual captures
            for key in &keys {
                let name_idx = self.code.add_constant(Value::str(format!("<{}>", key)));
                self.code.emit(OpCode::GetCaptureVar(name_idx));
            }
            self.code.emit(OpCode::MakeArray(keys.len() as u32));
        } else {
            let name_idx = self.code.add_constant(Value::str(format!("<{}>", name)));
            self.code.emit(OpCode::GetCaptureVar(name_idx));
        }
    }

    /// Compile Hash literal.
    pub(super) fn compile_expr_hash(&mut self, pairs: &[(String, Option<Expr>)]) {
        let n = pairs.len() as u32;
        for (key, val_opt) in pairs {
            // Push key as string constant
            let key_idx = self.code.add_constant(Value::str(key.clone()));
            self.code.emit(OpCode::LoadConst(key_idx));
            // Push value (or True if none, for bare colonpairs like :a)
            if let Some(val_expr) = val_opt {
                self.compile_expr(val_expr);
            } else {
                self.code.emit(OpCode::LoadTrue);
            }
        }
        self.code.emit(OpCode::MakeHash(n));
    }

    /// Compile Exists check (:exists).
    pub(super) fn compile_expr_exists(
        &mut self,
        target: &Expr,
        negated: bool,
        delete: bool,
        arg: &Option<Box<Expr>>,
        adverb: &crate::ast::ExistsAdverb,
    ) {
        use crate::ast::ExistsAdverb;
        let adverb_bits: u32 = match adverb {
            ExistsAdverb::None => 0,
            ExistsAdverb::Kv => 1,
            ExistsAdverb::NotKv => 2,
            ExistsAdverb::P => 3,
            ExistsAdverb::NotP => 4,
            ExistsAdverb::NotV => 5,
            ExistsAdverb::InvalidK => 6,
            ExistsAdverb::InvalidNotK => 7,
            ExistsAdverb::InvalidV => 8,
        };
        // Env variable special cases
        if !negated && arg.is_none() && *adverb == ExistsAdverb::None {
            match target {
                Expr::EnvIndex(key) => {
                    let key_idx = self.code.add_constant(Value::str(key.clone()));
                    self.code.emit(OpCode::ExistsEnvIndex(key_idx));
                    return;
                }
                Expr::Index {
                    target: t, index, ..
                } if matches!(t.as_ref(), Expr::HashVar(name) if name == "*ENV")
                    && matches!(index.as_ref(), Expr::Literal(Value::Str(_))) =>
                {
                    if let Expr::Literal(Value::Str(key)) = index.as_ref() {
                        let key_idx = self.code.add_constant(Value::str((**key).clone()));
                        self.code.emit(OpCode::ExistsEnvIndex(key_idx));
                        return;
                    }
                }
                _ => {}
            }
            // Non-index, non-env, non-multidim: use ExistsExpr
            if !matches!(
                target,
                Expr::Index { .. } | Expr::ZenSlice(_) | Expr::MultiDimIndex { .. }
            ) {
                self.compile_expr(target);
                self.code.emit(OpCode::ExistsExpr);
                return;
            }
        }
        // MultiDimIndex targets: compile as a call to
        // __mutsu_multidim_exists_adverb(target, negated, adverb, dim0, dim1, ...)
        if let Expr::MultiDimIndex {
            target: mdtarget,
            dimensions,
        } = target
        {
            let adverb_str = match adverb {
                ExistsAdverb::Kv => "kv",
                ExistsAdverb::NotKv => "not-kv",
                ExistsAdverb::P => "p",
                ExistsAdverb::NotP => "not-p",
                ExistsAdverb::InvalidK => "k",
                ExistsAdverb::InvalidNotK => "not-k",
                ExistsAdverb::InvalidV => "v",
                ExistsAdverb::None => "none",
                ExistsAdverb::NotV => "not-v",
            };
            // Build args: target, negated_bool, adverb_name, dim0, dim1, ...
            let mut call_args = vec![
                mdtarget.as_ref().clone(),
                Expr::Literal(Value::Bool(negated)),
                Expr::Literal(Value::str(adverb_str.to_string())),
            ];
            call_args.extend(dimensions.iter().cloned());
            let call = Expr::Call {
                name: crate::symbol::Symbol::intern("__mutsu_multidim_exists_adverb"),
                args: call_args,
            };
            self.compile_expr(&call);
            return;
        }

        // Rich case: use ExistsIndexAdv opcode
        let is_zen = matches!(target, Expr::ZenSlice(_));
        let mut flags: u32 = 0;
        if negated {
            flags |= 1;
        }
        if arg.is_some() {
            flags |= 2;
        }
        if is_zen {
            flags |= 4;
        }
        flags |= adverb_bits << 4;

        match target {
            Expr::ZenSlice(inner) => {
                self.compile_expr(inner);
            }
            Expr::Index { target: t, index } => {
                self.compile_expr(t);
                self.compile_expr(index);
            }
            _ => {
                self.compile_expr(target);
            }
        }
        if let Some(a) = arg {
            self.compile_expr(a);
        }
        self.code.emit(OpCode::ExistsIndexAdv(flags));

        if delete
            && let Expr::Index {
                target: delete_target,
                index: delete_index,
            } = target
        {
            if let Some(var_name) = Self::postfix_index_name(delete_target) {
                if Self::index_assign_target_requires_eval(delete_target) {
                    self.compile_expr(delete_target);
                    self.code.emit(OpCode::Pop);
                }
                self.compile_expr(delete_index);
                let name_idx = self.code.add_constant(Value::str(var_name));
                self.code.emit(OpCode::DeleteIndexNamed(name_idx));
            } else {
                self.compile_expr(delete_target);
                self.compile_expr(delete_index);
                self.code.emit(OpCode::DeleteIndexExpr);
            }
            // Keep the :exists result on the stack.
            self.code.emit(OpCode::Pop);
        }
    }

    /// Compile PhaserExpr (INIT/CHECK/END as rvalue).
    pub(super) fn compile_expr_phaser(&mut self, kind: &PhaserKind, body: &[Stmt]) {
        if matches!(kind, crate::ast::PhaserKind::End) {
            let end_stmt = Stmt::Phaser {
                kind: crate::ast::PhaserKind::End,
                body: body.to_vec(),
            };
            let idx = self.code.add_stmt(end_stmt);
            self.code.emit(OpCode::PhaserEnd(idx));
            self.code.emit(OpCode::LoadNil);
        } else {
            self.compile_block_inline(body);
        }
    }

    /// Compile `once { ... }` expression.
    pub(super) fn compile_expr_once(&mut self, body: &[Stmt]) {
        let key = format!(
            "__once_{}::{}",
            self.current_package,
            STATE_COUNTER.fetch_add(1, Ordering::Relaxed)
        );
        let key_idx = self.code.add_constant(Value::str(key));
        let once_idx = self.code.emit(OpCode::OnceExpr {
            key_idx,
            body_end: 0,
        });
        self.compile_block_inline(body);
        self.code.patch_body_end(once_idx);
    }

    /// Compile BracketArray expression ([...]).
    pub(super) fn compile_expr_bracket_array(&mut self, elems: &[Expr], trailing_comma: bool) {
        let single_with_trailing_comma = trailing_comma && elems.len() == 1;
        if elems.len() == 1 && !single_with_trailing_comma {
            if let Expr::ArrayLiteral(items) = &elems[0] {
                for item in items {
                    self.compile_expr(item);
                }
                self.code.emit(OpCode::MakeRealArray(items.len() as u32));
            } else {
                self.compile_expr(&elems[0]);
                self.code.emit(OpCode::MakeRealArray(1));
            }
        } else {
            for elem in elems {
                self.compile_expr(elem);
            }
            if single_with_trailing_comma {
                self.code
                    .emit(OpCode::MakeRealArrayNoFlatten(elems.len() as u32));
            } else {
                self.code.emit(OpCode::MakeRealArray(elems.len() as u32));
            }
        }
    }

    /// Compile Index expression (target[index]).
    pub(super) fn compile_expr_index(&mut self, target: &Expr, index: &Expr) {
        // Special case: %*ENV<key> compiles to GetEnvIndex
        if let Expr::HashVar(name) = target {
            if name == "*ENV" {
                if let Expr::Literal(Value::Str(key)) = index {
                    let key_idx = self.code.add_constant(Value::Str(key.clone()));
                    self.code.emit(OpCode::GetEnvIndex(key_idx));
                } else {
                    self.compile_expr(target);
                    self.compile_expr(index);
                    self.code.emit(OpCode::Index);
                }
            } else {
                self.compile_expr(target);
                self.compile_expr(index);
                self.code.emit(OpCode::Index);
            }
        } else {
            self.compile_expr(target);
            self.compile_expr(index);
            self.code.emit(OpCode::Index);
        }
    }

    /// Compile StringInterpolation expression.
    pub(super) fn compile_expr_string_interpolation(&mut self, parts: &[Expr]) {
        let n = parts.len() as u32;
        for part in parts {
            match part {
                Expr::ArrayVar(name) => self.compile_expr(&Expr::Call {
                    name: crate::symbol::Symbol::intern("join"),
                    args: vec![
                        Expr::Literal(Value::str(" ".to_string())),
                        Expr::ArrayVar(name.clone()),
                    ],
                }),
                Expr::HashVar(name) => self.compile_expr(&Expr::Unary {
                    op: crate::token_kind::TokenKind::Tilde,
                    expr: Box::new(Expr::HashVar(name.clone())),
                }),
                _ => self.compile_expr(part),
            }
        }
        self.code.emit(OpCode::StringConcat(n));
    }
}
