use super::*;
use crate::symbol::Symbol;

impl Compiler {
    /// Dispatch for operator-like expression forms.
    pub(super) fn compile_expr_op(&mut self, expr: &Expr) {
        match expr {
            Expr::Subst {
                pattern,
                replacement,
                samecase,
                sigspace,
                samemark,
                samespace,
                global,
                nth,
                x,
                perl5,
            } => {
                self.compile_expr_subst(
                    pattern,
                    replacement,
                    *samecase,
                    *sigspace,
                    *samemark,
                    *samespace,
                    *global,
                    nth,
                    x,
                    *perl5,
                );
            }
            Expr::NonDestructiveSubst {
                pattern,
                replacement,
                samecase,
                sigspace,
                samemark,
                samespace,
                global,
                nth,
                x,
                perl5,
            } => {
                self.compile_expr_nondestructive_subst(
                    pattern,
                    replacement,
                    *samecase,
                    *sigspace,
                    *samemark,
                    *samespace,
                    *global,
                    nth,
                    x,
                    *perl5,
                );
            }
            Expr::Transliterate {
                from,
                to,
                delete,
                complement,
                squash,
                non_destructive,
            } => {
                self.compile_expr_transliterate(
                    from,
                    to,
                    *delete,
                    *complement,
                    *squash,
                    *non_destructive,
                );
            }
            Expr::HyperOp {
                op,
                left,
                right,
                dwim_left,
                dwim_right,
            } => {
                self.compile_expr_hyper_op(op, left, right, *dwim_left, *dwim_right);
            }
            Expr::HyperFuncOp {
                func_name,
                left,
                right,
                dwim_left,
                dwim_right,
            } => {
                self.compile_expr_hyper_func_op(func_name, left, right, *dwim_left, *dwim_right);
            }
            Expr::MetaOp {
                meta,
                op,
                left,
                right,
            } => {
                self.compile_expr_meta_op(meta, op, left, right);
            }
            Expr::InfixFunc {
                name,
                left,
                right,
                modifier,
            } => {
                self.compile_expr_infix_func(name, left, right, modifier);
            }
            _ => unreachable!(),
        }
    }

    /// Compile s/// substitution.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_expr_subst(
        &mut self,
        pattern: &str,
        replacement: &str,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth: &Option<String>,
        x: &Option<usize>,
        perl5: bool,
    ) {
        let pattern_idx = self.code.add_constant(Value::str(pattern.to_string()));
        let replacement_idx = self.code.add_constant(Value::str(replacement.to_string()));
        let nth_idx = nth
            .as_ref()
            .map(|raw| self.code.add_constant(Value::str(raw.clone())));
        self.code.emit(OpCode::Subst {
            pattern_idx,
            replacement_idx,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth_idx,
            x_count: x.map(|n| n as u32),
            perl5,
        });
    }

    /// Compile S/// non-destructive substitution.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_expr_nondestructive_subst(
        &mut self,
        pattern: &str,
        replacement: &str,
        samecase: bool,
        sigspace: bool,
        samemark: bool,
        samespace: bool,
        global: bool,
        nth: &Option<String>,
        x: &Option<usize>,
        perl5: bool,
    ) {
        let pattern_idx = self.code.add_constant(Value::str(pattern.to_string()));
        let replacement_idx = self.code.add_constant(Value::str(replacement.to_string()));
        let nth_idx = nth
            .as_ref()
            .map(|raw| self.code.add_constant(Value::str(raw.clone())));
        self.code.emit(OpCode::NonDestructiveSubst {
            pattern_idx,
            replacement_idx,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth_idx,
            x_count: x.map(|n| n as u32),
            perl5,
        });
    }

    /// Compile tr/// transliteration.
    pub(super) fn compile_expr_transliterate(
        &mut self,
        from: &str,
        to: &str,
        delete: bool,
        complement: bool,
        squash: bool,
        non_destructive: bool,
    ) {
        let from_idx = self.code.add_constant(Value::str(from.to_string()));
        let to_idx = self.code.add_constant(Value::str(to.to_string()));
        self.code.emit(OpCode::Transliterate {
            from_idx,
            to_idx,
            delete,
            complement,
            squash,
            non_destructive,
        });
    }

    /// Compile HyperOp (>>op<<).
    pub(super) fn compile_expr_hyper_op(
        &mut self,
        op: &str,
        left: &Expr,
        right: &Expr,
        dwim_left: bool,
        dwim_right: bool,
    ) {
        self.compile_expr(left);
        self.compile_expr(right);
        let op_idx = self.code.add_constant(Value::str(op.to_string()));
        self.code.emit(OpCode::HyperOp {
            op_idx,
            dwim_left,
            dwim_right,
        });
    }

    /// Compile HyperFuncOp (>>[&func]<<).
    pub(super) fn compile_expr_hyper_func_op(
        &mut self,
        func_name: &str,
        left: &Expr,
        right: &Expr,
        dwim_left: bool,
        dwim_right: bool,
    ) {
        self.compile_expr(left);
        self.compile_expr(right);
        let name_idx = self.code.add_constant(Value::str(func_name.to_string()));
        self.code.emit(OpCode::HyperFuncOp {
            name_idx,
            dwim_left,
            dwim_right,
        });
    }

    /// Compile MetaOp (Rop, Xop, Zop).
    pub(super) fn compile_expr_meta_op(&mut self, meta: &str, op: &str, left: &Expr, right: &Expr) {
        if meta == "X" && matches!(op, "and" | "&&" | "or" | "||" | "andthen" | "orelse") {
            let thunked = Expr::AnonSub {
                body: vec![Stmt::Expr(right.clone())],
                is_rw: false,
            };
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_cross_shortcircuit"),
                args: vec![
                    Expr::Literal(Value::str(op.to_string())),
                    left.clone(),
                    thunked,
                ],
            };
            self.compile_expr(&rewritten);
            return;
        }
        if meta == "X" && op == "xx" && matches!(left, Expr::ArrayLiteral(_)) {
            let thunked = Expr::AnonSub {
                body: vec![Stmt::Expr(left.clone())],
                is_rw: false,
            };
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_reverse_xx"),
                args: vec![right.clone(), thunked],
            };
            self.compile_expr(&rewritten);
            return;
        }
        // Z with short-circuit operators: thunk the right side
        if meta == "Z" && matches!(op, "and" | "&&" | "or" | "||" | "andthen" | "orelse") {
            let thunked = Expr::AnonSub {
                body: vec![Stmt::Expr(right.clone())],
                is_rw: false,
            };
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_zip_shortcircuit"),
                args: vec![
                    Expr::Literal(Value::str(op.to_string())),
                    left.clone(),
                    thunked,
                ],
            };
            self.compile_expr(&rewritten);
            return;
        }
        // Zxx with list left side: thunk the left side
        if meta == "Z" && op == "xx" && matches!(left, Expr::ArrayLiteral(_)) {
            let thunked = Expr::AnonSub {
                body: vec![Stmt::Expr(left.clone())],
                is_rw: false,
            };
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_zip_xx"),
                args: vec![right.clone(), thunked],
            };
            self.compile_expr(&rewritten);
            return;
        }
        if meta == "R" {
            // R-meta can stack (RRop, RRRop, ...). Normalize to base operator and parity.
            let mut base = op;
            let mut reverse_count = 1usize;
            while let Some(rest) = base.strip_prefix('R') {
                reverse_count += 1;
                base = rest;
            }
            let reversed = reverse_count % 2 == 1;
            let (eval_left, eval_right) = if reversed {
                (right, left)
            } else {
                (left, right)
            };

            if base == "andthen" && reversed {
                let thunked = Expr::AnonSub {
                    body: vec![Stmt::Expr(eval_right.clone())],
                    is_rw: false,
                };
                let rewritten = Expr::Call {
                    name: Symbol::intern("__mutsu_reverse_andthen"),
                    args: vec![eval_left.clone(), thunked],
                };
                self.compile_expr(&rewritten);
                return;
            }

            let logical_token = match base {
                "and" | "&&" => Some(TokenKind::AndAnd),
                "or" | "||" => Some(TokenKind::OrOr),
                "//" | "orelse" => Some(TokenKind::OrElse),
                "andthen" => Some(TokenKind::AndThen),
                "notandthen" => Some(TokenKind::NotAndThen),
                _ => None,
            };
            if let Some(op_tok) = logical_token {
                let rewritten = Expr::Binary {
                    left: Box::new(eval_left.clone()),
                    op: op_tok,
                    right: Box::new(eval_right.clone()),
                };
                self.compile_expr(&rewritten);
                return;
            }

            if base == "xx" {
                let thunked = Expr::AnonSub {
                    body: vec![Stmt::Expr(eval_left.clone())],
                    is_rw: false,
                };
                let rewritten = Expr::Call {
                    name: Symbol::intern("__mutsu_reverse_xx"),
                    args: vec![eval_right.clone(), thunked],
                };
                self.compile_expr(&rewritten);
                return;
            }
        }
        self.compile_expr(left);
        self.compile_expr(right);
        let meta_idx = self.code.add_constant(Value::str(meta.to_string()));
        let op_idx = self.code.add_constant(Value::str(op.to_string()));
        self.code.emit(OpCode::MetaOp { meta_idx, op_idx });
    }

    /// Compile InfixFunc (atan2, sprintf, flip-flop, etc.).
    pub(super) fn compile_expr_infix_func(
        &mut self,
        name: &str,
        left: &Expr,
        right: &[Expr],
        modifier: &Option<String>,
    ) {
        if modifier.is_none()
            && matches!(name, "any" | "all" | "one" | "none")
            && right.len() == 1
            && let Expr::BareWord(mop_name) = left
            && matches!(mop_name.as_str(), "WHAT" | "HOW")
            && let Expr::ArrayLiteral(junction_args) = &right[0]
        {
            let normalized = Expr::Call {
                name: Symbol::intern(mop_name),
                args: vec![Expr::Call {
                    name: Symbol::intern(name),
                    args: junction_args.clone(),
                }],
            };
            self.compile_expr(&normalized);
            return;
        }
        let flip_flop_mode = match name {
            "ff" => Some((false, false, false)),
            "^ff" => Some((true, false, false)),
            "ff^" => Some((false, true, false)),
            "^ff^" => Some((true, true, false)),
            "fff" => Some((false, false, true)),
            "^fff" => Some((true, false, true)),
            "fff^" => Some((false, true, true)),
            "^fff^" => Some((true, true, true)),
            _ => None,
        };
        if let Some((exclude_start, exclude_end, is_fff)) = flip_flop_mode
            && right.len() == 1
            && modifier.is_none()
        {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            name.hash(&mut hasher);
            format!("{:?}", left).hash(&mut hasher);
            format!("{:?}", &right[0]).hash(&mut hasher);
            let ff_idx = self.code.emit(OpCode::FlipFlopExpr {
                lhs_end: 0,
                rhs_end: 0,
                site_id: hasher.finish(),
                exclude_start,
                exclude_end,
                is_fff,
            });
            self.compile_expr(left);
            self.code.patch_flip_flop_lhs_end(ff_idx);
            self.compile_expr(&right[0]);
            self.code.patch_flip_flop_rhs_end(ff_idx);
            return;
        }
        // For junction operators (|, &, ^), flatten chains of same-name
        // InfixFunc nodes so the user-defined operator is called once
        // with all operands (list-associative behavior).
        if modifier.is_none() && matches!(name, "|" | "&" | "^") && right.len() == 1 {
            let mut operands = Vec::new();
            // Collect rightmost operands from the chain
            operands.extend(right.iter());
            let mut current = left;
            while let Expr::InfixFunc {
                name: inner_name,
                left: inner_left,
                right: inner_right,
                modifier: inner_mod,
            } = current
            {
                if inner_name != name || inner_mod.is_some() || inner_right.len() != 1 {
                    break;
                }
                operands.extend(inner_right.iter());
                current = inner_left.as_ref();
            }
            if operands.len() > 1 {
                operands.reverse();
                self.compile_expr(current);
                for op in &operands {
                    self.compile_expr(op);
                }
                let name_idx = self.code.add_constant(Value::str(name.to_string()));
                self.code.emit(OpCode::InfixFunc {
                    name_idx,
                    right_arity: operands.len() as u32,
                    modifier_idx: None,
                });
                return;
            }
        }
        self.compile_expr(left);
        for r in right {
            self.compile_expr(r);
        }
        let name_idx = self.code.add_constant(Value::str(name.to_string()));
        let modifier_idx = modifier
            .as_ref()
            .map(|m| self.code.add_constant(Value::str(m.clone())));
        self.code.emit(OpCode::InfixFunc {
            name_idx,
            right_arity: right.len() as u32,
            modifier_idx,
        });
    }
}
