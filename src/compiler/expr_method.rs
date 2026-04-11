use super::*;
use crate::symbol::Symbol;

impl Compiler {
    /// Compile method call on indexed target: .VAR on @a[0] / %h<k>
    pub(super) fn compile_expr_method_var_on_index(&mut self, target: &Expr) {
        if let Expr::Index {
            target: index_target,
            index,
            ..
        } = target
            && let Some(source_name) = Self::index_assign_target_name(index_target)
        {
            // Preserve side effects of the indexed expression before producing
            // element variable metadata for .VAR on @a[0] / %h<k>.
            self.compile_expr(index_target);
            self.code.emit(OpCode::Pop);
            // Pass the index key so __mutsu_index_var_meta can look up the
            // actual value for Map containers (which decontainerize values).
            let name_idx = self.code.add_constant(Value::str(source_name));
            self.code.emit(OpCode::LoadConst(name_idx));
            self.compile_expr(index);
            let builtin_idx = self
                .code
                .add_constant(Value::str("__mutsu_index_var_meta".to_string()));
            self.code.emit(OpCode::CallFunc {
                name_idx: builtin_idx,
                arity: 2,
                arg_sources_idx: None,
            });
        }
    }

    /// Compile method call on variable target (needs writeback).
    pub(super) fn compile_expr_method_on_var(
        &mut self,
        target: &Expr,
        name: &Symbol,
        args: &[Expr],
        modifier: &Option<char>,
        quoted: bool,
    ) {
        let target_name = match target {
            Expr::Var(n) => n.clone(),
            Expr::ArrayVar(n) => format!("@{}", n),
            Expr::HashVar(n) => format!("%{}", n),
            Expr::CodeVar(n) => format!("&{}", n),
            Expr::BareWord(n) => n.clone(),
            Expr::DoStmt(stmt) => {
                if let Stmt::VarDecl { name, .. } = stmt.as_ref() {
                    name.clone()
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        };
        self.compile_expr(target);
        let arity = args.len() as u32;
        let arg_sources_idx = self.add_arg_sources_constant(args);
        for arg in args {
            self.compile_method_arg(arg);
        }
        let name_idx = self.code.add_constant(Value::str(name.resolve()));
        let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
        // Use CallMethod (non-mut) for read-only special variables like $!
        // so they benefit from the Nil dispatch path in CallMethod.
        if target_name == "!" {
            self.code.emit(OpCode::CallMethod {
                name_idx,
                arity,
                modifier_idx,
                quoted,
                arg_sources_idx,
            });
        } else {
            let target_name_idx = self.code.add_constant(Value::str(target_name));
            self.code.emit(OpCode::CallMethodMut {
                name_idx,
                arity,
                target_name_idx,
                modifier_idx,
                quoted,
                arg_sources_idx,
            });
        }
    }

    /// Compile method call on indexed target with mutating method (needs writeback).
    /// e.g., %hash<key>.push(4) or @array[0].push(5)
    pub(super) fn compile_expr_method_on_index(
        &mut self,
        target: &Expr,
        name: &Symbol,
        args: &[Expr],
        modifier: &Option<char>,
        quoted: bool,
    ) {
        if let Expr::Index {
            target: idx_target,
            index: idx_key,
            ..
        } = target
        {
            let var_name = Self::postfix_index_name(idx_target).unwrap_or_default();
            let name_resolved = name.resolve();
            let arity = args.len() as u32;
            let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
            if matches!(name_resolved.as_str(), "pop" | "shift") {
                let tmp_target_name = format!(
                    "__mutsu_tmp_index_method_target_{}",
                    self.code.constants.len()
                );
                let tmp_result_name = format!(
                    "__mutsu_tmp_index_method_result_{}",
                    self.code.constants.len()
                );
                let tmp_target_idx = self.code.add_constant(Value::str(tmp_target_name.clone()));
                let tmp_result_idx = self.code.add_constant(Value::str(tmp_result_name.clone()));
                let name_idx = self.code.add_constant(Value::str(name_resolved));
                let var_name_idx = self.code.add_constant(Value::str(var_name));

                self.compile_expr(target);
                self.code.emit(OpCode::SetGlobal(tmp_target_idx));
                self.code.emit(OpCode::GetGlobal(tmp_target_idx));
                for arg in args {
                    self.compile_method_arg(arg);
                }
                self.code.emit(OpCode::CallMethodMut {
                    name_idx,
                    arity,
                    target_name_idx: tmp_target_idx,
                    modifier_idx,
                    quoted,
                    arg_sources_idx: None,
                });
                self.code.emit(OpCode::SetGlobal(tmp_result_idx));
                self.code.emit(OpCode::GetGlobal(tmp_target_idx));
                self.compile_expr(idx_key);
                self.code.emit(OpCode::IndexAssignExprNamed(var_name_idx));
                self.code.emit(OpCode::Pop);
                self.code.emit(OpCode::GetGlobal(tmp_result_idx));
            } else {
                self.compile_expr(target);
                for arg in args {
                    self.compile_method_arg(arg);
                }
                let name_idx = self.code.add_constant(Value::str(name_resolved));
                self.code.emit(OpCode::CallMethod {
                    name_idx,
                    arity,
                    modifier_idx,
                    quoted,
                    arg_sources_idx: None,
                });
                self.compile_expr(idx_key);
                let var_name_idx = self.code.add_constant(Value::str(var_name));
                self.code.emit(OpCode::IndexAssignExprNamed(var_name_idx));
            }
        } else {
            unreachable!()
        }
    }

    /// Compile method call on non-variable target (no writeback needed).
    pub(super) fn compile_expr_method_generic(
        &mut self,
        target: &Expr,
        name: &Symbol,
        args: &[Expr],
        modifier: &Option<char>,
        quoted: bool,
    ) {
        // Lower index :delete adverb to dedicated delete opcodes.
        if name == "DELETE-KEY"
            && args.is_empty()
            && modifier.is_none()
            && let Expr::Index {
                target: delete_target,
                index: delete_index,
                ..
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
            return;
        }
        if name == "DELETE-KEY"
            && args.is_empty()
            && modifier.is_none()
            && let Expr::Call {
                name: sub_name,
                args: sub_args,
            } = target
            && *sub_name == Symbol::intern("__mutsu_subscript_adverb")
        {
            let mut call_args = sub_args.clone();
            call_args.push(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str_from("delete"))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::Literal(Value::Bool(true))),
            });
            self.compile_expr(&Expr::Call {
                name: *sub_name,
                args: call_args,
            });
            return;
        }
        self.compile_expr(target);
        let arity = args.len() as u32;
        let arg_sources_idx = self.add_arg_sources_constant(args);
        for arg in args {
            self.compile_method_arg(arg);
        }
        let name_idx = self.code.add_constant(Value::str(name.resolve()));
        let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
        self.code.emit(OpCode::CallMethod {
            name_idx,
            arity,
            modifier_idx,
            quoted,
            arg_sources_idx,
        });
    }

    /// Compile dynamic method call: target."$name"(args)
    pub(super) fn compile_expr_dynamic_method(
        &mut self,
        target: &Expr,
        name_expr: &Expr,
        args: &[Expr],
    ) {
        let target_var_name = match target {
            Expr::Var(n) => Some(n.clone()),
            Expr::ArrayVar(n) => Some(format!("@{}", n)),
            Expr::HashVar(n) => Some(format!("%{}", n)),
            _ => None,
        };
        self.compile_expr(target);
        self.compile_expr(name_expr);
        let arity = args.len() as u32;
        for arg in args {
            self.compile_method_arg(arg);
        }
        if let Some(var_name) = target_var_name {
            let target_name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::CallMethodDynamicMut {
                arity,
                target_name_idx,
            });
        } else {
            self.code.emit(OpCode::CallMethodDynamic { arity });
        }
    }

    /// Compile hyper method call: target>>.method(args)
    pub(super) fn compile_expr_hyper_method(
        &mut self,
        target: &Expr,
        name: &Symbol,
        args: &[Expr],
        modifier: &Option<char>,
        quoted: bool,
    ) {
        self.compile_expr(target);
        let arity = args.len() as u32;
        for arg in args {
            self.compile_method_arg(arg);
        }
        let name_idx = self.code.add_constant(Value::str(name.resolve()));
        let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
        self.code.emit(OpCode::HyperMethodCall {
            name_idx,
            arity,
            modifier_idx,
            quoted,
        });
    }

    /// Compile hyper method call with dynamic name.
    pub(super) fn compile_expr_hyper_method_dynamic(
        &mut self,
        target: &Expr,
        name_expr: &Expr,
        args: &[Expr],
        modifier: &Option<char>,
    ) {
        self.compile_expr(target);
        self.compile_expr(name_expr);
        let arity = args.len() as u32;
        for arg in args {
            self.compile_method_arg(arg);
        }
        let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
        self.code.emit(OpCode::HyperMethodCallDynamic {
            arity,
            modifier_idx,
        });
    }
}
