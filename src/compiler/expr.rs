use super::*;

impl Compiler {
    pub(super) fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Whatever => {
                let idx = self.code.add_constant(Value::Whatever);
                self.code.emit(OpCode::LoadConst(idx));
            }
            Expr::HyperWhatever => {
                let idx = self.code.add_constant(Value::HyperWhatever);
                self.code.emit(OpCode::LoadConst(idx));
            }
            Expr::Literal(v) => match v {
                Value::Nil => {
                    self.code.emit(OpCode::LoadNil);
                }
                Value::Bool(true) => {
                    self.code.emit(OpCode::LoadTrue);
                }
                Value::Bool(false) => {
                    self.code.emit(OpCode::LoadFalse);
                }
                _ => {
                    let idx = self.code.add_constant(v.clone());
                    self.code.emit(OpCode::LoadConst(idx));
                }
            },
            // Grouped (parenthesized) expression — transparent wrapper
            Expr::Grouped(inner) => {
                self.compile_expr(inner);
            }
            // m/regex/ -- compile as $_ ~~ /regex/, matching against $_
            Expr::MatchRegex(v) => {
                self.compile_match_regex(v);
            }
            Expr::Var(name) => {
                self.compile_expr_var(name);
            }
            Expr::ArrayVar(name) => {
                let sigiled = format!("@{}", name);
                let var_name = if self.local_map.contains_key(sigiled.as_str()) {
                    sigiled
                } else {
                    self.qualify_variable_name(&format!("@{}", name))
                };
                let name_idx = self.code.add_constant(Value::str(var_name));
                self.code.emit(OpCode::GetArrayVar(name_idx));
            }
            Expr::HashVar(name) => {
                let sigiled = format!("%{}", name);
                let var_name = if self.local_map.contains_key(sigiled.as_str()) {
                    sigiled
                } else {
                    self.qualify_variable_name(&format!("%{}", name))
                };
                let name_idx = self.code.add_constant(Value::str(var_name));
                self.code.emit(OpCode::GetHashVar(name_idx));
            }
            Expr::BareWord(name) => {
                if let Some(&slot) = self.local_map.get(name.as_str()) {
                    if crate::vm::VM::is_builtin_type(name) {
                        let name_idx = self.code.add_constant(Value::str(name.clone()));
                        self.code.emit(OpCode::GetBareWord(name_idx));
                    } else {
                        self.code.emit(OpCode::GetLocal(slot));
                    }
                } else {
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    self.code.emit(OpCode::GetBareWord(name_idx));
                }
            }
            Expr::PseudoStash(name) => {
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::GetPseudoStash(name_idx));
            }
            Expr::Unary { op, expr } => {
                self.compile_expr_unary(op, expr);
            }
            Expr::Binary { left, op, right } => {
                self.compile_expr_binary(expr, left, op, right);
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.compile_expr(cond);
                let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
                self.compile_expr(then_expr);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_else);
                self.compile_expr(else_expr);
                self.code.patch_jump(jump_end);
            }
            Expr::ArrayLiteral(elems) => {
                for elem in elems {
                    self.compile_expr(elem);
                    if Self::expr_is_scalar_var(elem) {
                        self.code.emit(OpCode::Itemize);
                    }
                }
                self.code.emit(OpCode::MakeArray(elems.len() as u32));
            }
            Expr::BracketArray(elems, trailing_comma) => {
                self.compile_expr_bracket_array(elems, *trailing_comma);
            }
            Expr::CaptureLiteral(items) => {
                for item in items {
                    self.compile_expr(item);
                }
                self.code.emit(OpCode::MakeCapture(items.len() as u32));
            }
            // Expression-level function call
            Expr::Call { name, args } => {
                self.compile_expr_call(name, args);
            }
            // Method call on mutable variable target (needs writeback)
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } if name == "VAR"
                && args.is_empty()
                && modifier.is_none()
                && !quoted
                && matches!(target.as_ref(), Expr::Index { .. }) =>
            {
                self.compile_expr_method_var_on_index(target);
            }
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } if matches!(
                target.as_ref(),
                Expr::Var(_)
                    | Expr::ArrayVar(_)
                    | Expr::HashVar(_)
                    | Expr::CodeVar(_)
                    | Expr::BareWord(_)
            ) || Self::is_dostmt_vardecl(target) =>
            {
                self.compile_expr_method_on_var(target, name, args, modifier, *quoted);
            }
            // Method call on indexed target with mutating method -- needs writeback.
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } if Self::is_mutating_method_on_index(target, name) => {
                self.compile_expr_method_on_index(target, name, args, modifier, *quoted);
            }
            // Method call on non-variable target (no writeback needed)
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } => {
                self.compile_expr_method_generic(target, name, args, modifier, *quoted);
            }
            // Dynamic method call: target."$name"(args)
            Expr::DynamicMethodCall {
                target,
                name_expr,
                args,
            } => {
                self.compile_expr_dynamic_method(target, name_expr, args);
            }
            // Hyper method call: target>>.method(args)
            Expr::HyperMethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } => {
                self.compile_expr_hyper_method(target, name, args, modifier, *quoted);
            }
            Expr::HyperMethodCallDynamic {
                target,
                name_expr,
                args,
                modifier,
            } => {
                self.compile_expr_hyper_method_dynamic(target, name_expr, args, modifier);
            }
            // Indexing
            Expr::Index { target, index } => {
                self.compile_expr_index(target, index);
            }
            // Multi-dimensional indexing: @a[$x;$y;$z]
            Expr::MultiDimIndex { target, dimensions } => {
                self.compile_expr(target);
                self.compile_expr(&Expr::ArrayLiteral(dimensions.clone()));
                self.code.emit(OpCode::Index);
            }
            // Hash hyperslice: %hash{**}:adverb
            Expr::HyperSlice { target, adverb } => {
                self.compile_expr(target);
                self.code.emit(OpCode::HyperSlice(*adverb as u8));
            }
            // Hash hyperindex: %hash{||@keys}
            Expr::HyperIndex { target, keys } => {
                self.compile_expr(target);
                self.compile_expr(keys);
                self.code.emit(OpCode::HyperIndex);
            }
            // Deferred heredoc interpolation
            Expr::HeredocInterpolation(content) => {
                let resolved = crate::parser::interpolate_heredoc_content(content);
                self.compile_expr(&resolved);
            }
            // String interpolation
            Expr::StringInterpolation(parts) => {
                self.compile_expr_string_interpolation(parts);
            }
            // Postfix ++ on variable
            Expr::PostfixOp {
                op: TokenKind::PlusPlus,
                expr,
            } => {
                self.compile_expr_postfix_inc(expr);
            }
            // Postfix -- on variable
            Expr::PostfixOp {
                op: TokenKind::MinusMinus,
                expr,
            } => {
                self.compile_expr_postfix_dec(expr);
            }
            // Assignment as expression
            Expr::AssignExpr { name, expr } => {
                self.compile_expr_assign(name, expr);
            }
            // Capture variable ($0, $1, etc.)
            Expr::CaptureVar(name) => {
                self.compile_expr_capture_var(name);
            }
            // Code variable (&foo)
            Expr::CodeVar(name) => {
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::GetCodeVar(name_idx));
            }
            // Hash literal
            Expr::Hash(pairs) => {
                self.compile_expr_hash(pairs);
            }
            // Environment variable access (%*ENV<key>)
            Expr::EnvIndex(key) => {
                let key_idx = self.code.add_constant(Value::str(key.clone()));
                self.code.emit(OpCode::GetEnvIndex(key_idx));
            }
            // Exists check (:exists)
            Expr::Exists {
                target,
                negated,
                delete,
                arg,
                adverb,
            } => {
                self.compile_expr_exists(target, *negated, *delete, arg, adverb);
            }
            Expr::ZenSlice(inner) => {
                // Outside of :exists, zen slice is identity
                self.compile_expr(inner);
            }
            // Reduction ([+] @arr)
            Expr::Reduction { op, expr } => {
                self.compile_expr(expr);
                let op_idx = self.code.add_constant(Value::str(op.clone()));
                self.code.emit(OpCode::Reduction(op_idx));
            }
            // Phaser expression (INIT/CHECK/END as rvalue)
            Expr::PhaserExpr { kind, body } => {
                self.compile_expr_phaser(kind, body);
            }
            Expr::Once { body } => {
                self.compile_expr_once(body);
            }
            // __ROUTINE__ magic
            Expr::RoutineMagic => {
                self.code.emit(OpCode::RoutineMagic);
            }
            // __BLOCK__ magic
            Expr::BlockMagic => {
                self.code.emit(OpCode::BlockMagic);
            }
            // s///, S///, tr///, HyperOp, HyperFuncOp, MetaOp, InfixFunc
            Expr::Subst { .. }
            | Expr::NonDestructiveSubst { .. }
            | Expr::Transliterate { .. }
            | Expr::HyperOp { .. }
            | Expr::HyperFuncOp { .. }
            | Expr::MetaOp { .. }
            | Expr::InfixFunc { .. } => {
                self.compile_expr_op(expr);
            }
            Expr::Try { body, catch } => {
                self.compile_try(body, catch);
            }
            Expr::DoBlock { .. } => {
                if let Expr::DoBlock { body, label } = expr {
                    self.compile_do_block_expr(body, label);
                }
            }
            Expr::DoStmt(stmt) => {
                self.compile_expr_do_stmt(stmt);
            }
            Expr::Gather(_) => {
                if let Expr::Gather(body) = expr {
                    let idx = self.code.add_stmt(Stmt::Block(body.clone()));
                    self.code.emit(OpCode::MakeGather(idx));
                }
            }
            Expr::Eager(inner) => {
                self.compile_expr(inner);
                self.code.emit(OpCode::Eager);
            }
            Expr::PositionalPair(inner) => {
                self.compile_expr(inner);
                self.code.emit(OpCode::ContainerizePair);
            }
            Expr::CallOn { target, args } => {
                self.compile_expr_call_on(target, args);
            }
            Expr::AnonSub { body, is_rw } => {
                self.compile_expr_anon_sub(body, *is_rw);
            }
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw,
                is_whatever_code,
            } => {
                self.compile_expr_anon_sub_params(
                    params,
                    param_defs,
                    return_type,
                    body,
                    *is_rw,
                    *is_whatever_code,
                );
            }
            Expr::Lambda {
                param,
                body,
                is_whatever_code,
            } => {
                self.compile_expr_lambda(param, body, *is_whatever_code);
            }
            Expr::IndexAssign {
                target,
                index,
                value,
            } => {
                self.compile_expr_index_assign(target, index, value);
            }
            // Multi-dimensional index assignment: @a[$x;$y;$z] = value
            Expr::MultiDimIndexAssign {
                target,
                dimensions,
                value,
            } => {
                self.compile_expr_multidim_index_assign(target, dimensions, value);
            }
            Expr::IndirectTypeLookup(inner) => {
                self.compile_expr(inner);
                self.code.emit(OpCode::IndirectTypeLookup);
            }
            Expr::IndirectCodeLookup { package, name } => {
                self.compile_expr(package);
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::IndirectCodeLookup(name_idx));
            }
            Expr::SymbolicDeref { sigil, expr } => {
                self.compile_expr(expr);
                let sigil_idx = self.code.add_constant(Value::str(sigil.clone()));
                self.code.emit(OpCode::SymbolicDeref(sigil_idx));
            }
            Expr::SymbolicDerefAssign { sigil, expr, value } => {
                self.compile_expr(value);
                self.compile_expr(expr);
                let sigil_idx = self.code.add_constant(Value::str(sigil.clone()));
                self.code.emit(OpCode::SymbolicDerefStore(sigil_idx));
            }
            Expr::IndirectTypeLookupAssign { expr, value } => {
                self.compile_expr(value);
                self.compile_expr(expr);
                self.code.emit(OpCode::IndirectTypeLookupStore);
            }
            Expr::ControlFlow { kind, label } => {
                use crate::ast::ControlFlowKind;
                let op = match kind {
                    ControlFlowKind::Last => OpCode::Last(label.clone()),
                    ControlFlowKind::Next => OpCode::Next(label.clone()),
                    ControlFlowKind::Redo => OpCode::Redo(label.clone()),
                };
                self.code.emit(op);
            }
            // Block inlining: compile inline if no placeholders
            Expr::Block(stmts) => {
                self.compile_expr_block(stmts);
            }
            // Remaining expression forms not yet bytecode-native.
            Expr::PostfixOp { .. } => {
                self.code.emit(OpCode::LoadNil);
            }
        }
    }
}
