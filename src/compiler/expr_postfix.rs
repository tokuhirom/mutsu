use super::*;
use crate::symbol::Symbol;

impl Compiler {
    /// Compile `++`/`--` applied to a call of a named routine (`++f()`,
    /// `f()--`, ...).
    ///
    /// Raku's `prefix:<++>` binds its argument `is rw`, so this is legal exactly
    /// when the routine hands back a container — an `is rw` routine, or one
    /// whose tail is an explicit `return-rw`. The compiler cannot decide that
    /// here (the routine may be declared later in the file), so it emits a call
    /// to `__mutsu_incdec_named_sub_lvalue`, which resolves the routine at
    /// runtime and falls back to the very same `X::Multi::NoMatch` the plain
    /// `__mutsu_incdec_nomatch` path raises when it is not rw-capable.
    ///
    /// Returns `false` for compiler-internal lowerings, so the caller keeps its
    /// existing fallback.
    pub(super) fn compile_incdec_named_sub_lvalue(
        &mut self,
        expr: &Expr,
        inc: bool,
        prefix: bool,
    ) -> bool {
        let Expr::Call { name, args } = expr else {
            return false;
        };
        if name.with_str(|n| n.starts_with("__mutsu_") || n.starts_with("nqp::")) {
            return false;
        }
        let op_label = match (prefix, inc) {
            (true, true) => "prefix:<++>",
            (true, false) => "prefix:<-->",
            (false, true) => "postfix:<++>",
            (false, false) => "postfix:<-->",
        };
        self.compile_expr(&Expr::Call {
            name: Symbol::intern("__mutsu_incdec_named_sub_lvalue"),
            args: vec![
                Expr::Literal(Value::str(name.resolve())),
                Expr::ArrayLiteral(args.clone()),
                Expr::Literal(Value::str_from(op_label)),
            ],
        });
        true
    }

    /// Compile prefix `++`/`--` on an rw-accessor lvalue (`++$obj.count`).
    ///
    /// The postfix forms already route a method-call target through the
    /// `__mutsu_assign_method_lvalue` writeback; the prefix forms had no
    /// `MethodCall` arm at all and fell through to `__mutsu_incdec_nomatch`,
    /// so `++$obj.count` died with "Cannot resolve caller prefix:<++>(...); the
    /// parameter requires mutable arguments" while `$obj.count++` and
    /// `$obj.count += 1` both worked. Cro's session middleware writes
    /// `content 'text/plain', 'Visit ' ~ ++$session.count`, so every session
    /// route answered with an empty body.
    ///
    /// A `BareWord` target (`++Foo.counter`, the type object itself) is
    /// accepted too: `Foo.counter += 1` already routes the same way through
    /// `method_lvalue_roundtrip_assign_expr` (`compound_expr.rs`), which
    /// re-evaluates the *original* target expression — `Expr::BareWord`, not
    /// `Expr::Var` — to fetch the class-level-attribute accessor's Package
    /// invocant. Reusing `Self::method_call_incdec_lvalue_target` here keeps
    /// the increment/decrement forms consistent with that existing compound-
    /// assignment behavior instead of only ever looking up an env variable.
    ///
    /// Returns `false` when `expr` is not a method call on a recognized
    /// target, so the caller keeps its existing fallback.
    pub(super) fn compile_prefix_incdec_method_lvalue(&mut self, expr: &Expr, inc: bool) -> bool {
        let Expr::MethodCall {
            target, name, args, ..
        } = expr
        else {
            return false;
        };
        let Some((target_reeval, target_var)) = Self::method_call_incdec_lvalue_target(target)
        else {
            return false;
        };
        let tmp_value_name = format!("__mutsu_tmp_method_preinc_{}", self.code.constants.len());
        let tmp_value_idx = self.code.add_constant(Value::str(tmp_value_name.clone()));
        // Read the accessor, increment the temp in place (prefix leaves the NEW
        // value on the stack), then write the temp back through the accessor and
        // yield the new value.
        self.compile_expr(expr);
        // The accessor read may hand back the target's own container
        // (a Pair whose value was captured from a variable, an `is rw`
        // accessor returning its cell). The temp is a value SNAPSHOT,
        // never an alias: without this deref the temp global stays
        // bound to that cell, and on the next iteration of a loop
        // `SetGlobal` writes the freshly-read cell *through* it —
        // storing the container into itself, which stalls the
        // increment and then makes every later read recurse forever.
        self.code.emit(OpCode::DerefContainer);
        self.code.emit(OpCode::SetGlobal(tmp_value_idx));
        if inc {
            self.code.emit(OpCode::PreIncrement(tmp_value_idx, None));
        } else {
            self.code.emit(OpCode::PreDecrement(tmp_value_idx, None));
        }
        self.code.emit(OpCode::Pop);
        let assign_expr = Expr::Call {
            name: Symbol::intern("__mutsu_assign_method_lvalue"),
            args: vec![
                target_reeval,
                Expr::Literal(Value::str(name.resolve())),
                Expr::ArrayLiteral(args.clone()),
                Expr::Var(tmp_value_name.clone()),
                Expr::Literal(Value::str(target_var)),
            ],
        };
        self.compile_expr(&assign_expr);
        self.code.emit(OpCode::Pop);
        self.compile_expr(&Expr::Var(tmp_value_name));
        true
    }

    /// Shared target extraction for postfix/prefix `++`/`--` on a method-call
    /// lvalue (`$obj.count++`, `++$obj.count`, `Foo.counter++`).
    ///
    /// Returns `(expr-to-re-evaluate-the-invocant, name-string-for-writeback)`.
    /// For a plain/array/hash variable the re-evaluation expression is an
    /// `Expr::Var` under the sigil-prefixed name (matching the long-standing
    /// convention here and in `method_lvalue_roundtrip_assign_expr`). For a
    /// `BareWord` target — a package/type-object invocant such as `Foo` in
    /// `Foo.counter++`, which is not itself a lexical variable — the
    /// re-evaluation expression must stay `Expr::BareWord` so it resolves to
    /// the package rather than failing an env lookup; the name string is only
    /// ever used by the callee for an optional (and, for a type object,
    /// unused) env write-back, so reusing the bareword text there is
    /// harmless — the class-level-attribute arm of
    /// `assign_method_lvalue_with_values` never consults it.
    fn method_call_incdec_lvalue_target(target: &Expr) -> Option<(Expr, String)> {
        match target {
            Expr::Var(n) => Some((Expr::Var(n.clone()), n.clone())),
            Expr::ArrayVar(n) => {
                let name = format!("@{}", n);
                Some((Expr::Var(name.clone()), name))
            }
            Expr::HashVar(n) => {
                let name = format!("%{}", n);
                Some((Expr::Var(name.clone()), name))
            }
            Expr::BareWord(n) => Some((Expr::BareWord(n.clone()), n.clone())),
            _ => None,
        }
    }

    /// Compile postfix ++ on variable/index/method target.
    pub(super) fn compile_expr_postfix_inc(&mut self, expr: &Expr) {
        if let Some(var) = Self::temp_call_var(expr) {
            // `(temp $c)++`: `temp $c` temporizes `$c` (saved for restoration at
            // scope exit) and yields it as an lvalue, so `++` post-increments the
            // live variable — mirrors the prefix `++temp $c` handling.
            self.emit_temp_save(&var);
            let slot = self.local_map.get(&var).copied();
            let name_idx = self.code.add_constant(Value::str(var));
            self.code.emit(OpCode::PostIncrement(name_idx, slot));
        } else if let Expr::Var(name) = expr {
            if name.starts_with('!') && name.len() > 1 {
                self.alloc_local(name);
            }
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            let slot = self.local_map.get(name).copied();
            self.code.emit(OpCode::PostIncrement(name_idx, slot));
        } else if let Expr::BareWord(name) = expr {
            if self.sigilless_locals.contains(name.as_str()) {
                // Sigilless rw binding (e.g. for-loop `-> \v { v++ }`): the bare
                // word IS the env var (no local slot), so increment it by name.
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::PostIncrement(name_idx, None));
            } else {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("__mutsu_incdec_nomatch"),
                    args: vec![Expr::Literal(Value::str_from("postfix:<++>"))],
                });
            }
        } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
            // state/my declarator in expression position: `state $x++`, `my $x.++`
            self.compile_expr(expr);
            self.code.emit(OpCode::Pop);
            let slot = self.local_map.get(&var_name).copied();
            let name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::PostIncrement(name_idx, slot));
        } else if let Expr::Index { target, index, .. } = expr {
            if let Some(name) = Self::postfix_index_name(target) {
                self.compile_expr(index);
                // §1.5: bake the base container's scope-correct slot (as
                // `IndexAssignExprNamed` already does), so a shadowing inner
                // `my $b` is not resolved to the outer `$b`'s slot by name.
                let target_slot = self.local_map.get(&name).copied();
                let name_idx = self.code.add_constant(Value::str(name));
                self.code
                    .emit(OpCode::PostIncrementIndex(name_idx, target_slot));
            } else {
                // Nested index (e.g. $foo[0][0]++): read old value, increment,
                // write back via IndexAssign, and return old value.
                self.compile_nested_postfix_incdec(expr, true);
            }
        } else if let Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } = expr
        {
            // Extract the invocant target from Var/ArrayVar/HashVar/BareWord
            // targets (`Foo.counter++` on a class-level attribute accessor's
            // type object goes through the BareWord arm — see
            // `method_call_incdec_lvalue_target`).
            let target_info = Self::method_call_incdec_lvalue_target(target);
            if let Some((target_reeval, target_var)) = target_info {
                let tmp_value_name =
                    format!("__mutsu_tmp_method_inc_{}", self.code.constants.len());
                let tmp_result_name = format!(
                    "__mutsu_tmp_method_inc_result_{}",
                    self.code.constants.len()
                );
                let tmp_value_idx = self.code.add_constant(Value::str(tmp_value_name.clone()));
                let tmp_result_idx = self.code.add_constant(Value::str(tmp_result_name.clone()));
                self.compile_expr(expr);
                // The accessor read may hand back the target's own container
                // (a Pair whose value was captured from a variable, an `is rw`
                // accessor returning its cell). The temp is a value SNAPSHOT,
                // never an alias: without this deref the temp global stays
                // bound to that cell, and on the next iteration of a loop
                // `SetGlobal` writes the freshly-read cell *through* it —
                // storing the container into itself, which stalls the
                // increment and then makes every later read recurse forever.
                self.code.emit(OpCode::DerefContainer);
                self.code.emit(OpCode::SetGlobal(tmp_value_idx));
                self.code.emit(OpCode::PostIncrement(tmp_value_idx, None));
                self.code.emit(OpCode::SetGlobal(tmp_result_idx));
                let assign_expr = Expr::Call {
                    name: Symbol::intern("__mutsu_assign_method_lvalue"),
                    args: vec![
                        target_reeval,
                        Expr::Literal(Value::str(name.resolve())),
                        Expr::ArrayLiteral(args.clone()),
                        Expr::Var(tmp_value_name),
                        Expr::Literal(Value::str(target_var)),
                    ],
                };
                self.compile_expr(&assign_expr);
                self.code.emit(OpCode::Pop);
                self.compile_expr(&Expr::Var(tmp_result_name));
            } else {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("__mutsu_incdec_nomatch"),
                    args: vec![Expr::Literal(Value::str_from("postfix:<++>"))],
                });
            }
        } else if !self.compile_incdec_named_sub_lvalue(expr, true, false) {
            self.compile_expr(&Expr::Call {
                name: Symbol::intern("__mutsu_incdec_nomatch"),
                args: vec![Expr::Literal(Value::str_from("postfix:<++>"))],
            });
        }
    }

    /// Compile postfix -- on variable/index/method target.
    pub(super) fn compile_expr_postfix_dec(&mut self, expr: &Expr) {
        if let Some(var) = Self::temp_call_var(expr) {
            // `(temp $c)--`: temporize `$c` then post-decrement it (see the
            // `(temp $c)++` case above).
            self.emit_temp_save(&var);
            let slot = self.local_map.get(&var).copied();
            let name_idx = self.code.add_constant(Value::str(var));
            self.code.emit(OpCode::PostDecrement(name_idx, slot));
        } else if let Expr::Var(name) = expr {
            if name.starts_with('!') && name.len() > 1 {
                self.alloc_local(name);
            }
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            let slot = self.local_map.get(name).copied();
            self.code.emit(OpCode::PostDecrement(name_idx, slot));
        } else if let Expr::BareWord(name) = expr {
            if self.sigilless_locals.contains(name.as_str()) {
                // Sigilless rw binding (e.g. for-loop `-> \v { v-- }`) — env var.
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::PostDecrement(name_idx, None));
            } else {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("__mutsu_incdec_nomatch"),
                    args: vec![Expr::Literal(Value::str_from("postfix:<-->"))],
                });
            }
        } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
            self.compile_expr(expr);
            self.code.emit(OpCode::Pop);
            let slot = self.local_map.get(&var_name).copied();
            let name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::PostDecrement(name_idx, slot));
        } else if let Expr::Index { target, index, .. } = expr {
            if let Some(name) = Self::postfix_index_name(target) {
                self.compile_expr(index);
                let target_slot = self.local_map.get(&name).copied();
                let name_idx = self.code.add_constant(Value::str(name));
                self.code
                    .emit(OpCode::PostDecrementIndex(name_idx, target_slot));
            } else {
                // Nested index (e.g. $foo[0][0]--): read old value, decrement,
                // write back via IndexAssign, and return old value.
                self.compile_nested_postfix_incdec(expr, false);
            }
        } else if let Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } = expr
        {
            // Extract the invocant target from Var/ArrayVar/HashVar/BareWord
            // targets (see `method_call_incdec_lvalue_target`).
            let target_info = Self::method_call_incdec_lvalue_target(target);
            if let Some((target_reeval, target_var)) = target_info {
                let tmp_value_name =
                    format!("__mutsu_tmp_method_dec_{}", self.code.constants.len());
                let tmp_result_name = format!(
                    "__mutsu_tmp_method_dec_result_{}",
                    self.code.constants.len()
                );
                let tmp_value_idx = self.code.add_constant(Value::str(tmp_value_name.clone()));
                let tmp_result_idx = self.code.add_constant(Value::str(tmp_result_name.clone()));
                self.compile_expr(expr);
                // The accessor read may hand back the target's own container
                // (a Pair whose value was captured from a variable, an `is rw`
                // accessor returning its cell). The temp is a value SNAPSHOT,
                // never an alias: without this deref the temp global stays
                // bound to that cell, and on the next iteration of a loop
                // `SetGlobal` writes the freshly-read cell *through* it —
                // storing the container into itself, which stalls the
                // increment and then makes every later read recurse forever.
                self.code.emit(OpCode::DerefContainer);
                self.code.emit(OpCode::SetGlobal(tmp_value_idx));
                self.code.emit(OpCode::PostDecrement(tmp_value_idx, None));
                self.code.emit(OpCode::SetGlobal(tmp_result_idx));
                let assign_expr = Expr::Call {
                    name: Symbol::intern("__mutsu_assign_method_lvalue"),
                    args: vec![
                        target_reeval,
                        Expr::Literal(Value::str(name.resolve())),
                        Expr::ArrayLiteral(args.clone()),
                        Expr::Var(tmp_value_name),
                        Expr::Literal(Value::str(target_var)),
                    ],
                };
                self.compile_expr(&assign_expr);
                self.code.emit(OpCode::Pop);
                self.compile_expr(&Expr::Var(tmp_result_name));
            } else {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("__mutsu_incdec_nomatch"),
                    args: vec![Expr::Literal(Value::str_from("postfix:<-->"))],
                });
            }
        } else if !self.compile_incdec_named_sub_lvalue(expr, false, false) {
            self.compile_expr(&Expr::Call {
                name: Symbol::intern("__mutsu_incdec_nomatch"),
                args: vec![Expr::Literal(Value::str_from("postfix:<-->"))],
            });
        }
    }

    /// Compile postfix ++/-- on a nested index expression (e.g. `$foo[0][0]++`).
    /// `expr` is the full Index expression (the operand of PostfixOp).
    /// `increment` is true for ++, false for --.
    ///
    /// Strategy:
    /// 1. Read old value into tmp_val
    /// 2. PostIncrement/PostDecrement on tmp_val (returns old value, stores new)
    /// 3. Save old value to tmp_old
    /// 4. Write back tmp_val (which now has new value) via IndexAssign
    /// 5. Return tmp_old (the old value before increment)
    fn compile_nested_postfix_incdec(&mut self, expr: &Expr, increment: bool) {
        if let Expr::Index {
            target,
            index,
            is_positional,
            ..
        } = expr
        {
            let tmp_val = format!("__mutsu_nested_incdec_val_{}", self.code.constants.len());
            let tmp_val_idx = self.code.add_constant(Value::str(tmp_val.clone()));
            let tmp_old = format!("__mutsu_nested_incdec_old_{}", self.code.constants.len());
            let tmp_old_idx = self.code.add_constant(Value::str(tmp_old.clone()));

            // 1. Read current value and store in tmp_val
            self.compile_expr(expr);
            self.code.emit(OpCode::SetGlobal(tmp_val_idx));
            self.code.emit(OpCode::Pop);

            // 2. PostIncrement/PostDecrement on tmp_val:
            //    - pushes old value on stack
            //    - sets tmp_val = old +/- 1
            if increment {
                self.code.emit(OpCode::PostIncrement(tmp_val_idx, None));
            } else {
                self.code.emit(OpCode::PostDecrement(tmp_val_idx, None));
            }
            // Stack now has old value; tmp_val has new value
            self.code.emit(OpCode::SetGlobal(tmp_old_idx));
            self.code.emit(OpCode::Pop);

            // 3. Write back the new value (tmp_val) via IndexAssign. Preserve the
            //    subscript's positional/associative kind so an autovivified
            //    intermediate container is a Hash for `%h<a><b>++` (associative)
            //    and an Array for `@a[0][1]++` (positional) — a hardcoded `true`
            //    here wrongly made `%h<a><b>++` create an Array.
            let assign_expr = Expr::IndexAssign {
                target: target.clone(),
                index: index.clone(),
                value: Box::new(Expr::Var(tmp_val)),
                is_positional: *is_positional,
            };
            self.compile_expr(&assign_expr);
            self.code.emit(OpCode::Pop);

            // 4. Push the old value as the result of the post-increment expression
            self.code.emit(OpCode::GetGlobal(tmp_old_idx));
        }
    }

    /// Compile prefix ++/-- on a nested index expression (e.g. `++$foo[0][0]`).
    /// `expr` is the full Index expression (the operand of UnaryOp).
    /// `increment` is true for ++, false for --.
    ///
    /// Strategy:
    /// 1. Read old value into tmp_val
    /// 2. PreIncrement/PreDecrement on tmp_val (returns new value, stores new)
    /// 3. Write back tmp_val via IndexAssign
    /// 4. Return the new value
    pub(super) fn compile_nested_prefix_incdec(&mut self, expr: &Expr, increment: bool) {
        if let Expr::Index {
            target,
            index,
            is_positional,
            ..
        } = expr
        {
            let tmp_val = format!("__mutsu_nested_preincdec_val_{}", self.code.constants.len());
            let tmp_val_idx = self.code.add_constant(Value::str(tmp_val.clone()));

            // 1. Read current value and store in tmp_val
            self.compile_expr(expr);
            self.code.emit(OpCode::SetGlobal(tmp_val_idx));
            self.code.emit(OpCode::Pop);

            // 2. PreIncrement/PreDecrement on tmp_val:
            //    - modifies tmp_val in place
            //    - pushes new value on stack
            if increment {
                self.code.emit(OpCode::PreIncrement(tmp_val_idx, None));
            } else {
                self.code.emit(OpCode::PreDecrement(tmp_val_idx, None));
            }
            // Stack now has new value; tmp_val also has new value
            // Save new value, we'll push it back at the end
            let tmp_new = format!("__mutsu_nested_preincdec_new_{}", self.code.constants.len());
            let tmp_new_idx = self.code.add_constant(Value::str(tmp_new.clone()));
            self.code.emit(OpCode::SetGlobal(tmp_new_idx));
            self.code.emit(OpCode::Pop);

            // 3. Write back the new value via IndexAssign (preserve subscript kind
            //    so `%h<a><b>` autovivifies a Hash, `@a[0][1]` an Array).
            let assign_expr = Expr::IndexAssign {
                target: target.clone(),
                index: index.clone(),
                value: Box::new(Expr::Var(tmp_val)),
                is_positional: *is_positional,
            };
            self.compile_expr(&assign_expr);
            self.code.emit(OpCode::Pop);

            // 4. Push the new value as the result
            self.code.emit(OpCode::GetGlobal(tmp_new_idx));
        }
    }
}
