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
                // `%.attr` is `self.attr` in *hash* context — call the accessor and
                // coerce the result to a Hash (e.g. an `@.a = (1,2,3,4)` attribute
                // read as `%.a` yields `{1 => 2, 3 => 4}`). Mirrors the `$.attr`
                // item-context accessor in `compile_expr_var`.
                if let Some(attr_name) = name.strip_prefix('.')
                    && !attr_name.is_empty()
                {
                    self.emit_load_self_for_accessor(&format!("%.{}", attr_name));
                    let method_idx = self.code.add_constant(Value::str(attr_name.to_string()));
                    self.code.emit(OpCode::CallMethod {
                        name_idx: method_idx,
                        arity: 0,
                        modifier_idx: None,
                        quoted: false,
                        arg_sources_idx: None,
                    });
                    let hash_idx = self.code.add_constant(Value::str_from("hash"));
                    self.code.emit(OpCode::CallMethod {
                        name_idx: hash_idx,
                        arity: 0,
                        modifier_idx: None,
                        quoted: false,
                        arg_sources_idx: None,
                    });
                    return;
                }
                let sigiled = format!("%{}", name);
                let var_name = if self.local_map.contains_key(sigiled.as_str()) {
                    sigiled
                } else {
                    self.qualify_variable_name(&format!("%{}", name))
                };
                let name_idx = self.code.add_constant(Value::str(var_name));
                self.code.emit(OpCode::GetHashVar(name_idx));
            }
            Expr::BareWord(name) if name == "done" => {
                // `done` as a bare term in expression position (e.g. the `!!`
                // branch of `$cond ?? die !! done`) is the supply/react
                // completion control flow, not an ordinary bareword. Emit
                // ReactDone so it signals instead of evaluating to a no-op string
                // (which left `whenever ... { ... !! done }` never completing).
                self.code.emit(OpCode::ReactDone);
            }
            Expr::BareWord(name) => {
                // Only resolve to GetLocal for sigilless bindings (e.g. `my \Foo = ...`)
                // or builtin-type-safe locals.  `$`-sigiled variables whose `$` was
                // stripped share the same key in local_map but must NOT shadow type
                // names, so they go through GetBareWord which checks the type registry.
                if let Some(&slot) = self.local_map.get(name.as_str()) {
                    if crate::runtime::Interpreter::is_builtin_type(name) {
                        let name_idx = self.code.add_constant(Value::str(name.clone()));
                        self.code.emit(OpCode::GetBareWord(name_idx));
                    } else if self.sigilless_locals.contains(name.as_str())
                        || self.constant_vars_in_scope.contains(name.as_str())
                        || name == "self"
                        || name == "__ANON_STATE__"
                    {
                        // Sigilless bindings and in-scope constants: the bare word
                        // IS the variable, so read directly from the local slot.
                        // (Out-of-scope constants fall through to GetBareWord, which
                        // resolves them as `our`-scoped package globals.)
                        self.code.emit(OpCode::GetLocal(slot));
                    } else {
                        // The local is a `$`-sigiled variable — a bare word with the
                        // same name should resolve as a type/package, not the variable.
                        let name_idx = self.code.add_constant(Value::str(name.clone()));
                        self.code.emit(OpCode::GetBareWord(name_idx));
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
                // Elements are stored into the list -> a closure element escapes.
                self.with_escape(true, |c| {
                    for elem in elems {
                        c.compile_expr(elem);
                        if Self::expr_is_scalar_var(elem) {
                            c.code.emit(OpCode::Itemize);
                        }
                    }
                });
                self.code.emit(OpCode::MakeArray(elems.len() as u32));
            }
            Expr::BracketArray(elems, trailing_comma) => {
                self.compile_expr_bracket_array(elems, *trailing_comma);
            }
            Expr::CaptureLiteral(items) => {
                self.with_escape(true, |c| {
                    for item in items {
                        // A named scalar-var element (`\(:$a)` -> `a => $a`)
                        // captures the variable's container too, so `$c<a>` aliases
                        // `$a`. Compile key + value, tag the value with WrapVarRef,
                        // then MakePair; MakeCapture boxes the named local.
                        if let Expr::Binary { op, left, right } = item
                            && *op == crate::token_kind::TokenKind::FatArrow
                            && let Expr::Var(name) = right.as_ref()
                            && !name.contains("::")
                        {
                            c.compile_expr(left);
                            c.compile_expr(right);
                            let name_idx = c.code.add_constant(Value::str(name.clone()));
                            c.code.emit(OpCode::WrapVarRef(name_idx));
                            c.code.emit(OpCode::MakePair);
                            continue;
                        }
                        c.compile_expr(item);
                        // A plain scalar variable positional (`\($a)`) captures the
                        // variable's *container*, so `$c[0]` aliases `$a` and
                        // `$c[0]++` writes through. Tag it with its source name via
                        // WrapVarRef; MakeCapture boxes the named local into a shared
                        // cell. Non-variable items capture by value.
                        if let Expr::Var(name) = item
                            && !name.contains("::")
                        {
                            let name_idx = c.code.add_constant(Value::str(name.clone()));
                            c.code.emit(OpCode::WrapVarRef(name_idx));
                        }
                    }
                });
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
            // Compile-time fold: Nil.gist / Nil.raku / Nil.perl → "Nil"
            // Value::Nil is also used for uninitialized variables (which are Any type objects),
            // so we can only constant-fold when the target is the *literal* Nil keyword.
            Expr::MethodCall {
                target, name, args, ..
            } if matches!(target.as_ref(), Expr::Literal(Value::Nil))
                && args.is_empty()
                && matches!(name.resolve().as_str(), "gist" | "raku" | "perl") =>
            {
                let idx = self.code.add_constant(Value::str_from("Nil"));
                self.code.emit(OpCode::LoadConst(idx));
            }
            // `Nil.push` / `.append` / `.unshift` / `.prepend` on a *literal* Nil
            // is illegal in Raku ("Use of Nil.<method> not allowed"). This must be
            // distinguished from autovivification of a runtime-undefined container
            // element (`@a[5].push`) or variable (`$x.push`), which is legal -- so
            // we only reject the literal `Nil` keyword here, matching the gist/raku
            // fold above.
            Expr::MethodCall { target, name, .. }
                if matches!(target.as_ref(), Expr::Literal(Value::Nil))
                    && matches!(
                        name.resolve().as_str(),
                        "push" | "append" | "unshift" | "prepend"
                    ) =>
            {
                let msg = format!("Use of Nil.{} not allowed", name.resolve());
                let idx = self.code.add_constant(Value::str(msg));
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die);
            }
            // Method call on nested-index target with mutating method -- writeback via IndexAssign.
            // e.g. %h<a><b>.push(1, 2) => %h<a><b> = %h<a><b>.push(1, 2)
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } if Self::is_nested_mutating_method_on_index(target, name) => {
                self.compile_expr_nested_method_writeback(target, name, args, modifier, *quoted);
            }
            // `lazy for ITERABLE { BODY }` => MakeGather wrapping the for loop
            // so the body is not evaluated prematurely.
            Expr::MethodCall {
                target, name, args, ..
            } if name.resolve().as_str() == "lazy"
                && args.is_empty()
                && Self::is_dostmt_for(target) =>
            {
                if let Some(gather_block) = Self::make_lazy_for_gather(target) {
                    let idx = self.code.add_stmt(Stmt::Block(gather_block));
                    self.code.emit(OpCode::MakeGather(idx));
                } else {
                    self.compile_expr_method_generic(target, name, args, &None, false);
                }
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
                modifier,
            } => {
                self.compile_expr_dynamic_method(target, name_expr, args, modifier);
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
            Expr::Index {
                target,
                index,
                is_positional,
                ..
            } => {
                self.compile_expr_index(target, index, *is_positional);
            }
            // Multi-dimensional indexing: @a[$x;$y;$z]
            Expr::MultiDimIndex { target, dimensions } => {
                self.compile_expr(target);
                for dim in dimensions {
                    self.compile_expr(dim);
                }
                self.code
                    .emit(OpCode::MultiDimIndex(dimensions.len() as u32));
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
            Expr::AssignExpr {
                name,
                expr,
                is_bind,
            } => {
                self.compile_expr_assign(name, expr, *is_bind);
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
                // For short-circuit operators with a literal argument list, compile
                // inline with short-circuit bytecode to preserve lazy evaluation semantics.
                let base_op = op.strip_prefix('\\').unwrap_or(op.as_str());
                let is_scan = op.starts_with('\\');
                // `[Z&&]` / `[Z||]` / `[Zand]` / `[Zor]` etc. reduce a list of
                // lists by left-folding the binary short-circuiting `Z` meta-op,
                // which thunks its right operand. Rewrite into a left-nested
                // MetaOp tree so the per-element thunking is preserved.
                if !is_scan
                    && let Some(zop) = base_op.strip_prefix('Z')
                    && matches!(zop, "and" | "&&" | "or" | "||" | "andthen" | "orelse")
                    && let Expr::ArrayLiteral(items) = expr.as_ref()
                    && items.len() >= 2
                {
                    let mut iter = items.iter();
                    let mut acc = iter.next().unwrap().clone();
                    for item in iter {
                        acc = Expr::MetaOp {
                            meta: "Z".to_string(),
                            op: zop.to_string(),
                            left: Box::new(acc),
                            right: Box::new(item.clone()),
                        };
                    }
                    self.compile_expr(&acc);
                    return;
                }
                // `[=:=]` / `[!=:=]` are chain-associative container-identity
                // reductions. The binary `=:=` operator resolves container
                // identity from the operand expressions (variable names / index
                // sources), which is lost once operands are evaluated into a
                // value list. Rewrite into a conjunction of pairwise binary
                // comparisons so identity is preserved.
                if !is_scan
                    && matches!(base_op, "=:=" | "!=:=")
                    && let Expr::ArrayLiteral(items) = expr.as_ref()
                    && items.len() >= 2
                {
                    let negate = base_op == "!=:=";
                    let mut acc: Option<Expr> = None;
                    for pair in items.windows(2) {
                        // `=:=` resolves container identity from the operand
                        // expressions; `!=:=` is parsed as `!(... =:= ...)`.
                        let mut cmp = Expr::Binary {
                            left: Box::new(pair[0].clone()),
                            op: TokenKind::Ident("=:=".to_string()),
                            right: Box::new(pair[1].clone()),
                        };
                        if negate {
                            cmp = Expr::Unary {
                                op: TokenKind::Bang,
                                expr: Box::new(cmp),
                            };
                        }
                        acc = Some(match acc {
                            None => cmp,
                            Some(prev) => Expr::Binary {
                                left: Box::new(prev),
                                op: TokenKind::AndAnd,
                                right: Box::new(cmp),
                            },
                        });
                    }
                    // Each `=:=` / `!=:=` yields a Bool, so the &&-chain result
                    // is already a Bool (no further coercion needed).
                    let combined = acc.unwrap();
                    self.compile_expr(&combined);
                    return;
                }
                // `[=] $a, $b, $c, 42` is a right-associative assignment reduce:
                // `$a = ($b = ($c = 42))`. Rewrite into nested assignments so the
                // values are written back through the variable containers.
                if !is_scan
                    && base_op == "="
                    && let Expr::ArrayLiteral(items) = expr.as_ref()
                    && items.len() >= 2
                    && items[..items.len() - 1]
                        .iter()
                        .all(|e| matches!(e, Expr::Var(_)))
                {
                    let mut acc = items.last().unwrap().clone();
                    for target in items[..items.len() - 1].iter().rev() {
                        if let Expr::Var(name) = target {
                            acc = Expr::AssignExpr {
                                name: name.clone(),
                                expr: Box::new(acc),
                                is_bind: false,
                            };
                        }
                    }
                    self.compile_expr(&acc);
                    return;
                }
                if matches!(
                    base_op,
                    "&&" | "||" | "and" | "or" | "//" | "andthen" | "orelse" | "^^" | "xor"
                ) && let Expr::ArrayLiteral(items) = expr.as_ref()
                {
                    // Only use the short-circuit path when no element might
                    // produce a Slip (which needs flattening into more args).
                    let has_slip = items.iter().any(Self::expr_may_produce_slip);
                    if !has_slip {
                        // xor/^^ always use thunk approach (both scan and non-scan)
                        // for other operators, use inline for non-scan
                        if is_scan || matches!(base_op, "^^" | "xor") {
                            self.compile_thunk_reduction(base_op, items, is_scan);
                        } else {
                            self.compile_shortcircuit_reduction(base_op, items);
                        }
                        return;
                    }
                }
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
            Expr::Itemize(inner) => {
                self.compile_expr(inner);
                self.code.emit(OpCode::Itemize);
            }
            Expr::PositionalPair(inner) => {
                self.compile_expr(inner);
                self.code.emit(OpCode::ContainerizePair);
            }
            Expr::CallOn { target, args } => {
                self.compile_expr_call_on(target, args);
            }
            Expr::AnonSub {
                body,
                is_rw,
                is_block,
            } => {
                self.compile_expr_anon_sub(body, *is_rw, *is_block);
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
                is_positional,
            } => {
                self.compile_expr_index_assign(target, index, value, *is_positional);
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

    /// Returns true if the expression might produce a Slip value when evaluated.
    /// Used to detect cases where argument flattening is needed before reduction.
    fn expr_may_produce_slip(expr: &Expr) -> bool {
        // slip() call (which is how |expr is parsed)
        matches!(expr, Expr::Call { name, .. } if name.resolve() == "slip")
    }

    /// Compile a non-scan short-circuit reduction `[op] items` inline.
    /// This ensures that elements after a short-circuit point are not evaluated.
    ///
    /// For `&&` / `and`: short-circuit on first false (like chained `&&`)
    /// For `||` / `or`: short-circuit on first truthy (like chained `||`)
    /// For `//`:        short-circuit on first defined (like chained `//`)
    fn compile_shortcircuit_reduction(&mut self, op: &str, items: &[Expr]) {
        if items.is_empty() {
            // Identity value
            match op {
                "&&" | "and" => {
                    self.code.emit(OpCode::LoadTrue);
                }
                "//" | "orelse" => {
                    // [//] () and [orelse] () return Any (type object)
                    let any_idx = self
                        .code
                        .add_constant(Value::Package(crate::symbol::Symbol::intern("Any")));
                    self.code.emit(OpCode::LoadConst(any_idx));
                }
                _ => {
                    self.code.emit(OpCode::LoadFalse);
                }
            }
            return;
        }
        if items.len() == 1 {
            self.compile_expr(&items[0]);
            return;
        }

        match op {
            "&&" | "and" => {
                // [&&] a, b, c, d  =>  a && b && c && d
                // If any element is false, return it and don't evaluate the rest.
                // Pattern:
                //   compile a; dup; JumpIfFalse end; pop
                //   compile b; dup; JumpIfFalse end; pop
                //   ...
                //   compile last
                //   end: (all jumps patch here)
                let mut jump_ends = Vec::new();
                let last_idx = items.len() - 1;
                for (i, item) in items.iter().enumerate() {
                    self.compile_expr(item);
                    if i < last_idx {
                        self.code.emit(OpCode::Dup);
                        let je = self.code.emit(OpCode::JumpIfFalse(0));
                        jump_ends.push(je);
                        self.code.emit(OpCode::Pop);
                    }
                }
                for je in jump_ends {
                    self.code.patch_jump(je);
                }
            }
            "||" | "or" => {
                // [||] a, b, c, d  =>  a || b || c || d
                // If any element is truthy, return it and don't evaluate the rest.
                // Pattern (for n > 1):
                //   compile a; dup; JumpIfTrue found; pop; pop
                //   compile b; dup; JumpIfTrue found; pop; pop
                //   ...
                //   compile last; Jump done
                //   found: pop
                //   done:
                let mut jump_founds = Vec::new();
                let last_idx = items.len() - 1;
                for (i, item) in items.iter().enumerate() {
                    self.compile_expr(item);
                    if i < last_idx {
                        self.code.emit(OpCode::Dup);
                        let jf = self.code.emit(OpCode::JumpIfTrue(0));
                        jump_founds.push(jf);
                        self.code.emit(OpCode::Pop);
                        self.code.emit(OpCode::Pop);
                    }
                }
                // Last element: jump over "found: pop"
                let jump_done = self.code.emit(OpCode::Jump(0));
                // found: pop (remove dup copy, original stays as result)
                let found_pos = self.code.current_pos();
                self.code.emit(OpCode::Pop);
                // done:
                let done_pos = self.code.current_pos();
                // Patch all JumpIfTrue to "found"
                for jf in jump_founds {
                    self.code.patch_jump_to(jf, found_pos);
                }
                // Patch jump_done to "done"
                self.code.patch_jump_to(jump_done, done_pos);
            }
            "//" => {
                // [//] a, b, c, d  =>  a // b // c // d
                // Return first defined value.
                // Same pattern as || but using JumpIfNotNil (peek-based).
                let mut jump_founds = Vec::new();
                let last_idx = items.len() - 1;
                for (i, item) in items.iter().enumerate() {
                    self.compile_expr(item);
                    if i < last_idx {
                        self.code.emit(OpCode::Dup);
                        let jf = self.code.emit(OpCode::JumpIfNotNil(0));
                        jump_founds.push(jf);
                        self.code.emit(OpCode::Pop);
                        self.code.emit(OpCode::Pop);
                    }
                }
                let jump_done = self.code.emit(OpCode::Jump(0));
                let found_pos = self.code.current_pos();
                self.code.emit(OpCode::Pop);
                let done_pos = self.code.current_pos();
                for jf in jump_founds {
                    self.code.patch_jump_to(jf, found_pos);
                }
                self.code.patch_jump_to(jump_done, done_pos);
            }
            "andthen" => {
                // [andthen] a, b, c  =>  a andthen b andthen c
                // If any element is undefined, return Empty and don't evaluate the rest.
                // Pattern:
                //   compile a; dup; CallDefined; JumpIfFalse undef; pop
                //   compile b; dup; CallDefined; JumpIfFalse undef; pop
                //   compile last
                //   Jump done
                //   undef: pop; LoadEmpty
                //   done:
                let mut jump_undefs = Vec::new();
                let last_idx = items.len() - 1;
                for (i, item) in items.iter().enumerate() {
                    self.compile_expr(item);
                    if i < last_idx {
                        self.code.emit(OpCode::Dup);
                        self.code.emit(OpCode::CallDefined);
                        let ju = self.code.emit(OpCode::JumpIfFalse(0));
                        jump_undefs.push(ju);
                        self.code.emit(OpCode::Pop);
                    }
                }
                let jump_done = self.code.emit(OpCode::Jump(0));
                let undef_pos = self.code.current_pos();
                // undef: pop acc and push Empty
                self.code.emit(OpCode::Pop);
                let empty_idx = self.code.add_constant(Value::slip(vec![]));
                self.code.emit(OpCode::LoadConst(empty_idx));
                let done_pos = self.code.current_pos();
                for ju in jump_undefs {
                    self.code.patch_jump_to(ju, undef_pos);
                }
                self.code.patch_jump_to(jump_done, done_pos);
            }
            "orelse" => {
                // [orelse] a, b, c  =>  a orelse b orelse c
                // If any element is defined, return it and don't evaluate the rest.
                // Same pattern as // (defined-or).
                let mut jump_founds = Vec::new();
                let last_idx = items.len() - 1;
                for (i, item) in items.iter().enumerate() {
                    self.compile_expr(item);
                    if i < last_idx {
                        self.code.emit(OpCode::Dup);
                        let jf = self.code.emit(OpCode::JumpIfNotNil(0));
                        jump_founds.push(jf);
                        self.code.emit(OpCode::Pop);
                        self.code.emit(OpCode::Pop);
                    }
                }
                let jump_done = self.code.emit(OpCode::Jump(0));
                let found_pos = self.code.current_pos();
                self.code.emit(OpCode::Pop);
                let done_pos = self.code.current_pos();
                for jf in jump_founds {
                    self.code.patch_jump_to(jf, found_pos);
                }
                self.code.patch_jump_to(jump_done, done_pos);
            }
            _ => {
                // Fallback: compile as normal array and use Reduction opcode
                for item in items {
                    self.compile_expr(item);
                    if Self::expr_is_scalar_var(item) {
                        self.code.emit(OpCode::Itemize);
                    }
                }
                self.code.emit(OpCode::MakeArray(items.len() as u32));
                let op_idx = self.code.add_constant(Value::str(op.to_string()));
                self.code.emit(OpCode::Reduction(op_idx));
            }
        }
    }

    /// Compile a thunk-based short-circuit reduction.
    /// Each item is wrapped as an anonymous block (thunk), then a special
    /// lazy reduction opcode evaluates them with short-circuit semantics.
    ///
    /// `is_scan` controls whether to collect intermediate results (scan/triangle
    /// reduction `[\op]`) or just return the final result (`[op]`).
    fn compile_thunk_reduction(&mut self, op: &str, items: &[Expr], is_scan: bool) {
        if items.is_empty() {
            // Identity value for empty reduction
            match op {
                "^^" | "xor" => {
                    self.code.emit(OpCode::LoadFalse);
                }
                "//" | "orelse" => {
                    let any_idx = self
                        .code
                        .add_constant(Value::Package(crate::symbol::Symbol::intern("Any")));
                    self.code.emit(OpCode::LoadConst(any_idx));
                }
                "&&" | "and" => {
                    self.code.emit(OpCode::LoadTrue);
                }
                _ => {
                    self.code.emit(OpCode::LoadFalse);
                }
            }
            return;
        }
        // Wrap each expression as an anonymous block (thunk) so the VM can
        // evaluate them lazily.
        for item in items {
            let body = vec![crate::ast::Stmt::Expr(item.clone())];
            self.compile_expr_anon_sub(&body, false, true);
        }
        self.code.emit(OpCode::MakeArray(items.len() as u32));
        // Emit special lazy reduction operator marker.
        // Use "\\_sc_" prefix for scan (triangle), "_sc_" for non-scan.
        let lazy_op = if is_scan {
            format!("\\_sc_{}", op)
        } else {
            format!("_sc_{}", op)
        };
        let op_idx = self.code.add_constant(Value::str(lazy_op));
        self.code.emit(OpCode::Reduction(op_idx));
    }

    /// Returns true if `expr` is `DoStmt(For { .. })`.
    fn is_dostmt_for(expr: &Expr) -> bool {
        if let Expr::DoStmt(stmt) = expr {
            matches!(stmt.as_ref(), Stmt::For { .. })
        } else {
            false
        }
    }

    /// Build a gather body for `lazy for ITERABLE { BODY }`.
    /// The result is a block wrapping a for loop that calls `take` on each value.
    fn make_lazy_for_gather(target: &Expr) -> Option<Vec<Stmt>> {
        if let Expr::DoStmt(stmt) = target
            && let Stmt::For {
                iterable,
                param,
                param_def,
                params,
                params_def,
                body,
                label,
                mode,
                rw_block,
                explicit_zero_params,
            } = stmt.as_ref()
        {
            let mut new_body = body.clone();
            // Wrap the last expression statement with `take(...)`.
            // Otherwise append `take $_`.
            let has_last_expr = matches!(new_body.last(), Some(Stmt::Expr(_)));
            if has_last_expr {
                let last = new_body.pop().unwrap();
                if let Stmt::Expr(inner_expr) = last {
                    new_body.push(Stmt::Expr(Expr::Call {
                        name: crate::symbol::Symbol::intern("take"),
                        args: vec![inner_expr],
                    }));
                }
            } else {
                new_body.push(Stmt::Expr(Expr::Call {
                    name: crate::symbol::Symbol::intern("take"),
                    args: vec![Expr::Var("_".to_string())],
                }));
            }
            let gather_body = vec![Stmt::For {
                iterable: iterable.clone(),
                param: param.clone(),
                param_def: param_def.clone(),
                params: params.clone(),
                params_def: params_def.clone(),
                body: new_body,
                label: label.clone(),
                mode: *mode,
                rw_block: *rw_block,
                explicit_zero_params: *explicit_zero_params,
            }];
            return Some(gather_body);
        }
        None
    }
}
