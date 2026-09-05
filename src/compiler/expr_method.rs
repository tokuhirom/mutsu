use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Compiler {
    /// Is this subscript's index a *slice* rather than a single element?
    ///
    /// raku answers `.VAR` from the subscript's result: `@a[0]` hands back the
    /// element's `Scalar` container, while `@a[0,1]` hands back a `List` of
    /// containers, and `.VAR` on a `List` is identity — so `@a[0,1].VAR.^name`
    /// is `List`, not `Scalar`. A slice compiles as an ordinary subscript and
    /// lets `.VAR`'s normal dispatch (identity on a `List`) answer.
    ///
    /// Only the statically-recognizable slice spellings are covered:
    /// `@a[0,1]` / `%h<a b>` (an `ArrayLiteral` index — a single `<a>`
    /// collapses to a `Literal`, so it is not caught here), a range
    /// (`@a[0..1]`, `@a[^2]`), and an `@`-sigiled index (`@a[@idx]`). Every
    /// other slice spelling — `@a[*]`, and an index whose *runtime* value
    /// happens to be a list (`my $i = (0,1); @a[$i]`) — is caught instead by
    /// the value-side discriminator in `builtin_index_var_meta` (ADR-0064): a
    /// real container itemizes every element it stores, so a bare,
    /// non-itemized `List` arriving there never came out of one element slot.
    /// This gate stays because it is also the only one that works for a
    /// `LazyList`-backed container, where itemization is still incomplete.
    pub(super) fn index_expr_is_slice(index: &Expr) -> bool {
        match index {
            Expr::ArrayLiteral(_) | Expr::ArrayVar(_) => true,
            Expr::Binary { op, .. } => matches!(
                op,
                TokenKind::DotDot
                    | TokenKind::DotDotCaret
                    | TokenKind::CaretDotDot
                    | TokenKind::CaretDotDotCaret
            ),
            _ => false,
        }
    }

    /// The named container a `.VAR`-carrying subscript reads from, for both the
    /// one-dimensional (`@a[0]`, `%h<k>`) and multi-dimensional (`@sh[0;0]`)
    /// spellings. A shaped array's elements are `Scalar` containers exactly
    /// like a flat one's, so `@sh[0;0].VAR` takes the same element-descriptor
    /// path -- it just reaches the compiler as a different `Expr` node.
    ///
    /// Returns `None` for a statically-recognizable slice, which must compile
    /// as an ordinary subscript instead (see `index_expr_is_slice`).
    pub(super) fn var_on_index_source_name(target: &Expr) -> Option<String> {
        let index_target = match target {
            Expr::Index {
                target: index_target,
                index,
                ..
            } => {
                if Self::index_expr_is_slice(index) {
                    return None;
                }
                index_target
            }
            Expr::MultiDimIndex {
                target: index_target,
                ..
            } => index_target,
            _ => return None,
        };
        Self::index_assign_target_name(index_target)
    }

    /// Does this `.VAR` target take the element-descriptor path at all?
    ///
    /// A NAMED container's subscript always does (`var_on_index_source_name`).
    /// An un-named one does too -- a chained `%d<a><b>` / `@g[0][1]` (whose
    /// parent is an intermediate value with no variable name), a literal
    /// `[1,2][0]`, a `bump()[0]` -- because raku answers from the parent
    /// regardless of whether it has a name: `[1,2][0].VAR` is `Scalar` while
    /// `(1,2)[0].VAR` is `Int`.
    ///
    /// The two exclusions are the subscript shapes whose own compile path this
    /// helper's manual `parent; Dup; index; Index` emission would bypass: a
    /// `PseudoStash` subscript compiles to `GetCallerVar`/`GetOuterVar`, and
    /// `%*ENV<k>` to `GetEnvIndex`. Neither is a real container whose elements
    /// would answer `Scalar` anyway.
    pub(super) fn var_on_index_takes_element_path(&self, target: &Expr) -> bool {
        if Self::var_on_index_source_name(target).is_some() {
            return true;
        }
        // A `:=` bind compiles subscripts through the autovivify opcodes; leave
        // that path alone entirely.
        if self.scalar_bind_autovivify {
            return false;
        }
        matches!(
            target,
            Expr::Index { target: it, index, .. }
                if !Self::index_expr_is_slice(index)
                    && !matches!(
                        it.as_ref(),
                        Expr::PseudoStash(_) | Expr::HashVar(_)
                    )
        )
    }

    /// Compile method call on indexed target: .VAR on @a[0] / %h<k> / @sh[0;0]
    pub(super) fn compile_expr_method_var_on_index(&mut self, target: &Expr) {
        if let Some(source_name) = Self::var_on_index_source_name(target) {
            // Read the element with the ordinary subscript machinery and hand
            // the result to `__mutsu_index_var_meta` along with the name of the
            // container it came from. The builtin needs the value itself for
            // every source whose elements are NOT `Scalar` containers -- a
            // `Map`, and (ADR-0040 slice 3) a `List`/`Seq`/`Range` -- where
            // `.VAR` IS the element; for a real `Array`/`Hash` it discards the
            // value and answers from the container's metadata. Compiling the
            // whole subscript here also keeps the indexed expression's side
            // effects to exactly one evaluation.
            self.compile_expr(target);
            let name_idx = self.code.add_constant(Value::str(source_name));
            self.code.emit(OpCode::LoadConst(name_idx));
            let builtin_idx = self
                .code
                .add_constant(Value::str("__mutsu_index_var_meta".to_string()));
            self.code.emit(OpCode::CallFunc {
                name_idx: builtin_idx,
                arity: 2,
                arg_sources_idx: None,
            });
            return;
        }
        // The source container has no NAME: a chained subscript (`%d<a><b>`,
        // `@g[0][1]` -- the parent is the intermediate value `%d<a>`, which
        // lives under no variable) or a subscript of a literal (`[1,2][0]`).
        // Raku still answers from the parent -- `[1,2][0].VAR` is `Scalar`
        // while `(1,2)[0].VAR` is `Int` -- so put the parent on the stack next
        // to the element instead of naming it. `Dup` after compiling the
        // parent is what keeps this to ONE evaluation of the parent
        // expression, which matters as soon as it has side effects
        // (`@a[f()][0].VAR`).
        if let Expr::Index {
            target: index_target,
            index,
            is_positional,
        } = target
        {
            self.compile_expr(index_target);
            self.code.emit(OpCode::Dup);
            self.compile_subscript_index(index);
            self.code.emit(OpCode::Index {
                is_positional: *is_positional,
            });
            let builtin_idx = self
                .code
                .add_constant(Value::str("__mutsu_anon_index_var_meta".to_string()));
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
        // `.name` on an array/hash variable returns the sigil'd variable name
        // (e.g. `%h.name` → "%h", `@a.name` → "@a"), matching Raku's container
        // `.name`. (`$x.name` operates on the contained value, and `&f.name`
        // returns the routine name, so those are left to normal dispatch.)
        if name.resolve() == "name"
            && args.is_empty()
            && modifier.is_none()
            && !quoted
            && matches!(target, Expr::ArrayVar(_) | Expr::HashVar(_))
        {
            let idx = self.code.add_constant(Value::str(target_name));
            self.code.emit(OpCode::LoadConst(idx));
            return;
        }
        // `.dynamic` on an array/hash variable reports whether the *variable* is
        // a dynamic (`@*a`/`%*h`) one — the container's dynamism, exactly like
        // `.VAR.dynamic` (which mutsu already resolves via `is_var_dynamic`).
        // `$x.dynamic` operates on the contained value (Int/Str have no
        // `.dynamic`), so only `@`/`%` — which pass the container itself to the
        // method — are rerouted here; scalars fall to normal dispatch. A literal
        // `[1,2,3].dynamic` (no variable) reaches the value-level `.dynamic`
        // (native, always False), matching raku.
        if name.resolve() == "dynamic"
            && args.is_empty()
            && modifier.is_none()
            && !quoted
            && matches!(target, Expr::ArrayVar(_) | Expr::HashVar(_))
        {
            let via_var = Expr::MethodCall {
                target: Box::new(Expr::MethodCall {
                    target: Box::new(target.clone()),
                    name: Symbol::intern("VAR"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                }),
                name: Symbol::intern("dynamic"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
            self.compile_expr(&via_var);
            return;
        }
        // Fast path: @arr.push(single_expr) with no modifiers → ArrayPush opcode
        // Only for local variables (not captured closures) to avoid COW env sync issues
        if name.resolve() == "push"
            && args.len() == 1
            && modifier.is_none()
            && !quoted
            && matches!(target, Expr::ArrayVar(_))
            && self.code.locals.contains(&target_name)
        {
            for arg in args {
                self.compile_method_arg(arg);
            }
            // When the pushed argument is a bare container variable (`@a.push(@b)`
            // / `@a.push(%h)`), record its name so the VM can share a cell with
            // the source (Raku stores the container by reference, not a snapshot).
            let value_source_idx = match &args[0] {
                Expr::ArrayVar(n) => Some(self.code.add_constant(Value::str(format!("@{}", n)))),
                Expr::HashVar(n) => Some(self.code.add_constant(Value::str(format!("%{}", n)))),
                _ => None,
            };
            let target_name_idx = self.code.add_constant(Value::str(target_name));
            self.code.emit(OpCode::ArrayPush {
                target_name_idx,
                value_source_idx,
            });
            return;
        }
        // ADR-0057: `.VAR` on a captured/free scalar needs the SAME decl-site
        // boxing guarantee `WrapVarRef` sites get, so a reflection Instance
        // built from the shared cell's own address (see the VAR dispatch in
        // `methods_mut_dispatch.rs`) is identical no matter which frame calls
        // `.VAR` — the target's container must actually exist as a shared
        // cell by the time a nested closure/named-sub/method reads it, not
        // just when the SAME frame that declared it reads it back. Restricted
        // to a bare `$`-sigil variable target (matching ADR-0032 D1's own
        // `$`-only restriction) so `@`/`%` and non-variable targets (`$!`,
        // sigilless attrs) are unaffected.
        if name.resolve() == "VAR"
            && args.is_empty()
            && modifier.is_none()
            && !quoted
            && matches!(target, Expr::Var(_))
        {
            self.register_container_ref_capture_if_free(&target_name);
        }
        self.compile_expr(target);
        let arity = args.len() as u32;
        let arg_sources_idx = self.add_arg_sources_constant(args);
        let mname = name.resolve();
        let esc = Self::method_escapes_closure_args(&mname);
        // `Thread.start` / `Promise.start` hand the block to a thread.
        let thread_esc = mname == "start";
        // `Pair.new($k, $v)` binds its value parameter raw, so the built Pair's
        // value aliases `$v`'s container (write-through, traps.rakudoc; only the
        // 2-positional form — the named form and the key decontainerize). Tag the
        // value argument with `WrapVarRef` — the same capture the fat-arrow
        // `key => $var` path uses — and let the CallMethodMut exec box the source
        // local into a shared cell when the receiver really is the native Pair
        // type (any other receiver unwraps to the plain value).
        let pair_value_capture = matches!(target, Expr::BareWord(_))
            && target_name == "Pair"
            && name.resolve() == "new"
            && modifier.is_none()
            && !quoted
            && args.len() == 2
            && matches!(&args[1], Expr::Var(n) if !n.contains("::"));
        let immutable_topic_cb = Self::method_binds_immutable_topic(target, &mname);
        for (i, arg) in args.iter().enumerate() {
            // Only the closure literal itself is escaping (see
            // `is_closure_literal_arg`); the legacy `then`/`tap`/`act`/`start`
            // names keep marking every argument, as they always did.
            let arg_esc = esc
                && (Self::is_closure_literal_arg(Self::unwrap_named_arg_value(arg))
                    || matches!(mname.as_str(), "then" | "tap" | "act" | "start"));
            self.pending_immutable_topic_block = immutable_topic_cb && Self::is_bare_block_arg(arg);
            self.with_thread_escape(thread_esc, |s| {
                s.compile_method_arg_with_escape(arg, arg_esc)
            });
            self.pending_immutable_topic_block = false;
            if pair_value_capture
                && i == 1
                && let Expr::Var(n) = arg
            {
                // ADR-0021's `ContainerizePair` (just emitted above by
                // `compile_method_arg_with_escape`, since a bare `$v` is not
                // a syntactically-named arg) is semantically inert here —
                // `Pair.new`'s value parameter is bound raw, not classified
                // by named-ness. It used to have to be popped back off to
                // preserve a GetGlobal-immediately-followed-by-WrapVarRef
                // adjacency a peephole scan required; ADR-0032 D1 replaced
                // that scan with an unconditional check inside
                // `emit_wrap_var_ref` itself (does `n` resolve to a local of
                // THIS frame?), which does not care what op precedes it, so
                // the op is simply left in place now.
                self.emit_wrap_var_ref(n);
            }
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

    /// Compile method call on indexed target with mutating method.
    /// e.g., %hash<key>.push(4) or @array[0].push(5)
    ///
    /// Container identity (§3.2): the method mutates the element's shared
    /// node in place, so there is NO post-call writeback. push/append/
    /// unshift/prepend autovivify a missing element via `IndexElemAutoviv`
    /// before the call (`my @a; @a[2].push(3)`); pop/shift/splice do not
    /// autovivify (Raku dies without growing the container) and dispatch
    /// through `CallMethodMut` on a temp binding so the full mut-path
    /// semantics (WhateverCode splice offsets, X::OutOfRange, typed empty
    /// Failures, ...) apply to the removed-element result.
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
            is_positional,
        } = target
        {
            let var_name = Self::postfix_index_name(idx_target).unwrap_or_default();
            let target_slot = self.local_map.get(&var_name).copied();
            let name_resolved = name.resolve();
            let arity = args.len() as u32;
            let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
            if matches!(name_resolved.as_str(), "pop" | "shift" | "splice") {
                let tmp_target_name = format!(
                    "__mutsu_tmp_index_method_target_{}",
                    self.code.constants.len()
                );
                let tmp_target_idx = self.code.add_constant(Value::str(tmp_target_name));
                let name_idx = self.code.add_constant(Value::str(name_resolved));
                let var_name_idx = self.code.add_constant(Value::str(var_name));

                self.compile_expr(idx_target);
                self.compile_expr(idx_key);
                self.code.emit(OpCode::IndexElemAutoviv {
                    name_idx: var_name_idx,
                    is_positional: *is_positional,
                    target_slot,
                    autoviv: false,
                    viv_hash: false,
                });
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
            } else {
                let var_name_idx = self.code.add_constant(Value::str(var_name));
                self.compile_expr(idx_target);
                self.compile_expr(idx_key);
                self.code.emit(OpCode::IndexElemAutoviv {
                    name_idx: var_name_idx,
                    is_positional: *is_positional,
                    target_slot,
                    autoviv: true,
                    viv_hash: false,
                });
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
            }
        } else {
            unreachable!()
        }
    }

    /// Compile a mutating method on a *nested* Index target
    /// (`%h<a><b>.push(1)`, `@a[0]<x>[1].pop`) with NO post-call writeback
    /// (container identity §3.2), mirroring `compile_expr_method_on_index`:
    /// each intermediate subscript is loaded as an element-for-mutation via
    /// `IndexElemAutoviv` and bound to a temp, so a missing intermediate is
    /// autovivified (push family only — Raku's pop/shift/splice die without
    /// growing anything) through the named index-assign machinery into the
    /// parent's shared node. The fresh intermediate's kind follows the NEXT
    /// subscript (positional → Array, associative → Hash). The final level
    /// reuses the single-level machinery on `tmp[last_key]`, whose CallMethod
    /// mutates the element's shared node in place.
    pub(super) fn compile_expr_nested_method_on_index(
        &mut self,
        target: &Expr,
        name: &Symbol,
        args: &[Expr],
        modifier: &Option<char>,
        quoted: bool,
    ) {
        // Flatten the subscript chain: chain[0] is the innermost key
        // (closest to the base expression).
        let mut chain: Vec<(&Expr, bool)> = Vec::new();
        let mut base = target;
        while let Expr::Index {
            target: inner,
            index,
            is_positional,
        } = base
        {
            chain.push((index.as_ref(), *is_positional));
            base = inner;
        }
        chain.reverse();
        debug_assert!(chain.len() >= 2);
        let autoviv = !matches!(name.resolve().as_str(), "pop" | "shift" | "splice");

        // Load the base container and pick the name the first level's
        // autoviv stores through. A non-variable base (`f()<x><y>.push`,
        // `$obj.attr<a><b>.push`) is bound to a temp: the temp's value is
        // node-shared with the produced container, so a store through the
        // temp name writes through to the real container.
        let (mut cur_name, mut cur_slot) = match Self::postfix_index_name(base) {
            Some(n) => {
                let slot = self.local_map.get(&n).copied();
                self.compile_expr(base);
                (n, slot)
            }
            None => {
                self.compile_expr(base);
                let tmp = format!("__mutsu_tmp_nested_base_{}", self.code.constants.len());
                let tmp_idx = self.code.add_constant(Value::str(tmp.clone()));
                self.code.emit(OpCode::SetGlobal(tmp_idx));
                self.code.emit(OpCode::GetGlobal(tmp_idx));
                (tmp, None)
            }
        };

        // Chain the intermediate levels (every key but the last): the stack
        // holds the current container; load/autoviv the element and rebind
        // the temp that the next level stores through.
        let (&(last_key, last_positional), inters) = chain.split_last().unwrap();
        for (j, &(key, is_positional)) in inters.iter().enumerate() {
            let next_positional = chain[j + 1].1;
            let name_idx = self.code.add_constant(Value::str(cur_name.clone()));
            self.compile_expr(key);
            self.code.emit(OpCode::IndexElemAutoviv {
                name_idx,
                is_positional,
                target_slot: cur_slot,
                autoviv,
                viv_hash: !next_positional,
            });
            let tmp = format!("__mutsu_tmp_nested_lvl_{}", self.code.constants.len());
            let tmp_idx = self.code.add_constant(Value::str(tmp.clone()));
            self.code.emit(OpCode::SetGlobal(tmp_idx));
            if j + 1 < inters.len() {
                self.code.emit(OpCode::GetGlobal(tmp_idx));
            }
            cur_name = tmp;
            cur_slot = None;
        }

        // Final level: single-level element-mutation machinery on the temp.
        let final_target = Expr::Index {
            target: Box::new(Expr::Var(cur_name)),
            index: Box::new(last_key.clone()),
            is_positional: last_positional,
        };
        self.compile_expr_method_on_index(&final_target, name, args, modifier, quoted);
    }

    /// Does a `.map`/`.grep` written directly against `target` bind its
    /// callback's implicit `$_` to an item with no container of its own?
    ///
    /// Reuses the `for`-loop oracle
    /// ([`Compiler::for_iterable_yields_bare_items`]): `(1, 2).map({ $_ = 5 })`
    /// and `for 1, 2 { $_ = 5 }` are the same rejection in raku, for the same
    /// reason. Conservative — a variable/derived receiver answers `false`, so
    /// `@a.map({ $_ = 5 })` and every shape mutsu cannot prove bare keep
    /// today's writable topic. See [`crate::opcode::CompiledCode::immutable_topic`].
    pub(super) fn method_binds_immutable_topic(target: &Expr, mname: &str) -> bool {
        matches!(mname, "map" | "grep") && Self::for_iterable_yields_bare_items(target)
    }

    /// A directly-written bare block argument (`{ ... }`), the only shape whose
    /// implicit topic this call site can speak for.
    pub(super) fn is_bare_block_arg(arg: &Expr) -> bool {
        matches!(arg, Expr::AnonSub { is_block: true, .. })
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
                is_positional: delete_positional,
                ..
            } = target
        {
            // Nested `:delete` (`%h<a><x>:delete`, `@a[0][1]:delete`): the delete
            // target is itself a subscript, so there is no simple name to write the
            // modified container back through — the generic `DeleteIndexExpr` path
            // would delete from a copy and lose it. Bind a temp to the nested slot
            // (`my $t := %h<a>`) — which shares its `ContainerRef` cell — and delete
            // through it, so the deletion reaches the real inner container.
            if matches!(delete_target.as_ref(), Expr::Index { .. }) {
                let tmp = format!("__mutsu_nested_del_{}", self.code.constants.len());
                let bind_decl = Stmt::VarDecl {
                    name: tmp.clone(),
                    expr: (**delete_target).clone(),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: vec![("__scalar_bind".to_string(), None)],
                    where_constraint: None,
                };
                let delete_through = Expr::MethodCall {
                    target: Box::new(Expr::Index {
                        target: Box::new(Expr::Var(tmp)),
                        index: delete_index.clone(),
                        is_positional: *delete_positional,
                    }),
                    name: Symbol::intern("DELETE-KEY"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                };
                self.compile_expr(&Expr::DoBlock {
                    body: vec![Stmt::MarkBind, bind_decl, Stmt::Expr(delete_through)],
                    label: None,
                });
                return;
            }
            if let Some(var_name) = Self::postfix_index_name(delete_target) {
                if Self::index_assign_target_requires_eval(delete_target) {
                    self.compile_expr(delete_target);
                    self.code.emit(OpCode::Pop);
                }
                self.compile_expr(delete_index);
                let slot = self.local_map.get(&var_name).copied();
                let name_idx = self.code.add_constant(Value::str(var_name));
                self.code.emit(OpCode::DeleteIndexNamed(name_idx, slot));
            } else if let Expr::MethodCall {
                target: method_target,
                name: method_name,
                args: method_args,
                ..
            } = &**delete_target
                && method_args.is_empty()
                && let Some(var_name) = Self::method_call_target_var_name(method_target)
            {
                // `$obj.attr<key>:delete` — delete through an accessor and write
                // the modified container back to the attribute (mirrors the
                // `$obj.attr<key> = v` lvalue path).
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("__mutsu_index_delete_method_lvalue"),
                    args: vec![
                        (**method_target).clone(),
                        Expr::Literal(Value::str(method_name.resolve().to_string())),
                        (**delete_index).clone(),
                        Expr::Literal(Value::str(var_name)),
                    ],
                });
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
                right: Box::new(Expr::Literal(Value::TRUE)),
            });
            self.compile_expr(&Expr::Call {
                name: *sub_name,
                args: call_args,
            });
            return;
        }
        // `@a[...]:delete:kv` (delete listed first) reaches the compiler as
        // `DELETE-KEY` on an `Index` carrying a single `:k`/`:v`/`:p`/`:kv` adverb
        // pair (`"kv" => True`, `:!kv` => `"kv" => False`). Route it to the same
        // `__mutsu_subscript_adverb` slice path with the `delete` flag set, so the
        // key/value slice is computed *and* the addressed slots are removed.
        if name == "DELETE-KEY"
            && modifier.is_none()
            && let Expr::Index {
                target: idx_target,
                index: idx_index,
                ..
            } = target
            && args.len() == 1
            && let Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
            } = &args[0]
            && let Expr::Literal(mode_lit) = left.as_ref()
            && let ValueView::Str(mode) = mode_lit.view()
            && matches!(mode.as_ref().as_str(), "k" | "v" | "p" | "kv")
            && let Expr::Literal(pos_lit) = right.as_ref()
            && let ValueView::Bool(positive) = pos_lit.view()
        {
            let mode_str = if positive {
                mode.to_string()
            } else {
                format!("not-{}", *mode)
            };
            let var_name = Self::postfix_index_name(idx_target).unwrap_or_default();
            let call_args = vec![
                (**idx_target).clone(),
                (**idx_index).clone(),
                Expr::Literal(Value::str(mode_str)),
                Expr::Literal(Value::str(var_name)),
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str_from("delete"))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(Expr::Literal(Value::TRUE)),
                },
            ];
            self.compile_expr(&Expr::Call {
                name: Symbol::intern("__mutsu_subscript_adverb"),
                args: call_args,
            });
            return;
        }
        self.compile_expr(target);
        let arity = args.len() as u32;
        let arg_sources_idx = self.add_arg_sources_constant(args);
        let mname = name.resolve();
        let esc = Self::method_escapes_closure_args(&mname);
        // `Thread.start` / `Promise.start` hand the block to a thread.
        let thread_esc = mname == "start";
        let immutable_topic_cb = Self::method_binds_immutable_topic(target, &mname);
        for arg in args {
            // See the sibling loop in `compile_expr_method_call`.
            let arg_esc = esc
                && (Self::is_closure_literal_arg(Self::unwrap_named_arg_value(arg))
                    || matches!(mname.as_str(), "then" | "tap" | "act" | "start"));
            self.pending_immutable_topic_block = immutable_topic_cb && Self::is_bare_block_arg(arg);
            self.with_thread_escape(thread_esc, |s| {
                s.compile_method_arg_with_escape(arg, arg_esc)
            });
            self.pending_immutable_topic_block = false;
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
        modifier: &Option<char>,
        quoted: bool,
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
        // ADR-0054 S3: bake `|EXPR` positions before compiling the args
        // themselves, matching `compile_expr_method_on_var`/`_generic`.
        let arg_sources_idx = self.add_arg_sources_constant(args);
        for arg in args {
            self.compile_method_arg(arg);
        }
        let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
        if let Some(var_name) = target_var_name {
            let target_name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::CallMethodDynamicMut {
                arity,
                target_name_idx,
                modifier_idx,
                quoted,
                arg_sources_idx,
            });
        } else {
            self.code.emit(OpCode::CallMethodDynamic {
                arity,
                modifier_idx,
                quoted,
                arg_sources_idx,
            });
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
        // A mutating hyper postfix on a subscript/slice (`%h<a b c>»++`,
        // `@a[0,1]»--`) has no simple `@`/`%` variable name for the generic hyper
        // path to write back through, so the in/decremented values were computed
        // and discarded. Desugar it to a post-increment: read the OLD slice, then
        // mutate it in place through the working `»+=»`/`»-=»` metaop, and leave
        // the OLD values as the expression result.
        let resolved = name.resolve();
        if args.is_empty()
            && modifier.is_none()
            && matches!(resolved.as_str(), "postfix:<++>" | "postfix:<-->")
            && matches!(target, Expr::Index { .. })
        {
            let op = if resolved == "postfix:<++>" {
                "+="
            } else {
                "-="
            };
            let mutate = Expr::HyperOp {
                op: op.to_string(),
                left: Box::new(target.clone()),
                right: Box::new(Expr::Literal(Value::int(1))),
                dwim_left: false,
                dwim_right: true,
            };
            self.compile_expr(target); // OLD slice values (post-increment result)
            self.compile_expr(&mutate); // mutate in place, pushes NEW
            self.code.emit(OpCode::Pop); // discard NEW, keep OLD
            return;
        }
        self.compile_expr(target);
        let arity = args.len() as u32;
        // ADR-0054 S3: bake `|EXPR` positions before compiling the args
        // themselves, matching the scalar `CallMethod`/`CallMethodMut` sites.
        let arg_sources_idx = self.add_arg_sources_constant(args);
        for arg in args {
            self.compile_method_arg(arg);
        }
        let name_idx = self.code.add_constant(Value::str(name.resolve()));
        let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
        // For a plain `@`/`%` variable target, record its name so a mutating
        // hyper (`@a>>++`) writes back precisely to that binding rather than to
        // every Arc-identity-sharing binding (which corrupts COW copies like
        // `my @x = @a`).
        let target_name_idx = match target {
            Expr::ArrayVar(n) => Some(self.code.add_constant(Value::str(format!("@{}", n)))),
            Expr::HashVar(n) => Some(self.code.add_constant(Value::str(format!("%{}", n)))),
            // A scalar holding a QuantHash (`$b>>--`, `$b := <...>.BagHash`) also
            // needs its name for precise weight writeback. The `@`/`%` writeback
            // paths below are sigil-guarded so a bare scalar name only drives the
            // QuantHash postfix path, leaving Array/Hash behavior unchanged.
            Expr::Var(n) => Some(self.code.add_constant(Value::str(n.clone()))),
            _ => None,
        };
        self.code.emit(OpCode::HyperMethodCall {
            name_idx,
            arity,
            modifier_idx,
            quoted,
            target_name_idx,
            arg_sources_idx,
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
        // ADR-0054 S3: bake `|EXPR` positions before compiling the args
        // themselves, matching the scalar `CallMethod`/`CallMethodMut` sites.
        let arg_sources_idx = self.add_arg_sources_constant(args);
        for arg in args {
            self.compile_method_arg(arg);
        }
        let modifier_idx = modifier.map(|m| self.code.add_constant(Value::str(m.to_string())));
        self.code.emit(OpCode::HyperMethodCallDynamic {
            arity,
            modifier_idx,
            arg_sources_idx,
        });
    }
}
