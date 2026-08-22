use super::*;
use crate::ast::{CallArg, make_anon_sub};
use crate::symbol::Symbol;

impl Compiler {
    /// Compile the single operand of `return-rw` so that it yields a *container*
    /// rather than a decontainerized value.
    ///
    /// An `is rw` routine's contract is that it hands its caller something
    /// writable; `sub g(\c) is rw { return-rw c<a> }; g(%h) = 1` must write the
    /// element of the caller's `%h`. A subscript is therefore compiled in the
    /// same container-producing mode a `:=` bind RHS uses
    /// (`scalar_bind_autovivify` + `bind_terminal`), which promotes the element
    /// to its shared `ContainerRef` cell (or hands back a deferred
    /// `HashEntryRef` for a not-yet-existent hash key, so the eventual write
    /// autovivifies the path). `assign_lvalue_container` at the call site writes
    /// through whichever of those comes back.
    ///
    /// Every other operand shape (a bare variable in particular) compiles
    /// unchanged: a variable tail already resolves in the caller's frame
    /// through the legacy name-based path, and forcing a container there would
    /// change what an ordinary `my $v = g()` read observes.
    pub(super) fn compile_return_rw_arg(&mut self, arg: &Expr) {
        let saved_rw = self.rw_return_operand;
        self.rw_return_operand = true;
        match arg {
            Expr::Index { .. } | Expr::MultiDimIndex { .. } => {
                let saved_av = self.scalar_bind_autovivify;
                let saved_terminal = self.bind_terminal;
                self.scalar_bind_autovivify = true;
                self.bind_terminal = true;
                self.compile_expr(arg);
                self.scalar_bind_autovivify = saved_av;
                self.bind_terminal = saved_terminal;
            }
            _ => self.compile_expr(arg),
        }
        self.rw_return_operand = saved_rw;
    }

    /// Compile a subscript call argument as the element's container, for the
    /// lvalue chain of a `return-rw` operand (see `rw_return_operand`). This is
    /// the single-dimension twin of the `MultiDimIndexBindRef` argument path
    /// above: the element is promoted to its shared `ContainerRef` cell (a
    /// missing hash key yields the deferred `HashEntryRef` token instead, so a
    /// read is still non-vivifying), and a `\raw` / `is rw` parameter binds that
    /// container rather than a value snapshot. Without it the recursive descent
    /// of a path-addressing routine writes into a detached copy.
    fn compile_rw_chain_index_arg(&mut self, arg: &Expr) {
        let saved_av = self.scalar_bind_autovivify;
        let saved_terminal = self.bind_terminal;
        self.scalar_bind_autovivify = true;
        self.bind_terminal = true;
        self.compile_expr(arg);
        self.scalar_bind_autovivify = saved_av;
        self.bind_terminal = saved_terminal;
    }

    pub(super) fn is_normalized_stmt_call_name(name: &str) -> bool {
        matches!(
            name,
            "shift"
                | "pop"
                | "push"
                | "unshift"
                | "append"
                | "prepend"
                | "splice"
                | "undefine"
                | "VAR"
                | "indir"
        ) || crate::parser::is_imported_function(name)
    }

    pub(super) fn rewrite_stmt_call_args(name: &str, args: &[CallArg]) -> Vec<CallArg> {
        let rewrites_needed = matches!(
            name,
            "lives-ok"
                | "dies-ok"
                | "exits-ok"
                | "throws-like"
                | "warns-like"
                | "doesn't-warn"
                | "is_run"
        );
        if !rewrites_needed {
            return args.to_vec();
        }
        let mut positional_index = 0usize;
        args.iter()
            .map(|arg| match arg {
                CallArg::Positional(expr) => {
                    let rewritten = if matches!(
                        name,
                        "lives-ok"
                            | "dies-ok"
                            | "exits-ok"
                            | "throws-like"
                            | "warns-like"
                            | "doesn't-warn"
                    ) && positional_index == 0
                    {
                        match expr {
                            Expr::Block(body) => make_anon_sub(body.clone()),
                            _ => expr.clone(),
                        }
                    } else if name == "is_run" && positional_index == 1 {
                        Self::rewrite_hash_block_values(expr)
                    } else {
                        expr.clone()
                    };
                    positional_index += 1;
                    CallArg::Positional(rewritten)
                }
                CallArg::Named { name, value } => CallArg::Named {
                    name: name.clone(),
                    value: value.clone(),
                },
                CallArg::Slip(expr) => CallArg::Slip(expr.clone()),
                CallArg::Invocant(expr) => CallArg::Invocant(expr.clone()),
            })
            .collect()
    }

    /// Rewrite block values inside a hash literal to anonymous subs.
    /// Used for `is_run`'s expectation hash: `{ out => { ... } }`.
    pub(super) fn rewrite_hash_block_values(expr: &Expr) -> Expr {
        if let Expr::Hash(pairs) = expr {
            let rewritten_pairs = pairs
                .iter()
                .map(|(name, value)| {
                    let rewritten_value = value.as_ref().map(|v| {
                        if let Expr::Block(body) = v {
                            make_anon_sub(body.clone())
                        } else {
                            v.clone()
                        }
                    });
                    (name.clone(), rewritten_value)
                })
                .collect();
            Expr::Hash(rewritten_pairs)
        } else {
            expr.clone()
        }
    }

    pub(super) fn has_phasers(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Phaser { kind, .. } if matches!(kind, PhaserKind::Enter | PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo | PhaserKind::First | PhaserKind::Next | PhaserKind::Last | PhaserKind::Pre | PhaserKind::Post)))
    }

    /// Check if a block body contains placeholder variables ($^a, $^b, etc.)
    pub(super) fn has_block_placeholders(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            if Self::stmt_has_placeholder(stmt) {
                return true;
            }
        }
        false
    }

    /// A closure passed as a **method argument** escapes the creating frame.
    ///
    /// The callee decides whether the closure is invoked immediately or stored,
    /// and the caller cannot know which — `$reg.register({ $c++ })` keeps it
    /// alive long after the call returns. Raku closes over *containers*, so any
    /// captured-and-mutated local a stored closure names must be a shared
    /// `ContainerRef` cell; snapshotting it by value makes two closures over the
    /// same `my $c` disagree about its value (see
    /// `t/closure-arg-shares-its-captured-container.t`), which is the shape a
    /// Cro `route { my $i; get -> { $i++ } }` counter has.
    ///
    /// This used to be an allowlist (`then`/`tap`/`act`/`start`); everything else
    /// was classified non-escaping as a boxing-cost guard (#2746).
    pub(super) fn method_escapes_closure_args(_name: &str) -> bool {
        true
    }

    /// Whether an argument is a **closure literal** written at the call site.
    ///
    /// Only such an argument is compiled in an escaping position. Marking the
    /// whole argument list escaping instead costs 2.2x on
    /// `sub foo () { $ = 42 }; for ^2000000 { $ = foo }`
    /// (roast S04-declarations/state.t, which then times out under parallel
    /// load): `escaping_position` is read at EVERY closure-creation op compiled
    /// anywhere inside the argument expression, so a non-closure argument
    /// silently promoted unrelated inner blocks. The escape claim is about the
    /// literal the callee might store, and this is exactly that literal.
    pub(super) fn is_closure_literal_arg(arg: &Expr) -> bool {
        matches!(
            arg,
            Expr::Block(_)
                | Expr::AnonSub { .. }
                | Expr::AnonSubParams { .. }
                | Expr::Lambda { .. }
                // ADR-0033: a Whatever-curried argument (`.grep(* > $x)`) is a
                // closure literal written at the call site exactly like a
                // hand-written one — it just hasn't been expanded into its
                // `Lambda`/`AnonSubParams` form yet (that happens when this
                // argument itself gets compiled).
                | Expr::WhateverCurry(_)
        )
    }

    /// Strip a fat-arrow named-argument wrapper (`key => value`) down to the
    /// value expression, so a closure literal named argument (`now => { $x }`)
    /// is recognized by [`is_closure_literal_arg`] the same way a positional
    /// one is. Every call-argument-escaping check must unwrap through this
    /// (not just re-match `Expr::Binary { op: FatArrow, .. }` locally) so the
    /// function-call and method-call compile paths cannot drift apart again.
    pub(super) fn unwrap_named_arg_value(arg: &Expr) -> &Expr {
        match arg {
            Expr::Binary {
                op: TokenKind::FatArrow,
                right,
                ..
            } => right.as_ref(),
            other => other,
        }
    }

    /// Compile a method call argument.
    pub(super) fn compile_method_arg(&mut self, arg: &Expr) {
        self.compile_method_arg_with_escape(arg, false);
    }

    /// Like [`compile_method_arg`] but lets the caller force the closure
    /// argument into an escaping position (for supply-consuming methods; see
    /// [`method_escapes_closure_args`]).
    pub(super) fn compile_method_arg_with_escape(&mut self, arg: &Expr, escaping: bool) {
        // A method argument is normally passed to the callee, not stored in the
        // caller frame, so a closure argument is conservatively NON-escaping
        // (the #2746 guard). `tap`/`act` override this with `escaping = true`.
        self.with_escape(escaping, |s| {
            s.with_suppress_pair_capture(true, |s| {
                // An `AssignExpr` in argument position is always a real
                // assignment, evaluating to the assigned value
                // (`@r.push($x += 5)` pushes the assigned value, not a Pair).
                // This used to special-case a sigilless `name` as a named-arg
                // sugar (`foo(arg = 1)` -> `:arg(1)`), but raku itself rejects
                // a bareword assignment target as a parse error, and
                // `AssignExpr.name` never carries the `$` sigil for a genuine
                // scalar target (only `@`/`%` targets get one prepended) -- so
                // that check could never actually distinguish the two shapes
                // and instead misfired on every real `$x = ...`/`$x += ...`
                // argument. See todo/tickets/compound-assign-as-call-argument-yields-pair.md.
                // ADR-0021 I2/I3: a bareword-keyed fat-arrow (or colonpair,
                // same AST shape) written directly as this argument mints
                // the named-argument flavour, not the data default.
                if matches!(arg, Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow)
                {
                    s.mint_named_pair = true;
                }
                s.compile_expr(arg);
                if Self::needs_decont(arg) {
                    s.code.emit(OpCode::Decont);
                }
            })
        });
        // ADR-0021 (argument named-ness is a call-site property): named-ness
        // is decided by call-site syntax, not by what flavour of Pair the
        // argument expression happens to evaluate to. The function-call path
        // (`compile_call_arg_with_escape`, below) already normalizes every
        // non-syntactically-named argument at the call boundary; the method
        // path lacked this, so a Pair-valued variable/array-element/return
        // value leaked its named flavour straight into method dispatch
        // (`Pair.new($k,$v)` misbinding as a named arg, etc). Mirror the
        // function path here so both call kinds erase the flavour identically.
        if !Self::is_named_arg_expr(arg) {
            self.code.emit(OpCode::ContainerizePair);
        }
    }

    /// Check if an expression produces an array value that needs decontainerization
    /// for slurpy flattening at call sites.
    fn needs_decont(expr: &Expr) -> bool {
        match expr {
            Expr::ArrayVar(_) => true,
            // Assignment to @-variable returns an array
            Expr::AssignExpr { name, .. } => name.starts_with('@'),
            // VarDecl/Assign in expression position (my @a = ...)
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => name.starts_with('@'),
                _ => false,
            },
            _ => false,
        }
    }

    /// Compile a function-call positional argument.
    /// Variable-like args are wrapped with source-name metadata so sigilless
    /// parameters (`\x`) can bind as writable aliases.
    pub(super) fn is_named_arg_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow => true,
            Expr::Literal(lit) if matches!(lit.view(), crate::value::ValueView::Pair(..)) => true,
            Expr::Unary { op, .. } if *op == crate::token_kind::TokenKind::Pipe => true,
            _ => false,
        }
    }

    pub(super) fn compile_call_arg(&mut self, arg: &Expr) {
        self.compile_call_arg_with_escape(arg, false);
    }

    /// Like `compile_call_arg` but lets the caller force the argument into an
    /// escaping position. Used for thread-spawning constructs (`start`) whose
    /// block argument genuinely outlives the call frame (it is stored in a
    /// Promise and run later on another thread), so the locals it captures and
    /// mutates must be promoted to shared `ContainerRef` cells (escape analysis).
    pub(super) fn compile_call_arg_with_escape(&mut self, arg: &Expr, escaping: bool) {
        // Read-and-clear immediately: this call is the *direct* bind-target
        // compile iff the caller just set the flag for us. Clearing it up
        // front (before any nested `compile_expr`/`compile_call_arg`
        // recursion below) means a genuine call nested inside a bind RHS
        // (`my $x := f(@a[$i])`) sees `false` for its own argument compile,
        // so `f`'s `is rw` writeback machinery is untouched. See the field
        // doc on `bind_target_direct`.
        let is_bind_target = self.bind_target_direct;
        self.bind_target_direct = false;
        // A multi-dimensional subscript (`@a[0;1;2]`, `%h{"a";"b"}`) passed as a
        // raw `\target` / `is rw` argument must alias the underlying nested
        // slot, so a later `target = v` inside the callee mutates the real
        // container and is visible immediately. Emit a `MultiDimIndexBindRef`
        // that descends to the leaf and promotes it to a shared `ContainerRef`
        // cell (a missing hash leaf gets a deferred `HashEntryRef`); the callee
        // binds through it. Slice dimensions that can't collapse to one cell
        // yield a list of leaf cells, or fall back to the plain read value.
        if let Expr::MultiDimIndex { target, dimensions } = arg {
            self.compile_expr(target);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            self.code
                .emit(OpCode::MultiDimIndexBindRef(dimensions.len() as u32));
            return;
        }
        // Inside a `return-rw` operand a single-dimension subscript argument is
        // part of the lvalue chain and must alias the element's container, the
        // same way the multi-dim form above always does.
        if self.rw_return_operand && matches!(arg, Expr::Index { .. }) {
            self.compile_rw_chain_index_arg(arg);
            return;
        }
        // A call argument's value is normally passed to the callee, not stored
        // in the caller frame, so a closure argument is conservatively
        // NON-escaping (the #2746 guard: `map {...}` / `lives-ok {...}` must not
        // box even when the whole call sits in an escaping position like
        // `my @r = map {...}`). `start` overrides this with `escaping = true`.
        // ADR-0021 I2/I3: a bareword-keyed fat-arrow (or colonpair, same AST
        // shape) written directly as this argument mints the named-argument
        // flavour, not the data default.
        if matches!(arg, Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow) {
            self.mint_named_pair = true;
        }
        self.with_escape(escaping, |c| {
            c.with_suppress_pair_capture(true, |c| c.compile_expr(arg))
        });
        if Self::needs_decont(arg) {
            self.code.emit(OpCode::Decont);
        }
        if !Self::is_named_arg_expr(arg) {
            self.code.emit(OpCode::ContainerizePair);
        }
        let source_name = match arg {
            Expr::Var(n) => Some(n.clone()),
            Expr::ArrayVar(n) => Some(format!("@{}", n)),
            Expr::HashVar(n) => Some(format!("%{}", n)),
            Expr::CodeVar(n) => Some(format!("&{}", n)),
            Expr::BareWord(n) => Some(n.clone()),
            // Anonymous scalar assignment (`$ = value`) produces a writable
            // container, so wrap it with VarRef so `is rw` dispatch can match.
            Expr::AssignExpr { name, .. } => Some(name.clone()),
            // An inline declaration used as an argument (`$y := my $x`,
            // `f(my $z)`) parses to `DoStmt(VarDecl { .. })`. Compiling it
            // declares the variable in the enclosing scope and leaves its value
            // on the stack; wrap that with a VarRef to the freshly-declared
            // variable so a `:=` bind (or an `is rw` parameter) can alias the
            // new container rather than snapshotting its value. The `VarDecl`
            // `name` already carries the sigil convention WrapVarRef expects
            // ("x" for `$x`, "@x" for `@x`, "%y" for `%y`).
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } => Some(name.clone()),
                _ => None,
            },
            _ => None,
        };
        // For Index expressions, create temp variables for `is rw` writeback
        // and wrap with VarRef so `is rw` parameters can bind through.
        if matches!(arg, Expr::Index { .. }) && is_bind_target {
            // `:=` bind to an Index expression (`my $x := @a[$i]`): the Index
            // compile already promoted the element to a first-class
            // `ContainerRef` cell on the stack (IndexAutovivifyLazyTerminal /
            // array_slot_ref). Just wrap it with VarRef so SetLocal's
            // `extract_varref_binding` sees `is_bind = true`. Skip the is-rw
            // *call-argument* writeback temps entirely: there is no function
            // call to writeback after here, and those temps are
            // compile-time-fixed global names — reused verbatim on every
            // iteration of a loop wrapping this same bind statement, whose
            // "write through an existing ContainerRef" semantics would
            // corrupt the *previous* iteration's bound cell instead of
            // storing a fresh reference to this one.
            let tmp = format!("__mutsu_bind_index_ref_{}", self.code.constants.len());
            let name_idx = self.code.add_constant(Value::str(tmp));
            self.code.emit(OpCode::WrapVarRef {
                name_idx,
                slot: u32::MAX,
            });
        } else if matches!(arg, Expr::Index { .. }) {
            let tmp = format!("__mutsu_index_rw_arg_{}", self.code.constants.len());
            let orig = format!("__mutsu_index_rw_orig_{}", self.code.constants.len());
            let tmp_idx = self.code.add_constant(Value::str(tmp.clone()));
            let orig_idx = self.code.add_constant(Value::str(orig.clone()));
            // RAW stores: these compile-time-fixed temp names are re-used on
            // every execution of this call site (each loop iteration). The
            // element snapshot may be a promoted `ContainerRef` cell; a plain
            // SetGlobal would then WRITE THROUGH the previous execution's cell
            // (corrupting that element in its source hash/array — the
            // `%args{$k} := f(%j{$k})` loop clobbered `%j` at the first key
            // with each later iteration's value) instead of replacing the temp.
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetGlobalRaw(tmp_idx));
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetGlobalRaw(orig_idx));
            self.pending_index_rw_writebacks
                .push((arg.clone(), tmp.clone(), orig.clone()));
            let name_idx = self.code.add_constant(Value::str(tmp));
            self.code.emit(OpCode::WrapVarRef {
                name_idx,
                slot: u32::MAX,
            });
        } else if let Some(name) = source_name {
            // Deliberately the non-ADR-0032-D1 emitter here, for EVERY arg
            // shape (including a genuine `Expr::Var`/`Expr::DoStmt` VarDecl
            // read). This call site fires for every plain call argument in
            // the language (not only an `is rw`/`:=`-bound one) purely to
            // tag its shape for LATER is-rw dispatch matching — unlike the
            // narrow, deliberate WrapVarRef sites (fat-arrow value, Pair.new
            // value-arg, Capture item, list-literal element, meta-identity
            // operand), it is not itself a container-capture-semantics site.
            // Registering it anyway is not just imprecise for a bareword
            // (`emit_wrap_var_ref_arg_tag`'s doc comment) — it also over-
            // boxes a genuine free-variable argument passed to an ordinary
            // (non-`is rw`) function: `t/hash-attr-map-default-element-
            // assign.t` broke because `lives-ok { $c.h{3} = Str }` compiles
            // `$c` as an rw-tagged argument to an internal hash-element-
            // assign helper, and boxing `$c`'s OWN declaration into a
            // `ContainerRef` cell (Half A) corrupted class-instance
            // attribute-hash access through it. Probes `V`/`W` (an `is rw`
            // argument / `:=` bind performed inside a closure) do not need
            // D1 to pass — they already work through the pre-existing
            // `free_var_writes` write-tracking machinery (ADR-0032 §1.4).
            self.emit_wrap_var_ref_arg_tag(&name);
        } else if is_bind_target && matches!(arg, Expr::MethodCall { .. }) {
            // `:=` bind to a method-call RHS (`my $ref := $obj.attr`): flag the
            // dispatch so a public attribute accessor returns the attribute
            // slot's `ContainerRef` cell instead of a value copy — the bound
            // variable then aliases the attribute container (writes through
            // either side are seen by both). A non-accessor method ignores the
            // flag and the bind degrades to today's bind-by-value.
            self.mark_trailing_method_call_as_accessor_ref();
        }
    }

    /// Insert a `MarkAccessorRefContext` immediately before the trailing
    /// `CallMethod`/`CallMethodMut` op (skipping the post-call `Decont` /
    /// `ContainerizePair` the arg compile may have appended), so that ONE
    /// dispatch sees the accessor-ref flag. Inserting (rather than emitting
    /// after the fact) is safe here: any jump patched to the call op's old
    /// index now lands on the marker and falls through to the same call.
    /// No-op when the compiled tail is not a method call.
    pub(super) fn mark_trailing_method_call_as_accessor_ref(&mut self) {
        let mut i = self.code.ops.len();
        while i > 0 {
            match &self.code.ops[i - 1] {
                OpCode::Decont | OpCode::ContainerizePair => i -= 1,
                OpCode::CallMethod { .. } | OpCode::CallMethodMut { .. } => {
                    // Keep the ip -> line table (`op_lines`) aligned with `ops`:
                    // the marker inherits the call's line.
                    let line = self.code.op_lines[i - 1];
                    self.code.ops.insert(i - 1, OpCode::MarkAccessorRefContext);
                    self.code.op_lines.insert(i - 1, line);
                    return;
                }
                _ => return,
            }
        }
    }

    /// Emit writeback code for Index expressions passed as function arguments.
    /// After a function call, if any `is rw` parameter modified the temp variable,
    /// we write the new value back to the original hash/array slot.
    /// Only writes back when the temp value differs from the original value
    /// (using `===` identity check).
    pub(super) fn emit_index_rw_writebacks(&mut self) {
        let writebacks = std::mem::take(&mut self.pending_index_rw_writebacks);
        if writebacks.is_empty() {
            return;
        }
        for (index_expr, tmp_name, orig_name) in writebacks {
            if let Expr::Index {
                target,
                index,
                is_positional,
            } = &index_expr
            {
                // Save the call result. RAW store — same fixed-name reuse
                // hazard as the arg/orig temps above: a cell-valued result
                // must replace the temp, not write through a stale cell.
                let result_tmp = format!("__mutsu_call_result_{}", self.code.constants.len());
                let result_idx = self.code.add_constant(Value::str(result_tmp));
                self.code.emit(OpCode::SetGlobalRaw(result_idx));

                // Compare current temp value with original value.
                // If they're identical (===), skip writeback.
                let tmp_idx = self.code.add_constant(Value::str(tmp_name.clone()));
                let orig_idx = self.code.add_constant(Value::str(orig_name));
                self.code.emit(OpCode::GetGlobal(tmp_idx));
                self.code.emit(OpCode::GetGlobal(orig_idx));
                self.code.emit(OpCode::StrictEq);
                // If equal (True), skip writeback
                let skip_idx = self.code.emit(OpCode::JumpIfTrue(0));
                // Values differ: pop comparison result
                self.code.emit(OpCode::Pop); // pop False from StrictEq
                // Second guard: the tmp/orig globals are compile-time-fixed
                // names, so a RECURSIVE execution of this same call site inside
                // the callee clobbers them (and the callee's plain-@-param exit
                // merge writes its final param value into tmp). If tmp is
                // structurally `eqv` to what the source slot holds RIGHT NOW,
                // there is no real mutation to apply — writing back would
                // re-assign the slot with its own value, which explodes on an
                // immutable List source (`g(@xs[1..*])` recursion,
                // 99problems-21-to-30.t P26). A genuine `is rw` mutation leaves
                // tmp differing from the (not-yet-updated) slot, so it still
                // writes back.
                self.code.emit(OpCode::GetGlobal(tmp_idx));
                self.compile_expr(&Expr::Index {
                    target: target.clone(),
                    index: index.clone(),
                    is_positional: *is_positional,
                });
                self.code.emit(OpCode::Eqv);
                let skip2_idx = self.code.emit(OpCode::JumpIfTrue(0));
                self.code.emit(OpCode::Pop); // pop False from Eqv
                let writeback = Expr::IndexAssign {
                    target: target.clone(),
                    index: index.clone(),
                    value: Box::new(Expr::Var(tmp_name)),
                    is_positional: *is_positional,
                };
                self.compile_expr(&writeback);
                self.code.emit(OpCode::Pop); // discard assignment result
                let jump_to_restore = self.code.emit(OpCode::Jump(0));
                // Skip targets: pop the True left by StrictEq / Eqv
                self.code.patch_jump(skip_idx);
                self.code.patch_jump(skip2_idx);
                self.code.emit(OpCode::Pop); // pop True
                // Restore point
                self.code.patch_jump(jump_to_restore);
                self.code.emit(OpCode::GetGlobal(result_idx));
            }
        }
    }

    pub(super) fn stmt_has_placeholder(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) => {
                Self::expr_has_placeholder(e)
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                Self::expr_has_placeholder(expr)
            }
            Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
                es.iter().any(Self::expr_has_placeholder)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                Self::expr_has_placeholder(cond)
                    || then_branch.iter().any(Self::stmt_has_placeholder)
                    || else_branch.iter().any(Self::stmt_has_placeholder)
            }
            Stmt::Block(stmts) => stmts.iter().any(Self::stmt_has_placeholder),
            _ => false,
        }
    }

    pub(super) fn expr_has_placeholder(expr: &Expr) -> bool {
        match expr {
            Expr::Var(name) => name.starts_with('^'),
            Expr::CodeVar(name) => name.starts_with('^'),
            Expr::Binary { left, right, .. } => {
                Self::expr_has_placeholder(left) || Self::expr_has_placeholder(right)
            }
            Expr::Unary { expr, .. } => Self::expr_has_placeholder(expr),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::expr_has_placeholder(cond)
                    || Self::expr_has_placeholder(then_expr)
                    || Self::expr_has_placeholder(else_expr)
            }
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                args.iter().any(Self::expr_has_placeholder)
            }
            Expr::MethodCall { target, args, .. }
            | Expr::DynamicMethodCall { target, args, .. }
            | Expr::HyperMethodCall { target, args, .. }
            | Expr::HyperMethodCallDynamic { target, args, .. } => {
                Self::expr_has_placeholder(target) || args.iter().any(Self::expr_has_placeholder)
            }
            Expr::Index { target, index, .. } | Expr::IndexAssign { target, index, .. } => {
                Self::expr_has_placeholder(target) || Self::expr_has_placeholder(index)
            }
            Expr::CallOn { target, args } => {
                Self::expr_has_placeholder(target) || args.iter().any(Self::expr_has_placeholder)
            }
            Expr::StringInterpolation(parts)
            | Expr::ArrayLiteral(parts)
            | Expr::BracketArray(parts, _)
            | Expr::CaptureLiteral(parts) => parts.iter().any(Self::expr_has_placeholder),
            _ => false,
        }
    }

    /// Check for placeholder variable conflicts in a block/sub body.
    /// Returns a Value to die with if a conflict is found.
    /// `decl_kind` is Some("sub") for named subs, None for blocks.
    pub(super) fn check_placeholder_conflicts(
        &self,
        params: &[String],
        body: &[Stmt],
        decl_kind: Option<&str>,
    ) -> Option<Value> {
        use crate::ast::has_var_decl;
        use crate::placeholder_order::{
            bare_name_shadowed_by_nested_placeholder, bare_precedes_placeholder,
        };
        for param in params {
            let bare_name = if let Some(b) = param.strip_prefix("&^") {
                b
            } else if let Some(b) = param.strip_prefix('^') {
                b
            } else {
                continue;
            };
            // Check for `my $name` in the same scope → X::Redeclaration
            if has_var_decl(body, bare_name) {
                return Some(Value::str(format!(
                    "X::Redeclaration: Redeclaration of symbol '$^{}'",
                    bare_name
                )));
            }
            // Check if bare var precedes placeholder in the body
            if bare_precedes_placeholder(body, bare_name) {
                // If outer scope has this variable → X::Placeholder::NonPlaceholder
                if self.local_map.contains_key(bare_name) {
                    let decl = decl_kind.unwrap_or("block");
                    let message = format!(
                        "'${}' has already been used as a non-placeholder in the surrounding {}, \
                         so you will confuse the reader if you suddenly declare $^{} here",
                        bare_name, decl, bare_name
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "variable_name".to_string(),
                        Value::str(format!("${}", bare_name)),
                    );
                    attrs.insert(
                        "placeholder".to_string(),
                        Value::str(format!("$^{}", bare_name)),
                    );
                    attrs.insert("decl".to_string(), Value::str(decl.to_string()));
                    attrs.insert("message".to_string(), Value::str(message));
                    return Some(Value::make_instance(
                        Symbol::intern("X::Placeholder::NonPlaceholder"),
                        attrs,
                    ));
                } else {
                    // No outer declaration → X::Undeclared. Verified against
                    // `raku`: a bare `$name` preceding its own `$^name` does
                    // NOT get a "Did you mean" suggestion (the placeholder is
                    // not a candidate the suggestion mechanism considers) —
                    // it falls to the same default message as the
                    // nested-placeholder-shadow case below.
                    let symbol = format!("${}", bare_name);
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("name".to_string(), Value::str(symbol.clone()));
                    attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
                    attrs.insert("post".to_string(), Value::str(symbol.clone()));
                    attrs.insert("highexpect".to_string(), Value::array(vec![]));
                    attrs.insert("suggestions".to_string(), Value::array(vec![]));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Variable '{}' is not declared. Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?",
                            symbol
                        )),
                    );
                    return Some(Value::make_instance(Symbol::intern("X::Undeclared"), attrs));
                }
            }
        }
        // A bare `$name` used in THIS block's own scope, where `$^name` is
        // declared only by a block STRICTLY NESTED inside this one (a nested
        // `if`/`for`/`given` BLOCK body, `whenever`, or closure) — e.g.
        // `{ for 1 { $^b }; say $b }`. The inner block owns that placeholder;
        // it does not make `$b` this block's parameter, so `$b` here was
        // simply never declared — the same generic X::Undeclared rakudo
        // raises for any undeclared bare variable (unrelated to the nested
        // `$^name`, which is why the message does not mention it).
        if let Some(bare_name) = bare_name_shadowed_by_nested_placeholder(body, params)
            && !has_var_decl(body, &bare_name)
            && !self.local_map.contains_key(bare_name.as_str())
            // `bare_name` may also be legitimately declared as THIS block's own
            // (non-placeholder) signature parameter, e.g. `-> $b, $i { ... }`
            // where a totally separate nested closure happens to use `$^b` —
            // that inner closure's placeholder does not conflict with the
            // outer pointy block's own `$b` (see
            // `t/placeholder-nested-block-scope.t`'s "bitwise placeholder
            // blocks, slipped arguments" case).
            && !params.iter().any(|p| p == &bare_name)
        {
            let symbol = format!("${}", bare_name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(symbol.clone()));
            attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
            attrs.insert("post".to_string(), Value::str(symbol.clone()));
            attrs.insert("highexpect".to_string(), Value::array(vec![]));
            attrs.insert("suggestions".to_string(), Value::array(vec![]));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Variable '{}' is not declared. Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?",
                    symbol
                )),
            );
            return Some(Value::make_instance(Symbol::intern("X::Undeclared"), attrs));
        }
        None
    }

    /// Check for assignment to native-typed read-only parameters inside a
    /// sub/method/block body. Returns an X::Assignment::RO::Comp error value
    /// if such an assignment is found.
    pub(crate) fn check_native_readonly_param_assignment(
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) -> Option<Value> {
        // Build set of native-typed param names that are NOT `is rw` or `is copy`
        let readonly_native_params: std::collections::HashSet<&str> = param_defs
            .iter()
            .filter(|pd| {
                let is_native = pd.type_constraint.as_deref().is_some_and(|c| {
                    matches!(
                        c,
                        "int"
                            | "int8"
                            | "int16"
                            | "int32"
                            | "int64"
                            | "uint"
                            | "uint8"
                            | "uint16"
                            | "uint32"
                            | "uint64"
                            | "num"
                            | "num32"
                            | "num64"
                            | "str"
                    )
                });
                let has_rw_or_copy = pd
                    .traits
                    .iter()
                    .any(|t| t == "rw" || t == "copy" || t == "raw");
                is_native && !has_rw_or_copy
            })
            .map(|pd| pd.name.as_str())
            .collect();
        if readonly_native_params.is_empty() {
            return None;
        }
        fn scan_stmts(
            stmts: &[Stmt],
            readonly: &std::collections::HashSet<&str>,
        ) -> Option<String> {
            for stmt in stmts {
                if let Some(name) = scan_stmt(stmt, readonly) {
                    return Some(name);
                }
            }
            None
        }
        fn scan_stmt(stmt: &Stmt, readonly: &std::collections::HashSet<&str>) -> Option<String> {
            match stmt {
                Stmt::Assign { name, .. } => {
                    if readonly.contains(name.as_str()) {
                        return Some(format!("${}", name));
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if let Some(n) = scan_stmts(then_branch, readonly) {
                        return Some(n);
                    }
                    if let Some(n) = scan_stmts(else_branch, readonly) {
                        return Some(n);
                    }
                }
                Stmt::For { body, .. }
                | Stmt::While { body, .. }
                | Stmt::Loop { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::Default(body)
                | Stmt::Catch(body)
                | Stmt::Control(body) => {
                    if let Some(n) = scan_stmts(body, readonly) {
                        return Some(n);
                    }
                }
                Stmt::Given { body, .. } | Stmt::When { body, .. } => {
                    if let Some(n) = scan_stmts(body, readonly) {
                        return Some(n);
                    }
                }
                _ => {}
            }
            None
        }
        if let Some(var_name) = scan_stmts(body, &readonly_native_params) {
            let msg = format!("Cannot assign to readonly variable {}", var_name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("variable".to_string(), Value::str(var_name));
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            return Some(Value::make_instance(
                crate::symbol::Symbol::intern("X::Assignment::RO::Comp"),
                attrs,
            ));
        }
        None
    }
}
