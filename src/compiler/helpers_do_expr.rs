use super::*;
use crate::compiler::block_shape::BlockShape;

impl Compiler {
    pub(super) fn compile_do_block_expr(&mut self, body: &[Stmt], label: &Option<String>) {
        // A `do {}` block does not take a signature, so a placeholder variable
        // used directly inside it cannot be captured -> X::Placeholder::Block.
        // Exception: inside a method, the legacy argument variable `%_` refers
        // to the method's implicit `*%_` slurpy and is valid here. `@_` is NOT
        // exempted: `raku` only auto-adds `*%_` to a method, never `*@_` —
        // referencing `@_` anywhere in a method body (directly or nested in a
        // `do {}`) is a compile-time error there too (`raku -e 'class A {
        // method m { @_.raku.say } }'` => "Placeholder variables (eg. @_)
        // cannot be used in a method. Please specify an explicit signature").
        // Pin: t/placeholder-named-in-method-do.t.
        // Exception: a placeholder that is already a bound parameter of the
        // ENCLOSING block — its local exists in `local_map` — is attached, not
        // stray. The chained-comparison desugar (`{ 0 <= $^p <= 5 }`) wraps the
        // body in a compiler-generated DoBlock inside the very AnonSubParams
        // that owns `^p`; dying here broke every subset/where written that way
        // (Cro::Core's `Cro::Port`). Pin: t/subset-where-placeholder-chain.t.
        if let Some(ph) = crate::ast::collect_unattached_placeholders(body)
            .into_iter()
            .find(|ph| {
                if self.lexically_in_method && ph == "%_" {
                    return false;
                }
                // A CARET placeholder (`$^p`) already bound as the enclosing
                // block's parameter is attached, not stray: the local exists in
                // `local_map` (same-compiler case), or the interpret-path
                // caller bound it in env and seeded `prebound_placeholder_params`
                // (re-entrant block eval — `call_sub_value` → `eval_block_value`
                // re-compiles the body alone). The chained-comparison desugar
                // (`{ 0 <= $^p <= 5 }`) wraps the body in a compiler-generated
                // DoBlock inside the very block that owns `^p`; dying here broke
                // every subset/where written that way (Cro::Core's `Cro::Port`).
                // Pin: t/subset-where-placeholder-chain.t. The `%_`/`@_` implicit
                // slurpies keep the strict rule (only a METHOD provides them) —
                // pin: t/placeholder-named-in-method-do.t.
                let bare = ph.trim_start_matches(['$', '@', '%', '&']);
                let attached_caret = bare.starts_with('^')
                    && (self.local_map.contains_key(ph.as_str())
                        || self.local_map.contains_key(bare)
                        || self.prebound_placeholder_params.contains(bare));
                !attached_caret
            })
        {
            let err = crate::method_signature_shared::placeholder_scope_error("block", &ph);
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die { user_throw: false });
            return;
        }
        // DoBlocks from lifted CHECK phasers carry a sentinel label so we can
        // wrap them in CheckPhaserStart/CheckPhaserEnd, ensuring errors inside
        // are wrapped in X::Comp::BeginTime.
        if matches!(label, Some(l) if l == "__mutsu_check_phaser__") {
            let start_idx = self.code.emit(OpCode::CheckPhaserStart { end_ip: 0 });
            // Compile the inner DoBlock normally (without the sentinel label)
            self.compile_do_block_expr(body, &None);
            self.code.emit(OpCode::CheckPhaserEnd);
            let end_ip = self.code.ops.len() as u32;
            if let OpCode::CheckPhaserStart { end_ip: ref mut e } = self.code.ops[start_idx] {
                *e = end_ip;
            }
            return;
        }
        // A value-position block (`do { … }`, a routine's tail `{ … }`, a
        // string-interpolation `{ … }`) is a block literal re-cloned every time
        // its ENCLOSING block runs, so its own `state` restarts per execution —
        // see `OpCode::ResetStateLocals`. This is what makes raku's documented
        // trap `sub count-it { say "Count is {$++}" }` print `0` every call.
        //
        // Emitted BEFORE the shape dispatch, exactly as the statement form does
        // it in `Stmt::Block`. It used to sit after the CATCH/phaser arms'
        // early returns, so `do { state $n = 0; $n++; CATCH {…}; $n }` counted
        // 1, 2, 3 across calls where the same block without the `CATCH` — and
        // the statement-position spelling of either — correctly restarted at 1.
        let state_reset = self.emit_value_block_state_reset(body);
        // The shape decision is shared with the statement-position form
        // (`Stmt::Block`) so the two passes cannot disagree about what this
        // block is — see `BlockShape`.
        match Self::classify_block_shape(body) {
            // Compile as try so exceptions are handled.
            BlockShape::ImplicitTry => {
                self.compile_implicit_try(body);
            }
            // Wrap in DoBlockExpr + BlockScope so phaser semantics are preserved.
            BlockShape::PhaserScope => {
                let do_idx = self.code.emit(OpCode::DoBlockExpr {
                    body_end: 0,
                    label: label.clone(),
                    scope_isolate: false,
                    isolate_decls_idx: u32::MAX,
                    scope_routines: Self::stmts_declare_routines(body),
                });
                let saved = self.push_dynamic_scope_lexical();
                self.compile_phaser_block_scope(body, PhaserBlockResult::Push);
                self.pop_dynamic_scope_lexical(saved);
                self.code.patch_body_end(do_idx);
            }
            // `BlockShape::LetBlock` is deliberately NOT taken here, and falls
            // through to the plain arm: `Expr::DoBlock` is not a Raku block.
            // The parser and a dozen compiler desugars use it as a generic
            // "run these statements, yield a value" node — item context
            // (`$( let $a = 23; $a )`), the chained-comparison desugar, `cas`,
            // compound-assignment lowering — and NONE of those introduce a
            // scope a `let` may resolve at. Making this arm emit its own
            // `LetBlock` resolved the save at the innermost wrapper instead of
            // the enclosing block, so roast's
            // `{ is($(let $a = 23; $a), 23, …); Mu }` stopped restoring `$a`
            // (S04-blocks-and-statements/let.t, temp.t).
            //
            // A genuine source `do { let $x = 2; Nil }` therefore still fails
            // to roll back, matching the behaviour that predates the shared
            // classifier. Fixing it needs a marker distinguishing a real source
            // block from a synthesized wrapper on the AST node itself — see
            // GH-7635 and ADR-0076 §5.
            shape @ (BlockShape::LetBlock | BlockShape::ImportScope | BlockShape::Plain) => {
                // An import is lexical to the block that asked for it, and a
                // `do {}` block is a block: `my (&plan) = do { use Test;
                // (&plan) }` must take the routines it names as values and
                // leave everything else the module exports out of the enclosing
                // scope (roast/S32-list/skip.t imports selectively precisely so
                // the CORE `skip` stays visible). The statement-form bare block
                // already does this in `Stmt::Block`.
                let import_scoped = shape == BlockShape::ImportScope;
                if import_scoped {
                    self.code.emit(OpCode::PushImportScope);
                }
                let idx = self.code.emit(OpCode::DoBlockExpr {
                    body_end: 0,
                    label: label.clone(),
                    scope_isolate: false,
                    isolate_decls_idx: u32::MAX,
                    scope_routines: Self::stmts_declare_routines(body),
                });
                self.compile_block_inline(body);
                self.code.patch_body_end(idx);
                if import_scoped {
                    self.code.emit(OpCode::PopImportScope);
                }
            }
        }
        self.patch_nested_block_state_reset(state_reset);
    }

    /// [`Compiler::emit_nested_block_state_reset`] for a value-position block,
    /// honouring the sole-block loop-body suppression the statement form
    /// consumes in `Stmt::Block` (`do { state $n … } for @xs` is the loop's own
    /// body, cloned once for the whole loop).
    fn emit_value_block_state_reset(&mut self, body: &[Stmt]) -> Option<usize> {
        let suppress = std::mem::take(&mut self.suppress_loop_block_state_reset);
        (!suppress)
            .then(|| self.emit_nested_block_state_reset(body))
            .flatten()
    }

    pub(super) fn compile_do_block_expr_scoped(&mut self, body: &[Stmt], label: &Option<String>) {
        // Same per-execution `state` restart as the unscoped sibling above.
        let state_reset = self.emit_value_block_state_reset(body);
        let idx = self.code.emit(OpCode::DoBlockExpr {
            body_end: 0,
            label: label.clone(),
            scope_isolate: true,
            isolate_decls_idx: u32::MAX,
            scope_routines: Self::stmts_declare_routines(body),
        });
        // Record every `my`/`state` declaration compiled in the body (including
        // ones nested in expressions like `(state $a)++` and ones shadowing an
        // outer same-name) so the scope-isolating exit reverts exactly those
        // while letting OUTER-variable mutations persist. A nested closure
        // compiles in a fresh `Compiler`, so it never contributes here.
        self.block_decl_tracker.push(Vec::new());
        self.compile_block_inline(body);
        let mut decls = self.block_decl_tracker.pop().unwrap_or_default();
        // Hashes are intentionally NOT isolated: a `my %h` (e.g.
        // `:into(my %h := :{})`) must survive into the enclosing scope.
        decls.retain(|n| !n.starts_with('%') && !n.starts_with('&'));
        if !decls.is_empty() {
            let decls_idx = self.code.add_constant(Value::array(
                decls.into_iter().map(Value::str).collect::<Vec<_>>(),
            ));
            if let OpCode::DoBlockExpr {
                isolate_decls_idx, ..
            } = &mut self.code.ops[idx]
            {
                *isolate_decls_idx = decls_idx;
            }
        }
        self.code.patch_body_end(idx);
        self.patch_nested_block_state_reset(state_reset);
    }

    /// Compile an `if`/`elsif` chain in value (expression) position.
    ///
    /// A thin wrapper over the shared lowering in `control_if.rs`; the only
    /// difference from the statement form is
    /// [`IfPosition::Value`](crate::compiler::control_if::IfPosition::Value).
    pub(super) fn compile_do_if_expr_bound(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
        binding_var: &Option<String>,
        is_statement_modifier: bool,
    ) {
        self.compile_if_construct(
            cond,
            then_branch,
            else_branch,
            binding_var,
            is_statement_modifier,
            crate::compiler::control_if::IfPosition::Value,
        );
    }

    /// Compile a `for` in expression position.
    ///
    /// Identical to the statement form apart from `collect`: the shared
    /// lowering in `control_for.rs` gathers each iteration's value and leaves
    /// the resulting list on the stack.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_do_for_expr(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        params: &[String],
        params_def: &[crate::ast::ParamDef],
        rw_block: bool,
        explicit_zero_params: bool,
        body: &[Stmt],
        label: &Option<String>,
        is_statement_modifier: bool,
        mode: crate::ast::ForMode,
        uses_block_magic: bool,
    ) {
        // Parser currently lowers labeled `do { ... }` / labeled bare blocks into
        // a dummy single-iteration `for Nil` with a label. Preserve block semantics
        // here so control flow like `LABEL.leave(...)` returns the block value.
        // A real `for Nil -> { ... }` is NOT one of those: its explicit empty
        // signature makes it an ordinary (and immediately failing) loop.
        if param.is_none()
            && params.is_empty()
            && !explicit_zero_params
            && matches!(
                iterable,
                Expr::ArrayLiteral(items)
                    if items.len() == 1
                        && matches!(&items[0], Expr::Literal(lit) if lit.is_nil())
            )
        {
            self.compile_do_block_expr(body, label);
            return;
        }
        self.compile_for_construct(crate::compiler::control_for::ForParts {
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
            is_statement_modifier,
            uses_block_magic,
            collect: true,
        });
    }

    /// Compile `lazy for` expression: lower to `gather { for @items -> $param { take do { body } } }`.
    /// This defers execution of the body until the resulting Seq is consumed.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_lazy_for_expr(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        params: &[String],
        params_def: &[crate::ast::ParamDef],
        rw_block: bool,
        explicit_zero_params: bool,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        // Build `take <last_expr>` body: replace the last expression with `take <expr>`
        // and wrap the body in a for loop inside a gather block.
        let take_body = Self::wrap_loop_body_last_in_take(body);
        // Build inner for loop with the take body
        let inner_for = AStmt::For {
            iterable: iterable.clone(),
            param: param.clone(),
            param_def: Box::new(param_def.clone()),
            params: params.to_vec(),
            params_def: params_def.to_vec(),
            body: take_body,
            label: label.clone(),
            mode: crate::ast::ForMode::Normal,
            rw_block,
            explicit_zero_params,
            // Placeholders were already resolved on the source `lazy for` node;
            // this synthesized loop wraps an ordinary block body.
            is_statement_modifier: false,
            uses_block_magic: false,
        };
        // Build gather block wrapping the for loop, then mark it `.lazy` so the
        // body does not run until the resulting Seq is consumed (`lazy for`
        // semantics — S04 for.t "Lazy for loop does not execute until asked").
        let gather_body = vec![inner_for];
        let gather_expr = AExpr::Gather(gather_body);
        let lazy_expr = AExpr::MethodCall {
            target: Box::new(gather_expr),
            name: crate::symbol::Symbol::intern("lazy"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        };
        self.compile_expr(&lazy_expr);
    }

    /// Rewrite a loop body so its per-iteration value is `take`n: the last
    /// expression statement becomes `take <expr>`; a value-bearing `if`/`given`
    /// last statement is taken through `do` (so a false `if`-modifier yields
    /// `Empty`, which `take` slips away); any other shape appends `take Nil`.
    /// The loop's KEEP/UNDO result capture sees through the trailing `Take`
    /// (see `expand_loop_phasers`). Shared by the `lazy for` lowering and the
    /// `while`/`loop` expression forms.
    fn wrap_loop_body_last_in_take(body: &[Stmt]) -> Vec<Stmt> {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let mut stmts = body.to_vec();
        let last_idx = stmts.iter().rposition(|s| !matches!(s, AStmt::SetLine(_)));
        if let Some(idx) = last_idx {
            match stmts[idx].clone() {
                AStmt::Expr(expr) => stmts[idx] = AStmt::Take(expr, false),
                s @ (AStmt::If { .. } | AStmt::Given { .. }) => {
                    stmts[idx] = AStmt::Take(AExpr::DoStmt(Box::new(s)), false);
                }
                _ => {
                    stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::NIL), false));
                }
            }
        } else {
            stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::NIL), false));
        }
        stmts
    }

    /// Compile `do while` / `do until` (and parenthesized `(while ...)`)
    /// expression: lower to `gather { while COND { take do { body } } }` so
    /// the result is a lazy Seq pulled on demand, matching raku —
    /// `(while $++ < 2 { 42.say; 43 }).map: *.say` interleaves 42/43, and
    /// the whole loop only runs to completion when the Seq is reified.
    pub(super) fn compile_do_while_expr(
        &mut self,
        cond: &Expr,
        body: &[Stmt],
        label: &Option<String>,
        is_until: bool,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let inner = AStmt::While {
            cond: cond.clone(),
            body: Self::wrap_loop_body_last_in_take(body),
            label: label.clone(),
            is_statement_modifier: false,
            is_until,
        };
        let gather_expr = AExpr::Gather(vec![inner]);
        self.compile_expr(&gather_expr);
    }

    /// Compile `do loop (...) { ... }` / `(loop { ... })` expression: lower to
    /// `gather { loop (...) { take do { body } } }` — a lazy Seq, so an
    /// infinite `(loop { 42.say })[2]` pulls exactly three iterations.
    /// The C-style init runs inside the gather, deferred until first pull.
    pub(super) fn compile_do_loop_expr(
        &mut self,
        init: &Option<Box<Stmt>>,
        cond: &Option<Expr>,
        step: &Option<Expr>,
        body: &[Stmt],
        label: &Option<String>,
        is_until: bool,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let inner = AStmt::Loop {
            init: init.clone(),
            cond: cond.clone(),
            step: step.clone(),
            body: Self::wrap_loop_body_last_in_take(body),
            repeat: false,
            label: label.clone(),
            is_until,
        };
        let gather_expr = AExpr::Gather(vec![inner]);
        self.compile_expr(&gather_expr);
    }

    pub(super) fn do_if_branch_supported(stmts: &[Stmt]) -> bool {
        if Self::has_phasers(stmts) {
            return false;
        }
        let last = stmts.len().saturating_sub(1);
        for (i, stmt) in stmts.iter().enumerate() {
            match stmt {
                // A `given` in final position is handled by compile_block_inline
                // (it leaves the block value on the stack); only a non-final
                // `given` is unsupported here.
                Stmt::Given { .. } if i != last => return false,
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } if !Self::do_if_branch_supported(then_branch)
                    || !Self::do_if_branch_supported(else_branch) =>
                {
                    return false;
                }
                _ => {}
            }
        }
        true
    }
}
