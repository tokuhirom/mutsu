//! One bare-`{ ... }`-block lowering, shared by both source positions.
//!
//! Like `for` (`control_for.rs`) and `if` (`control_if.rs`) before it, a bare
//! block can be written as a statement (`Stmt::Block`) or as something that
//! produces a value (`do { ... }`, a block used as a term). The two used to be
//! compiled by two independent passes — the `Stmt::Block` arm of
//! [`Compiler::compile_stmt`] and `compile_do_block_expr` in
//! `helpers_do_expr.rs` — that each decided, on their own, whether the body
//! declares routines, imports, needs a per-execution `state` reset, is an
//! implicit `try`, is a phaser block scope, or is a `let`/`temp` save-restore
//! scope. Predictably the two answers drifted, and the value copy was the one
//! left behind:
//!
//! - it never grew the `let`/`temp` branch, so `do { temp $x = 2 }` never
//!   restored `$x` (the statement form did);
//! - it never grew the sigilless-shadow bookkeeping, so a `my \str` declared
//!   inside it kept shadowing the native type name after the block ended;
//!
//! and the statement copy had drifted the other way: it treated a `use` in the
//! body as an exclusive *shape*, so `{ use Foo; my $y = 42 }` got an import
//! scope and no block scope at all, and leaked `$y`.
//!
//! [`BlockPlan::analyze`] is now the one place those questions are answered and
//! [`Compiler::compile_block_construct`] the one skeleton that consumes the
//! answer. [`BlockPosition`] names everything that genuinely differs between
//! the two positions.
//!
//! The two *opcodes* (`OpCode::BlockScope` and `OpCode::DoBlockExpr`) stay
//! distinct; see `docs/adr/0076-bare-block-lowering-and-block-scope-opcodes.md`
//! for what each guarantees and why merging them is a separate, later step.

use super::*;
use crate::opcode::DoBlockIsolation;

/// The source position a bare block is being compiled for.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum BlockPosition {
    /// `{ ... }` as a statement: the body is compiled statement-by-statement
    /// and the construct leaves nothing on the value stack. Emits
    /// `OpCode::BlockScope`, which is a full lexical scope (env restore,
    /// callframe, PRE/ENTER/KEEP/UNDO/POST sections).
    Statement,
    /// `do { ... }` / a block used as a term: the body is compiled with
    /// [`Compiler::compile_block_inline`] and the construct leaves exactly one
    /// value on the stack. Emits `OpCode::DoBlockExpr`.
    Value {
        /// `OpCode::DoBlockExpr`'s `scope_isolate`: revert the block's own
        /// scalar/array `my`/`state` declarations on exit while letting
        /// mutations of outer variables persist.
        isolate: bool,
        /// Whether the node came from real source braces that *are* a Raku
        /// block, or from one of the ~40 desugars that use `Expr::DoBlock` as a
        /// generic sequencing vehicle. Only the first owns a `let`/`temp` save
        /// frame — see [`crate::ast::DoBlockOrigin`] and GH-7635.
        origin: crate::ast::DoBlockOrigin,
    },
}

impl BlockPosition {
    fn is_value(self) -> bool {
        matches!(self, Self::Value { .. })
    }

    /// What `OpCode::DoBlockExpr` reverts on exit. A genuine source block is
    /// a lexical scope, so its own declarations never outlive it
    /// (`do { my $zz = 9; 1 }; $::("zz")` finds nothing — #9897); a desugar
    /// declares into the enclosing scope unless it asks for the
    /// interpolation policy (`isolate`).
    fn isolation(self) -> DoBlockIsolation {
        match self {
            Self::Statement => DoBlockIsolation::None,
            Self::Value {
                origin: crate::ast::DoBlockOrigin::SourceBlock,
                ..
            } => DoBlockIsolation::Lexical,
            Self::Value { isolate: true, .. } => DoBlockIsolation::Interpolation,
            Self::Value { .. } => DoBlockIsolation::None,
        }
    }

    /// Whether a `let`/`temp` in the body resolves at THIS block.
    ///
    /// A statement `{ ... }` always is a block. A value-position node is one
    /// only when the parser minted it from source braces; a desugar's node
    /// opens no scope, so a save inside it belongs to whatever real block
    /// encloses it.
    fn owns_let_scope(self) -> bool {
        match self {
            Self::Statement => true,
            Self::Value { origin, .. } => origin == crate::ast::DoBlockOrigin::SourceBlock,
        }
    }
}

/// The exclusive shape a body forces on the block's lowering, decided once by
/// [`BlockPlan::analyze`]. The order the variants are tested in is the order
/// they are declared in here, and it is load-bearing: a `CATCH` wins over a
/// `LEAVE`, which wins over a `temp`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum BlockShape {
    /// A `CATCH`/`CONTROL` phaser makes the block an implicit `try`.
    ImplicitTry,
    /// `ENTER`/`LEAVE`/`KEEP`/`UNDO` make it a real phaser block scope.
    PhaserScope,
    /// `let`/`temp` need the save/restore frame of `OpCode::LetBlock`.
    ///
    /// This is the plain shape PLUS a save frame, not an alternative to it: the
    /// frame nests inside the position's ordinary scope opcode so a `let` block
    /// keeps the env restore that stops its own `my` from leaking (GH-7645).
    LetScope,
    /// An ordinary block scope.
    Plain,
}

/// The `let`/`temp` save frame a block body is bracketed by.
#[derive(Clone, Copy)]
pub(super) struct LetFrame {
    /// A real `let` (not just a `temp`) is present, so the block's own value
    /// decides whether the saves are kept or rolled back and has to reach the
    /// place `exec_let_block_op` reads it from.
    pub(super) needs_value: bool,
}

/// Every question both positions used to answer for themselves.
pub(super) struct BlockPlan {
    /// See [`BlockShape`].
    shape: BlockShape,
    /// The body has a `use`/`no`/`import` directly in it. An import is lexical
    /// to the block that asked for it, so the block is bracketed by
    /// an `ImportScope` region — in BOTH positions:
    /// `my (&plan) = do { use Test; (&plan) }` must take the routines it names
    /// as values and leave the rest of the module's exports out of the
    /// enclosing scope.
    import_scope: bool,
    /// The body declares a `sub`/`proto` directly in its own scope, so the
    /// routine registry has to be snapshotted around it (`OpCode::DoBlockExpr`'s
    /// `scope_routines`; `OpCode::BlockScope` always snapshots).
    declares_routines: bool,
    /// A real `let` (not just a `temp`) is present. See [`LetFrame::needs_value`].
    let_needs_value: bool,
    /// An escaping `when`/`default` succeed stops unwinding at this block.
    /// Statement position only: the value form catches `succeed` inside
    /// `OpCode::DoBlockExpr` itself, because it has to push the escaping value.
    succeed_barrier: bool,
}

impl BlockPlan {
    fn analyze(stmts: &[Stmt], position: BlockPosition) -> Self {
        let shape = if Compiler::has_catch_or_control(stmts) {
            BlockShape::ImplicitTry
        } else if Compiler::has_block_enter_leave_phasers(stmts) {
            BlockShape::PhaserScope
        } else if position.owns_let_scope() && Compiler::has_let_deep(stmts) {
            BlockShape::LetScope
        } else {
            BlockShape::Plain
        };
        Self {
            shape,
            import_scope: Compiler::has_use_stmt(stmts),
            declares_routines: Compiler::stmts_declare_routines(stmts),
            let_needs_value: shape == BlockShape::LetScope && Compiler::has_real_let_deep(stmts),
            succeed_barrier: !position.is_value() && Compiler::body_has_toplevel_when(stmts),
        }
    }

    /// The save frame this block's body is bracketed by, if any.
    fn let_frame(&self) -> Option<LetFrame> {
        (self.shape == BlockShape::LetScope).then_some(LetFrame {
            needs_value: self.let_needs_value,
        })
    }
}

impl Compiler {
    /// Compile a bare `{ ... }` block in either source position.
    ///
    /// `label` is the block's own label (only the value position carries one
    /// today: the parser lowers a labelled bare block into a single-iteration
    /// `for Nil`, which `compile_do_for_expr` routes back here).
    pub(super) fn compile_block_construct(
        &mut self,
        stmts: &[Stmt],
        label: &Option<String>,
        position: BlockPosition,
    ) {
        if self.emit_block_placeholder_gate(stmts, position) {
            return;
        }
        // DoBlocks lifted out of a CHECK phaser carry a sentinel label so the
        // body can be bracketed by CheckPhaserStart/CheckPhaserEnd, which is
        // what wraps an error inside it in X::Comp::BeginTime.
        if position.is_value() && matches!(label, Some(l) if l == "__mutsu_check_phaser__") {
            let start_idx = self.code.emit(OpCode::CheckPhaserStart { end_ip: 0 });
            self.compile_block_construct(stmts, &None, position);
            self.code.emit(OpCode::CheckPhaserEnd);
            let end_ip = self.code.ops.len() as u32;
            if let OpCode::CheckPhaserStart { end_ip: ref mut e } = self.code.ops[start_idx] {
                *e = end_ip;
            }
            return;
        }

        let plan = BlockPlan::analyze(stmts, position);

        // A bare block is where an escaping `when`/`default` succeed stops
        // unwinding when nothing closer (a `given`, another bare block, an `if`
        // branch, ...) already caught it: `given 5 { { when Int { } }; say
        // "after" }` still runs the `say` (see `OpCode::SucceedBarrier`), and so
        // does a `when` with no topicalizer at all reached through a nested
        // expression (`{ $a = do when .so { "x" } }`). Emitted after the
        // placeholder bail-out above so the barrier is never left unpatched.
        let succeed_barrier_idx = plan
            .succeed_barrier
            .then(|| self.code.emit(OpCode::SucceedBarrier { body_end: 0 }));
        // A lexical scope frame is what `OUTER::` counts, so exactly one may be
        // pushed per block. The statement position pushes it here; the value
        // position's is pushed by the body emitter it delegates to
        // (`compile_block_inline`, `compile_phaser_block_scope`), so pushing a
        // second one here would add a spurious level to every `$OUTER::x`
        // resolved inside a `do { ... }`.
        let saved_dynamic_scope = match position {
            BlockPosition::Statement => Some(self.push_dynamic_scope_lexical()),
            BlockPosition::Value { .. } => None,
        };
        // A `sub push`/`sub pop`/... declared in the block shadows the list-op
        // form of that builtin for the block's own body only.
        let saved_listop_shadows = self.user_listop_shadows.clone();
        self.seed_user_listop_shadows(stmts);
        // Snapshot the sigilless bindings that name a native lowercase type
        // (`str`/`int`/...). A `my \str` declared *inside* this block is
        // lexically scoped to it, so it must stop shadowing the native type once
        // the block ends; drop any such name the block newly registers on exit.
        // Scoped to type names only to keep the (pre-existing) leak behaviour of
        // ordinary sigilless names.
        let sigilless_types_before = self.snapshot_sigilless_type_names();
        // A genuine source `{ ... }` is a Raku callframe (it contributes an
        // anonymous frame to a backtrace captured inside it); a synthesized
        // if/while/loop body is not. `synthetic_block_body` is set by those
        // compile sites and consumed here. A value-position block is always
        // genuine — no construct synthesizes one.
        let is_bare = match position {
            BlockPosition::Statement => !std::mem::take(&mut self.synthetic_block_body),
            BlockPosition::Value { .. } => true,
        };
        // A genuine source block is re-cloned every time its enclosing block
        // runs, so its own `state` restarts per execution — see
        // `OpCode::ResetStateLocals`. This is what makes raku's documented trap
        // `sub count-it { say "Count is {$++}" }` print `0` every call. A
        // SYNTHETIC body is excluded: a loop body is the block the loop
        // statement clones ONCE (its iterations share the state), and an `if`
        // branch already got its reset at the branch site. A sole-block loop
        // body (`{ ... } for @xs`) is likewise the loop's own body — the loop
        // compile sites set `suppress_loop_block_state_reset` so its `state`
        // persists across iterations.
        let suppress_loop_reset = std::mem::take(&mut self.suppress_loop_block_state_reset);
        let state_reset = (is_bare && !suppress_loop_reset)
            .then(|| self.emit_nested_block_state_reset(stmts))
            .flatten();
        // #9521: mirror the runtime save/restore this opcode pair performs on
        // `fatal_mode` at compile time, on `Compiler::fatal_pragma_active`
        // (only a block that owns an import scope can have changed it).
        let saved_fatal_pragma_active = plan.import_scope.then_some(self.fatal_pragma_active);
        // `plan.import_scope` is the same `has_use_stmt` test the region makes.
        self.with_import_scope_region(stmts, |c| {
            c.emit_block_shape(stmts, label, position, &plan, is_bare)
        });
        if let Some(saved) = saved_fatal_pragma_active {
            self.fatal_pragma_active = saved;
        }
        self.restore_sigilless_type_names(sigilless_types_before);
        self.user_listop_shadows = saved_listop_shadows;
        if let Some(saved) = saved_dynamic_scope {
            self.pop_dynamic_scope_lexical(saved);
        }
        self.patch_nested_block_state_reset(state_reset);
        if let Some(idx) = succeed_barrier_idx {
            self.code.patch_succeed_barrier_body_end(idx);
        }
    }

    /// Emit the scope opcode(s) the plan's [`BlockShape`] calls for, plus the
    /// body itself.
    fn emit_block_shape(
        &mut self,
        stmts: &[Stmt],
        label: &Option<String>,
        position: BlockPosition,
        plan: &BlockPlan,
        is_bare: bool,
    ) {
        match plan.shape {
            BlockShape::ImplicitTry => {
                if position.is_value() {
                    self.compile_implicit_try(stmts);
                } else {
                    self.next_try_is_bare_block = is_bare;
                    self.compile_implicit_try(stmts);
                    self.next_try_is_bare_block = false;
                    self.code.emit(OpCode::Pop);
                }
            }
            BlockShape::PhaserScope => {
                if position.is_value() {
                    // The phaser block scope is nested inside a `DoBlockExpr` so
                    // the block still catches `leave`/`succeed` and normalizes
                    // the value it leaves behind, exactly like the plain shape.
                    let do_idx = self.emit_do_block_expr(label, position, plan);
                    let saved = self.push_dynamic_scope_lexical();
                    self.compile_phaser_block_scope(stmts, PhaserBlockResult::Push);
                    self.pop_dynamic_scope_lexical(saved);
                    self.code.patch_body_end(do_idx);
                } else {
                    self.compile_phaser_block_scope(stmts, PhaserBlockResult::Discard);
                }
            }
            // A `let`/`temp` block is the plain block PLUS an
            // `OpCode::LetBlock` save frame, so both arms go through
            // `emit_plain_block` and differ only in where the frame nests.
            //
            // A real `let` rolls its saves back unless the block succeeded, so
            // `exec_let_block_op` needs the block's own value — and the two
            // positions leave it in different places. The statement form routes
            // its last statement through `compile_last_stmt_as_topic` and the op
            // reads the topic; the value form already has the value on the stack
            // and must NOT write the topic, or a `do { ... }` would clobber `$_`
            // for the enclosing scope (GH-7635). `value_on_stack` is which of the
            // two the op reads.
            //
            // Value position: the frame wraps `OpCode::DoBlockExpr`, whose value
            // is on the stack for the op to peek once the block is done.
            // Statement position: the frame nests INSIDE `OpCode::BlockScope`
            // (`emit_body_let_frame`), because the topic it reads is one
            // `BlockScope`'s exit deliberately does not propagate outwards.
            BlockShape::LetScope if position.is_value() => {
                let idx = self.code.emit(OpCode::LetBlock {
                    body_end: 0,
                    value_on_stack: true,
                });
                self.emit_plain_block(stmts, label, position, plan, is_bare);
                self.code.patch_let_block_end(idx);
            }
            BlockShape::LetScope | BlockShape::Plain => {
                self.emit_plain_block(stmts, label, position, plan, is_bare)
            }
        }
    }

    /// The ordinary block scope: `OpCode::BlockScope` in statement position,
    /// `OpCode::DoBlockExpr` in value position.
    fn emit_plain_block(
        &mut self,
        stmts: &[Stmt],
        label: &Option<String>,
        position: BlockPosition,
        plan: &BlockPlan,
        is_bare: bool,
    ) {
        if !position.is_value() {
            self.emit_statement_block_scope(stmts, is_bare, plan.let_frame());
            return;
        }
        let idx = self.emit_do_block_expr(label, position, plan);
        let isolation = position.isolation();
        match isolation {
            // Record every `my`/`state` declaration compiled in the body's own
            // scope (including ones nested in expressions like `(state $a)++`
            // and ones shadowing an outer same-name) so the scope-isolating
            // exit reverts exactly those while letting OUTER-variable mutations
            // persist. `compile_block_inline_body` opens that scope, one level
            // below the current one. A nested closure compiles in a fresh
            // `Compiler`, so it never contributes here.
            DoBlockIsolation::Lexical | DoBlockIsolation::Interpolation => {
                self.block_decl_tracker
                    .push((self.decl_scope_depth + 1, Vec::new()));
            }
            // A desugar is not a scope of its own: a `my` in it belongs to the
            // enclosing block, and so to that block's tracker.
            DoBlockIsolation::None => self.next_scope_decl_transparent = true,
        }
        // `compile_block_construct` already opened this block's import scope.
        self.compile_block_inline_body(stmts);
        if isolation != DoBlockIsolation::None {
            let (_, mut decls) = self.block_decl_tracker.pop().unwrap_or_default();
            // The interpolation policy intentionally does NOT isolate hashes:
            // a `my %h` (e.g. `:into(my %h := :{})`) must survive into the
            // enclosing scope.
            if isolation == DoBlockIsolation::Interpolation {
                decls.retain(|n| !n.starts_with('%') && !n.starts_with('&'));
            }
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
        }
        self.code.patch_body_end(idx);
    }

    fn emit_do_block_expr(
        &mut self,
        label: &Option<String>,
        position: BlockPosition,
        plan: &BlockPlan,
    ) -> usize {
        self.code.emit(OpCode::DoBlockExpr {
            body_end: 0,
            label: label.clone(),
            isolation: position.isolation(),
            isolate_decls_idx: u32::MAX,
            scope_routines: plan.declares_routines,
        })
    }

    fn snapshot_sigilless_type_names(&self) -> std::collections::HashSet<String> {
        self.sigilless_locals
            .iter()
            .filter(|n| crate::runtime::Interpreter::is_builtin_type(n))
            .cloned()
            .collect()
    }

    fn restore_sigilless_type_names(&mut self, before: std::collections::HashSet<String>) {
        self.sigilless_locals
            .retain(|n| before.contains(n) || !crate::runtime::Interpreter::is_builtin_type(n));
    }
}
