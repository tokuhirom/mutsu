//! One `if`/`elsif`/`else` chain lowering, shared by both source positions.
//!
//! Like `for` (see `control_for.rs`), an `if` chain can be written as a
//! statement or as an expression that produces the taken branch's value. The
//! two used to be compiled by two independent implementations — the `Stmt::If`
//! arm of [`Compiler::compile_stmt`] and `compile_do_if_expr_bound` in
//! `helpers_do_expr.rs` — whose shared skeleton (condition, the duplicated
//! condition value that feeds `@_`/placeholder binding, the jump/patch
//! structure, the pointy-topic scope, the `elsif` recursion) was written out
//! twice and drifted: the statement copy was the one that grew the
//! statement-modifier placeholder guard, and only later did the value copy catch
//! up.
//!
//! This module is the single lowering. [`IfPosition`] selects the two things
//! that genuinely differ: how a branch body is emitted, and whether the chain
//! leaves a value behind.

use super::*;

/// The source position an `if` chain is being compiled for.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum IfPosition {
    /// `if EXPR { ... }` as a statement: the chain leaves nothing on the stack,
    /// and a branch body may be any statement shape (phasers, topic mutation,
    /// block-local declarations).
    Statement,
    /// `do if EXPR { ... }` / a routine's tail `if`: the chain leaves exactly
    /// one value on the stack. Callers gate this on
    /// [`Compiler::do_if_branch_supported`], which rejects the branch shapes the
    /// statement-only emitters above handle.
    Value,
}

impl Compiler {
    /// Compile an `if`/`elsif`/`else` chain, honouring an optional per-branch
    /// topic binding (`if EXPR -> $v { ... }`).
    ///
    /// When `binding_var` is `Some`, the condition value is bound to `$v` for
    /// the `then` branch — desugared exactly like `{ my $v = EXPR; if $v { ... }
    /// }` — so the branch does not leave `$v` (or `$_`) reading the enclosing
    /// topic. Inner `elsif`s thread their own binding through the recursion.
    ///
    /// Statement callers run the statement-level pre-checks (heredoc scope
    /// errors, constant-condition branch resolution) before calling this; both
    /// rewrite or replace the whole chain, so they are not part of the lowering.
    pub(super) fn compile_if_construct(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
        binding_var: &Option<String>,
        is_statement_modifier: bool,
        position: IfPosition,
    ) {
        let value_mode = position == IfPosition::Value;
        // A pointy `if EXPR -> $_ { }` binds a FRESH lexical `$_` (like `for ->
        // $_`), so its topic must NOT flow back to an enclosing `given $x`'s
        // source variable. `EnterPointyTopic` saves + clears `topic_source_var`
        // for the branch; `ExitPointyTopic` (at the end) restores it and the
        // outer `$_`. Only the topic var `$_` needs it — a named pointy (`->
        // $v`) declares its own lexical.
        let pointy_topic_scope = binding_var
            .as_deref()
            .is_some_and(|v| v.trim_start_matches('$') == "_");
        if pointy_topic_scope {
            self.code.emit(OpCode::EnterPointyTopic);
        }
        // A bare `if EXPR { ... }` block receives the condition value as `@_` in
        // Raku, and a scalar placeholder in it receives that same value (like
        // `if EXPR -> $a { ... }`), so `if 42 { $^a.say }` prints 42. The bind
        // itself (and the arity failure when the branch declares more
        // placeholders than the single condition value satisfies) is ADR-0048
        // D3's shared `emit_inlined_body_placeholder_binds`.
        //
        // An `if`/`unless`/`with`/`without` STATEMENT MODIFIER (including the
        // synthetic `If` that `with`/`without` desugar to) introduces no block of
        // its own — the oracle classifies it `Transparent` — so its "body"
        // placeholders are the enclosing routine's own parameters: `sub f { say
        // "$^a" if 1; 0 }; f(7)` must print 7, not the condition.
        let needs_at_underscore = binding_var.is_none() && Self::body_uses_legacy_args(then_branch);
        let bind_cond_placeholders = binding_var.is_none() && !is_statement_modifier;
        let binds_cond_placeholder =
            bind_cond_placeholders && Self::inlined_body_binds_supplied_value(then_branch);
        let needs_cond_value = needs_at_underscore || binds_cond_placeholder;

        let mut deferred_container_decl = None;
        if let Some(var_name) = binding_var {
            // Desugar `if EXPR -> $var { BODY }` into `{ my $var = EXPR; if $var
            // { BODY } }`. A binding never coexists with `needs_cond_value`
            // (both placeholder paths require `binding_var.is_none()`).
            let (desugared_cond, deferred) = self.compile_if_binding_decl(var_name, cond);
            deferred_container_decl = deferred;
            self.compile_condition_expr(&desugared_cond);
        } else {
            self.compile_condition_expr(cond);
        }
        if needs_cond_value {
            // Duplicate the condition value: one copy for `JumpIfFalse`'s
            // truthiness test, one for `@_` / the placeholder in the then branch.
            self.code.emit(OpCode::Dup);
        }
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        self.compile_if_binding_container_decl(&deferred_container_decl);
        if needs_at_underscore {
            // Flatten the duplicated condition into `@_` (like a `*@` slurpy).
            self.code.emit(OpCode::FlattenSlurpy);
            self.emit_set_named_var("@_");
        } else if bind_cond_placeholders {
            // ADR-0048 D3's shared bind: binds every placeholder the branch
            // declares that the single condition value can satisfy, and raises
            // raku's "Too few positionals passed" for the rest. Emitted inside
            // the taken branch so a never-taken `if 0 { "$^a $^b" }` raises
            // nothing, matching raku.
            self.emit_inlined_body_placeholder_binds(then_branch, ArgSupply::Condition);
        }
        // A branch is a block literal the enclosing block re-clones on every
        // execution, so its own `state` restarts each time — see
        // `OpCode::ResetStateLocals`.
        let then_state_reset = self.emit_branch_state_reset(then_branch, is_statement_modifier);
        self.compile_if_branch(then_branch, position);
        self.patch_nested_block_state_reset(then_state_reset);

        // A statement-position chain with no `else` simply falls through; a
        // value-position one still has to leave something behind.
        if else_branch.is_empty() && !value_mode {
            self.code.patch_jump(jump_else);
            if needs_cond_value {
                // Pop the leftover duplicated condition value on the false
                // branch (JumpIfFalse consumed only one copy).
                self.code.emit(OpCode::Pop);
            }
        } else {
            let jump_end = self.code.emit(OpCode::Jump(0));
            self.code.patch_jump(jump_else);
            if needs_cond_value {
                self.code.emit(OpCode::Pop);
            }
            if else_branch.is_empty() {
                // Value position: an untaken `if` yields an empty Slip, so it
                // vanishes in list context.
                let empty_idx = self.code.add_constant(Value::slip(vec![]));
                self.code.emit(OpCode::LoadConst(empty_idx));
            } else if else_branch.len() == 1
                && let Stmt::If {
                    cond: inner_cond,
                    then_branch: inner_then,
                    else_branch: inner_else,
                    binding_var: inner_binding,
                    is_statement_modifier: inner_is_modifier,
                    ..
                } = &else_branch[0]
            {
                match position {
                    // Statement position re-enters `compile_stmt` so the `elsif`
                    // gets the statement-level pre-checks (heredoc scope,
                    // constant-condition folding) too, inside the same branch
                    // state-reset bracket the non-`elsif` else gets.
                    IfPosition::Statement => {
                        let else_state_reset =
                            self.emit_branch_state_reset(else_branch, is_statement_modifier);
                        self.compile_stmt(&else_branch[0]);
                        self.patch_nested_block_state_reset(else_state_reset);
                    }
                    // Value position recurses directly: `compile_stmt` would
                    // compile the `elsif` for effect and leave no value.
                    IfPosition::Value => self.compile_if_construct(
                        inner_cond,
                        inner_then,
                        inner_else,
                        inner_binding,
                        *inner_is_modifier,
                        position,
                    ),
                }
            } else {
                let else_state_reset =
                    self.emit_branch_state_reset(else_branch, is_statement_modifier);
                self.compile_if_branch(else_branch, position);
                self.patch_nested_block_state_reset(else_state_reset);
            }
            self.code.patch_jump(jump_end);
        }
        if pointy_topic_scope {
            self.code.emit(OpCode::ExitPointyTopic);
        }
    }

    /// Emit one branch body of an `if` chain for the position it is compiled in.
    fn compile_if_branch(&mut self, branch: &[Stmt], position: IfPosition) {
        match position {
            // The branch must leave exactly one value. `do_if_branch_supported`
            // (checked by every value-position caller) has already rejected the
            // phaser shapes the statement emitters below exist for, so
            // `compile_block_inline` covers everything that reaches here.
            IfPosition::Value => self.compile_block_inline(branch),
            IfPosition::Statement => {
                if Self::has_block_enter_leave_phasers(branch) {
                    // A branch with ENTER/LEAVE/KEEP/UNDO phasers is a real
                    // block scope: its LEAVE must fire when the branch exits
                    // (OO::Monitors unlocks its monitor lock this way).
                    self.compile_phaser_block_scope(branch, PhaserBlockResult::Discard);
                } else if Self::body_mutates_topic(branch) {
                    self.synthetic_block_body = true;
                    self.compile_stmt(&Stmt::Block(branch.to_vec()));
                } else if Self::branch_declares_block_local(branch) {
                    self.compile_block_local_branch(branch);
                } else {
                    self.compile_body_with_implicit_try(branch);
                }
            }
        }
    }
}
