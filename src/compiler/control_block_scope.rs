//! The statement-position bare-block scope emitter.
//!
//! Split out of the `Stmt::Block` arm of `Compiler::compile_stmt` when the two
//! bare-block lowerings were unified (see `control_block.rs`): this is the
//! `OpCode::BlockScope` half of `BlockShape::Plain`, and it is long enough
//! (block-entry shadow resets, `sub` hoisting, typed-declaration hoisting) to
//! deserve its own file.

use super::*;

impl Compiler {
    /// Emit `OpCode::BlockScope` around `stmts` compiled as statements.
    ///
    /// `is_bare` distinguishes a genuine source `{ ... }` (a Raku callframe)
    /// from a synthesized `if`/`while`/`loop` body re-wrapped in `Stmt::Block`.
    pub(super) fn emit_statement_block_scope(&mut self, stmts: &[Stmt], is_bare: bool) {
        // Plain blocks still create a lexical routine scope.
        // `BlockScope` snapshots env before the body and restores
        // it after (dropping any new env key), so a `my TYPE $x`
        // compiled while this flag is set can safely use the
        // env-only SetVarTypeScoped opcode — see
        // `lexically_in_block`'s doc comment.
        let saved_lexically_in_block = std::mem::replace(&mut self.lexically_in_block, true);
        let idx = self.code.emit(OpCode::BlockScope {
            pre_end: 0,
            enter_end: 0,
            body_end: 0,
            keep_start: 0,
            undo_start: 0,
            post_start: 0,
            end: 0,
            is_bare_block: is_bare,
        });
        self.code.patch_block_pre_end(idx);
        self.code.patch_block_enter_end(idx);
        // Raku's `my` declarations are visible for the entire
        // enclosing block, even though the value is only
        // (re-)initialized when execution reaches the declaration
        // statement. mutsu's per-routine local-slot storage
        // (`alloc_local` reuses the slot for a same-named
        // declaration) means a block's own `my $x` only shadows a
        // same-named outer local once its VarDecl statement
        // actually runs — so a hoisted nested `sub` invoked (via
        // forward reference) before that point would wrongly
        // observe the OUTER value instead of an undefined one.
        // Reset such shadowing slots to Nil right at block entry,
        // before hoisting nested subs, so the shadow is visible
        // from the very start of the block (roast
        // S04-declarations/my-6e.t: "declared below the calling
        // position"). Scoped narrowly to blocks that actually
        // declare a nested `sub` (the only way to observe the
        // early value) and to plain `$`-sigil scalars, to avoid
        // clobbering a lingering type constraint that a sibling
        // block left on a reused `@`/`%` slot (e.g. `my Int @a`
        // followed later by an untyped `my @a` sharing the slot —
        // an unconditional Nil reset there would fail the typed
        // slot's type check; a real initializer's value normally
        // satisfies it, so only the premature reset is unsafe).
        if stmts.iter().any(|s| matches!(s, Stmt::SubDecl { .. })) {
            for s in stmts.iter() {
                if let Stmt::VarDecl {
                    name,
                    is_state: false,
                    is_our: false,
                    custom_traits,
                    ..
                } = s
                    // Plain lexical scalars store a bare, sigil-stripped
                    // name (`my $x` -> "x"); `@`/`%`/`&` sigils and
                    // twigils (`.`/`!`/`*`) keep their marker prefix.
                    && !name.starts_with('@')
                    && !name.starts_with('%')
                    && !name.starts_with('&')
                    && !name.starts_with('.')
                    && !name.starts_with('!')
                    && !name.starts_with('*')
                    && !name.contains("::")
                    && let Some(&slot) = self.local_map.get(name.as_str())
                    && !custom_traits.iter().any(|(t, _)| t == "__constant")
                {
                    self.code.emit(OpCode::LoadNil);
                    self.code.emit(OpCode::SetLocal(slot));
                }
            }
        }
        // Hoist sub declarations: register subs first so forward
        // references like `&fa` work before the sub is textually
        // declared (Raku sub hoisting semantics).
        // Strip non-internal custom traits during hoisting — types/roles
        // may not be registered yet; traits are applied during the normal pass.
        for s in stmts.iter() {
            if let Stmt::SubDecl { .. } = s {
                let mut hoisted = s.clone();
                if let Stmt::SubDecl { custom_traits, .. } = &mut hoisted {
                    custom_traits.retain(|(t, _)| {
                        t.starts_with("__") || t == "default" || t.starts_with("DEPRECATED")
                    });
                    // Mark this copy as a hoist-pass registration,
                    // exactly as `hoist_sub_decls` does for the
                    // value-position (inline) block path. Without the
                    // marker the pre-pass looked like a real
                    // declaration, and an `our multi` inside a
                    // statement-form bare block hit the "Cannot
                    // declare individual multi candidates in 'our'
                    // scope" check here — before the block's own
                    // `our proto` had run. The in-sequence
                    // registration below runs after the proto and
                    // enforces the check for real.
                    if !custom_traits.iter().any(|(t, _)| t == "__lexical_hoist") {
                        custom_traits.push(("__lexical_hoist".to_string(), None));
                    }
                    if !custom_traits.iter().any(|(t, _)| t == "__hoisted") {
                        custom_traits.push(("__hoisted".to_string(), None));
                    }
                }
                self.compile_stmt(&hoisted);
            }
        }
        // Raku's `my TYPE $x` is in effect for the WHOLE block, not
        // just from its textual position, so an earlier statement
        // that reaches the name (an `EVAL '$x = ...'`, a nested sub
        // called before the declaration) must already see the
        // constraint. The value-position/inline block path does this
        // in `compile_block_inline`; this statement-position
        // `BlockScope` path did not, so the very same block silently
        // lost its declared types just because a statement followed
        // it (`t/typed-decl-hoist-block-forms.t`).
        self.hoist_typed_var_decls(stmts);
        for s in stmts {
            self.compile_stmt(s);
        }
        self.code.patch_block_body_end(idx);
        self.code.patch_block_keep_start(idx);
        self.code.patch_block_undo_start(idx);
        self.code.patch_block_post_start(idx);
        self.code.patch_loop_end(idx);
        self.lexically_in_block = saved_lexically_in_block;
    }
}
