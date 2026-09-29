//! The placeholder rules of the bare-block lowering (`control_block.rs`): the
//! one part of it that is genuinely position-specific (ADR-0048 D3/D6).

use super::*;
use crate::compiler::control_block::BlockPosition;

impl Compiler {
    /// The placeholder rules, which are the one part of the lowering that is
    /// genuinely position-specific (ADR-0048 D3/D6). Returns true when a fatal
    /// die was emitted and the body must not be compiled at all.
    pub(super) fn emit_block_placeholder_gate(
        &mut self,
        stmts: &[Stmt],
        position: BlockPosition,
    ) -> bool {
        match position {
            BlockPosition::Statement => self.emit_statement_block_placeholder_gate(stmts),
            BlockPosition::Value { .. } => self.emit_value_block_placeholder_gate(stmts),
        }
    }

    fn emit_statement_block_placeholder_gate(&mut self, stmts: &[Stmt]) -> bool {
        // Check for placeholder conflicts in blocks. Use the *shallow*
        // collector: a placeholder belongs to its innermost enclosing block, so
        // placeholders nested inside an inner closure (`{ my $a; { $^a } }`)
        // must NOT be attributed to this block and falsely flagged as
        // redeclaring this block's `my $a`.
        let placeholders = crate::ast::collect_placeholders_shallow(stmts);
        if !placeholders.is_empty()
            && let Some(err_val) = self.check_placeholder_conflicts(&placeholders, stmts, None)
        {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die { user_throw: false });
            return true;
        }
        // ADR-0048 D3/D6: a bare `{ ... }` STATEMENT is a Block raku invokes
        // with ZERO arguments, so a placeholder it declares is that block's own
        // unsatisfied parameter -- `{ $^c }` dies with "Too few positionals
        // passed; expected 1 argument but got 0".
        //
        // Two shapes are NOT such a block. A SYNTHESIZED body -- an
        // `if`/`while`/`loop` branch the compile sites re-wrap in `Stmt::Block`
        // -- is not a block of its own; `synthetic_block_body` marks those, so
        // peek it here rather than consuming it (it is taken by
        // `compile_block_construct` as `is_bare`). And a statement MODIFIER's
        // modified statement (`{ $a = $^x } unless 0`) IS this construct's own
        // block, supplied the modifier's value -- see `is_construct_body_block`.
        !self.synthetic_block_body
            && !self.is_construct_body_block(stmts)
            && self.emit_inlined_body_placeholder_binds(stmts, ArgSupply::None)
    }

    fn emit_value_block_placeholder_gate(&mut self, body: &[Stmt]) -> bool {
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
        let Some(ph) = crate::ast::collect_unattached_placeholders(body)
            .into_iter()
            .find(|ph| {
                if self.lexically_in_method && ph == "%_" {
                    return false;
                }
                // A CARET placeholder (`$^p`) already bound as the enclosing
                // block's parameter is attached, not stray: the local exists in
                // `local_map` (same-compiler case), or the interpret-path caller
                // bound it in env and seeded `prebound_placeholder_params`
                // (re-entrant block eval — `call_sub_value` → `eval_block_value`
                // re-compiles the body alone). The `%_`/`@_` implicit slurpies
                // keep the strict rule (only a METHOD provides them) — pin:
                // t/placeholder-named-in-method-do.t.
                let bare = ph.trim_start_matches(['$', '@', '%', '&']);
                let attached_caret = bare.starts_with('^')
                    && (self.local_map.contains_key(ph.as_str())
                        || self.local_map.contains_key(bare)
                        || self.prebound_placeholder_params.contains(bare));
                !attached_caret
            })
        else {
            return false;
        };
        let err = crate::method_signature_shared::placeholder_scope_error("block", &ph);
        let idx = self.code.add_constant(err);
        self.code.emit(OpCode::LoadConst(idx));
        self.code.emit(OpCode::Die { user_throw: false });
        true
    }
}
