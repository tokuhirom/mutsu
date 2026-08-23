//! ADR-0048 D3: the single shared emitter for binding an *inlined* construct
//! body's placeholder parameters.
//!
//! Every `{ ... }` body is a Block in raku, and a construct invokes it with
//! some number of arguments (ADR-0048's block-invocation contract). mutsu
//! compiles `if`/`given`/`when`/bare-`{}` bodies inline into the enclosing
//! frame (D1), so the bind has to be reconstructed here from the AST oracle's
//! [`ArgSupply`] classification instead of falling out of a real call.
//!
//! Before this module the bind was copy-pasted at five sites, each of which
//! found only the *first* placeholder
//! (`collect_placeholders_shallow(..).find(|n| n.starts_with('^'))`) and
//! silently let any further placeholder fall through to the *enclosing*
//! block's signature — so `if 42 { "$^a $^b" }` printed `42 True` where raku
//! raises `Too few positionals passed; expected 2 arguments but got 1`.

use super::*;

impl Compiler {
    /// The caret (positional) placeholders `body` declares, in
    /// `collect_placeholders_shallow` order.
    ///
    /// Names come back sigil-prefixed for the non-scalar forms (`^a`, `@^b`,
    /// `%^c`, `&^d`), and *all* of them are positional parameters of the body:
    /// raku reports `if 42 { $^a; @^b }` as `expected 2 arguments but got 1`.
    /// Named placeholders (`$:name`) are deliberately excluded — they are
    /// named parameters, and under-supplying one is raku's separate
    /// "Required named parameter 'a' not passed", not an arity failure.
    pub(super) fn inlined_body_caret_placeholders(body: &[Stmt]) -> Vec<String> {
        crate::ast::collect_placeholders_shallow(body)
            .into_iter()
            .filter(|n| n.trim_start_matches(['@', '%', '&']).starts_with('^'))
            .collect()
    }

    /// Does this inlined body need the construct's supplied value kept on the
    /// stack so [`Compiler::emit_inlined_body_placeholder_binds`] can bind it?
    ///
    /// Only a *scalar* `$^name` can receive a single supplied value, so a body
    /// whose first positional placeholder is `@^a`/`%^a` consumes nothing
    /// (raku type-checks that bind and fails; mutsu leaves the placeholder
    /// unbound rather than assigning a scalar into an array parameter). The
    /// stack contract of the emitter is exactly this predicate: it pops one
    /// value iff this returns true and the construct supplies at least one.
    pub(super) fn inlined_body_binds_supplied_value(body: &[Stmt]) -> bool {
        Self::inlined_body_caret_placeholders(body)
            .first()
            .is_some_and(|n| n.starts_with('^'))
    }

    /// ADR-0048 D3/D6 — remember that this construct's body IS a source
    /// `{ ... }` block, so that block is not *additionally* a zero-argument
    /// nested one.
    ///
    /// A statement MODIFIER modifies a statement, and that statement can be a
    /// bare block: `{ $a = $^x } unless 0` and `{ $a = $^x } given 69` both
    /// parse to a construct whose body is exactly `[Stmt::Block(inner)]`, and
    /// raku supplies the modifier's value to that block (it prints `0` and
    /// `69` respectively, not an arity failure — the parser already lowers
    /// those two into a `VarDecl` of `^x` at the head of the block). The same
    /// holds for `{ @a.push($^x) } for 1, 2` and for a `while` modifier. Only
    /// a block that is *genuinely nested* inside a construct's braces
    /// (`if 1 { { $^a } }`) is a second, separately-invoked Block.
    ///
    /// The two shapes are indistinguishable from inside the `Stmt::Block` arm,
    /// so record the body block's address here (the AST outlives the whole
    /// compile, so the address is a stable identity) and let that arm skip its
    /// zero-supply check for exactly that node. `Stmt::While` has no
    /// `is_statement_modifier` flag, so a sole-block `while` body is treated
    /// as the modifier form either way — a deliberate false negative for the
    /// rare `while C { { $^a } }`, whose real fix is D4/Phase 4 anyway.
    pub(super) fn note_construct_body_block(&mut self, stmt: &Stmt) {
        let body = match stmt {
            Stmt::If {
                then_branch,
                is_statement_modifier: true,
                ..
            } => then_branch,
            Stmt::Given {
                body,
                is_statement_modifier: true,
                ..
            }
            | Stmt::For {
                body,
                is_statement_modifier: true,
                ..
            }
            | Stmt::While { body, .. } => body,
            _ => return,
        };
        self.note_construct_body_block_stmts(body);
    }

    /// Re-note a construct's body block after the body list was REBUILT.
    /// `expand_loop_phasers` returns a fresh `Vec<Stmt>`, so the address
    /// [`Compiler::note_construct_body_block`] recorded for a `for`/`while`
    /// modifier's body no longer identifies the block that actually gets
    /// compiled; the loop arms call this with the expanded list.
    pub(super) fn note_construct_body_block_stmts(&mut self, body: &[Stmt]) {
        if let [Stmt::Block(inner)] = body {
            self.construct_body_block = Some(inner.as_ptr() as usize);
        }
    }

    /// Is `stmts` the body block a statement modifier (or a `while`) supplies
    /// its own value to, rather than a separately-invoked nested block?
    /// See [`Compiler::note_construct_body_block`].
    pub(super) fn is_construct_body_block(&self, stmts: &[Stmt]) -> bool {
        !stmts.is_empty() && self.construct_body_block == Some(stmts.as_ptr() as usize)
    }

    /// ADR-0048 D3 — bind an inlined body's placeholder parameters from what
    /// its construct supplies, and raise raku's arity error when the body
    /// declares more positionals than the construct provides.
    ///
    /// `supplied` comes from the AST oracle (`placeholder_body_kind`): an
    /// `if`/`elsif`/`unless`/`with`/`without` branch is handed the raw
    /// condition ([`ArgSupply::Condition`]), a `given`/`with` body the topic
    /// ([`ArgSupply::Topic`]), and a `when` body or a bare `{ ... }` statement
    /// nothing at all ([`ArgSupply::None`]) — which is why
    /// `given 5 { when 5 { $^c } }` and `{ $^c }` both die with
    /// `Too few positionals passed; expected 1 argument but got 0` in raku.
    ///
    /// Stack contract: when the construct supplies a value it must be on top
    /// of the stack, and is consumed exactly when
    /// [`Compiler::inlined_body_binds_supplied_value`] is true. Emit this
    /// *inside* the body's own control-flow region — the arity failure is a
    /// runtime error raised when the block is invoked, so a never-taken
    /// branch (`if 0 { "$^a $^b" }`) and a non-matching `when` must not raise
    /// it at all.
    ///
    /// Returns true when a fatal arity die was emitted, so callers that can
    /// bail out of compiling the body (the bare-`{}` statement) may do so.
    pub(super) fn emit_inlined_body_placeholder_binds(
        &mut self,
        body: &[Stmt],
        supplied: ArgSupply,
    ) -> bool {
        let phs = Self::inlined_body_caret_placeholders(body);
        if phs.is_empty() {
            return false;
        }
        let supplied_n = match supplied {
            ArgSupply::None => 0,
            // Every inlined construct that supplies anything supplies exactly
            // one value: the raw condition, the topic, or (`repeat`, D4/Phase
            // 4) `Mu` on the first pass and the condition afterwards.
            ArgSupply::Condition | ArgSupply::ConditionAfterFirstPass | ArgSupply::Topic => 1,
            // A real invocation binds the body's own declared arity, so it can
            // never under-supply here; these never reach an inlined body.
            ArgSupply::CallerArgs | ArgSupply::Elements => phs.len(),
        };
        if supplied_n >= 1 && phs[0].starts_with('^') {
            self.emit_set_named_var(&phs[0]);
        }
        if phs.len() > supplied_n {
            self.emit_too_few_positionals_die(phs.len(), supplied_n);
            return true;
        }
        false
    }

    /// Emit raku's runtime arity failure for an under-supplied inlined body.
    ///
    /// The wording is raku's verbatim (`Too few positionals passed; expected 2
    /// arguments but got 1`), including the singular "argument" at
    /// `expected == 1`, so `{ $^c }` reports exactly what rakudo reports.
    fn emit_too_few_positionals_die(&mut self, expected: usize, got: usize) {
        let msg = format!(
            "Too few positionals passed; expected {} argument{} but got {}",
            expected,
            if expected == 1 { "" } else { "s" },
            got
        );
        let idx = self.code.add_constant(Value::str(msg));
        self.code.emit(OpCode::LoadConst(idx));
        self.code.emit(OpCode::Die);
    }
}
