//! Compile scopeless parser expansions without leaking intermediate values.

use super::*;
use crate::ast::var_marks::TermMark;

impl Compiler {
    pub(super) fn compile_synthetic_stmt(&mut self, stmts: &[Stmt]) {
        // Detect `:=` bind context via a bound declaration: the parser wraps `my @a :=
        // expr` (and the `%`/`$` forms) in a SyntheticBlock that starts with
        // it. The VarDecl compilation reads the mark and emits
        // MarkBindContext before SetLocal, making the VM preserve the
        // container type (e.g. List stays List).
        let restore_pointy_given_topic = stmts
            .iter()
            .any(|s| matches!(s, Stmt::VarDecl { marks, .. } if marks.restore_pointy_given_topic));
        // Collect the bound array variable name for skipping the
        // trailing Var statement that would force the LazyList via SinkPop.
        let mut bound_array_var: Option<String> = None;
        for s in stmts {
            if let Stmt::VarDecl { name, marks, .. } = s
                && marks.bound
                && name.starts_with('@')
            {
                bound_array_var = Some(name.clone());
            }
            // Skip the trailing Var(@name) expression for bound array
            // declarations. Without this, SinkPop would eagerly force a
            // lazy gather/take list bound via `:=`.
            if let Some(ref bav) = bound_array_var
                && let Stmt::Expr(Expr::Var(vname)) = s
                && vname == bav
            {
                // Emit a Nil instead of the Var to avoid forcing.
                self.code.emit(OpCode::LoadNil);
                self.code.emit(OpCode::SinkPop(false, true));
                continue;
            }
            self.compile_stmt_discarding_value(s);
        }
        if restore_pointy_given_topic {
            self.code.emit(OpCode::ExitPointyTopic);
        }
    }

    /// [`Compiler::begin_term_decl`] for `stmt` when it declares a sigilless
    /// term; the name and mark to hand to [`Compiler::end_term_decl`] after it.
    pub(super) fn begin_term_decl_of<'a>(
        &mut self,
        stmt: &'a Stmt,
    ) -> Option<(&'a str, TermMark, bool)> {
        if let Stmt::VarDecl {
            name, marks, expr, ..
        } = stmt
            && marks.term != TermMark::No
        {
            self.begin_term_decl(name, marks.term, Self::term_rhs_is_value(expr));
            return Some((name, marks.term, Self::term_rhs_is_expr_result(expr)));
        }
        None
    }

    /// Whether a bound sigilless term's initializer is a subscript or a call:
    /// it binds whatever container or value that expression hands back, never
    /// a variable by name (ADR-17376, "bound terms").
    // Cost: O(1).
    fn term_rhs_is_expr_result(expr: &crate::ast::Expr) -> bool {
        use crate::ast::Expr;
        matches!(
            expr,
            Expr::Index { .. }
                | Expr::MethodCall { .. }
                | Expr::Call { .. }
                | Expr::UserRoutineCall { .. }
        )
    }

    /// What a sigilless term declaration needs before its store: the name
    /// reads from its local slot, and (for a readonly term) a previous
    /// iteration's readonly mark is cleared so a re-declaration in a loop
    /// succeeds. A raw `\x = ...` binds the value itself, with no Scalar
    /// container, so the name is registered BEFORE the declaration compiles.
    /// Whether a sigilless term's initializer certainly produces a fresh
    /// value rather than handing back a container: a literal, an
    /// interpolated string, or a string/arithmetic infix. A call or a method
    /// may return a container (`{$_}` returns its argument's), which the term
    /// then binds, so only these make its slot a readonly term (ADR-17376).
    // Cost: O(1).
    fn term_rhs_is_value(expr: &crate::ast::Expr) -> bool {
        use crate::ast::Expr;
        use crate::token_kind::TokenKind;
        match expr {
            Expr::Literal(_) | Expr::StringInterpolation(..) => true,
            Expr::Binary { op, .. } => matches!(
                op,
                TokenKind::Tilde
                    | TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Star
                    | TokenKind::Slash
                    | TokenKind::Percent
            ),
            _ => false,
        }
    }

    pub(super) fn begin_term_decl(&mut self, name: &str, kind: TermMark, value_rhs: bool) {
        self.sigilless_locals.insert(name.to_string());
        // A term declared after a `use` of the same name is the nearer binding.
        self.use_declared_names.remove(name);
        if kind == TermMark::Readonly {
            let key =
                crate::runtime::sigilless_readonly_key(&crate::runtime::term_names::term_key(name));
            let key_idx = self.code.add_constant(Value::str(key.as_str().to_string()));
            // The slot is allocated by the declaration itself; `end_term_decl`
            // fills it in once it exists.
            let ip = self.code.emit(OpCode::SetTermMark {
                key_idx,
                on: false,
                slot: u32::MAX,
            });
            self.pending_term_mark_clears.push((ip, value_rhs));
        }
    }

    /// What a sigilless term declaration needs after its store: a readonly
    /// term is marked so, and a bound one has its writability settled from what
    /// it was bound to, which only the run time knows. The verdict is taken with
    /// the bind source still on the stack, by the `MarkSigillessBindSource` the
    /// declaration itself emits; the op emitted here writes the readonly
    /// marker, and has to run AFTER the store because a declaration clears
    /// that marker.
    pub(super) fn end_term_decl(&mut self, name: &str, kind: TermMark, bound_expr: bool) {
        let slot = self
            .local_map
            .get(&crate::runtime::term_names::term_key(name))
            .copied();
        match kind {
            TermMark::No => {}
            TermMark::Slot => self.note_other_term_binding(slot),
            TermMark::Readonly => {
                // ADR-17376: a readonly term is a fact of its slot; the
                // markers stay for a slot something outside this chunk's ops
                // can name, and the run time skips them otherwise.
                let pending = self.pending_term_mark_clears.pop();
                let value_rhs = pending.is_some_and(|(_, value_rhs)| value_rhs);
                if let Some(slot) = slot
                    && let Some(desc) = self.code.binding_descs.get_mut(slot as usize)
                {
                    if value_rhs {
                        desc.flags.note_readonly_term_decl();
                    } else {
                        desc.flags.note_other_term_decl();
                    }
                }
                if let Some((ip, _)) = pending
                    && let OpCode::SetTermMark { slot: s, .. } = &mut self.code.ops[ip]
                {
                    *s = slot.unwrap_or(u32::MAX);
                }
                self.emit_term_readonly_mark(name, slot);
            }
            TermMark::Bound => {
                if bound_expr
                    && let Some(slot) = slot
                    && let Some(desc) = self.code.binding_descs.get_mut(slot as usize)
                {
                    desc.flags.note_bound_expr_term_decl();
                } else {
                    self.note_other_term_binding(slot);
                }
                let name_idx = self
                    .code
                    .add_constant(Value::str(crate::runtime::term_names::term_key(name)));
                self.code.emit(OpCode::MarkSigillessBind(name_idx));
            }
        }
    }

    /// Mark the sigilless term `name` readonly: a bare-word read of it
    /// resolves its local slot, and `__mutsu_sigilless_readonly::\NAME` is set
    /// in `env`.
    pub(super) fn emit_sigilless_readonly(&mut self, name: &str) {
        // A parameter (or any binding other than a readonly declaration)
        // sharing the slot keeps the env marker (ADR-17376).
        let slot = self
            .local_map
            .get(&crate::runtime::term_names::term_key(name))
            .copied();
        self.note_other_term_binding(slot);
        self.emit_term_readonly_mark(name, slot);
    }

    /// The `SetTermMark { on: true }` of the sigilless term `name`.
    fn emit_term_readonly_mark(&mut self, name: &str, slot: Option<u32>) {
        self.sigilless_locals.insert(name.to_string());
        let key =
            crate::runtime::sigilless_readonly_key(&crate::runtime::term_names::term_key(name));
        let key_idx = self.code.add_constant(Value::str(key.as_str().to_string()));
        self.code.emit(OpCode::SetTermMark {
            key_idx,
            on: true,
            slot: slot.unwrap_or(u32::MAX),
        });
    }

    /// Record that `slot` is bound other than by a readonly term
    /// declaration, so it keeps the env marker (ADR-17376).
    pub(super) fn note_other_term_binding(&mut self, slot: Option<u32>) {
        if let Some(slot) = slot
            && let Some(desc) = self.code.binding_descs.get_mut(slot as usize)
        {
            desc.flags.note_other_term_decl();
        }
    }
}
