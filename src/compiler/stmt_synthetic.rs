//! Compile scopeless parser expansions without leaking intermediate values.

use super::*;

impl Compiler {
    pub(super) fn compile_synthetic_stmt(&mut self, stmts: &[Stmt]) {
        // Detect `:=` bind context via MarkBind: the parser wraps `my @a :=
        // expr` (and the `%`/`$` forms) in a SyntheticBlock that starts with
        // it. Set bind_vardecl so the VarDecl compilation emits
        // MarkBindContext before SetLocal, making the VM preserve the
        // container type (e.g. List stays List).
        let has_mark_bind = stmts.iter().any(|s| matches!(s, Stmt::MarkBind));
        let restore_pointy_given_topic = stmts
            .iter()
            .any(|s| matches!(s, Stmt::VarDecl { marks, .. } if marks.restore_pointy_given_topic));
        // A trailing `MarkSigilless` marks the bind target as a
        // sigilless term, which changes how an immutable `List`
        // element's mutability is settled (see
        // `OpCode::IndexAutovivifyLazyTerminal`). It compiles AFTER the
        // declaration, so the block has to tell the VarDecl itself.
        let has_mark_sigilless = stmts.iter().any(|s| matches!(s, Stmt::MarkSigilless(_)));
        // The names this expansion declares as sigilless TERMS: a `VarDecl`
        // its `MarkSigilless`/`MarkSigillessReadonly` names is stored under
        // the term key (ADR-12441).
        let term_names = Self::sigilless_mark_names(stmts);
        // Collect sigilless readonly names so we can clear the flag
        // before the VarDecl assignment (allows re-declaration in loops).
        let sigilless_readonly_names: Vec<String> = stmts
            .iter()
            .filter_map(|s| {
                if let Stmt::MarkSigillessReadonly(name) = s {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();
        // Collect the bound array variable name for skipping the
        // trailing Var statement that would force the LazyList via SinkPop.
        let mut bound_array_var: Option<String> = None;
        for s in stmts {
            if has_mark_bind
                && let Stmt::VarDecl { name, .. } = s
                && name.starts_with('@')
            {
                bound_array_var = Some(name.clone());
            }
            if has_mark_bind && matches!(s, Stmt::VarDecl { .. }) {
                self.bind_vardecl = true;
                self.sigilless_bind_vardecl = has_mark_sigilless;
            }
            // Before compiling a VarDecl that will be followed by
            // MarkSigillessReadonly, clear the old readonly flag so
            // that re-declaration in a loop iteration succeeds.
            if let Stmt::VarDecl { name, .. } = s
                && sigilless_readonly_names.contains(name)
            {
                // Register the sigilless name BEFORE compiling its
                // VarDecl so the decl emits MarkConstantContext (a raw
                // `\x = ...` binds the value itself — no Scalar
                // container, so SetLocal must not itemize it).
                self.sigilless_locals.insert(name.clone());
                let key = crate::runtime::sigilless_readonly_key(
                    &crate::runtime::term_names::term_key(name),
                );
                let key_idx = self.code.add_constant(Value::str(key.as_str().to_string()));
                let false_idx = self.code.add_constant(Value::FALSE);
                self.code.emit(OpCode::LoadConst(false_idx));
                self.code.emit(OpCode::SetGlobal(key_idx));
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
            self.decl_is_term =
                matches!(s, Stmt::VarDecl { name, .. } if term_names.contains(&name.as_str()));
            self.compile_stmt_discarding_value(s);
            self.decl_is_term = false;
        }
        if restore_pointy_given_topic {
            self.code.emit(OpCode::ExitPointyTopic);
        }
    }

    /// The names a parser expansion's `MarkSigilless` /
    /// `MarkSigillessReadonly` statements mark as sigilless terms.
    // Cost: O(n), n = statements.
    pub(super) fn sigilless_mark_names(stmts: &[Stmt]) -> Vec<&str> {
        stmts
            .iter()
            .filter_map(|s| match s {
                Stmt::MarkSigilless(n) | Stmt::MarkSigillessReadonly(n) => Some(n.as_str()),
                _ => None,
            })
            .collect()
    }
}
