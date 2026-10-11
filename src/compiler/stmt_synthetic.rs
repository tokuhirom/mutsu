//! Compile scopeless parser expansions without leaking intermediate values.

use super::*;

impl Compiler {
    pub(super) fn compile_synthetic_stmt(&mut self, stmts: &[Stmt]) {
        // Set by a caller that binds parameters (the `for` bind stmts).
        let params_bind = std::mem::take(&mut self.decl_is_sigilless);
        // Detect `:=` bind context for `@` variables: the parser wraps
        // `my @a := expr` in a SyntheticBlock containing VarDecl followed
        // by `__mutsu_record_bound_array_len`.  Set bind_vardecl so the
        // VarDecl compilation emits MarkBindContext before SetLocal,
        // making the VM preserve the container type (e.g. List stays List).
        let has_bound_array_len = stmts.iter().any(|s| {
            matches!(s,
                Stmt::Expr(Expr::Call { name, .. })
                if name.resolve() == "__mutsu_record_bound_array_len"
            )
        });
        // Detect `:=` bind context for scalar variables via MarkBind.
        let has_mark_bind = stmts.iter().any(|s| matches!(s, Stmt::MarkBind));
        let restore_pointy_given_topic = stmts.iter().any(|s| {
            matches!(s, Stmt::VarDecl { custom_traits, .. }
            if custom_traits.iter().any(|(name, _)| name == "__restore_pointy_given_topic"))
        });
        // A trailing `MarkSigilless` marks the bind target as a
        // sigilless term, which changes how an immutable `List`
        // element's mutability is settled (see
        // `OpCode::IndexAutovivifyLazyTerminal`). It compiles AFTER the
        // declaration, so the block has to tell the VarDecl itself.
        let has_mark_sigilless = stmts.iter().any(|s| matches!(s, Stmt::MarkSigilless(_)));
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
            if has_bound_array_len
                && let Stmt::VarDecl { name, .. } = s
                && name.starts_with('@')
            {
                self.bind_vardecl = true;
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
                self.sigilless_declared.insert(name.clone());
                let key = crate::runtime::sigilless_readonly_key(name);
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
            self.decl_is_sigilless = params_bind
                || matches!(s, Stmt::VarDecl { name, .. }
                if has_mark_sigilless || sigilless_readonly_names.contains(name));
            self.compile_stmt_discarding_value(s);
            self.decl_is_sigilless = false;
        }
        if restore_pointy_given_topic {
            self.code.emit(OpCode::ExitPointyTopic);
        }
    }
}
