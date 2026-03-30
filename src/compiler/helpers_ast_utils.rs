use super::*;

impl Compiler {
    pub(super) fn is_dostmt_vardecl(expr: &Expr) -> bool {
        matches!(expr, Expr::DoStmt(s) if matches!(s.as_ref(), Stmt::VarDecl { .. }))
    }

    /// Check if a method call is a known mutating method on an indexed target
    /// (e.g., `%hash<key>.push(4)` or `@array[0].push(5)`).
    pub(super) fn is_mutating_method_on_index(
        target: &Expr,
        method_name: &crate::symbol::Symbol,
    ) -> bool {
        let method = method_name.resolve();
        let is_mutating = matches!(
            method.as_str(),
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        );
        if !is_mutating {
            return false;
        }
        if let Expr::Index {
            target: idx_target, ..
        } = target
        {
            Self::postfix_index_name(idx_target).is_some()
        } else {
            false
        }
    }

    /// Extract the variable name from a DoStmt(VarDecl { .. }) expression,
    /// used for `++state $` patterns.
    pub(super) fn extract_vardecl_name(expr: &Expr) -> Option<String> {
        if let Expr::DoStmt(stmt) = expr
            && let Stmt::VarDecl { name, .. } = stmt.as_ref()
        {
            Some(name.clone())
        } else {
            None
        }
    }

    pub(super) fn postfix_index_name(target: &Expr) -> Option<String> {
        match target {
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::Var(name) => Some(name.clone()),
            Expr::AssignExpr { name, .. } => Some(name.clone()),
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    pub(super) fn index_assign_target_name(target: &Expr) -> Option<String> {
        match target {
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::Var(name) => Some(name.clone()),
            // Sigilless variables appear as BareWord in the AST.
            // Treat them as named targets so IndexAssignExprNamed writes
            // through the sigilless alias back to the original container.
            Expr::BareWord(name) => Some(name.clone()),
            Expr::AssignExpr { name, .. } => Some(name.clone()),
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name.clone()),
                _ => None,
            },
            // (temp %hash){key} = value → treat as %hash{key} = value
            // TODO: implement proper temp save/restore semantics
            Expr::Call { name, args } if name == "temp" => {
                args.first().and_then(Self::index_assign_target_name)
            }
            _ => None,
        }
    }

    pub(super) fn index_assign_target_requires_eval(target: &Expr) -> bool {
        matches!(target, Expr::AssignExpr { .. } | Expr::DoStmt(_))
    }

    /// Extract the variable name from a method call target (e.g., `$foo.bar` → "foo").
    pub(super) fn method_call_target_var_name(target: &Expr) -> Option<String> {
        match target {
            Expr::Var(name) => Some(name.clone()),
            _ => None,
        }
    }

    pub(super) fn index_assign_nested_target(target: &Expr) -> Option<(String, &Expr)> {
        if let Expr::Index {
            target: inner_target,
            index: inner_index,
        } = target
            && let Some(name) = Self::index_assign_target_name(inner_target)
        {
            return Some((name, inner_index));
        }
        None
    }

    /// Hoist sub declarations: emit RegisterSub for all SubDecl statements
    /// before executing the rest of the block, so that `&name` references
    /// are available before the sub declaration appears in source order.
    /// Note: we only emit RegisterSub here, not compile_sub_body, because
    /// the normal SubDecl handling will compile the body. Compiling here
    /// would cause the compiled_functions map (which is flat) to be overwritten
    /// by later hoists from other scopes.
    pub(super) fn hoist_sub_decls(&mut self, stmts: &[Stmt], lexical_hoist: bool) {
        for stmt in stmts {
            if let Stmt::SubDecl { .. } = stmt {
                let mut hoisted = stmt.clone();
                if lexical_hoist
                    && let Stmt::SubDecl { custom_traits, .. } = &mut hoisted
                    && !custom_traits
                        .iter()
                        .any(|trait_name| trait_name == "__lexical_hoist")
                {
                    custom_traits.push("__lexical_hoist".to_string());
                }
                let idx = self.code.add_stmt(hoisted);
                self.code.emit(OpCode::RegisterSub(idx));
            }
        }
    }

    /// Check if a class body is a stub (contains only `...`, `!!!`, or `???`).
    pub(super) fn is_stub_class_body(body: &[Stmt]) -> bool {
        let filtered: Vec<_> = body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        filtered.len() == 1
            && matches!(filtered[0], Stmt::Expr(Expr::Call { name, .. })
                if name.resolve() == "__mutsu_stub_die" || name.resolve() == "__mutsu_stub_warn")
    }

    /// Reorder statements so that when a stub class is followed later by its
    /// real definition, the real definition is moved right after the stub.
    /// This emulates Raku's compile-time class registration: by the time
    /// runtime code executes, stub classes have been filled in.
    ///
    /// Only applies to non-lexical ClassDecl statements, and only when there
    /// are no intervening class/role declarations between the stub and the
    /// real definition (to preserve inter-class dependency ordering).
    pub(super) fn reorder_stub_class_decls(stmts: &[Stmt]) -> Option<Vec<Stmt>> {
        // Collect names of stubbed classes and the index of the real definition.
        let mut stub_names = std::collections::HashMap::<String, usize>::new();
        let mut real_defs = std::collections::HashMap::<String, usize>::new();
        for (i, stmt) in stmts.iter().enumerate() {
            if let Stmt::ClassDecl {
                name,
                is_lexical,
                body,
                ..
            } = stmt
            {
                if *is_lexical {
                    continue;
                }
                let n = name.resolve();
                if Self::is_stub_class_body(body) {
                    stub_names.entry(n).or_insert(i);
                } else {
                    real_defs.insert(n, i);
                }
            }
        }
        // Find names that have both a stub and a later real definition,
        // with no intervening class/role declarations between them.
        let mut needs_move: Vec<(String, usize, usize)> = Vec::new();
        for (name, stub_idx) in &stub_names {
            if let Some(&real_idx) = real_defs.get(name) {
                if real_idx <= *stub_idx + 1 {
                    continue; // Already adjacent
                }
                // Check for intervening class/role declarations that could
                // have inter-dependencies with the stub class.
                let has_intervening_decls = stmts[stub_idx + 1..real_idx]
                    .iter()
                    .any(|s| matches!(s, Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. }));
                if !has_intervening_decls {
                    needs_move.push((name.clone(), *stub_idx, real_idx));
                }
            }
        }
        if needs_move.is_empty() {
            return None;
        }
        // Build reordered statement list: for each stub, insert the real
        // definition immediately after it, and skip the real def at its
        // original position.
        let moved_indices: std::collections::HashSet<usize> = needs_move
            .iter()
            .map(|(_, _, real_idx)| *real_idx)
            .collect();
        let stub_to_real: std::collections::HashMap<usize, usize> = needs_move
            .iter()
            .map(|(_, stub_idx, real_idx)| (*stub_idx, *real_idx))
            .collect();
        let mut result = Vec::with_capacity(stmts.len());
        for (i, stmt) in stmts.iter().enumerate() {
            if moved_indices.contains(&i) {
                // Skip — this real definition was moved after its stub.
                continue;
            }
            result.push(stmt.clone());
            if let Some(&real_idx) = stub_to_real.get(&i) {
                // Insert the real definition right after the stub.
                result.push(stmts[real_idx].clone());
            }
        }
        Some(result)
    }
}
