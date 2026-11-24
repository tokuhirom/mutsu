//! A scope's `my $*x` is bound from the scope's entry (#19174).
//!
//! Rakudo keeps a block's lexicals in its pad, which exists from the block's
//! entry: a dynamic the block declares further down already shadows the
//! callers' binding before its declaration runs, and reads as the
//! declaration's unset value (`Any` for an untyped scalar) until then. So a
//! routine called before `my $*x = ...` sees the pending one, not the
//! caller's, through `$*x` and `DYNAMIC::<$*x>` alike; and a dynamic declared
//! in a nested block that has not been entered shadows nothing.
//!
//! mutsu binds a dynamic in the env tier of the scope that runs its
//! declaration, and both reads resolve through that env chain. So the scope
//! entry declares each dynamic of its own statement list, without its
//! initializer, before anything else runs; the declaration in source order
//! then binds it again with its initializer. A declaration that no code can
//! precede (the scope's inert prefix) needs no early copy.

use super::*;

impl Compiler {
    /// Declare, at a scope's entry, every `my` dynamic its statement list
    /// declares after code that could observe it.
    // Cost: O(s), s = statements in `stmts`.
    pub(super) fn hoist_pending_dynamic_decls(&mut self, stmts: &[Stmt]) {
        // A phaser (`ENTER`, `FIRST`, `PRE`) can run code at the scope's entry.
        let mut code_ran = stmts.iter().any(|s| matches!(s, Stmt::Phaser { .. }));
        let mut hoisted: Vec<&str> = Vec::new();
        for stmt in stmts {
            if stmt.is_marker() {
                continue;
            }
            if code_ran
                && let Stmt::VarDecl {
                    name,
                    is_state: false,
                    is_our: false,
                    constant: None,
                    ..
                } = stmt
                && name.trim_start_matches(['@', '%', '&']).starts_with('*')
                && !hoisted.contains(&name.as_str())
            {
                hoisted.push(name);
                self.compile_stmt(&Self::pending_dynamic_decl(stmt));
                // Reads before the declaration still count for its
                // X::Dynamic::Postdeclaration check.
                self.hoisted_dynamic_decls.insert(name.clone());
            }
            code_ran = code_ran || !Self::stmt_is_inert_prefix(stmt);
        }
    }

    /// The declaration `decl` makes before its initializer runs: the same
    /// variable, type and traits, holding its unset value.
    // Cost: O(|decl|), one clone.
    fn pending_dynamic_decl(decl: &Stmt) -> Stmt {
        let mut pending = decl.clone();
        if let Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            has_initializer,
            bind,
            ..
        } = &mut pending
        {
            *expr = crate::parser::default_decl_expr(
                name.starts_with('@'),
                name.starts_with('%'),
                None,
                type_constraint.as_deref(),
            );
            *has_initializer = false;
            *bind = None;
        }
        pending
    }
}
