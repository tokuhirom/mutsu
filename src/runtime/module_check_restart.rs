//! The restart of a module whose CHECK phase rewrote its `$*CU` (Red's
//! `CHECK red-compile` in a module that builds queries, #14094).
//!
//! In rakudo the rewritten tree is what gets compiled: the module's routines,
//! its exports and its mainline are those of the tree. The main program and an
//! `EVAL` unit run the tree as an EVAL'd unit in place of the rest of their
//! mainline (`OpCode::RunCheckCode`); a module runs it as the rest of its own
//! mainline instead, in the module's scope, so what the tree declares is the
//! module's: its routines, the export aliases they install, and the file-scope
//! lexicals those routines read once the load is over.

use crate::ast::Stmt;
use crate::runtime::{Interpreter, RuntimeError};
use crate::value::{Value, ValueView};

impl Interpreter {
    /// Run the CHECK-rewritten tree `cu` of the module being loaded as the
    /// rest of its mainline, whose code is `outer`.
    // Cost: O(n) to lower and compile the tree, n = its nodes, plus its run;
    // O(l) to refresh `outer`'s local slots, l = its locals.
    pub(crate) fn run_module_check_restart(
        &mut self,
        cu: &Value,
        outer: &crate::opcode::CompiledCode,
    ) -> Result<(), RuntimeError> {
        let ValueView::RakuAst(node) = cu.view() else {
            return Ok(());
        };
        let operators = self.collect_operator_sub_names();
        let names = crate::rakuast::frontend::lower_scope(&operators);
        let caller_version = crate::parser::current_language_version();
        let lowered = crate::rakuast::lower(&names, node);
        crate::parser::set_current_language_version(&caller_version);
        let mut stmts = lowered?;
        // The unit is the restart: its BEGIN and CHECK phasers ran in the
        // first compile and are skipped, and its initializer-less top-level
        // `my` declarations adopt the first compile's bindings.
        crate::unit_source_text::mark_next_tree_rewritten();
        let _unit = crate::unit_source_text::UnitSourceTextGuard::enter_tree(cu.clone());
        Self::adopt_rewritten_restart_toplevel_decls(&mut stmts);
        // The mainline already entered the package of its `unit module` /
        // `unit package`; compiled again, the declarator would nest the
        // package in itself (`M::M`).
        let current = self.current_package_sym();
        stmts.retain(|stmt| {
            !matches!(stmt, Stmt::Package { name, is_unit: true, .. } if *name == current)
        });
        crate::runtime::begin_prologue::order_unit(&mut stmts);
        crate::runtime::begin_prologue::split_package_bodies(&mut stmts);
        let (code, fns) = self.compile_module_mainline_uncached(&stmts);
        self.run_compiled_block_raw(&code, &fns)?;
        // The first compile's mainline ends here, and its frame publishes its
        // local slots on the way out: they hold the bindings of a mainline
        // that never ran, so take the tree's from `env` first.
        for (slot, name) in outer.locals.iter().enumerate() {
            if let Some(value) = self.env.get(name).cloned()
                && let Some(local) = self.locals.get_mut(slot)
            {
                *local = value;
            }
        }
        Ok(())
    }
}
