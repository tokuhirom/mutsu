//! The restart of a module whose CHECK phase rewrote `$*CU` (#17886).
//!
//! Rakudo compiles a unit from its `$*CU` after the CHECK phase, so a CHECK
//! phaser or `add-check-code` closure that rewrites the tree changes what the
//! unit runs. The main unit and an EVAL restart in place, from `RunCheckCode`
//! (`vm_exec_dispatch`). A module file cannot: what it declares, binds and
//! exports has to land in the loader's env and tables exactly as its first
//! run's would, which an EVAL inside the module (with its own frame and
//! isolation) does not do. So `RunCheckCode` only ends the module's first
//! mainline and leaves the tree on the unit
//! (`unit_source_text::request_module_restart`), and the loader runs the
//! module's body again from the lowered tree, here, in the same load.

use super::*;

impl Interpreter {
    /// After a module's mainline: when its CHECK phase rewrote `$*CU`, run the
    /// module again from the rewritten tree. Its BEGIN prologue and CHECK
    /// phasers ran in the first run and are skipped
    /// (`active_unit_is_rewritten`); its declarations replace the first run's.
    // Cost: O(1) when nothing was rewritten; otherwise lowering the tree,
    // O(n), n = its nodes, plus running the module body again.
    pub(crate) fn restart_module_from_rewritten_cu(&mut self) -> Result<(), RuntimeError> {
        let Some(tree) = crate::unit_source_text::take_module_restart() else {
            return Ok(());
        };
        let ValueView::RakuAst(node) = tree.view() else {
            return Ok(());
        };
        let operators = self.collect_operator_sub_names();
        let names = crate::rakuast::frontend::lower_scope(&operators);
        let caller_version = crate::parser::current_language_version();
        let lowered = crate::rakuast::lower(&names, node);
        crate::parser::set_current_language_version(&caller_version);
        let mut stmts = lowered?;
        // A top-level `my` without an initializer keeps what the first run's
        // BEGIN/CHECK phasers wrote into it, as the main unit's restart does.
        Self::adopt_rewritten_restart_toplevel_decls(&mut stmts);
        let prologue_len = crate::runtime::begin_prologue::order_unit(&mut stmts);
        crate::runtime::begin_prologue::split_package_bodies(&mut stmts);
        crate::runtime::begin_prologue::insert_module_run_check_code(&mut stmts, prologue_len);
        // The module's body compiles from GLOBAL, as its first run did: its own
        // `unit module X` names `X`, not one nested in where the first run
        // stopped.
        let saved_package = self.current_package();
        self.set_current_package("GLOBAL".to_string());
        // The tree declares the module's routines and types again, as the
        // restart of the main unit does (`RunCheckCode`): each replaces the
        // first run's instead of redeclaring it.
        let outer_suppress = std::mem::replace(
            &mut self.module.suppress_cross_eval_class_redeclaration_check,
            true,
        );
        let outer_candidates = self
            .dispatch
            .rewritten_unit_candidates
            .replace(Default::default());
        let result = self.run_block_with(&stmts, None, None, true);
        self.dispatch.rewritten_unit_candidates = outer_candidates;
        self.module.suppress_cross_eval_class_redeclaration_check = outer_suppress;
        self.set_current_package(saved_package);
        result
    }
}
