//! A `MAIN` a module exported into a block is lexical to that block.
//!
//! `MAIN` is the one routine an import never binds in a block's lexical
//! import set: the program reads `&MAIN` by name when it ends, not through a
//! call site (`bind_hook_routine_lexically`), so a module's exported `MAIN`
//! family is installed under `GLOBAL::MAIN`. When the importing block's scope
//! closes (`pop_import_scope`), the candidates and the `proto` the module
//! declared go again: only the program's own `MAIN` is dispatched at the end
//! (rakudo's `RUN-MAIN` takes the mainline's lexical `&MAIN`).

use super::*;

impl Interpreter {
    /// Remove the `GLOBAL::MAIN` candidates a module (not the program)
    /// declared: lexical to the import scope that loaded the module, even when
    /// they were registered before it opened (see `pop_import_scope`).
    // Cost: O(k + m), k = interned `GLOBAL::MAIN/…` names, m = symbols interned
    // since the family index last caught up.
    pub(super) fn drop_foreign_global_main_candidates(&mut self, program_path: Option<Symbol>) -> Vec<Symbol> {
        // The routine table this check last found clean is still the one
        // installed: versions are never reused, so nothing was added since.
        let version = self.routines().functions.version();
        if self.module.foreign_main_clean_version == Some(version) {
            return Vec::new();
        }
        let main = crate::symbol::wk::global_main();
        let mut keys = crate::qualified_tail_index::names_in_family("GLOBAL::MAIN");
        keys.push(main);
        let foreign: Vec<Symbol> = {
            let reg = self.routines();
            keys.into_iter()
                .filter(|key| {
                    reg.functions.get(key).is_some_and(|def| {
                        def.source_file
                            .as_deref()
                            .map(Symbol::intern)
                            .is_some_and(|f| Some(f) != program_path)
                    })
                })
                .collect()
        };
        if !foreign.is_empty() {
            let mut reg = self.routines_mut();
            let functions = reg.functions_mut();
            for key in &foreign {
                functions.remove(key);
            }
        }
        let version = self.routines().functions.version();
        self.module.foreign_main_clean_version = Some(version);
        foreign
    }

    /// Remove a `proto MAIN` a module (not the program) declared from the
    /// `GLOBAL::MAIN` slot, for the same reason as
    /// [`Self::drop_foreign_global_main_candidates`]: left behind, its body
    /// ran in front of the program's own `sub MAIN` when the program ended
    /// (rakudoc's `t/01-cmd.t`, whose `run-test` sub does `use Rakudoc::CMD`).
    /// Returns the removed key.
    // Cost: O(1).
    pub(super) fn drop_foreign_global_main_proto(
        &mut self,
        program_path: Option<Symbol>,
    ) -> Option<Symbol> {
        let main = crate::symbol::wk::global_main();
        let foreign = self.routines().proto_functions.get(&main).is_some_and(|def| {
            def.source_file
                .as_deref()
                .map(Symbol::intern)
                .is_some_and(|f| Some(f) != program_path)
        });
        if !foreign {
            return None;
        }
        let mut routines = self.routines_mut();
        routines.proto_functions_mut().remove(&main);
        let marker = main.resolve();
        routines.proto_subs_retain(|key| *key != marker);
        Some(main)
    }
}
