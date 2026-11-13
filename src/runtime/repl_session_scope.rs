//! What a REPL session unit does not see of the program running the session
//! (#17642).
//!
//! A `Perl6::Compiler.eval` session started without an outer context gets a
//! fresh `GLOBAL` in rakudo: its first unit's outer scope is the setting, and
//! the types the running program declared are neither in its lexical chain
//! nor in its `GLOBAL`. A later unit sees what the earlier units of the
//! session declared, merged or imported, and nothing else.
//!
//! mutsu's type registry is process-global, so the session's view is rebuilt
//! on top of it:
//!
//! - the types a module declared are gated by ADR-11136
//!   (`module_name_hidden_here`): a session unit merges no module unless it
//!   `use`s it, and since #17087 the unit is compiled inside its session only;
//! - the types the main program (and the `EVAL`s it ran) declared are recorded
//!   as they are registered ([`Interpreter::note_main_unit_type`]), and a unit
//!   of a session does not see them by their bare name
//!   ([`Interpreter::main_type_hidden_in_unit`]): neither its code at run time
//!   (bareword resolution), nor its parse (the EVAL type-name preseed), nor
//!   EVAL's undeclared-name check.
//!
//! A session's units are the ones `Perl6::Compiler.eval` ran with no outer
//! context, the units later run in their contexts, and every `EVAL` nested in
//! one of those ([`Interpreter::unit_in_repl_session`]).

use super::*;

impl Interpreter {
    /// Record that the type `name` was just declared: a declaration the main
    /// program's lineage made (the main unit or an `EVAL` under it) is the
    /// program's own, which a REPL session does not see.
    // Cost: O(d), d = EVAL nesting of the executing unit (at most 64); plus a
    // copy-on-write insert the first time a name is recorded.
    pub(crate) fn note_main_unit_type(&mut self, name: Symbol) {
        if self
            .module
            .module_visibility
            .main_declared_types
            .contains(&name)
            || !self.is_main_lineage(self.executing_unit_sym())
        {
            return;
        }
        crate::runtime::cow_table_mut(&mut self.module.module_visibility.main_declared_types)
            .insert(name);
    }

    /// Whether `unit` is a unit of a REPL session, or an `EVAL` nested in one.
    // Cost: O(1) with no session; otherwise O(d), d = EVAL nesting of `unit`
    // (at most 64).
    pub(crate) fn unit_in_repl_session(&self, unit: Symbol) -> bool {
        if self.repl_compiler.session_units.is_empty() {
            return false;
        }
        let mut unit = Some(unit);
        for _ in 0..64 {
            let Some(sym) = unit else {
                return false;
            };
            if self.repl_compiler.session_units.contains(&sym) {
                return true;
            }
            unit = crate::runtime::eval_unit_parent(sym);
        }
        false
    }

    /// Whether the bare type name `name` is one the main program declared and
    /// code of the compilation unit `unit` therefore does not see, `unit`
    /// being a REPL session's.
    // Cost: O(1) with no session or for a name the main program did not
    // declare; otherwise `unit_in_repl_session`.
    pub(crate) fn main_type_hidden_in_unit(&self, unit: Symbol, name: Symbol) -> bool {
        !self.repl_compiler.session_units.is_empty()
            && self
                .module
                .module_visibility
                .main_declared_types
                .contains(&name)
            && self.unit_in_repl_session(unit)
    }

    /// [`Self::main_type_hidden_in_unit`] for the code chunk running now.
    // Cost: as `main_type_hidden_in_unit`.
    pub(crate) fn main_type_hidden_in_code(&self, code: &CompiledCode, name: Symbol) -> bool {
        !self.repl_compiler.session_units.is_empty()
            && self.main_type_hidden_in_unit(self.unit_of_source_sym(code.source_file), name)
    }

    /// The main program's types a REPL session unit's parse must not know, if
    /// the unit being compiled now is one; `None` otherwise.
    // Cost: O(1) with no session; otherwise O(d + t), d = EVAL nesting of the
    // running unit, t = recorded main-program types.
    pub(crate) fn session_hidden_type_names(&self) -> Option<HashSet<String>> {
        if !self.unit_in_repl_session(self.current_unit) {
            return None;
        }
        Some(
            self.module
                .module_visibility
                .main_declared_types
                .iter()
                .map(|name| name.as_str().to_string())
                .collect(),
        )
    }
}
