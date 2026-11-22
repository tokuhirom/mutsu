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
//! A type a session unit declares under the name of one of those hidden types
//! is the session's own, living alongside the program's (#17652): rakudo
//! declares it in the session's fresh `GLOBAL`. mutsu declares it the way a
//! `my class` is ([`declare_session_types_apart`]), under a storage name of its
//! own (ADR-0047), which the session's later units see as they see any lexical
//! of an earlier unit, and which leaves the program's type untouched.
//!
//! A session's units are the ones `Perl6::Compiler.eval` ran with no outer
//! context, the units later run in their contexts, and every `EVAL` nested in
//! one of those ([`Interpreter::unit_in_repl_session`]).

use super::*;

/// Declare each top-level `class` / `role` (a grammar is a class) of a REPL
/// session unit whose name is one of the program's types the session does not
/// see (`hidden`) as a lexical type, so it is a type of the session's own
/// instead of a redeclaration of the program's.
// Cost: O(s), s = top-level statements of the unit.
pub(crate) fn declare_session_types_apart(stmts: &mut [Stmt], hidden: &HashSet<String>) {
    for stmt in stmts {
        let (name, is_lexical) = match stmt {
            Stmt::ClassDecl {
                name, is_lexical, ..
            }
            | Stmt::RoleDecl {
                name, is_lexical, ..
            } => (*name, is_lexical),
            _ => continue,
        };
        if !*is_lexical && hidden.contains(name.as_str()) {
            *is_lexical = true;
        }
    }
}

impl Interpreter {
    /// Record that the type `name` was just declared: a declaration the main
    /// program's lineage made (the main unit or an `EVAL` under it) is the
    /// program's own, which a REPL session does not see.
    // Cost: O(d), d = EVAL nesting of the executing unit (at most 64); plus a
    // copy-on-write insert the first time a name is recorded.
    pub(crate) fn note_main_unit_type(&mut self, name: Symbol) {
        if self
            .module
            .world.module_visibility
            .main_declared_types
            .contains(&name)
            || !self.is_main_lineage(self.executing_unit_sym())
        {
            return;
        }
        crate::runtime::cow_table_mut(&mut self.module.world.module_visibility.main_declared_types)
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
                .world.module_visibility
                .main_declared_types
                .contains(&name)
            && self.unit_in_repl_session(unit)
    }

    /// The lexical (`my`) type `env` binds the bare name `name` to here, under
    /// its ADR-0047 storage name: what a name hidden from the code running now
    /// still means when the code's own scope declared a type of it (#17652).
    // Cost: O(1) expected, one env probe.
    pub(crate) fn lexical_type_in_scope(&self, name: &str) -> Option<Value> {
        self.env()
            .get(name)
            .filter(|value| {
                matches!(value.view(), ValueView::Package(sym)
                    if crate::qualified::unqualified_part(sym).with_str(|short| {
                        short.split_once('\u{0}').is_some_and(|(head, _)| head == name)
                    }))
            })
            .cloned()
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
                .world.module_visibility
                .main_declared_types
                .iter()
                .map(|name| name.as_str().to_string())
                .collect(),
        )
    }
}
