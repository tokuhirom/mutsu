//! Compilation-unit scoping for routines whose registration is process-global.
//!
//! mutsu's routine registry is flat and keyed by `PACKAGE::name`, which models
//! Raku's lexical scoping well enough for ordinary declarations and imports: a
//! module's own routines register under its package, and an import alias is
//! installed for — and removed with — the scope that asked for it.
//!
//! A *prelude splice* fits neither shape. mutsu has no `NativeCall.rakumod`, so
//! its five helpers (`nativecast`, `nativesizeof`, `cglobal`,
//! `explicitly-manage`, `refresh`) are injected as an `our sub` prelude into
//! every compunit whose source mentions the module, and each registers under
//! `GLOBAL::` rather than under the host compunit's package — because a method
//! body running under *any* package has to reach it by bare name (see
//! `NATIVECALL_SUB_PRELUDES`). That makes the registration process-global, and
//! until GH #7612 it made the *visibility* process-global too: once any compunit anywhere had pulled the prelude in, `&nativecast`
//! resolved from every scope in the process, including a script that merely
//! `use`d such a module two levels up. Rakudo leaves the name undeclared there.
//!
//! This module keeps the registration global and makes the visibility lexical:
//! the splice records which compunits it went into, and routine resolution asks
//! which compunit is executing.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// `Symbol` form of [`Self::executing_source_file`], normalized so that
    /// "the main script" is always [`crate::runtime::main_unit`] rather than
    /// `None` or the program path.
    ///
    /// Allocation-free (the `String` variant resolves the interned path), which
    /// matters because the prelude-visibility gate that reads it sits on the
    /// routine-resolution path.
    pub(crate) fn executing_unit_sym(&self) -> Symbol {
        for frame in self.routine_stack.iter().rev() {
            match frame.def_file {
                Some(file) => return self.unit_of_source_sym(Some(file)),
                None if frame.is_block => continue,
                None => break,
            }
        }
        self.unit_of_source_sym(self.current_source_file_sym())
    }

    /// The unit a routine *declared* in `file` belongs to. Mirrors
    /// [`Self::executing_unit_sym`]'s normalization so the two agree on the
    /// main script.
    pub(crate) fn declaring_unit_sym(&self) -> Symbol {
        self.unit_of_source_sym(self.current_source_file_sym())
    }

    fn unit_of_source_sym(&self, file: Option<Symbol>) -> Symbol {
        match (file, self.program_path.as_deref()) {
            (None, _) => crate::runtime::main_unit(),
            (Some(f), Some(prog)) if f.resolve() == prog => crate::runtime::main_unit(),
            (Some(f), _) => f,
        }
    }

    /// Install `file` as the ambient `?FILE`, handing back what to restore.
    ///
    /// `?FILE` is the dynamically-scoped "unit being compiled/loaded" marker
    /// that [`Self::current_source_file`] reads, and every declaration
    /// registered under it records it as its own `source_file`. A role's
    /// deferred body is re-run at each composition, from the composing scope,
    /// so it has to be re-established there (see [`crate::runtime::RoleDef`]'s
    /// `decl_file`). Returns `None` -- meaning "nothing to restore" -- when
    /// `file` is `None`, so a caller with no recorded file is a no-op.
    pub(crate) fn enter_source_file(&mut self, file: Option<&str>) -> Option<Option<Value>> {
        let file = file?;
        let saved = self.env.get("?FILE").cloned();
        self.env.insert("?FILE".to_string(), Value::str(file.to_string()));
        Some(saved)
    }

    /// Undo an [`Self::enter_source_file`].
    pub(crate) fn leave_source_file(&mut self, saved: Option<Option<Value>>) {
        match saved {
            None => {}
            Some(Some(prev)) => {
                self.env.insert("?FILE".to_string(), prev);
            }
            Some(None) => {
                self.env.remove("?FILE");
            }
        }
    }

    /// Whether a routine registered under `key` is visible to the code that is
    /// running right now.
    ///
    /// Only prelude splices (`prelude_registered_functions`) are ever hidden:
    /// they register process-globally under `GLOBAL::` so that a method body
    /// under any package can call them, but rakudo exports them from
    /// `NativeCall.rakumod`, so a compunit that never mentioned NativeCall must
    /// not see them. `use`ing a module that itself uses NativeCall therefore
    /// must not make `&nativecast` resolvable in the using scope (GH #7612).
    ///
    /// Every other key answers `true` without touching the map, and the whole
    /// check short-circuits on the (overwhelmingly common) empty prelude set.
    pub(crate) fn prelude_visible_here(&self, key: Symbol) -> bool {
        if self.prelude_registered_functions.is_empty()
            || !self.prelude_registered_functions.contains(&key)
        {
            return true;
        }
        match self.prelude_declaring_units.get(&key) {
            // No provenance recorded (a thread clone that predates the splice,
            // a synthetic registration): stay permissive rather than hide a
            // helper the running code legitimately declared.
            None => true,
            // Two anchors, because neither covers the other. `?FILE` (via
            // `executing_unit_sym`) is the only one that is right while a
            // module's MAINLINE runs -- `current_unit` is not switched around
            // `load_module`'s `run_block`, so it still names whoever triggered
            // the load. `current_unit` is the only one that names an EVAL unit
            // whose `?FILE` a nested frame has since moved on from. Both walk
            // the EVAL parent chain, since `EVAL` compiles in its caller's
            // lexical scope.
            Some(units) => {
                self.unit_chain_contains(self.executing_unit_sym(), units)
                    || self.unit_chain_contains(self.current_unit, units)
            }
        }
    }

    /// Whether `start`, or any unit it was `EVAL`ed inside of, is in `units`.
    ///
    /// `EVAL` compiles in its caller's lexical scope, so a declaration made by
    /// the enclosing unit is in scope for the `EVAL`ed code, while one made BY
    /// that code is scoped to the `EVAL` unit alone. Shared with operator
    /// scoping (`Interpreter::user_infix_override`), which asks the same
    /// question of `user_declared_infix_ops`.
    pub(crate) fn unit_chain_contains(&self, start: Symbol, units: &HashSet<Symbol>) -> bool {
        let mut unit = Some(start);
        // An EVAL nested in an EVAL nested in ... is bounded in practice; the
        // cap only stops a cycle from hanging the VM.
        for _ in 0..64 {
            let Some(sym) = unit else { return false };
            if units.contains(&sym) {
                return true;
            }
            unit = crate::runtime::eval_unit_parent(sym);
        }
        false
    }
}
