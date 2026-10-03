//! Package-qualified names a module's mainline declares at its top level,
//! kept off the per-frame `Env` (ADR-0084 §2 group 2, #7817).
//!
//! A module body runs in the IMPORTER's env, so every name a declaration bound
//! there stayed behind in each frame env of the program that loaded it. After
//! `use Cro::HTTP2::RequestParser` that was 456 of a frame env's ~630 entries:
//! the qualified names of the classes, roles and subsets the loaded modules
//! declare (`Cro::HTTP2::Frame`), and three spellings of every enum value
//! (`E::K`, `Pkg::E::K`, `Pkg::K`). Every copy-on-write deep copy of that env
//! (a call frame's setup, a closure capture) copied all of them.
//!
//! Neither kind is a lexical, and neither needs the frame env:
//!
//! - A **type** bound under its own qualified name, whose storage name is that
//!   same name, only repeats what the type registry already answers: bareword
//!   resolution falls back to `has_type`, indirect lookup to the registry, and
//!   both run the #7797 visibility gate before either. Such a binding is
//!   simply not made ([`Interpreter::qualified_identity_binding_is_redundant`]).
//!   A `my` type (mangled storage name) or a bare short name is untouched.
//! - An **enum value** is a value the registry does not hand out by name, so
//!   it goes to [`Interpreter::toplevel_package_symbols`], a per-interpreter
//!   table frames neither clone nor capture
//!   ([`Interpreter::bind_package_symbol`]). The readers that resolve a
//!   qualified name — bareword lookup, `::('…')`, the package stash and the
//!   `need` hiding scans — consult it after the env.
//!
//! As for group 1 ([`super::toplevel_callable_ids`]), "the top level of a
//! module's mainline" is decided by depth ([`Interpreter::at_module_toplevel`]):
//! a declaration nested in a routine or a block keeps its env binding, and so
//! keeps its lexical extent. A module's mainline runs once per process, so a
//! top-level binding has none to track. An env binding the importer already
//! holds under the same key is overwritten in place rather than shadowed by a
//! stale entry, so a clash resolves the way it did when both lived in the env.
//! A thread clone shares the table copy-on-write like the other program tables
//! (#7796).

use super::*;

impl Interpreter {
    /// Whether binding the type object `storage` under the package-qualified
    /// name `qualified` in the frame env would only repeat what the type
    /// registry answers: the two names agree, the name is qualified, a module
    /// mainline is declaring it directly, and the env does not already hold a
    /// binding the new one would have to replace.
    // Cost: O(|qualified|) for the comparison and the `::` scan, plus one env
    // probe.
    pub(crate) fn qualified_identity_binding_is_redundant(
        &self,
        qualified: &str,
        storage: &str,
    ) -> bool {
        qualified == storage
            && crate::runtime::utils::has_double_colon(qualified)
            && self.at_module_toplevel()
            && !self.env.contains_key(qualified)
    }

    /// Bind the package-qualified symbol `name` to `value`: in
    /// [`Self::toplevel_package_symbols`] when a module's mainline makes the
    /// binding directly, else in the frame env.
    // Cost: O(|name|) to intern, plus one amortized O(1) insert (a
    // copy-on-write table clone, O(t), only while a spawned thread still
    // shares the table, t = recorded symbols).
    pub(crate) fn bind_package_symbol(&mut self, name: String, value: Value) {
        if self.at_module_toplevel() && !self.env.contains_key(&name) {
            crate::runtime::cow_table_mut(&mut self.toplevel_package_symbols)
                .insert(Symbol::intern(&name), value);
            return;
        }
        self.env.insert(name, value);
    }

    /// The value [`Self::bind_package_symbol`] recorded for the qualified
    /// `name` in the module top-level table. Readers ask the env first, so a
    /// lexical binding of the same name shadows it.
    // Cost: O(1) when the table is empty, else O(|name|) to intern plus one
    // table probe.
    pub(crate) fn toplevel_package_symbol(&self, name: &str) -> Option<&Value> {
        if self.toplevel_package_symbols.is_empty() {
            return None;
        }
        self.toplevel_package_symbols.get(&Symbol::intern(name))
    }
}
