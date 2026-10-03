//! Package-qualified names a module's mainline declares at its top level,
//! kept off the per-frame `Env` (ADR-0084 §2 group 2, #7817).
//!
//! A module body runs in the IMPORTER's env, so every name a declaration bound
//! there stayed behind in each frame env of the program that loaded it. After
//! `use Cro::HTTP2::RequestParser` that was 456 of a frame env's ~630 entries:
//! the qualified names of the classes, roles and subsets the loaded modules
//! declare (`Cro::HTTP2::Frame`), and the qualified spellings of enum values
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
//!   it goes to [`ModuleToplevel::package_symbols`](super::toplevel_callable_ids::ModuleToplevel::package_symbols), a per-interpreter
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
    /// Bind `E::K` in the declaring unit package's scope when the enum is
    /// private. Its `Pkg::E::K` and `Pkg::K` spellings remain package symbols.
    // Cost: O(|enum_name| + |variant| + t) when copy-on-write clones a table
    // of t scoped names; otherwise an amortized O(1) insert.
    pub(crate) fn bind_enum_short_symbol(
        &mut self,
        enum_name: &str,
        variant: &str,
        value: Value,
        exported: bool,
    ) {
        let enum_sym = Symbol::intern(enum_name);
        let key = crate::qualified::qualified(enum_sym, Symbol::intern(variant))
            .as_str()
            .to_string();
        if !exported
            && self.at_module_toplevel()
            && !self.current_package_is_global()
            && !crate::qualified::is_qualified(enum_sym)
        {
            let owner = self.current_package();
            crate::runtime::cow_table_mut(&mut self.module_scope_lexicals)
                .entry(owner)
                .or_default()
                .insert(key, value);
        } else {
            self.bind_package_symbol(key, value);
        }
    }

    /// Whether binding the type object `storage` under the package-qualified
    /// name `qualified` in the frame env would only repeat what the type
    /// registry answers: the two names agree, the name is qualified, a module
    /// mainline is declaring it directly, and the env does not already hold a
    /// binding the new one would have to replace.
    // Cost: O(|qualified|) for the comparison and the intern (whether a
    // symbol is qualified is classified once per symbol), plus one env probe.
    pub(crate) fn qualified_identity_binding_is_redundant(
        &self,
        qualified: &str,
        storage: &str,
    ) -> bool {
        qualified == storage
            && crate::qualified::is_qualified(Symbol::intern(qualified))
            && self.at_module_toplevel()
            && !self.env.contains_key(qualified)
    }

    /// Bind the package-qualified symbol `name` to `value`: in
    /// [`ModuleToplevel::package_symbols`](super::toplevel_callable_ids::ModuleToplevel::package_symbols) when a module's mainline makes the
    /// binding directly, else in the frame env.
    // Cost: O(|name|) to intern, plus one amortized O(1) insert (a
    // copy-on-write table clone, O(t), only while a spawned thread still
    // shares the table, t = recorded symbols).
    pub(crate) fn bind_package_symbol(&mut self, name: String, value: Value) {
        if self.at_module_toplevel() && !self.env.contains_key(&name) {
            crate::runtime::cow_table_mut(&mut self.module_toplevel.package_symbols)
                .insert(Symbol::intern(&name), value);
            return;
        }
        self.env.insert(name, value);
    }

    /// A private enum's short qualified name in the running module's scope,
    /// or the value [`Self::bind_package_symbol`] recorded in the top-level
    /// table. Readers ask the env first, so a lexical binding shadows either.
    // Cost: O(c * d + |name|), c = running package candidates (at most 4),
    // d = package nesting depth, including the global table lookup.
    pub(crate) fn toplevel_package_symbol(&self, name: &str) -> Option<&Value> {
        if let Some(scoped) = self.lookup_in_running_package(&self.module_scope_lexicals, name) {
            return Some(scoped);
        }
        if self.module_toplevel.package_symbols.is_empty() {
            return None;
        }
        self.module_toplevel
            .package_symbols
            .get(&Symbol::intern(name))
    }
}
