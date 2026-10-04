//! The `__mutsu_constant_var::` and `__mutsu_type::` markers a module's
//! mainline leaves at its top level, kept off the per-frame `Env` (ADR-0084 §2
//! group 3, #7817).
//!
//! ## `constant` markers
//!
//! Declaring a scalar or sigilless `constant` leaves a companion marker,
//! `__mutsu_constant_var::<name>`, so the two run-time readers that need to
//! know a name is a compile-time constant can ask: regex `$name`
//! interpolation (`is_compile_time_constant_scalar`, ADR-0022 Slice 5) and the
//! EVAL parser's set of declared value terms
//! (`collect_eval_user_value_term_names`). The marker is scoped like the
//! binding it describes: a later `my $name` clears it for its own scope.
//!
//! A module body runs in the IMPORTER's env, so every marker its file-scope
//! constants wrote stayed behind in each frame env of the program that loaded
//! it — 35 of them after `use Cro::HTTP2::RequestParser`, about a sixth of the
//! entries every copy-on-write deep copy of a frame env copied.
//!
//! So a marker a module's mainline writes directly ([`Interpreter::at_module_toplevel`],
//! the depth rule of ADR-0084 §7.2) goes to
//! [`ModuleToplevel::constant_markers`](super::toplevel_callable_ids::ModuleToplevel::constant_markers)
//! instead, keyed by the package that declared it, and readers find it the
//! way the package's own `constant` values are found: through the running
//! package's chain ([`Interpreter::lookup_in_running_package`]). A unit
//! module's markers are therefore visible to its own routines and not to the
//! importer, whose view of the values was already cut (#7787); a package-less
//! module file declares under GLOBAL, where both its classes' methods and the
//! importer still see it, as they see the leaked values.
//!
//! The env keeps the first word: an env marker answers for its scope whatever
//! the table holds. A redeclaration that has to hide a table marker for its own
//! scope writes `False` rather than removing the env key, since a removal
//! would let the table answer again.
//!
//! ## Type markers
//!
//! `__mutsu_type::<name>` records a binding's declared type constraint and is
//! read from the env of the frame that assigns to it, so it belongs where the
//! binding is. A unit module's file-scope constants, `our` variables and `my`
//! variables lose their env binding once the body has run (#7787, #11009, the
//! `unit_lexicals` extraction); their type markers used to stay behind,
//! orphaned, in the importer's env (`our int32 constant VERSION` in
//! `OpenSSL::Version`, `my int $be16` in `CBOR::Simple`). They now leave with
//! the binding: [`Interpreter::snapshot_type_markers`] records the importer's
//! own markers under those names before the body runs, and
//! [`Interpreter::restore_type_markers`] puts them back (or removes the
//! module's) afterwards, exactly as the bindings themselves are restored. The
//! module's own routines are unaffected: they captured the markers along with
//! the rest of their scope when they were registered.

use super::*;
use crate::meta_ns::MetaNs;

impl Interpreter {
    /// Record that `name` (an env storage name: `x` for `$x`, `\X` for a
    /// sigilless `X`) was just declared `constant`.
    // Cost: O(|name|) for the memoized key, plus one amortized O(1) insert (a
    // copy-on-write table clone, O(t), only while a spawned thread still shares
    // the table, t = recorded markers).
    pub(crate) fn note_constant_marker(&mut self, name: &str) {
        let key = MetaNs::ConstantVar.key_for_str(name);
        if self.at_module_toplevel() && !self.env.contains_key_sym(key) {
            let owner = self.current_package_str();
            crate::runtime::cow_table_mut(&mut self.module.module_toplevel.constant_markers)
                .entry(owner.to_string())
                .or_default()
                .insert(name.to_string(), ());
            return;
        }
        self.env.insert_sym_noting(key, Value::TRUE);
    }

    /// A non-`constant` declaration of `name` hides any `constant` marker for
    /// the declaring scope.
    // Cost: O(|name| + c * d), c = running package candidates (at most 4),
    // d = package nesting depth.
    pub(crate) fn clear_constant_marker(&mut self, name: &str) {
        let key = MetaNs::ConstantVar.key_for_str(name);
        if self.toplevel_constant_marker(name) {
            self.env.insert_sym_noting(key, Value::FALSE);
        } else {
            self.env.remove_sym(key);
        }
    }

    /// Whether `name` is visibly declared `constant`: the env marker, else a
    /// module top-level marker of a package the running code belongs to.
    // Cost: O(|name| + c * d), as [`Self::clear_constant_marker`].
    pub(crate) fn constant_marker_visible(&self, name: &str) -> bool {
        match self.env.get_sym(MetaNs::ConstantVar.key_for_str(name)) {
            Some(marker) => marker.truthy(),
            None => self.toplevel_constant_marker(name),
        }
    }

    /// The module top-level `constant` marker names visible from the running
    /// code, for the EVAL parser's set of declared terms.
    // Cost: O(c * d * m), c = running package candidates (at most 4),
    // d = package nesting depth, m = markers of one package.
    pub(crate) fn visible_toplevel_constant_marker_names(&self) -> Vec<&str> {
        let table = &self.module.module_toplevel.constant_markers;
        if table.is_empty() {
            return Vec::new();
        }
        let mut names = Vec::new();
        for candidate in self.running_package_candidates().into_iter().flatten() {
            let mut pkg = candidate;
            loop {
                if let Some(entries) = table.get(pkg) {
                    names.extend(entries.keys().map(String::as_str));
                }
                match crate::runtime::utils::rsplit_once_double_colon(pkg) {
                    Some((parent, _)) => pkg = parent,
                    None => break,
                }
            }
        }
        names
    }

    /// The importer's type markers under the env names `names`, before a unit
    /// module's body runs.
    // Cost: O(n), n = names, one memoized key lookup and one env probe each.
    pub(crate) fn snapshot_type_markers(
        &self,
        names: impl Iterator<Item = String>,
    ) -> Vec<(Symbol, Option<Value>)> {
        names
            .map(|name| {
                let key = MetaNs::Type.key_for_str(&name);
                (key, self.env.get_sym(key).cloned())
            })
            .collect()
    }

    /// Put back the type markers [`Self::snapshot_type_markers`] recorded,
    /// dropping the ones the module's body left in their place.
    // Cost: O(n), n = recorded names.
    pub(crate) fn restore_type_markers(&mut self, saved: Vec<(Symbol, Option<Value>)>) {
        for (key, value) in saved {
            match value {
                Some(value) => {
                    self.env.insert_sym(key, value);
                }
                None => {
                    self.env.remove_sym(key);
                }
            }
        }
    }

    // Cost: O(c * d), as [`Self::lookup_in_running_package`].
    fn toplevel_constant_marker(&self, name: &str) -> bool {
        !self.module.module_toplevel.constant_markers.is_empty()
            && self
                .lookup_in_running_package(&self.module.module_toplevel.constant_markers, name)
                .is_some()
    }
}
