//! The lexical pad `CompUnit::Handle.unit` answers for a loaded unit.
//!
//! Rakudo's handle wraps the unit's mainline lexpad, a hash-like map of the
//! unit's lexical entries (`$=pod`, `EXPORT`, `GLOBALish`, `&EXPORT`, ...)
//! that `CompUnit::Util::at-unit` and friends read with `nqp::existskey` /
//! `nqp::atkey`. mutsu runs a module's body and keeps no pad per unit, so the
//! pad is built from what the unit left behind: its Pod (scanned from the
//! source, not executed) and its `EXPORT` package.

use super::*;

impl Interpreter {
    /// A lexical pad over `entries`: a hash typed `BOOTContext`, which the
    /// `nqp::` hash and iteration ops accept (see `value::type_name`).
    // Cost: O(1) beyond moving `entries`.
    pub(crate) fn lexical_pad_value(entries: ValueMap) -> Value {
        let mut data = crate::value::HashData::from(entries);
        data.declared_type = Some("BOOTContext".to_string());
        Value::hash_with_data(crate::gc::Gc::new(data))
    }

    /// The pad of the loaded unit `name`, the same `Hash` on every ask.
    // Cost: O(n + s) on the first ask, n = source bytes scanned for Pod, s =
    // symbols under the unit's `EXPORT` package; O(1) after.
    pub(crate) fn loaded_unit_pad(&mut self, name: &str) -> Value {
        if let Some(pad) = self.module.loaded_unit_pads.get(name) {
            return pad.clone();
        }
        let mut pad = ValueMap::default();
        pad.insert(
            "$?PACKAGE".to_string(),
            Value::package(crate::symbol::Symbol::intern("GLOBAL")),
        );
        if let Some((path, None)) = self.resolve_module_path(name)
            && let Ok(source) = std::fs::read_to_string(&path)
        {
            let (entries, _) = Self::scan_pod_source(&source);
            pad.insert("$=pod".to_string(), Value::real_array(entries));
        }
        let export = format!("{name}::EXPORT");
        if !crate::qualified_tail_index::names_under_package(&export).is_empty() {
            pad.insert(
                "EXPORT".to_string(),
                Value::package(crate::symbol::Symbol::intern(&export)),
            );
        }
        let pad = Self::lexical_pad_value(pad);
        // A unit still loading may yet add to its `EXPORT` package or Pod.
        if self.module.loaded_modules.contains(name) {
            self.module
                .loaded_unit_pads
                .insert(name.to_string(), pad.clone());
        }
        pad
    }
}
