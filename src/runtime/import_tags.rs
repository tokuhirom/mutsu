//! Which export tags a loaded module declares, and the error for a `use` that
//! asks for one it does not (`X::Import::NoSuchTag`).
//!
//! The run-time import checks the tags it imports, and the parse-time load of
//! a `use` (ADR-17558) checks them as soon as the module is loaded, so an
//! unknown tag is a compile-time error, as on rakudo. `:ALL` is a tag like any
//! other: it does not excuse an unknown one beside it (`use M :ALL, :NOPE`).

use super::*;

impl Interpreter {
    /// The first of `tags` (a `use`'s import tags; none means `:DEFAULT`) that
    /// `module`, which is loaded, does not declare: `DEFAULT`, `ALL` and
    /// `MANDATORY` always exist, any other tag only when an export of the
    /// module carries it.
    // Cost: O(e·t + r), e = the module's exports, t = tags per export, r = `tags`.
    pub(crate) fn unknown_import_tag(&self, module: &str, tags: &[String]) -> Option<String> {
        let world = &self.module.world;
        let mut known: HashSet<String> = ["DEFAULT", "ALL", "MANDATORY"]
            .iter()
            .map(|tag| tag.to_string())
            .collect();
        let tables = [
            world.exported_subs.read().get(module).cloned(),
            world.unit_module_exported_subs.read().get(module).cloned(),
            world.module_owned_exports.read().get(module).cloned(),
        ];
        for table in tables.into_iter().flatten() {
            known.extend(table.into_values().flatten());
        }
        if let Some(vars) = world.exported_vars.read().get(module) {
            known.extend(vars.values().flatten().cloned());
        }
        tags.iter().find(|tag| !known.contains(*tag)).cloned()
    }

    /// `X::Import::NoSuchTag` for a `use` of `module` asking for `tag`.
    // Cost: O(1).
    pub(crate) fn no_such_import_tag(module: &str, tag: &str) -> RuntimeError {
        let message = format!("Error while importing from '{module}': no such tag '{tag}'");
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("source-package".to_string(), Value::str(module.to_string()));
        attrs.insert("tag".to_string(), Value::str(tag.to_string()));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::Import::NoSuchTag"),
            attrs,
        )));
        err
    }
}
