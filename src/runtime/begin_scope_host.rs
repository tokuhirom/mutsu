//! The BEGIN-time scope surface a module such as `CompUnit::Util` calls on
//! `$*R` / `$*CU` (ADR-15371): the unit's live `EXPORT` package, its
//! `GLOBALish`, and the resolver's scope queries.
//!
//! mutsu runs a module's BEGIN phasers while its body executes, inside the
//! `use` that loads it, so "the unit being compiled" is the module on top of
//! `module_load_stack` and "its exports" are the records
//! `publish_package_stash_symbol` already keeps for an `EXPORT::<tag>`
//! package. Nothing here is a native substitute for the module: the vendored
//! upstream `CompUnit::Util` runs verbatim and this is only the host object it
//! calls (ADR-0096).

use super::*;
use crate::value::ValueView;

/// The package `$*R`'s `$!export-package` answers: the `EXPORT` package of the
/// unit being compiled.
pub(crate) const UNIT_EXPORT_PACKAGE: &str = "EXPORT";

impl Interpreter {
    /// Whether `target` is the stash of the loading unit's own `EXPORT`
    /// package (what `$*R`'s `$!export-package` stands for), as opposed to the
    /// `EXPORT` stash of an already loaded module.
    // Cost: O(1).
    pub(crate) fn stash_is_unit_export_target(&self, target: &Value) -> bool {
        if self.module.module_load_stack.is_empty() {
            return false;
        }
        let ValueView::Instance { attributes, .. } = target.view() else {
            return false;
        };
        attributes
            .as_map()
            .get("name")
            .is_some_and(|n| n.to_string_value().trim_end_matches("::") == UNIT_EXPORT_PACKAGE)
    }

    /// `EXPORT.WHO.merge-symbols($other-unit's-export-package)`: every symbol
    /// of every `EXPORT::<tag>` the incoming stash holds becomes an export of
    /// the unit being loaded under the same tag.
    // Cost: O(s), s = symbols in the incoming stash's tag packages.
    pub(crate) fn merge_unit_export_symbols(&mut self, incoming: &Value) -> Value {
        let ValueView::Instance { attributes, .. } = incoming.view() else {
            return Value::NIL;
        };
        let Some(symbols) = attributes.as_map().get("symbols").cloned() else {
            return Value::NIL;
        };
        let tags: Vec<(String, Value)> = match symbols.view() {
            ValueView::Hash(map) => map.iter().map(|(k, v)| (k.to_string(), v.clone())).collect(),
            _ => return Value::NIL,
        };
        for (tag, package) in tags {
            // A tag is a package type object (`Mod::EXPORT::DEFAULT`); its
            // stash holds the symbols.
            let package = match package.view() {
                ValueView::Package(name) => self.package_stash_value(&name.resolve()),
                _ => package,
            };
            let entries: Vec<(String, Value)> = match package.view() {
                ValueView::Instance { attributes, .. } => {
                    match attributes.as_map().get("symbols").map(Value::view) {
                        Some(ValueView::Hash(map)) => {
                            map.iter().map(|(k, v)| (k.to_string(), v.clone())).collect()
                        }
                        _ => continue,
                    }
                }
                _ => continue,
            };
            let target = format!("{UNIT_EXPORT_PACKAGE}::{tag}");
            for (key, value) in entries {
                let (sigil, bare) = match key.as_bytes().first() {
                    Some(b'&' | b'@' | b'%') => (&key[..1], &key[1..]),
                    _ => ("", key.as_str()),
                };
                if value.is_nil() {
                    continue;
                }
                self.publish_package_stash_symbol(target.clone(), sigil, bare, &value);
            }
        }
        Value::NIL
    }

    /// Native methods of the compile-time resolver `$*R` a BEGIN phaser sees,
    /// beyond `find-attach-target` (`dispatch_attach_target_method`).
    // Cost: O(1) per query.
    pub(crate) fn dispatch_begin_scope_method(
        &mut self,
        method: &str,
        _args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            // The scope being compiled is the unit's: `$*R.current-scope`
            // answers the compile-time `$*CU`, so `scope =:= $*CU` holds for
            // the unit scope.
            "current-scope" => Some(Ok(self
                .env()
                .get("*CU")
                .cloned()
                .or_else(|| self.compile_time_cu_value())
                .unwrap_or(Value::NIL))),
            // No lexical was declared at BEGIN time that the resolver could
            // answer: the module installs through its own table.
            "resolve-lexical-constant"
            | "resolve-lexical-constant-in-setting"
            | "resolve-lexical"
            | "find-generated-lexical" => Some(Ok(Value::NIL)),
            // The unit's `GLOBALish`.
            "get-global" => Some(Ok(Value::package(Symbol::intern("GLOBALish")))),
            _ => None,
        }
    }
}
