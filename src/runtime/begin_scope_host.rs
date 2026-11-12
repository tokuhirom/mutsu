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
            let target = crate::qualified::qualified_text(UNIT_EXPORT_PACKAGE, &tag)
                .as_str()
                .to_string();
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
            // A package the unit being compiled declared lexically before the
            // phaser (`my package EXPORTHOW { ... }`), as the resolver's view
            // of the current scope answers it.
            "resolve-lexical-constant" => Some(self.resolve_unit_lexical_package(_args)),
            // No other lexical was declared at BEGIN time that the resolver
            // could answer: the module installs through its own table.
            "resolve-lexical-constant-in-setting"
            | "resolve-lexical"
            | "find-generated-lexical" => Some(Ok(Value::NIL)),
            // The unit's `GLOBALish`: mutsu's units share `GLOBAL`, as the bare
            // `GLOBALish` term answers (a stash vivified under it is the
            // program's, #15371).
            "get-global" => Some(Ok(Value::package(Symbol::intern("GLOBAL")))),
            _ => None,
        }
    }

    /// Install `name` with `value` into the unit being compiled, as a BEGIN
    /// phaser's `$*CU.merge-generated-lexical-declaration` asks (#15371). An
    /// `&EXPORT` becomes the unit's export hook, run for its importer once its
    /// body is done; any other name is not supported yet.
    // Cost: O(1).
    pub(crate) fn merge_generated_lexical(
        &mut self,
        name: &str,
        value: Value,
        into_block: bool,
    ) -> Result<Value, RuntimeError> {
        if name == "&EXPORT" {
            let Some(unit) = self.module.module_load_stack.last().cloned() else {
                return Err(RuntimeError::new(
                    "merge-generated-lexical-declaration: &EXPORT outside a module load",
                ));
            };
            self.module.pending_inner_export_subs.insert(unit, value);
            return Ok(Value::NIL);
        }
        // A running `sub EXPORT` merges into its importer (`set-unit` /
        // `set-lexpad` from EXPORT): the importer's env is not the running one,
        // so the declaration is installed with the hook's map once it returns
        // (`install_hook_result`).
        if !self.module.export_hook_importer_envs.is_empty() {
            self.module
                .pending_generated_lexicals
                .push((into_block, name.to_string(), value));
            return Ok(Value::NIL);
        }
        // TODO: install a generated lexical into the unit's scope from a BEGIN
        // phaser of the module itself (#15371): the module's BEGIN runs inside
        // its body, whose top-level scope is not reachable from the phaser's
        // frame yet.
        Err(RuntimeError::new(format!(
            "merge-generated-lexical-declaration of '{name}' is not supported yet"
        )))
    }

    /// `$*R.resolve-lexical-constant($name)`: a package type object the
    /// running unit's lexical scope binds under `name` (and that is not the
    /// setting's), as the `RakuAST::Declaration::Import` that names it; `Nil`
    /// otherwise.
    // Cost: O(d) for the scope lookup, d = scope depth.
    fn resolve_unit_lexical_package(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(name) = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(..)))
            .map(Value::to_string_value)
        else {
            return Ok(Value::NIL);
        };
        if crate::core_type_names::contains(&name) || name.starts_with(['$', '@', '%', '&']) {
            return Ok(Value::NIL);
        }
        let Some(value) = self.env().get(&name).cloned() else {
            return Ok(Value::NIL);
        };
        let value = value.deref_container();
        if !matches!(value.view(), ValueView::Package(_)) {
            return Ok(Value::NIL);
        }
        // The resolver's own form of the declaration: its `compile-time-value`
        // reads the name in the scope that asks (`external_value`).
        let _ = value;
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("lexical-name".to_string(), Value::str(name));
        Ok(Value::make_instance(
            Symbol::intern("RakuAST::Declaration::Import"),
            attrs,
        ))
    }
}
