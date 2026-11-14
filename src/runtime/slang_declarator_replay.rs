//! A slang package declarator that parsed its own body reaches the unit by two
//! replays in a fixed order (ADR-17741 §2.4).
//!
//! The host ran the production: a `FOREIGN-LANG` grammar minted a type, and the
//! production's own action (Grammar::BNF's `modern-install`) bound it into a
//! scratch package of the host's resolver. The statement the host returns calls
//! `__mutsu_slang_replay_declarator` with the hook's class log, the slang's
//! action roles and the recorded actions. Here, in the unit:
//!
//! 1. the class log is replayed (ADR-13370 §3a), which re-creates the minted
//!    types and names the host type each stands for;
//! 2. every type the recorded Matches hold is mapped through that table, so
//!    `$<rules>.made` is the unit's type;
//! 3. the production's action re-runs (ADR-17243) with a resolver whose
//!    `current-package` is the package the statement stands in and whose
//!    `get-global` is `GLOBAL`: its stash bindings land in the unit's packages;
//! 4. the `Declaration::Import`s that run merged are kept by site, and the
//!    lexicals the statement declares (`__mutsu_slang_declared_import`) read
//!    their values from there.

use super::*;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    /// What each replayed production imported, by the site the host named it:
    /// a statement the unit executes again (a loop body, a routine) must not run
    /// the action twice, and its lexicals still read what the one run imported.
    static IMPORTS: RefCell<HashMap<i64, Vec<(String, Value)>>> = RefCell::new(HashMap::new());
}

/// How deep [`remap_minted`] follows nested values: a Match tree is as deep as
/// the grammar's nesting, which the source being parsed decides.
const MAX_REMAP_DEPTH: usize = 256;

/// `value` with every host type named in `renamed` replaced by the unit's;
/// `None` when it holds none.
// Cost: O(v), v = the values reachable from `value` (to `MAX_REMAP_DEPTH`).
fn remap_minted(value: &Value, renamed: &[(String, Value)], depth: usize) -> Option<Value> {
    if renamed.is_empty() || depth > MAX_REMAP_DEPTH {
        return None;
    }
    // Each part, and whether any changed.
    let parts = |items: &mut dyn Iterator<Item = &Value>| -> Option<Vec<Value>> {
        let mut changed = false;
        let mapped: Vec<Value> = items
            .map(|v| match remap_minted(v, renamed, depth + 1) {
                Some(now) => {
                    changed = true;
                    now
                }
                None => v.clone(),
            })
            .collect();
        changed.then_some(mapped)
    };
    match value.view() {
        ValueView::Package(name) => {
            let name = name.resolve();
            renamed
                .iter()
                .find(|(host, _)| *host == name)
                .map(|(_, unit)| unit.clone())
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => {
            let map = attributes.to_map();
            let keys: Vec<_> = map.keys().cloned().collect();
            let values = parts(&mut map.values())?;
            Some(Value::make_instance(
                class_name,
                keys.into_iter()
                    .zip(values)
                    .map(|(key, value)| (key.resolve().to_string(), value))
                    .collect::<HashMap<String, Value>>(),
            ))
        }
        ValueView::Pair(key, inner) => {
            remap_minted(inner, renamed, depth + 1).map(|now| Value::pair(key.to_string(), now))
        }
        ValueView::Array(..) => {
            let items = value.as_list_items()?;
            parts(&mut items.iter()).map(Value::array)
        }
        // A Match's named captures.
        ValueView::Hash(hash) => {
            let keys: Vec<String> = hash.iter().map(|(k, _)| k.to_string()).collect();
            let values = parts(&mut hash.iter().map(|(_, v)| v))?;
            let map: crate::value::ValueMap = keys.into_iter().zip(values).collect();
            Some(Value::hash(map))
        }
        _ => None,
    }
}

impl Interpreter {
    /// `__mutsu_slang_replay_declarator($log, @roles, @events)`: replay a
    /// package declarator production here (see the module docs).
    // Cost: O(k + v + a), k = the recorded metamodel calls, v = the values the
    // recorded Matches hold, a = the recorded actions (once per production).
    pub(super) fn builtin_slang_replay_declarator(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let log = args.first().cloned().unwrap_or(Value::NIL);
        let roles = args.get(1).cloned().unwrap_or(Value::NIL);
        let events: Vec<Value> = args
            .get(2)
            .and_then(Value::as_list_items)
            .map(|items| items.to_vec())
            .ok_or_else(|| RuntimeError::new("slang declarator replay without its actions"))?;
        let site = events
            .first()
            .and_then(Value::as_list_items)
            .and_then(|parts| parts.get(3).and_then(Value::as_int))
            .unwrap_or(0);
        if IMPORTS.with(|i| i.borrow().contains_key(&site)) {
            return Ok(Value::NIL);
        }
        // TODO: a slang whose action roles only the host can name (#17008)
        // keeps its effects in the host, as ADR-17243's replay does.
        if !self.slang_replay_roles_known(&roles) {
            return Err(RuntimeError::new(
                "slang package declarator: its actions role is not visible to the unit",
            ));
        }
        let renamed = self.replay_class_log(&log)?;
        let events: Vec<Value> = events
            .iter()
            .map(|event| remap_minted(event, &renamed, 0).unwrap_or_else(|| event.clone()))
            .collect();
        let package = Value::package(Symbol::intern(&self.current_package()));
        let resolver = self.slang_replay_resolver(package)?;
        self.replay_action_events(roles, &events, Some(resolver.clone()))?;
        let imports = self.call_method_with_values(resolver, "imports", Vec::new())?;
        let imports: Vec<(String, Value)> = imports
            .as_list_items()
            .map(|items| {
                items
                    .iter()
                    .filter_map(|pair| match pair.view() {
                        ValueView::Pair(name, value) => Some((name.to_string(), value.clone())),
                        ValueView::ValuePair(name, value) => {
                            Some((name.to_string_value(), value.clone()))
                        }
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or_default();
        IMPORTS.with(|i| i.borrow_mut().insert(site, imports));
        Ok(Value::NIL)
    }

    /// `__mutsu_slang_declared_import($site, $name)`: the value the replay of
    /// the production at `$site` imported as the lexical `$name`; the last one
    /// merged wins, as `:force` replaces.
    // Cost: O(i) plus at most two package lookups, i = the production's imports.
    pub(super) fn builtin_slang_declared_import(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let site = args.first().and_then(Value::as_int).unwrap_or(0);
        let name = args.get(1).map(Value::to_string_value).unwrap_or_default();
        let imported = IMPORTS.with(|i| {
            i.borrow().get(&site).and_then(|imports| {
                imports
                    .iter()
                    .rev()
                    .find(|(known, _)| *known == name)
                    .map(|(_, value)| value.clone())
            })
        });
        if let Some(value) = imported {
            return Ok(value);
        }
        // The host's scratch package lacks what the unit already has (`class A
        // {}` before `bnf-grammar A::B`): the host's run made a stub `A` and
        // imported it, the unit's run found the real `A` and imported nothing.
        // The lexical then names what the action found, the package as the
        // enclosing package or `GLOBAL` holds it.
        let package = Symbol::intern(&self.current_package());
        let bare = Symbol::intern(&name);
        let mut candidates = vec![bare];
        if !crate::qualified::is_global_package(package) {
            candidates.insert(0, crate::qualified::qualified(package, bare));
        }
        // Not through the lexical scope: the lexical being declared would
        // answer for itself.
        for candidate in candidates {
            if let Some(found) = self.get_our_var(candidate.as_str()) {
                return Ok(found.deref_container());
            }
            if self.has_type(candidate.as_str()) {
                return Ok(Value::package(candidate));
            }
        }
        Err(RuntimeError::new(format!(
            "slang package declarator: the unit's run of the action neither imported nor found '{name}'"
        )))
    }
}
