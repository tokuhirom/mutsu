//! A nested package published under its short name by a same-named inner type
//! (#13940).
//!
//! Rakudo makes `M::P` visible as `P` from the compunit's top level when a
//! type of the same short name is declared inside it: `module Enums { package
//! Gender { enum Gender <...> } }` lets the program (and a module's importer)
//! write `Gender::Gender(1)`, `Gender` naming the package `Enums::Gender`.
//! Intl::CLDR's `Enums` module is written that way. mutsu records the short
//! name as a package alias (`package_type_aliases`) of the compunit's top
//! level, and a module's aliases are copied into each importer.

use super::*;

impl Interpreter {
    /// Note that the type `qualified` (`Enums::Gender::Gender`) was just
    /// declared: when its short name repeats the last segment of a nested
    /// package, publish that package under the short name.
    // Cost: O(n), n = length of `qualified`.
    pub(crate) fn publish_same_named_outer_package(&mut self, qualified: &str) {
        let Some((outer, short)) = qualified.rsplit_once("::") else {
            return;
        };
        let Some((_, outer_short)) = outer.rsplit_once("::") else {
            return;
        };
        if short != outer_short || short.is_empty() || !self.is_declared_package(outer) {
            return;
        }
        let entry = (short.to_string(), outer.to_string());
        // The program's own publications are filed under the empty name.
        let module = self.module.module_load_stack.last().cloned();
        {
            let table = crate::runtime::cow_table_mut(&mut self.module.published_outer_packages);
            let list = table.entry(module.clone().unwrap_or_default()).or_default();
            if !list.contains(&entry) {
                list.push(entry.clone());
            }
        }
        if module.is_none() {
            let (short, outer) = entry;
            crate::runtime::cow_table_mut(&mut self.types.package_type_aliases)
                .entry(crate::symbol::wk::global_package().as_str().to_string())
                .or_default()
                .entry(short)
                .or_insert(outer);
        }
    }

    /// The type `qualified` names when its head is a package published under
    /// its short name here (`Gender::Gender` = `Enums::Gender::Gender`).
    // Cost: O(n + m + p), n = length of `qualified`, m and p as in
    // [`Self::is_published_outer_package`]; O(1) when nothing was published.
    pub(crate) fn published_head_type(&self, qualified: &str) -> Option<String> {
        if self.module.published_outer_packages.is_empty() {
            return None;
        }
        let (head, rest) = qualified.split_once("::")?;
        let target = self.lookup_in_running_package(&self.types.package_type_aliases, head)?;
        if !self.is_published_outer_package(head, target) {
            return None;
        }
        let resolved = format!("{target}::{rest}");
        self.has_type_direct(&resolved).then_some(resolved)
    }

    /// Whether `target` is a package published under the short name `short`
    /// ([`Self::publish_same_named_outer_package`]).
    // Cost: O(m + p), m = modules that published a package, p = their
    // publications; O(1) when none did.
    pub(crate) fn is_published_outer_package(&self, short: &str, target: &str) -> bool {
        self.module
            .published_outer_packages
            .values()
            .any(|list| list.iter().any(|(s, t)| s == short && t == target))
    }

    /// Give `importer_package` the short package names the module `module`
    /// published (see [`Self::publish_same_named_outer_package`]).
    // Cost: O(p), p = packages the module published.
    pub(crate) fn copy_published_outer_packages(&mut self, module: &str, importer_package: &str) {
        let Some(list) = self.module.published_outer_packages.get(module).cloned() else {
            return;
        };
        let table = crate::runtime::cow_table_mut(&mut self.types.package_type_aliases);
        let entry = table.entry(importer_package.to_string()).or_default();
        for (short, outer) in list {
            entry.entry(short).or_insert(outer);
        }
    }
}
