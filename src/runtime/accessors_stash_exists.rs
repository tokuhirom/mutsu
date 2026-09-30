//! Whether a spelled `Foo::Bar::` package stash names a package at all (#9845).
//!
//! Rakudo resolves the package part of `Foo::Bar::<$x>` / `Foo::Bar::.keys`
//! before the stash is read, and a qualified name that resolves to nothing
//! dies with `Could not find symbol '&Bar' in 'GLOBAL::Foo'`. mutsu builds the
//! stash from flat qualified keys, so without this check any spelling handed
//! back an empty stash.
use super::*;

/// First components that name a pseudo-package (or the root) rather than a
/// package that must exist: their stashes have their own rules.
const PSEUDO_ROOTS: &[&str] = &[
    "GLOBAL",
    "CORE",
    "SETTING",
    "UNIT",
    "OUR",
    "MY",
    "LEXICAL",
    "OUTER",
    "OUTERS",
    "CALLER",
    "CALLERS",
    "DYNAMIC",
    "CLIENT",
    "COMPILING",
    "PROCESS",
    "EXPORT",
];

impl Interpreter {
    /// Whether `package` is a package mutsu knows: a declared class, role,
    /// module, package or enum, a built-in type, an export view, a package
    /// bound in the env, or a namespace some live qualified symbol sits under
    /// (`our $Foo::Bar::x` makes `Foo` and `Foo::Bar` packages implicitly).
    // Cost: O(k + m), k = interned qualified names under `package`, m = symbols
    // interned since the last `names_under_package` catch-up (amortized O(1)
    // per symbol); every other probe is a hash lookup.
    pub(crate) fn stash_package_exists(&self, package: &str) -> bool {
        if self.is_known_package(package)
            || self.is_declared_package(package)
            || crate::runtime::utils::is_known_type_constraint(package)
            || Self::package_export_tag_parts(package).is_some()
            || Self::package_export_module(package).is_some()
            || self.exported_subs.contains_key(package)
            || self.exported_vars.contains_key(package)
            || self.registry().enum_types.contains_key(package)
            || self.get_env_with_main_alias(package).is_some()
        {
            return true;
        }
        let registry = self.registry();
        crate::qualified_tail_index::names_under_package(package)
            .into_iter()
            .any(|name| {
                self.env.overlay_get_sym(name).is_some()
                    || self.env.get(name.as_str()).is_some()
                    || self.get_our_var(name.as_str()).is_some()
                    || registry.functions.contains_key(&name)
                    || registry.proto_functions.contains_key(&name)
                    || registry.classes.contains_key(name.as_str())
                    || registry.package_kinds.contains_key(name.as_str())
            })
    }

    /// The error a read of the literally spelled stash `package` (already
    /// normalized, no trailing `::`) raises because its package does not
    /// exist, or `None` when the read may go ahead.
    ///
    /// Only a qualified spelling is checked: rakudo rejects an undeclared
    /// one-component `Foo::` at compile time, which mutsu does not model. A
    /// name rooted in a built-in type is left alone too, since mutsu does not
    /// model every core sub-namespace rakudo has. As in rakudo, the unknown
    /// package's prefix is reported under `GLOBAL::` exactly when its first
    /// component is itself unknown.
    // Cost: O(k), k = interned qualified names under the package and under its first
    // component (`stash_package_exists`).
    pub(crate) fn missing_stash_package_error(&self, package: &str) -> Option<RuntimeError> {
        let package_sym = Symbol::intern(package);
        let prefix = crate::qualified::package_parent(package_sym)?;
        let last = crate::qualified::unqualified_part(package_sym);
        let first = crate::qualified::package_ancestors(package_sym)
            .last()
            .unwrap_or(package_sym);
        let (prefix, last, first) = (prefix.as_str(), last.as_str(), first.as_str());
        if prefix.is_empty()
            || last.is_empty()
            || PSEUDO_ROOTS.contains(&first)
            || self.stash_package_exists(package)
        {
            return None;
        }
        let first_known = self.stash_package_exists(first);
        if first_known && crate::runtime::utils::is_known_type_constraint(first) {
            return None;
        }
        let qualified = if first_known {
            prefix.to_string()
        } else {
            format!("GLOBAL::{prefix}")
        };
        Some(RuntimeError::new(format!(
            "Could not find symbol '&{last}' in '{qualified}'"
        )))
    }
}
