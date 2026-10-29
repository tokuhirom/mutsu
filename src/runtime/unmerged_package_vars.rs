//! A package variable named through a package that is not merged here
//! (#13942).
//!
//! A `use` inside a block merges the module's packages into that block only
//! (ADR-11136). Rakudo resolves `$V::x` outside the block through the
//! unit's own `GLOBAL`, which never merged `V`: the name is a fresh,
//! autovivified `GLOBAL::V::x`, not the module's `our $x`. Reading it gives
//! `(Any)`, and assigning it leaves the module's variable alone.
//!
//! mutsu keeps the module's package variables under their qualified env keys
//! (`V::x`, `@V::list`) for the whole run, because the BEGIN-time preload
//! loads a block's module at the head of the unit. So a qualified variable
//! access whose package is not visible here is sent to a separate store: the
//! same name under [`UNMERGED_ROOT`], a package segment no source can spell.
//!
//! A module's `my package P` is lexical to the module in the same way
//! (#11156): another compunit's `$P::x` names its own GLOBAL `P`, so an
//! access from outside the declaring compunit takes the separate store too.

use crate::qualified::{package_ancestors, qualified, qualified_var, split_qualified_var};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;

/// The root package of the store a qualified variable of an unmerged
/// package lives in. The `\u{1}` keeps it apart from every package a program
/// can name, and from the `GLOBAL::` fallbacks that strip a real `GLOBAL`.
const UNMERGED_ROOT: &str = "GLOBAL\u{1}";

impl Interpreter {
    /// The env key the qualified package variable `name` (`V::x`, `$V::x`,
    /// `@V::list`, `%V::h`) is accessed under here, when its package came
    /// from a module that is not merged into the running code; `None` when
    /// `name` is accessed under its own key.
    ///
    /// Only a package some loaded module granted is a candidate
    /// (`package_granting_modules`): the module is merged here when a
    /// block-level merge of it is live, the running unit merged it, or the
    /// running code is the module's own. Anything else the qualified gate
    /// grants (`qualified_name_visible_here`: the running package's own
    /// nesting, a setting package, a lexically bound head) keeps the key.
    // Cost: O(1) amortized for a name that is not qualified or whose top
    // package no module granted; otherwise O(m·d + q), m = modules granting
    // the top package, d = EVAL nesting of the anchor units (at most 64),
    // q = the qualified gate.
    pub(crate) fn unmerged_package_var_key(&self, name: Symbol) -> Option<Symbol> {
        let split = split_qualified_var(name)?;
        // `$GLOBAL::V::x` names the same unit-GLOBAL variable as `$V::x`.
        let pkg =
            crate::qualified::known_symbol(split.pkg.strip_prefix("GLOBAL::").unwrap_or(split.pkg));
        let top = package_ancestors(pkg).last()?;
        // A module's `my package` is lexical to the module (#11156). The
        // two the import protocol itself reads from the module (`EXPORT`,
        // `EXPORTHOW`) are read here on the importer's behalf.
        if let Some(&unit) = self
            .module
            .module_visibility
            .lexical_package_units
            .get(&top)
            && !matches!(top.as_str(), "EXPORT" | "EXPORTHOW")
        {
            if self.unit_is_running(unit) {
                return None;
            }
            return Some(Self::unmerged_store_key(pkg, split.sigil, split.bare));
        }
        let modules = self
            .module
            .module_visibility
            .package_granting_modules
            .get(top.as_str())?;
        if self.any_module_merged_here(modules) {
            return None;
        }
        let unsigiled = qualified(pkg, Symbol::intern(split.bare));
        if self.qualified_name_visible_here(unsigiled.as_str()) {
            return None;
        }
        Some(Self::unmerged_store_key(pkg, split.sigil, split.bare))
    }

    /// The separate store's key for the variable `<sigil><pkg>::<bare>`.
    // Cost: O(1) amortized (memoized name joins, one intern).
    fn unmerged_store_key(pkg: Symbol, sigil: &str, bare: &str) -> Symbol {
        let root = Symbol::intern(UNMERGED_ROOT);
        let bare = Symbol::intern(&[sigil, bare].concat());
        qualified_var(qualified(root, pkg), bare)
    }

    /// Record that the running compunit declared the `my package` `name`.
    // Cost: O(1) expected.
    pub(crate) fn record_lexical_package_unit(&mut self, name: Symbol) {
        let unit = self.executing_unit_sym_for_module_load();
        crate::runtime::cow_table_mut(&mut self.module.module_visibility.lexical_package_units)
            .insert(name, unit);
    }

    /// Whether the running code belongs to `unit` (or to an `EVAL` of it),
    /// by the same two anchors `module_merged_in_anchor_units` reads.
    // Cost: O(d), d = EVAL nesting of the anchor units (at most 64), twice.
    fn unit_is_running(&self, unit: Symbol) -> bool {
        [self.executing_unit_sym_for_module_load(), self.current_unit]
            .into_iter()
            .any(|anchor| {
                let mut at = Some(anchor);
                for _ in 0..64 {
                    let Some(sym) = at else { return false };
                    if sym == unit {
                        return true;
                    }
                    at = crate::runtime::eval_unit_parent(sym);
                }
                false
            })
    }

    /// The value of the unmerged-package variable `key`
    /// ([`Self::unmerged_package_var_key`]): `Any` until something stored
    /// into it.
    // Cost: O(d), d = env tiers walked, plus one package-store probe.
    pub(crate) fn unmerged_package_var(&self, key: Symbol) -> crate::value::Value {
        self.env()
            .get_sym(key)
            .cloned()
            .or_else(|| self.get_our_var(key.as_str()).cloned())
            .unwrap_or_else(|| crate::value::Value::package(Symbol::intern("Any")))
    }

    /// [`Self::unmerged_package_var_key`] for a package-store key, with the
    /// cheap exits first: no loaded module granted a package, or the key has
    /// no `::`.
    // Cost: O(1) when no module granted a package or `key` is unqualified;
    // otherwise one intern plus `unmerged_package_var_key`.
    #[inline]
    pub(crate) fn unmerged_our_var_key(&self, key: &str) -> Option<Symbol> {
        let visibility = &self.module.module_visibility;
        if (visibility.package_granting_modules.is_empty()
            && visibility.lexical_package_units.is_empty())
            || !crate::qualified::is_qualified_str(key)
        {
            return None;
        }
        self.unmerged_package_var_key(Symbol::intern(key))
    }
}
