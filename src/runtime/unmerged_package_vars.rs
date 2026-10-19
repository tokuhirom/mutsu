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
        let modules = self
            .module
            .module_visibility
            .package_granting_modules
            .get(top.as_str())?;
        if modules
            .iter()
            .any(|&module| self.module_merged_here(module))
        {
            return None;
        }
        let unsigiled = qualified(pkg, Symbol::intern(split.bare));
        if self.qualified_name_visible_here(unsigiled.as_str()) {
            return None;
        }
        let root = Symbol::intern(UNMERGED_ROOT);
        let bare = Symbol::intern(&[split.sigil, split.bare].concat());
        Some(qualified_var(qualified(root, pkg), bare))
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
        if self
            .module
            .module_visibility
            .package_granting_modules
            .is_empty()
            || !crate::qualified::is_qualified_str(key)
        {
            return None;
        }
        self.unmerged_package_var_key(Symbol::intern(key))
    }
}
