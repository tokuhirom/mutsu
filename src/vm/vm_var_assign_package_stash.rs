//! Keyed assignment into a named package's stash: `Foo::{'&bar'} = sub {...}`,
//! `Foo::<&bar> := &f`, and above all the re-export idiom
//!
//! ```raku
//! my package EXPORT::DEFAULT { }
//! BEGIN for <&tags &bash> { EXPORT::DEFAULT::{$_} = ::($_) }
//! ```
//!
//! (`Sparrow6::DSL` re-exports its helper subs this way). The generic
//! index-assign writes into the throwaway hash `GetPseudoStash` builds, so a
//! routine stored there never became callable as `Foo::bar` nor exported.
use super::*;

impl Interpreter {
    /// Store `val` under `key` in the stash of the package the constant
    /// `stash_name_idx` names (`Foo::`).
    ///
    /// A `&`-sigiled key installs the routine as that package's symbol -- the
    /// same slot `our &bar := ...` inside `package Foo` writes -- and, when
    /// the package is the loading module's `EXPORT::<tag>` stash, records it
    /// as one of that module's exports. Every other key keeps the generic
    /// index-assign it always had (`Foo::<$x> = 42` writes through the stash
    /// entry's container).
    // Cost: O(m + r), m = key bytes, r = registry entries scanned when re-aliasing a multi. Rakudo: O(1) -- see #9665.
    pub(super) fn index_assign_named_package_stash(
        &mut self,
        code: &CompiledCode,
        stash_name_idx: u32,
        key: Value,
        val: Value,
    ) -> Result<(), RuntimeError> {
        let raw_key = key.to_string_value();
        let stash_name = Self::const_str(code, stash_name_idx);
        let package = Self::normalize_stash_package(stash_name);
        let Some(bare) = raw_key.strip_prefix('&').filter(|_| !package.is_empty()) else {
            self.push_pseudo_stash_for_key(code, stash_name_idx, &key);
            self.stack.push(key);
            self.stack.push(val);
            return self.exec_index_assign_generic_op(code, false);
        };
        let (val, _) = Self::unwrap_bind_index_value(val);
        self.register_package_code_alias(package.clone(), bare, &val);
        self.publish_package_stash_symbol(package, "&", bare, &val);
        self.stack.push(val);
        Ok(())
    }
}
